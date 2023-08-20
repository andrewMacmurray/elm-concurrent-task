module Concurrent.Task exposing
    ( Task, define, Error
    , Expect, expectJson, expectString, expectWhatever
    , succeed, fail, andThen
    , fromResult, andThenDo, return
    , batch, sequence
    , map, andMap, map2, map3, map4, map5
    , mapError, onError, errorToString
    , attempt, pool, onProgress, Pool
    )

{-| A Task very similar to `elm/core`'s `Task` but:

  - Allows concurrent execution of `map2`, `map3`, ...
  - Can safely call external JavaScript and chain the results (also known as Task Ports).

TODO: add note about builtin tasks


# Tasks

A `Task` represents an asynchronous unit of work with the possibility of failure.

Underneath, each task represents a call to a JavaScript function and the runner handles batching and sequencing the calls.

@docs Task, define, Error


# Expectations

Decode the response of a JS function into an Elm value.

@docs Expect, expectJson, expectString, expectWhatever


# Chaining Tasks

@docs succeed, fail, andThen


# Convenience Helpers

These are some general helpers that can make chaining and combining tasks more convenient.

@docs fromResult, andThenDo, return


# Bulk Helpers

When you need to combine many tasks together.


## Stack Safety

These helpers are carefully written to be stack safe. Use them if you're handling large lists of tasks (> 2000).

@docs batch, sequence


# Maps

Transform values returned from tasks.

@docs map, andMap, map2, map3, map4, map5


# Errors

@docs mapError, onError, errorToString


# Run a Task

@docs attempt, pool, onProgress, Pool

-}

import Concurrent.Internal.Task as Internal
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Tasks


{-| -}
type alias Task x a =
    Internal.Task x a


{-| Define a `Task` from an external JavaScript function with:

  - The `name` of the registered function you want to call
  - The encoded `args` to pass to the function
  - What you `expect` to come back from the function when it returns

Say you wanted to interact with the node filesystem:

Define your task in `Elm`:

    import Concurrent.Task as Task exposing (Error, Task)
    import Json.Encode as Encode

    readFile : String -> Task Error String
    readFile path =
        Task.define
            { function = "fs:readFile"
            , args = Encode.object [ ( "path", Encode.string path ) ]
            , expect = Task.expectString
            }

And in your `JavaScript` runner:

    import * as fs from "node:fs/promises"
    import * as Tasks from "@andrewMacmurray/elm-concurrent-task";


    const app = Elm.Main.init({});

    Tasks.register({
      tasks: {
        "fs:readFile": (args) => fs.readFile(args.path),
      },
      ports: {
        send: app.ports.send,
        receive: app.ports.receive,
      },
    });

NOTE:

  - If the function names don't match between Elm and JavaScript the task will complete with an `Err (MissingFunction name)`
  - If the function returns a value that doesn't match `expect` the task will complete with an `Err (ResponseError error)`
  - If the function throws an exception the task will complete with an `Err (JsException error)`

-}
define :
    { function : String
    , args : Encode.Value
    , expect : Expect a
    }
    -> Task Error a
define =
    Internal.define


{-| A defined `Task` can error in the following ways:

1.  `ResponseError Decode.Error` - The `JavaScript` function returned a value but it was the wrong shape
2.  `MissingFunction String` - The function name was not found in the registered tasks (the `String` is the name of the function that was not found)
3.  `JsException String` - The `JavaScript` function threw an exception

A Task can also error with `UnknownError String` (represents an internal decode failure).
However this should not happen in practice - if it does, [please leave an issue](https://github.com/andrewMacmurray/elm-concurrent-task/issues).

-}
type alias Error =
    Internal.Error



-- Expectations


{-| -}
type alias Expect a =
    Internal.Expect a


{-| Run a JSON decoder on the response of a Task
-}
expectJson : Decoder a -> Expect a
expectJson =
    Internal.expectJson


{-| Expect the response of a Task to be a String
-}
expectString : Expect String
expectString =
    Internal.expectString


{-| Ignore the response of a Task
-}
expectWhatever : Expect ()
expectWhatever =
    Internal.expectWhatever



-- Chaining Tasks


{-| A Task that succeeds immediately when it's run.
-}
succeed : a -> Task x a
succeed =
    Internal.succeed


{-| A Task that fails immediately when it's run.
-}
fail : x -> Task x a
fail =
    Internal.fail


{-| Chain the successful result of the previous Task into another one.

Maybe you want to do a timestamped Http request

    import Concurrent.Task as Task exposing (Task)
    import Concurrent.Task.Http as Http
    import Concurrent.Task.Time
    import Time

    task : Task Http.Error String
    task =
        Concurrent.Task.Time.now
            |> Task.andThen (createArticle "my article")

    createArticle : String -> Time.Posix -> Task Http.Error String
    createArticle title time =
        Http.request
            { url = "http://blog.com/articles"
            , method = "POST"
            , headers = []
            , expect = Http.expectString
            , body = Http.jsonBody (encodeArticle title time)
            }

-}
andThen : (a -> Task x b) -> Task x a -> Task x b
andThen =
    Internal.andThen



-- Convenience Helpers


{-| Create a Task from a `Result error value`. The task will either immediately succeed or fail when run.

Maybe you want to chain together tasks with CSV parsing:

    import Concurrent.Task as Task exposing (Task)
    import Csv

    task : Task Error CsvData
    task =
        readFile |> Task.andThen parseCsv

    parseCsv : String -> Task Error CsvData
    parseCsv raw =
        Csv.decode decoder raw
            |> Task.fromResult
            |> Task.mapError CsvError

-}
fromResult : Result x a -> Task x a
fromResult =
    Internal.fromResult


{-| Similar to `andThen` but ignores the successful result of the previous Task.

Maybe you want to save a file then log a message to the console:

    import Concurrent.Task as Task exposing (Task)

    task : Task Error ()
    task =
        saveFile |> Task.andThenDo (log "file saved")

-}
andThenDo : Task x b -> Task x a -> Task x b
andThenDo =
    Internal.andThenDo


{-| Succeed with a hardcoded value after the previous Task

Maybe you want to do some Tasks on a User but allow it to be chained onwards:

    import Concurrent.Task as Task exposing (Task)

    saveUser : User -> Task Error User
    saveUser user =
        saveToDatabase user
            |> Task.andThenDo (log "user saved")
            |> Task.return user

-}
return : b -> Task x a -> Task x b
return =
    Internal.return



-- Bulk Helpers


{-| Perform a List of tasks concurrently (similar to `Promise.all()` in JavaScript) and return the results in a List.

If any of the subtasks fail the whole Task will fail.

-}
batch : List (Task x a) -> Task x (List a)
batch =
    Internal.batch


{-| Perform a List of tasks one after the other and return the results in a List.

If any of the subtasks fail the whole Task will fail.

-}
sequence : List (Task x a) -> Task x (List a)
sequence =
    Internal.sequence



-- Maps


{-| Transform the value from a task.

Maybe you want to find what time it is in one hour.

    import Concurrent.Task as Task
    import Concurrent.Task.Time
    import Time

    timeInOneHour : Task x Time.Posix
    timeInOneHour =
        Task.map addOneHour Concurrent.Task.Time.now

    addOneHour : Time.Posix -> Time.Posix
    addOneHour time =
        Time.millisToPosix (Time.posixToMillis time + 60 * 60 * 1000)

-}
map : (a -> b) -> Task x a -> Task x b
map =
    Internal.map


{-| Combine an arbitrary number of tasks together concurrently.

Maybe you want to load multiple pieces of config into a record:

    import Concurrent.Task as Task exposing (Task)

    type alias Config =
        { dbConfig : DbConfig
        , yamlConfig : YamlConfig
        , envFile : EnvFile
        }

     loadConfig : Task Error Config
     loadConfig =
        Task.succeed Config
            |> Task.andMap loadDbConfig
            |> Task.andMap loadYamlConfig
            |> Task.andMap loadEnvFile

-}
andMap : Task x a -> Task x (a -> b) -> Task x b
andMap =
    Internal.andMap


{-| Run two tasks concurrently and combine their results.

    import Concurrent.Task as Task exposing (Task)
    import Concurrent.Task.Time
    import Time

    loadUserAndTime : Task Error ( User, Time.Posix )
    loadUserAndTime =
        Task.map2 Tuple.pair loadUser Concurrent.Task.Time.now

-}
map2 : (a -> b -> c) -> Task x a -> Task x b -> Task x c
map2 =
    Internal.map2


{-| Run three tasks concurrently and combine their results.
-}
map3 :
    (a -> b -> c -> d)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
map3 =
    Internal.map3


{-| Run four tasks concurrently and combine their results.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
map4 =
    Internal.map4


{-| Run five tasks concurrently and combine their results.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
    -> Task x f
map5 =
    Internal.map5



-- Errors


{-| -}
mapError : (x -> y) -> Task x a -> Task y a
mapError =
    Internal.mapError


{-| -}
onError : (x -> Task y a) -> Task x a -> Task y a
onError =
    Internal.onError


{-| -}
errorToString : Error -> String
errorToString =
    Internal.errorToString



-- Run a Task


{-| -}
type alias Pool msg x a =
    Internal.Pool msg x a


{-| -}
attempt :
    { pool : Pool msg x a
    , send : Decode.Value -> Cmd msg
    , onComplete : Result x a -> msg
    }
    -> Task x a
    -> ( Pool msg x a, Cmd msg )
attempt =
    Internal.attempt


{-| -}
pool : Pool msg x a
pool =
    Internal.pool


{-| -}
onProgress :
    { send : Decode.Value -> Cmd msg
    , receive : (Decode.Value -> msg) -> Sub msg
    , onProgress : ( Pool msg x a, Cmd msg ) -> msg
    }
    -> Pool msg x a
    -> Sub msg
onProgress =
    Internal.onProgress
