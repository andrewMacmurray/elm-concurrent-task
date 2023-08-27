module Concurrent.Task exposing
    ( Task, define
    , Expect, expectJson, expectString, expectWhatever
    , Errors, expectThrows, expectErrors, catchAll, onResponseDecoderFailure
    , mapError, onError
    , succeed, fail, andThen
    , fromResult, andThenDo, return
    , batch, sequence
    , map, andMap, map2, map3, map4, map5
    , attempt, Response(..), RunnerError(..), onProgress, Pool, pool
    )

{-| A Task similar to `elm/core`'s `Task` but:

  - Allows concurrent execution of `map2`, `map3`, ...
  - Can safely call external JavaScript and chain the results (also known as Task Ports).


## Built-in Tasks

Because `elm-concurrent-task` uses a different type to `elm/core` `Task` it is unfortunately not compatible with existing `elm/core` `Task`s.

However, there are a number of tasks built into the JavaScript runner and supporting modules that should cover a large amount of the existing functionality of `elm/core` `Task`s.

Check out the built-ins for more details:

  - [`Http.request`](Concurrent-Task-Http)
  - [`Process.sleep`](Concurrent-Task-Process)
  - [`Random.generate`](Concurrent-Task-Random)
  - [`Time.now`](Concurrent-Task-Time)


# Tasks

A `Task` represents an asynchronous unit of work with the possibility of failure.

Underneath, each task represents a call to a JavaScript function and the runner handles batching and sequencing the calls.

@docs Task, define


# Expectations

Decode the response of a JS function into an Elm value.

@docs Expect, expectJson, expectString, expectWhatever


# Error Handling

`Error` handlers provide different ways to capture errors for a `Task`.


## Understanding Errors

`Concurrent.Task` has two main kinds of `Errors`:


## TaskError

This is the `x` in the `Task x a` and represents an **expected error** as part of your task flow.
You can handle these with [mapError](Concurrent-Task#mapError) and [onError](Concurrent-Task#onError).


## RunnerError

You can think of these as **unhandled** errors that are not a normal part of your task flow.

The idea behind `RunnerError` is to keep your task flow types `Task x a` clean and meaningful,
and optionally lift some of them into your `TaskError` type where it makes sense

See the section on [RunnerError](Concurrent-Task#RunnerError)s for more details.


## Handling Runner Errors

Some of these can be captured as regular `TaskErrors` (The `x` in `Task x a`) using handlers:

  - `UnhandledJsException` - can be converted into a regular `TaskError` with [expectThrows](Concurrent-Task#expectThrows).
  - `UnhandledJsException` - can be converted into a `Success` with [catchAll](Concurrent-Task#catchAll).
  - `ResponseDecoderFailure` - can be lifted into regular task flow with [onResponseDecoderFailure](Concurrent-Task#onResponseDecoderFailure).


## Fatal Errors

Some `RunnerError`s cannot be caught, these are assumed to have no meaningful way to recover from:

  - `MissingFunction` will always be thrown if there is a mismatch between JS and Elm function names.
  - `ErrorsDecoderFailure` will always be thrown if a returned error didn't match a provided [expectErrors](Concurrent-Task#expectErrors) decoder.

@docs Errors, expectThrows, expectErrors, catchAll, onResponseDecoderFailure


# Transforming Errors

@docs mapError, onError


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


# Run a Task

Once you've constructed a Task it needs to be passed to the runner to perform all of the effects.

Here's a minimal complete example:


## A task to fetch 3 resources concurrently:

    type alias Titles =
        { todo : String
        , post : String
        , album : String
        }

    getAllTitles : Task Http.Error Titles
    getAllTitles =
        Task.map3 Titles
            (getTitle "/todos/1")
            (getTitle "/posts/1")
            (getTitle "/albums/1")

    getTitle : String -> Task Http.Error String
    getTitle path =
        Http.request
            { url = "https://jsonplaceholder.typicode.com" ++ path
            , method = "GET"
            , headers = []
            , body = Http.emptyBody
            , expect = Http.expectJson (Decode.field "title" Decode.string)
            }


## A program to run the task:

    port module Example exposing (main)

    import Concurrent.Task as Task exposing (Task)
    import Concurrent.Task.Http as Http
    import Json.Decode as Decode

    type alias Model =
        { tasks : Task.Pool Msg Http.Error Titles
        }

    type Msg
        = OnProgress ( Task.Pool Msg Http.Error Titles, Cmd Msg )
        | OnComplete (Task.Response Http.Error Titles)

    init : ( Model, Cmd Msg )
    init =
        let
            ( tasks, cmd ) =
                Task.attempt
                    { send = send
                    , pool = Task.pool
                    , onComplete = OnComplete
                    }
                    getAllTitles
        in
        ( { tasks = tasks }, cmd )

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            OnComplete response ->
                let
                    _ =
                        Debug.log "response" response
                in
                ( model, Cmd.none )

            OnProgress ( tasks, cmd ) ->
                ( { model | tasks = tasks }, cmd )

    subscriptions : Model -> Sub Msg
    subscriptions model =
        Task.onProgress
            { send = send
            , receive = receive
            , onProgress = OnProgress
            }
            model.tasks

    port send : Decode.Value -> Cmd msg

    port receive : (Decode.Value -> msg) -> Sub msg

    main : Program {} Model Msg
    main =
        Platform.worker
            { init = always init
            , update = update
            , subscriptions = subscriptions
            }

@docs attempt, Response, RunnerError, onProgress, Pool, pool

-}

import Concurrent.Internal.Task as Internal
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Tasks


{-| -}
type alias Task x a =
    Internal.Task x a


{-| Define a `Task` from an external JavaScript function with:

  - The `name` of the registered function you want to call.
  - What you `expect` to come back from the function when it returns.
  - How to interpret `errors` coming from the function (exceptions or explicitly returned errors).
  - The encoded `args` to pass to the function.

Say you wanted to interact with the node filesystem:

Define your task in `Elm`:

    import Concurrent.Task as Task exposing (Task)
    import Json.Encode as Encode

    type Error
        = Error String

    readFile : String -> Task Error String
    readFile path =
        Task.define
            { function = "fs:readFile"
            , expect = Task.expectString
            , errors = Task.expectThrows Error
            , args = Encode.object [ ( "path", Encode.string path ) ]
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


## A note on Errors:

The example `fs:readFile` Task has very simple error handling (turn any thrown exceptions into the Error type).
This can be a great way to start, but what if you want more detailed errors?

The `Errors` section goes into more detail on different error handling strategies, including:

  - how to define and handle more meaningful error types.
  - bypassing all errors for Tasks which never fail (e.g. get the current time, log to the console).
  - handling unexpected return data (e.g. the function returns an `Int` when you were expecting a `String`).

-}
define :
    { function : String
    , expect : Expect a
    , errors : Errors x a
    , args : Encode.Value
    }
    -> Task x a
define =
    Internal.define



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



-- Errors


{-| A handler passed to `Task.define`.
-}
type alias Errors x a =
    Internal.Errors x a


{-| The simplest Error handler. If a JS function throws an Exception, it will be wrapped in the provided `Error` type.

Maybe your JS function throws an `AccessError`:

    import Concurrent.Task as Task exposing (Task)

    type Error
        = MyError String

    example : Task Error String
    example =
        Task.define
            { function = "functionThatThrows"
            , expect = Task.expectString
            , errors = Task.expectThrows MyError
            , args = Encode.null
            }

When the task is run it will complete with `Task.Error (MyError "AccessError: access denied")`.
This can be transformed and chained using `Task.mapError` and `Task.onError`.


### Note:

This kind of error handling can be useful to get started quickly,
but it's often much more expressive and useful if you catch and explicitly return error data in your JS function that can be decoded with the `expectError` handler.

-}
expectThrows : (String -> x) -> Errors x a
expectThrows =
    Internal.expectThrows


{-| Decode explicit errors returned by a Task. Use this when you want more meaningful errors in your task.

This will decode the value from an `error` key returned by a JS function, e.g.:

    return {
      error: {
        code: "MY_ERROR_CODE",
        message: "Something Went Wrong",
      }
    }

**Important Notes**:

  - If your function doesn't return an `"error"` key it will be interpreted as a success response.
  - If your JS function throws an exception it will surface as a `RunnerError UnhandledJsException` -
    make sure to catch these in your JS function and return them as structured error responses.
  - If your error decoder fails the task will surface a `RunnerError ExpectErrorFailure`.

Maybe you want to handle different kinds of errors when writing to `localStorage`:

    import Concurrent.Task as Task exposing (Task)
    import Json.Decode as Decode
    import Json.Encode as Encode

    type WriteError
        = QuotaExceeded
        | WriteBlocked

    set : String -> String -> Task WriteError ()
    set key value =
        Task.define
            { function = "storage:set"
            , expect = Task.expectWhatever
            , errors = Task.expectErrors decodeWriteError
            , args =
                Encode.object
                    [ ( "key", Encode.string key )
                    , ( "value", Encode.string value )
                    ]
            }

    decodeWriteError : Decode.Decoder WriteError
    decodeWriteError =
        Decode.string
            |> Decode.andThen
                (\reason ->
                    case reason of
                        "QUOTA_EXCEEDED" ->
                            Decode.succeed QuotaExceeded

                        "WRITE_BLOCKED" ->
                            Decode.succeed WriteBlocked

                        _ ->
                            Decode.fail ("Unknown WriteError Reason: " ++ reason)
                )

And on the JS side:

    Tasks.register({
      tasks: {
        "storage:set": (args) => setItem(args),
      },
      ports: {
        send: app.ports.send,
        receive: app.ports.receive,
      },
    });


    function setItem(args) {
      try {
        localStorage.setItem(args.key, args.value);
      } catch (e) {
        if (e.name === "QuotaExceededError") {
          return {
            error: "QUOTA_EXCEEDED",
          };
        } else {
          return {
            error: "WRITE_BLOCKED",
          };
        }
      }
    }

-}
expectErrors : Decoder x -> Errors x a
expectErrors =
    Internal.expectErrors


{-| Using this handler transforms any `JS Exceptions` or `ResponseDecoderFailures` into a `Success` with the provided fallback.

Only use this handler for functions that can't fail.

e.g. logging to the console:

    import Concurrent.Task as Task exposing (Task)

    log : String -> Task x ()
    log msg =
        Task.define
            { function = "console:log"
            , expect = Task.expectWhatever ()
            , errors = Task.catchAll ()
            , args = Encode.string msg
            }

On the JS side:

    Tasks.register({
      tasks: {
        "console:log": (msg) => console.log(msg),
      },
      ports: {
        send: app.ports.send,
        receive: app.ports.receive,
      },
    });

-}
catchAll : a -> Errors x a
catchAll =
    Internal.catchAll


{-| Use this alongside other error handlers to lift a `ResponseDecoderFailure`'s `Json.Decode` error into regular task flow.

Maybe you want to represent an unexpected response as a `BadBody` error for a http request:

    import Concurrent.Task as Task

    type Error
        = Timeout
        | NetworkError
        | BadStatus Int
        | BadUrl String
        | BadBody Decode.Error

    request : Request a -> Task Error a
    request options =
        Task.define
            { function = "http:request"
            , expect = Task.expectJson options.expect
            , errors = Task.expectErrors decodeHttpErrors
            , args = encodeArgs options
            }
            |> Task.onResponseDecoderFailure (BadBody >> Task.fail)

-}
onResponseDecoderFailure : (Decode.Error -> Task x a) -> Task x a -> Task x a
onResponseDecoderFailure =
    Internal.onResponseDecoderFailure



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


{-| Transform the value of an Error (like `map` but for errors).
-}
mapError : (x -> y) -> Task x a -> Task y a
mapError =
    Internal.mapError


{-| If the previous Task fails, catch that error and return a new Task (like `andThen` but for errors).
-}
onError : (x -> Task y a) -> Task x a -> Task y a
onError =
    Internal.onError



-- Run a Task


{-| Start a Task.

This needs:

  - A task `Pool` (The internal model to keep track of task progress).
  - The `send` port.
  - The `Msg` to be called when the task completes.
  - Your `Task` to be run.

Make sure to update your `Model` and pass in the `Cmd` returned from `attempt`. e.g. in a branch of `update`:

    let
        ( tasks, cmd ) =
            Task.attempt
                { send = send
                , pool = model.pool
                , onComplete = OnComplete
                }
                myTask
    in
    ( { model | tasks = tasks }, cmd )

-}
attempt :
    { pool : Pool msg x a
    , send : Decode.Value -> Cmd msg
    , onComplete : Response x a -> msg
    }
    -> Task x a
    -> ( Pool msg x a, Cmd msg )
attempt options =
    Internal.attempt
        { pool = options.pool
        , send = options.send
        , onComplete = toResponse >> options.onComplete
        }


{-| The value returned from a task when it completes (returned in the `OnComplete` msg).

Can be either:

  - `Success a` - the task succeeded with no errors, woo!
  - `TaskError x` - the task failed with an expected error.
  - `RunnerError` - the task failed with an unexpected error (see the section on `Error Handling` for more details).

-}
type Response x a
    = Success a
    | TaskError x
    | RunnerError RunnerError


{-| An error returned from the runner if something **unexpected** has happened during the task flow.

These errors will be returned **if not handled** during task flow:

  - `UnhandledJsException` - a task threw an exception and was not caught with an error handler (can be caught with `expectThrows` and `catchAll`).
  - `ResponseDecoderFailure` - a task returned an unexpected response (can be caught with `onResponseDecoderFailure`).

These errors will **always surface**, as they are assumed to have no meaningful way to recover from during regular task flow:

  - `ErrorsDecoderFailure` - a task returned error data in an unexpected format.
  - `MissingFunction` - a task tried to call a function in the JS runner which was not registered.
  - `InternalError` - something went wrong with the runner internals - this should not happen, but if you see this error [please leave details and an issue](https://github.com/andrewMacmurray/elm-concurrent-task/issues/new).

-}
type RunnerError
    = UnhandledJsException { function : String, message : String }
    | ResponseDecoderFailure { function : String, error : Decode.Error }
    | ErrorsDecoderFailure { function : String, error : Decode.Error }
    | MissingFunction String
    | InternalError String


toResponse : Internal.Response x a -> Response x a
toResponse res =
    case res of
        Internal.Success a ->
            Success a

        Internal.TaskError x ->
            TaskError x

        Internal.RunnerError e ->
            RunnerError (toRunnerError e)


toRunnerError : Internal.RunnerError -> RunnerError
toRunnerError err =
    case err of
        Internal.UnhandledJsException e ->
            UnhandledJsException e

        Internal.ResponseDecoderFailure e ->
            ResponseDecoderFailure e

        Internal.ErrorsDecoderFailure e ->
            ErrorsDecoderFailure e

        Internal.MissingFunction e ->
            MissingFunction e

        Internal.InternalError e ->
            InternalError e


{-| Subscribe to updates from the JavaScript task runner.

This needs:

  - The `send` port.
  - The `receive` port.
  - The `Msg` to be called with the updated progress.
  - The task `Pool` stored in your model.

You can wire this in like so:

    subscriptions : Model -> Sub Msg
    subscriptions model =
        Task.onProgress
            { send = send
            , receive = receive
            , onProgress = OnProgress
            }
            model.tasks

Make sure to update your `Model` and pass in the `Cmd` in your `OnProgress` branch in `update`:

    OnProgress ( tasks, cmd ) ->
        ( { model | tasks = tasks }, cmd )

-}
onProgress :
    { send : Decode.Value -> Cmd msg
    , receive : (Decode.Value -> msg) -> Sub msg
    , onProgress : ( Pool msg x a, Cmd msg ) -> msg
    }
    -> Pool msg x a
    -> Sub msg
onProgress =
    Internal.onProgress


{-| -}
type alias Pool msg x a =
    Internal.Pool msg x a


{-| Create an empty Task Pool.

This is used to keep track of each Task's progress.

Right now it doesn't expose any functionality, but it could be used in the future to do things like:

  - Buffer the number of in-flight tasks (e.g. a server request queue, or database connection pool).
  - Handle graceful process termination (e.g. abort or cleanup all in-flight tasks).
  - Expose metrics on previous or running tasks.

-}
pool : Pool msg x a
pool =
    Internal.pool
