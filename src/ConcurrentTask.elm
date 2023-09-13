module ConcurrentTask exposing
    ( ConcurrentTask, define
    , Expect, expectJson, expectString, expectWhatever
    , Errors, expectThrows, expectErrors, expectNoErrors
    , onResponseDecoderFailure, onJsException
    , mapError, onError
    , succeed, fail, andThen
    , fromResult, andThenDo, return
    , batch, sequence
    , map, andMap, map2, map3, map4, map5
    , attempt, Response(..), UnexpectedError(..), onProgress, Pool, pool
    )

{-| A Task similar to `elm/core`'s `Task` but:

  - Allows concurrent execution of `map2`, `map3`, ...
  - Can safely call external JavaScript and chain the results (also known as Task Ports).


## Built-in Tasks

Because `elm-concurrent-task` uses a different type to `elm/core` `Task` it's unfortunately not compatible with `elm/core` `Task`s.

However, there are a number of tasks built into the JavaScript runner and supporting modules that should cover a large amount of the existing functionality of `elm/core` `Task`s.

Check out the built-ins for more details:

  - [`Browser.Dom`](ConcurrentTask-Browser-Dom)
  - [`Http`](ConcurrentTask-Http)
  - [`Process`](ConcurrentTask-Process)
  - [`Random`](ConcurrentTask-Random)
  - [`Time`](ConcurrentTask-Time)


# Concurrent Tasks

@docs ConcurrentTask, define


# Expectations

@docs Expect, expectJson, expectString, expectWhatever


# Error Handling

@docs Errors, expectThrows, expectErrors, expectNoErrors


# Error Hooks

Lift `UnexpectedError`s into regular task flow.

@docs onResponseDecoderFailure, onJsException


# Transforming Errors

@docs mapError, onError


# Chaining Tasks

@docs succeed, fail, andThen


# Convenience Helpers

These are some general helpers that can make chaining and combining tasks more convenient.

@docs fromResult, andThenDo, return


# Batch Helpers

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

    getAllTitles : ConcurrentTask Http.Error Titles
    getAllTitles =
        Task.map3 Titles
            (getTitle "/todos/1")
            (getTitle "/posts/1")
            (getTitle "/albums/1")

    getTitle : String -> ConcurrentTask Http.Error String
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

    import ConcurrentTask exposing (ConcurrentTask)
    import ConcurrentTask.Http as Http
    import Json.Decode as Decode

    type alias Model =
        { tasks : ConcurrentTask.Pool Msg Http.Error Titles
        }

    type Msg
        = OnProgress ( ConcurrentTask.Pool Msg Http.Error Titles, Cmd Msg )
        | OnComplete (ConcurrentTask.Response Http.Error Titles)

    init : ( Model, Cmd Msg )
    init =
        let
            ( tasks, cmd ) =
                ConcurrentTask.attempt
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
        ConcurrentTask.onProgress
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

@docs attempt, Response, UnexpectedError, onProgress, Pool, pool

-}

import ConcurrentTask.Internal.ConcurrentTask as Internal
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Concurrent Tasks


{-| A `ConcurrentTask` represents an asynchronous unit of work with the possibility of failure.

Underneath, each task represents a call to a JavaScript function and the runner handles batching and sequencing the calls.

-}
type alias ConcurrentTask x a =
    Internal.ConcurrentTask x a


{-| Define a `ConcurrentTask` from an external JavaScript function with:

  - The `name` of the registered function you want to call.
  - What you `expect` to come back from the function when it returns.
  - How to interpret `errors` coming from the function (exceptions or explicitly returned errors).
  - The encoded `args` to pass to the function.

Say you wanted to interact with the node filesystem:

Define your task in `Elm`:

    import ConcurrentTask exposing (ConcurrentTask)
    import Json.Encode as Encode

    type Error
        = Error String

    readFile : String -> ConcurrentTask Error String
    readFile path =
        ConcurrentTask.define
            { function = "fs:readFile"
            , expect = ConcurrentTask.expectString
            , errors = ConcurrentTask.expectThrows Error
            , args = Encode.object [ ( "path", Encode.string path ) ]
            }

And in your `JavaScript` runner:

    import * as fs from "node:fs/promises"
    import * as ConcurrentTask from "@andrewMacmurray/elm-concurrent-task";


    const app = Elm.Main.init({});

    ConcurrentTask.register({
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
    , errors : Errors x
    , args : Encode.Value
    }
    -> ConcurrentTask x a
define =
    Internal.define



-- Expectations


{-| Decode the response of a JS function into an Elm value.
-}
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


{-| The `Errors` type provides different ways to capture errors for a `ConcurrentTask`.


## Understanding Errors

`ConcurrentTask` has two main kinds of `Errors`:


### [Error](ConcurrentTask#Response)

This is the `x` in the `ConcurrentTask x a` and represents an **expected error** as part of your task flow.
You can handle these with [mapError](ConcurrentTask#mapError) and [onError](ConcurrentTask#onError).


### [UnexpectedError](ConcurrentTask#UnexpectedError)

You can think of these as **unhandled** errors that are not a normal part of your task flow.

The idea behind `UnexpectedError` is to keep your task flow types `ConcurrentTask x a` clean and meaningful,
and optionally lift some of them into regular task flow where it makes sense.
Two hooks you can use for this are [onResponseDecoderFailure](ConcurrentTask#onResponseDecoderFailure) and [onJsException](ConcurrentTask#onJsException).

See the section on [UnexpectedError](ConcurrentTask#UnexpectedError)s for more details.

-}
type alias Errors x =
    Internal.Errors x


{-| The simplest Error handler. If a JS function throws an Exception, it will be wrapped in the provided `Error` type.

Maybe your JS function throws an `AccessError`:

    import ConcurrentTask exposing (ConcurrentTask)

    type Error
        = MyError String

    example : ConcurrentTask Error String
    example =
        ConcurrentTask.define
            { function = "functionThatThrows"
            , expect = ConcurrentTask.expectString
            , errors = ConcurrentTask.expectThrows MyError
            , args = Encode.null
            }

When the task is run it will complete with `Task.Error (MyError "AccessError: access denied")`.
This can be transformed and chained using `Task.mapError` and `Task.onError`.

**NOTE:**

This kind of error handling can be useful to get started quickly,
but it's often much more expressive and useful if you catch and explicitly return error data in your JS function that can be decoded with the `expectError` handler.

-}
expectThrows : (String -> x) -> Errors x
expectThrows =
    Internal.expectThrows


{-| Decode explicit errors returned by a `ConcurrentTask`. Use this when you want more meaningful errors in your task.

This will decode the value from an `error` key returned by a JS function, e.g.:

    return {
      error: {
        code: "MY_ERROR_CODE",
        message: "Something Went Wrong",
      }
    }

**IMPORTANT NOTES**:

  - If your function doesn't return an `"error"` key it will be interpreted as a success response.
  - If your JS function throws an exception it will surface an `UnhandledJsException` -
    make sure to either:
      - catch these in your JS function and return them as structured errors.
      - catch them with the [onJsException](ConcurrentTask#onJsException) hook.
  - If your error decoder fails the task will surface an `ExpectErrorFailure`.
      - This error is uncatchable, make sure to return data that matches your error decoder.

Maybe you want to handle different kinds of errors when writing to `localStorage`:

    import ConcurrentTask exposing (ConcurrentTask)
    import Json.Decode as Decode
    import Json.Encode as Encode

    type WriteError
        = QuotaExceeded
        | WriteBlocked

    set : String -> String -> Task WriteError ()
    set key value =
        ConcurrentTask.define
            { function = "storage:set"
            , expect = ConcurrentTask.expectWhatever
            , errors = ConcurrentTask.expectErrors decodeWriteError
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

    ConcurrentTask.register({
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
expectErrors : Decoder x -> Errors x
expectErrors =
    Internal.expectErrors


{-| Only use this handler for functions that you don't expect to fail.

**NOTE**:
If the decoder fails or the function throws an exception, these will be surfaced as `UnexpectedError`s.

e.g. logging to the console:

    import ConcurrentTask exposing (ConcurrentTask)

    log : String -> ConcurrentTask x ()
    log msg =
        ConcurrentTask.define
            { function = "console:log"
            , expect = ConcurrentTask.expectWhatever ()
            , errors = ConcurrentTask.expectNoErrors
            , args = Encode.string msg
            }

On the JS side:

    ConcurrentTask.register({
      tasks: {
        "console:log": (msg) => console.log(msg),
      },
      ports: {
        send: app.ports.send,
        receive: app.ports.receive,
      },
    });

-}
expectNoErrors : Errors x
expectNoErrors =
    Internal.expectNoErrors


{-| Use this alongside other error handlers to lift a `ResponseDecoderFailure`'s `Json.Decode` error into regular task flow.

Maybe you want to represent an unexpected response as a `BadBody` error for a http request:

    import ConcurrentTask exposing (ConcurrentTask)

    type Error
        = Timeout
        | NetworkError
        | BadStatus Int
        | BadUrl String
        | BadBody Decode.Error

    request : Request a -> ConcurrentTask Error a
    request options =
        ConcurrentTask.define
            { function = "http:request"
            , expect = ConcurrentTask.expectJson options.expect
            , errors = ConcurrentTask.expectErrors decodeHttpErrors
            , args = encodeArgs options
            }
            |> ConcurrentTask.onResponseDecoderFailure (BadBody >> Task.fail)

-}
onResponseDecoderFailure : (Decode.Error -> ConcurrentTask x a) -> ConcurrentTask x a -> ConcurrentTask x a
onResponseDecoderFailure =
    Internal.onResponseDecoderFailure


{-| Use this to capture a raw JsException to lift it into the task flow.

**NOTE**: Tasks defined with [expectThrows](ConcurrentTask#expectThrows) will never trigger this hook,
make sure to only use it with [expectErrors](ConcurrentTask#expectErrors) and [expectNoErrors](ConcurrentTask#expectNoErrors).

-}
onJsException :
    ({ message : String, raw : Decode.Value } -> ConcurrentTask x a)
    -> ConcurrentTask x a
    -> ConcurrentTask x a
onJsException =
    Internal.onJsException



-- Chaining Tasks


{-| A Task that succeeds immediately when it's run.
-}
succeed : a -> ConcurrentTask x a
succeed =
    Internal.succeed


{-| A Task that fails immediately when it's run.
-}
fail : x -> ConcurrentTask x a
fail =
    Internal.fail


{-| Chain the successful result of the previous Task into another one.

Maybe you want to do a timestamped Http request

    import ConcurrentTask exposing (ConcurrentTask)
    import ConcurrentTask.Http as Http
    import ConcurrentTask.Time
    import Time

    task : ConcurrentTask Http.Error String
    task =
        ConcurrentTask.Time.now
            |> Task.andThen (createArticle "my article")

    createArticle : String -> Time.Posix -> ConcurrentTask Http.Error String
    createArticle title time =
        Http.request
            { url = "http://blog.com/articles"
            , method = "POST"
            , headers = []
            , expect = Http.expectString
            , body = Http.jsonBody (encodeArticle title time)
            }

-}
andThen : (a -> ConcurrentTask x b) -> ConcurrentTask x a -> ConcurrentTask x b
andThen =
    Internal.andThen



-- Convenience Helpers


{-| Create a Task from a `Result error value`. The task will either immediately succeed or fail when run.

Maybe you want to chain together tasks with CSV parsing:

    import ConcurrentTask exposing (ConcurrentTask)
    import Csv

    task : ConcurrentTask Error CsvData
    task =
        readFile |> ConcurrentTask.andThen parseCsv

    parseCsv : String -> ConcurrentTask Error CsvData
    parseCsv raw =
        Csv.decode decoder raw
            |> ConcurrentTask.fromResult
            |> ConcurrentTask.mapError CsvError

-}
fromResult : Result x a -> ConcurrentTask x a
fromResult =
    Internal.fromResult


{-| Similar to `andThen` but ignores the successful result of the previous Task.

Maybe you want to save a file then log a message to the console:

    import ConcurrentTask exposing (ConcurrentTask)

    task : ConcurrentTask Error ()
    task =
        saveFile |> ConcurrentTask.andThenDo (log "file saved")

-}
andThenDo : ConcurrentTask x b -> ConcurrentTask x a -> ConcurrentTask x b
andThenDo =
    Internal.andThenDo


{-| Succeed with a hardcoded value after the previous Task

Maybe you want to do some Tasks on a User but allow it to be chained onwards:

    import ConcurrentTask exposing (ConcurrentTask)

    saveUser : User -> ConcurrentTask Error User
    saveUser user =
        saveToDatabase user
            |> ConcurrentTask.andThenDo (log "user saved")
            |> ConcurrentTask.return user

-}
return : b -> ConcurrentTask x a -> ConcurrentTask x b
return =
    Internal.return



-- Batch Helpers


{-| Perform a List of tasks concurrently (similar to `Promise.all()` in JavaScript) and return the results in a List.

If any of the subtasks fail the whole ConcurrentTask will fail.

-}
batch : List (ConcurrentTask x a) -> ConcurrentTask x (List a)
batch =
    Internal.batch


{-| Perform a List of tasks one after the other and return the results in a List.

If any of the subtasks fail the whole ConcurrentTask will fail.

-}
sequence : List (ConcurrentTask x a) -> ConcurrentTask x (List a)
sequence =
    Internal.sequence



-- Maps


{-| Transform the value from a task.

Maybe you want to find what time it is in one hour.

    import ConcurrentTask as exposing (ConcurrentTask)
    import ConcurrentTask.Time
    import Time

    timeInOneHour : ConcurrentTask x Time.Posix
    timeInOneHour =
        ConcurrentTask.map addOneHour ConcurrentTask.Time.now

    addOneHour : Time.Posix -> Time.Posix
    addOneHour time =
        Time.millisToPosix (Time.posixToMillis time + 60 * 60 * 1000)

-}
map : (a -> b) -> ConcurrentTask x a -> ConcurrentTask x b
map =
    Internal.map


{-| Combine an arbitrary number of tasks together concurrently.

Maybe you want to load multiple pieces of config into a record:

    import ConcurrentTask exposing (ConcurrentTask)

    type alias Config =
        { dbConfig : DbConfig
        , yamlConfig : YamlConfig
        , envFile : EnvFile
        }

     loadConfig : ConcurrentTask Error Config
     loadConfig =
        ConcurrentTask.succeed Config
            |> ConcurrentTask.andMap loadDbConfig
            |> ConcurrentTask.andMap loadYamlConfig
            |> ConcurrentTask.andMap loadEnvFile

-}
andMap : ConcurrentTask x a -> ConcurrentTask x (a -> b) -> ConcurrentTask x b
andMap =
    Internal.andMap


{-| Run two tasks concurrently and combine their results.

    import ConcurrentTask exposing (ConcurrentTask)
    import ConcurrentTask.Time
    import Time

    loadUserAndTime : ConcurrentTask Error ( User, Time.Posix )
    loadUserAndTime =
        ConcurrentTask.map2 Tuple.pair loadUser ConcurrentTask.Time.now

-}
map2 : (a -> b -> c) -> ConcurrentTask x a -> ConcurrentTask x b -> ConcurrentTask x c
map2 =
    Internal.map2


{-| Run three tasks concurrently and combine their results.
-}
map3 :
    (a -> b -> c -> d)
    -> ConcurrentTask x a
    -> ConcurrentTask x b
    -> ConcurrentTask x c
    -> ConcurrentTask x d
map3 =
    Internal.map3


{-| Run four tasks concurrently and combine their results.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> ConcurrentTask x a
    -> ConcurrentTask x b
    -> ConcurrentTask x c
    -> ConcurrentTask x d
    -> ConcurrentTask x e
map4 =
    Internal.map4


{-| Run five tasks concurrently and combine their results.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> ConcurrentTask x a
    -> ConcurrentTask x b
    -> ConcurrentTask x c
    -> ConcurrentTask x d
    -> ConcurrentTask x e
    -> ConcurrentTask x f
map5 =
    Internal.map5



-- Errors


{-| Transform the value of an Error (like `map` but for errors).
-}
mapError : (x -> y) -> ConcurrentTask x a -> ConcurrentTask y a
mapError =
    Internal.mapError


{-| If the previous Task fails, catch that error and return a new Task (like `andThen` but for errors).
-}
onError : (x -> ConcurrentTask y a) -> ConcurrentTask x a -> ConcurrentTask y a
onError =
    Internal.onError



-- Run a Task


{-| Start a `ConcurrentTask`. This needs:

  - A task `Pool` (The internal model to keep track of task progress).
  - The `send` port.
  - The `Msg` to be called when the task completes.
  - Your `ConcurrentTask` to be run.

Make sure to update your `Model` and pass in the `Cmd` returned from `attempt`. e.g. in a branch of `update`:

    let
        ( tasks, cmd ) =
            ConcurrentTask.attempt
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
    -> ConcurrentTask x a
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
  - `Error x` - the task failed with an expected error.
  - `UnexpectedError` - the task failed with an unexpected error (see the section on `Error Handling` for more details).

-}
type Response x a
    = Success a
    | Error x
    | UnexpectedError UnexpectedError


{-| This error will surface if something **unexpected** has happened during the task flow.


## Catchable Errors

These errors will be surfaced **if not handled** during task flow:

  - `UnhandledJsException` - a task threw an exception and was not caught with an error handler (can be caught with [expectThrows](ConcurrentTask#expectThrows) and [onJsException](ConcurrentTask#onJsException)).
  - `ResponseDecoderFailure` - a task returned an unexpected response (can be caught with [onResponseDecoderFailure](ConcurrentTask#onResponseDecoderFailure)).


## Uncatchable Errors

These errors will **always surface**, as they are assumed to have no meaningful way to recover from during regular task flow:

  - `ErrorsDecoderFailure` - a task returned error data in an unexpected format when using an [expectErrors](ConcurrentTask#expectErrors) handler.
  - `MissingFunction` - a task tried to call a function in the JS runner which was not registered.
  - `InternalError` - something went wrong with the runner internals - this should not happen, but if you see this error [please leave details and an issue](https://github.com/andrewMacmurray/elm-concurrent-task/issues/new).

-}
type UnexpectedError
    = UnhandledJsException { function : String, message : String, raw : Decode.Value }
    | ResponseDecoderFailure { function : String, error : Decode.Error }
    | ErrorsDecoderFailure { function : String, error : Decode.Error }
    | MissingFunction String
    | InternalError String


toResponse : Internal.Response x a -> Response x a
toResponse res =
    case res of
        Internal.Success a ->
            Success a

        Internal.Error x ->
            Error x

        Internal.UnexpectedError e ->
            UnexpectedError (toUnexpectedError e)


toUnexpectedError : Internal.UnexpectedError -> UnexpectedError
toUnexpectedError err =
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
        ConcurrentTask.onProgress
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


{-| A Pool keeps track of each task's progress,
and allows multiple Task attempts to be in-flight at the same time.
-}
type alias Pool msg x a =
    Internal.Pool msg x a


{-| Create an empty ConcurrentTask Pool.

Right now it doesn't expose any functionality, but it could be used in the future to do things like:

  - Buffer the number of in-flight tasks (e.g. a server request queue, or database connection pool).
  - Handle graceful process termination (e.g. abort or cleanup all in-flight tasks).
  - Expose metrics on previous or running tasks.

-}
pool : Pool msg x a
pool =
    Internal.pool
