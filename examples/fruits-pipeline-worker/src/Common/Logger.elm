module Common.Logger exposing
    ( debug
    , error
    , info
    , inspect
    , warn
    , withInfo
    )

import Concurrent.Task as Task exposing (Task)
import Json.Encode as Encode



-- Inspect


inspect : (x -> String) -> (a -> String) -> Task x a -> Task x a
inspect onError onOk task_ =
    task_
        |> Task.andThen
            (\a ->
                info (onOk a)
                    |> Task.return a
            )
        |> Task.onError
            (\e ->
                error (onError e)
                    |> Task.return e
                    |> Task.andThen (\_ -> Task.fail e)
            )



-- Debug


debug : String -> Task x ()
debug =
    log_ "DEBUG"



-- Info


info : String -> Task x ()
info =
    log_ "INFO"


withInfo : String -> Task x a -> Task x a
withInfo message task =
    info message |> Task.andThenDo task



-- Warn


warn : String -> Task x ()
warn =
    log_ "WARN"



-- Error


error : String -> Task x ()
error =
    log_ "ERROR"



-- Logger


log_ : String -> String -> Task x ()
log_ level message =
    Task.define
        { function = "console:log"
        , expect = Task.expectWhatever
        , errors = Task.catchAll ()
        , args =
            Encode.object
                [ ( "level", Encode.string level )
                , ( "message", Encode.string message )
                ]
        }
