module Common.Logger exposing
    ( debug
    , error
    , info
    , inspect
    , warn
    , withError
    , withInfo
    , withWarn
    )

import Concurrent.Task as Task exposing (Task)
import Json.Encode as Encode



-- Inspect


inspect : (x -> String) -> (a -> String) -> Task x a -> Task x a
inspect onError onOk task_ =
    task_
        |> Task.andThen (\a -> withInfo (onOk a) a)
        |> Task.onError (\e -> withError (onError e) e |> Task.andThen (\_ -> Task.fail e))



-- Debug


debug : String -> Task x ()
debug =
    log_ "DEBUG"



-- Info


info : String -> Task x ()
info =
    log_ "INFO"


withInfo : String -> a -> Task x a
withInfo message a =
    info message |> Task.return a



-- Warn


warn : String -> Task x ()
warn =
    log_ "WARN"


withWarn : String -> a -> Task x a
withWarn message a =
    warn message |> Task.return a



-- Error


error : String -> Task x ()
error =
    log_ "ERROR"


withError : String -> a -> Task x a
withError message a =
    error message |> Task.return a



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
