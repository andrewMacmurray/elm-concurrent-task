module Common.Logger exposing
    ( debug
    , error
    , info
    , inspect
    , warn
    , withInfo
    )

import ConcurrentTask exposing (ConcurrentTask)
import Json.Encode as Encode



-- Inspect


inspect : (x -> String) -> (a -> String) -> ConcurrentTask x a -> ConcurrentTask x a
inspect onError onOk task_ =
    task_
        |> ConcurrentTask.andThen
            (\a ->
                info (onOk a)
                    |> ConcurrentTask.return a
            )
        |> ConcurrentTask.onError
            (\e ->
                error (onError e)
                    |> ConcurrentTask.return e
                    |> ConcurrentTask.andThen (\_ -> ConcurrentTask.fail e)
            )



-- Debug


debug : String -> ConcurrentTask x ()
debug =
    log_ "DEBUG"



-- Info


info : String -> ConcurrentTask x ()
info =
    log_ "INFO"


withInfo : String -> ConcurrentTask x a -> ConcurrentTask x a
withInfo message task =
    info message |> ConcurrentTask.andThenDo task



-- Warn


warn : String -> ConcurrentTask x ()
warn =
    log_ "WARN"



-- Error


error : String -> ConcurrentTask x ()
error =
    log_ "ERROR"



-- Logger


log_ : String -> String -> ConcurrentTask x ()
log_ level message =
    ConcurrentTask.define
        { function = "console:log"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.catchAll ()
        , args =
            Encode.object
                [ ( "level", Encode.string level )
                , ( "message", Encode.string message )
                ]
        }
