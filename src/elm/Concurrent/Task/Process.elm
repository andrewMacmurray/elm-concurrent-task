module Concurrent.Task.Process exposing (sleep)

import Concurrent.Task as Task exposing (Task)
import Json.Encode as Encode


sleep : Int -> Task x ()
sleep ms =
    Task.define
        { function = "builtin:sleep"
        , args = Encode.int ms
        , expect = Task.expectWhatever
        }
        |> Task.onError (\_ -> Task.succeed ())
