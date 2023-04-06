module Concurrent.Task.Time exposing (now)

import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode
import Json.Encode as Encode
import Time



-- Time Now


now : Task x Time.Posix
now =
    Task.ffi
        { function = "timeNow"
        , args = Encode.null
        , expect = Decode.map Time.millisToPosix Decode.int
        }
        |> Task.onError (\_ -> Task.succeed (Time.millisToPosix 0))
