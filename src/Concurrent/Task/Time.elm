module Concurrent.Task.Time exposing (now)

{-| A drop in replacement for `elm/time`'s `Time.now`

@docs now

-}

import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode
import Json.Encode as Encode
import Time


{-| -}
now : Task x Time.Posix
now =
    Task.define
        { function = "builtin:timeNow"
        , args = Encode.null
        , expect = Task.expectJson (Decode.map Time.millisToPosix Decode.int)
        }
        |> Task.onError (\_ -> Task.succeed (Time.millisToPosix 0))
