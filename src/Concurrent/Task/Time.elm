module Concurrent.Task.Time exposing (now)

{-| A drop in replacement for [elm/time's](https://package.elm-lang.org/packages/elm/time/latest/Time#now) `Time.now`

The JavaScript runner has this task builtin by default. If needed it can be overridden like so:

    Tasks.register({
      tasks: {},
      ports: app.ports,
      builtins: {
        timeNow: () => customTimeNow(),
      }
    });

@docs now

-}

import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode
import Json.Encode as Encode
import Time


{-| Get the POSIX time at the moment when this task is run.
-}
now : Task x Time.Posix
now =
    Task.define
        { function = "builtin:timeNow"
        , args = Encode.null
        , expect = Task.expectJson (Decode.map Time.millisToPosix Decode.int)
        }
        |> Task.onError (\_ -> Task.succeed (Time.millisToPosix 0))
