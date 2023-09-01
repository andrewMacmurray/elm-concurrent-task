module ConcurrentTask.Time exposing (now)

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

import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as Decode
import Json.Encode as Encode
import Time


{-| Get the POSIX time at the moment when this task is run.
-}
now : ConcurrentTask x Time.Posix
now =
    ConcurrentTask.define
        { function = "builtin:timeNow"
        , expect = ConcurrentTask.expectJson (Decode.map Time.millisToPosix Decode.int)
        , errors = ConcurrentTask.catchAll (Time.millisToPosix 0)
        , args = Encode.null
        }
