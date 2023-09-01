module ConcurrentTask.Process exposing (sleep)

{-| A drop in replacement for [elm/core's](https://package.elm-lang.org/packages/elm/core/latest/Process#sleep) `Process.sleep`

The JavaScript runner has this task builtin by default. If needed it can be overridden like so:

    Tasks.register({
      tasks: {},
      ports: app.ports,
      builtins: {
        sleep: (ms) => customSleep(ms)
      }
    });

@docs sleep

-}

import ConcurrentTask exposing (ConcurrentTask)
import Json.Encode as Encode


{-| Wait for a number of milliseconds before continuing with the next Task.
-}
sleep : Int -> ConcurrentTask x ()
sleep ms =
    ConcurrentTask.define
        { function = "builtin:sleep"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.catchAll ()
        , args = Encode.int ms
        }
