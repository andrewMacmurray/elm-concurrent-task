module Concurrent.Task.Process exposing (sleep)

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

import Concurrent.Task as Task exposing (Task)
import Json.Encode as Encode


{-| Wait for a number of milliseconds before continuing with the next Task.
-}
sleep : Int -> Task x ()
sleep ms =
    Task.define
        { function = "builtin:sleep"
        , expect = Task.expectWhatever
        , errors = Task.catchAll ()
        , args = Encode.int ms
        }
