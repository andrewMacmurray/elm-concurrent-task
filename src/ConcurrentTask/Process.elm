module ConcurrentTask.Process exposing (sleep)

{-| A drop in replacement for [elm/core's](https://package.elm-lang.org/packages/elm/core/latest/Process#sleep) `Process.sleep`.

The JavaScript runner has this task builtin by default. If needed it can be overridden like so:

**NOTE:** The custom example is the same as the built-in implementation.

    import * as ConcurrentTask from "@andrewmacmurray/elm-concurrent-task"

    ConcurrentTask.register({
      tasks: {},
      ports: app.ports,
      builtins: {
        sleep: customSleep
      }
    });

    function customSleep(ms: number): Promise<void> {
      return new Promise((resolve) => setTimeout(resolve, ms));
    }

@docs sleep

-}

import ConcurrentTask exposing (ConcurrentTask)
import Json.Encode as Encode


{-| Wait for a number of milliseconds before continuing with the next Task.

A direct replacement for `elm/core`'s [`Process.sleep`](https://package.elm-lang.org/packages/elm/core/latest/Process#sleep).

-}
sleep : Int -> ConcurrentTask x ()
sleep ms =
    ConcurrentTask.define
        { function = "builtin:sleep"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectNoErrors
        , args = Encode.int ms
        }
