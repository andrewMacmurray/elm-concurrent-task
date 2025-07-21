module ConcurrentTask.Process exposing (sleep, withTimeout)

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

@docs sleep, withTimeout

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


{-| Cancel a task, succeeding with the given value if the task takes longer than the given milliseconds.

    import ConcurrentTask exposing (ConcurrentTask)
    import ConcurrentTask.Http as Http
    import ConcurrentTask.Process

    type Fruits
        = Cached (List String)
        | Loaded (List String)

    loadFruits : ConcurrentTask Http.Error Fruits
    loadFruits =
        getFruits
            |> ConcurrentTask.withTimeout 2000 (Cached [ "Apple", "Orange", "Banana" ])

If `getFruits` takes longer than 2000ms the task will succeed with the `Cached` value.

-}
withTimeout : Int -> a -> ConcurrentTask x a -> ConcurrentTask x a
withTimeout ms a task =
    ConcurrentTask.race (sleep ms |> ConcurrentTask.return a) [ task ]
