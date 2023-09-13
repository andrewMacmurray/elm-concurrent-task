module ConcurrentTask.Random exposing (generate)

{-| Generate random values based on an [elm/random](https://package.elm-lang.org/packages/elm/random/latest/Random#Generator) `Generator`.

This Task uses a builtin randomSeed task from the JavaScript runner (the [same seed `elm/random` uses](https://github.com/elm/random/blob/ecf97bb43f0d5cd75243428f69f45323957bda25/src/Random.elm#L873-L875) - `Date.now()`).

If needed you can override the randomSeed task like so (e.g. use node or web crypto module to generate secure randomness):

    import * as ConcurrentTask from "@andrewMacmurray/elm-concurrent-task"
    import crypto from "node:crypto"

    ConcurrentTask.register({
      tasks: {},
      ports: app.ports,
      builtins: {
        randomSeed: customRandomSeed,
      },
    });

    function customRandomSeed(): number {
      return crypto.getRandomValues(new Uint32Array(1))[0];
    }

@docs generate

-}

import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as Decode
import Json.Encode as Encode
import Random


{-| Generate a random value based on an [elm/random](https://package.elm-lang.org/packages/elm/random/latest/Random) `Generator`.
-}
generate : Random.Generator a -> ConcurrentTask x a
generate generator =
    ConcurrentTask.map
        (Random.initialSeed
            >> Random.step generator
            >> Tuple.first
        )
        randomSeed


randomSeed : ConcurrentTask x Int
randomSeed =
    ConcurrentTask.define
        { function = "builtin:randomSeed"
        , expect = ConcurrentTask.expectJson Decode.int
        , errors = ConcurrentTask.expectNoErrors
        , args = Encode.null
        }
