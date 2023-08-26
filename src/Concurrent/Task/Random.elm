module Concurrent.Task.Random exposing (generate)

{-| Generate random values based on an [elm/random](https://package.elm-lang.org/packages/elm/random/latest/Random#Generator) `Generator`

This Task uses a builtin randomSeed task from the JavaScript runner (the [same seed `elm/random` uses](https://github.com/elm/random/blob/ecf97bb43f0d5cd75243428f69f45323957bda25/src/Random.elm#L873-L875) - `Date.now()`).

If needed you can override the randomSeed task like so (e.g. use node or web crypto module to generate secure randomness):

    Tasks.register({
      tasks: {},
      ports: app.ports,
      builtins: {
        randomSeed: () => crypto.getRandomValues(new Uint32Array(1))[0],
      },
    });

@docs generate

-}

import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode
import Json.Encode as Encode
import Random


{-| Generate a random value based on an [elm/random](https://package.elm-lang.org/packages/elm/random/latest/Random) `Generator`
-}
generate : Random.Generator a -> Task x a
generate generator =
    Task.map
        (Random.initialSeed
            >> Random.step generator
            >> Tuple.first
        )
        randomSeed


randomSeed : Task x Int
randomSeed =
    Task.define
        { function = "builtin:randomSeed"
        , expect = Task.expectJson Decode.int
        , errors = Task.catchAll 0
        , args = Encode.null
        }
