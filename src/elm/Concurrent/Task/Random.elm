module Concurrent.Task.Random exposing (generate)

import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode
import Json.Encode as Encode
import Random



-- Random


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
    Task.task
        { function = "builtin:randomSeed"
        , args = Encode.null
        , expect = Decode.int
        }
        |> Task.onError (\_ -> Task.succeed 0)
