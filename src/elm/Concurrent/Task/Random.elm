module Concurrent.Task.Random exposing (generate)

import Concurrent.Task as Task exposing (Task)
import Concurrent.Task.Time
import Random
import Time



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
    Task.map Time.posixToMillis Concurrent.Task.Time.now
