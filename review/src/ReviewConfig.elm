module ReviewConfig exposing (config)

import NoUnoptimizedRecursion
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
        |> Review.Rule.ignoreErrorsForFiles [ "src/elm/Main.elm" ]
    ]
