module Utils.Test exposing (forEach)

import Expect exposing (Expectation)
import Fuzz
import Test exposing (Test, fuzz)


forEach : List item -> String -> (item -> Expectation) -> Test
forEach items =
    fuzz (Fuzz.oneOf (List.map Fuzz.constant items))
