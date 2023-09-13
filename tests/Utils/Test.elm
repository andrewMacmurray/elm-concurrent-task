module Utils.Test exposing (forEach)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


forEach : List item -> String -> (item -> Expectation) -> Test
forEach items description testCase =
    describe description
        (List.indexedMap
            (\index item ->
                test (description ++ ": " ++ String.fromInt index) <|
                    \_ -> testCase item
            )
            items
        )
