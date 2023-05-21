module TaskTest exposing (suite)

import Concurrent.Internal.Id as Id
import Concurrent.Task as Task exposing (Task)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, string)
import Test exposing (..)


suite : Test
suite =
    describe "Hardcoded Tasks"
        [ fuzz3 int int int "tasks can combine" <|
            \a b c ->
                Task.map3 (\a_ b_ c_ -> a_ + b_ + c_)
                    (Task.succeed a)
                    (Task.succeed b)
                    (Task.succeed c)
                    |> runTask
                    |> Expect.equal (Ok (a + b + c))
        , fuzz3 string string string "tasks can chain" <|
            \a b c ->
                Task.map2 (++)
                    (Task.succeed a)
                    (Task.succeed b)
                    |> Task.andThen
                        (\ab ->
                            Task.map (\c_ -> ab ++ c_)
                                (Task.succeed c)
                        )
                    |> runTask
                    |> Expect.equal (Ok (a ++ b ++ c))
        , test "tasks can short circuit" <|
            \_ ->
                Task.succeed 1
                    |> Task.andThenDo (Task.succeed 2)
                    |> Task.andThen (\_ -> Task.fail "hardcoded error")
                    |> Task.mapError Task.InternalError
                    |> runTask
                    |> Expect.equal (Err (Task.InternalError "hardcoded error"))
        , fuzz2 int int "tasks can recover from an error" <|
            \a b ->
                Task.succeed a
                    |> Task.andThen (\_ -> Task.fail "error")
                    |> Task.mapError Task.InternalError
                    |> Task.onError (\_ -> Task.succeed b)
                    |> runTask
                    |> Expect.equal (Ok b)
        ]


runTask : Task Task.Error a -> Result Task.Error a
runTask task =
    Task.testEval
        { maxDepth = 100
        , results = []
        , task = task
        , ids = Id.init
        }
        |> Tuple.second
