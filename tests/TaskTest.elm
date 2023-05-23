module TaskTest exposing (suite)

import Concurrent.Internal.Id as Id
import Concurrent.Task as Task exposing (Task)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, string)
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "Tasks"
        [ hardcoded
        , responses
        ]


responses : Test
responses =
    describe "Tasks with Responses"
        [ fuzz3 string string string "successful responses combine" <|
            \a b c ->
                Task.map3 join3
                    createTask
                    createTask
                    createTask
                    |> runTask
                        [ ( 0, Encode.string a )
                        , ( 1, Encode.string b )
                        , ( 2, Encode.string c )
                        ]
                    |> Expect.equal (Ok (join3 a b c))
        , fuzz3 string string string "successful responses chain" <|
            \a b c ->
                Task.map2 (++)
                    createTask
                    createTask
                    |> Task.andThen
                        (\ab ->
                            Task.map2 (join3 ab)
                                createTask
                                createTask
                        )
                    |> runTask
                        [ ( 0, Encode.string a )
                        , ( 1, Encode.string b )
                        , ( 2, Encode.string c )
                        , ( 3, Encode.string c )
                        ]
                    |> Expect.equal (Ok (a ++ b ++ c ++ c))
        , test "responses can arrive out of order" <|
            \_ ->
                Task.map2 join2
                    createTask
                    createTask
                    |> Task.andThen
                        (\ab ->
                            Task.map2 (join3 ab)
                                createTask
                                createTask
                        )
                    |> runTask
                        [ ( 1, Encode.string "b" )
                        , ( 0, Encode.string "a" )
                        , ( 2, Encode.string "c" )
                        , ( 3, Encode.string "d" )
                        ]
                    |> Expect.equal (Ok "abcd")
        , test "can handle nested chains" <|
            \_ ->
                Task.map3 join3
                    (createTask |> Task.andThen withAnother)
                    (createTask |> Task.andThen withAnother)
                    (createTask |> Task.andThen withAnother)
                    |> runTask
                        [ ( 0, Encode.string "1" )
                        , ( 1, Encode.string "3" )
                        , ( 2, Encode.string "5" )
                        , ( 3, Encode.string "2" )
                        , ( 4, Encode.string "4" )
                        , ( 5, Encode.string "6" )
                        ]
                    |> Expect.equal (Ok "123456")
        , test "can handle deeply nested chains" <|
            \_ ->
                Task.map2 join2
                    (createTask
                        |> Task.andThen
                            (\x ->
                                withAnother x
                                    |> Task.andThen
                                        (\y ->
                                            withAnother y
                                                |> Task.andThen
                                                    (\z ->
                                                        withAnother z
                                                    )
                                        )
                            )
                    )
                    (createTask
                        |> Task.andThen
                            (\x ->
                                withAnother x
                                    |> Task.andThen (\y -> withAnother y)
                            )
                    )
                    |> Task.andThen withAnother
                    |> runTask
                        [ ( 0, Encode.string "0" )
                        , ( 1, Encode.string "4" )
                        , ( 2, Encode.string "1" )
                        , ( 3, Encode.string "5" )
                        , ( 4, Encode.string "2" )
                        , ( 5, Encode.string "6" )
                        , ( 6, Encode.string "3" )
                        , ( 7, Encode.string "7" )
                        ]
                    |> Expect.equal (Ok "01234567")
        , fuzz2 int string "can handle mixed response types" <|
            \a b ->
                Task.map2 Tuple.pair
                    (create Decode.int)
                    (create Decode.string)
                    |> runTask
                        [ ( 0, Encode.int a )
                        , ( 1, Encode.string b )
                        ]
                    |> Expect.equal (Ok ( a, b ))
        ]


withAnother : String -> Task Task.Error String
withAnother x =
    Task.map (join2 x) createTask


createTask : Task Task.Error String
createTask =
    create Decode.string


create : Decode.Decoder a -> Task Task.Error a
create decoder =
    Task.define
        { function = "aTask"
        , args = Encode.null
        , expect = Task.expectJson decoder
        }


hardcoded : Test
hardcoded =
    describe "Hardcoded Tasks"
        [ fuzz3 string string string "tasks can combine" <|
            \a b c ->
                Task.map3 join3
                    (Task.succeed a)
                    (Task.succeed b)
                    (Task.succeed c)
                    |> runTask []
                    |> Expect.equal (Ok (join3 a b c))
        , fuzz3 string string string "tasks can chain" <|
            \a b c ->
                Task.map2 (++)
                    (Task.succeed a)
                    (Task.succeed b)
                    |> Task.andThen (\ab -> Task.map (join2 ab) (Task.succeed c))
                    |> runTask []
                    |> Expect.equal (Ok (a ++ b ++ c))
        , test "tasks can short circuit" <|
            \_ ->
                Task.succeed 1
                    |> Task.andThenDo (Task.succeed 2)
                    |> Task.andThen (\_ -> Task.fail "hardcoded error")
                    |> Task.mapError Task.InternalError
                    |> runTask []
                    |> Expect.equal (Err (Task.InternalError "hardcoded error"))
        , fuzz2 int int "tasks can recover from an error" <|
            \a b ->
                Task.succeed a
                    |> Task.andThen (\_ -> Task.fail "error")
                    |> Task.mapError Task.InternalError
                    |> Task.onError (\_ -> Task.succeed b)
                    |> runTask []
                    |> Expect.equal (Ok b)
        ]


join3 : String -> String -> String -> String
join3 a b c =
    a ++ b ++ c


join2 : String -> String -> String
join2 a b =
    a ++ b


runTask : List ( Int, Encode.Value ) -> Task Task.Error a -> Result Task.Error a
runTask results task =
    Task.testEval
        { maxDepth = 100
        , results = List.map (Tuple.mapSecond toResponse) results
        , task = task
        , ids = Id.init
        }
        |> Tuple.second


toResponse : Encode.Value -> Encode.Value
toResponse v =
    Encode.object
        [ ( "status", Encode.string "success" )
        , ( "value", v )
        ]
