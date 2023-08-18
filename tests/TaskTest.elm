module TaskTest exposing (suite)

import Concurrent.Internal.Ids as Ids exposing (Ids)
import Concurrent.Internal.Task as Task exposing (Task)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, string)
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "Tasks"
        [ hardcoded
        , responses
        , errors
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
                Task.map4 join4
                    (createTask |> Task.andThen withAnother)
                    (createTask |> Task.andThen withAnother)
                    (createTask |> Task.andThen withAnother)
                    (createTask |> Task.andThen withAnother)
                    |> runTask
                        [ ( 0, Encode.string "1" )
                        , ( 1, Encode.string "3" )
                        , ( 2, Encode.string "5" )
                        , ( 3, Encode.string "7" )
                        , ( 4, Encode.string "2" )
                        , ( 5, Encode.string "4" )
                        , ( 6, Encode.string "6" )
                        , ( 7, Encode.string "8" )
                        ]
                    |> Expect.equal (Ok "12345678")
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
                                    |> Task.andThen
                                        (\y ->
                                            withAnother y
                                        )
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
        , fuzz3 int string int "can handle mixed response types" <|
            \a b c ->
                Task.map3 (\x y z -> ( x, y, z ))
                    (create Decode.int)
                    (create Decode.string)
                    (create Decode.int)
                    |> runTask
                        [ ( 0, Encode.int a )
                        , ( 1, Encode.string b )
                        , ( 2, Encode.int c )
                        ]
                    |> Expect.equal (Ok ( a, b, c ))
        , fuzz (intRange 1 1000) "the id always increments in step with the number of tasks" <|
            \n ->
                List.repeat n createTask
                    |> Task.sequence
                    |> evalTask
                        (List.range 0 n
                            |> List.map
                                (\i ->
                                    ( i
                                    , success (Encode.string (String.fromInt i))
                                    )
                                )
                        )
                    |> Tuple.first
                    |> Ids.get
                    |> Expect.equal (String.fromInt n)
        , test "handles large sequences" <|
            \_ ->
                let
                    n =
                        100000
                in
                List.repeat n (create Decode.int)
                    |> Task.sequence
                    |> Task.map List.sum
                    |> runTask
                        (List.range 0 n
                            |> List.map
                                (\i ->
                                    ( i
                                    , Encode.int 1
                                    )
                                )
                        )
                    |> Expect.equal (Ok n)
        , test "handles large batches" <|
            \_ ->
                let
                    n =
                        5000
                in
                List.repeat n (create Decode.int)
                    |> Task.batch
                    |> Task.map List.sum
                    |> runTask
                        (List.range 0 n
                            |> List.map
                                (\i ->
                                    ( i
                                    , Encode.int 1
                                    )
                                )
                        )
                    |> Expect.equal (Ok n)
        ]


errors : Test
errors =
    describe "Tasks With Errors"
        [ test "tasks with JS Exceptions" <|
            \_ ->
                Task.map2 join2
                    createTask
                    createTask
                    |> runTaskWith
                        [ ( 0, success (Encode.string "a") )
                        , ( 1, error "js_exception" "f threw an exception" )
                        ]
                    |> Expect.equal (Err (Task.JsException "f threw an exception"))
        , test "tasks with missing functions" <|
            \_ ->
                Task.map3 join3
                    createTask
                    createTask
                    createTask
                    |> runTaskWith
                        [ ( 0, success (Encode.string "a") )
                        , ( 1, error "missing_function" "f is missing" )
                        , ( 2, success (Encode.string "c") )
                        ]
                    |> Expect.equal (Err (Task.MissingFunction "f is missing"))
        , test "task with an unknown error reason" <|
            \_ ->
                Task.map2 join2
                    createTask
                    createTask
                    |> runTaskWith
                        [ ( 0, success (Encode.string "a") )
                        , ( 1, error "other_error" "..." )
                        ]
                    |> Expect.equal (Err (Task.UnknownError "Unknown error reason: other_error"))
        ]


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
                    |> Task.mapError Task.UnknownError
                    |> runTask []
                    |> Expect.equal (Err (Task.UnknownError "hardcoded error"))
        , fuzz2 int int "tasks can recover from an error" <|
            \a b ->
                Task.succeed a
                    |> Task.andThen (\_ -> Task.fail "error")
                    |> Task.mapError Task.UnknownError
                    |> Task.onError (\_ -> Task.succeed b)
                    |> runTask []
                    |> Expect.equal (Ok b)
        ]



-- Task Runner


runTask : List ( Int, Encode.Value ) -> Task Task.Error a -> Result Task.Error a
runTask results =
    runTaskWith (List.map (Tuple.mapSecond success) results)


runTaskWith : List ( Int, Encode.Value ) -> Task Task.Error a -> Result Task.Error a
runTaskWith results task =
    Tuple.second (evalTask results task)


evalTask : List ( Int, Encode.Value ) -> Task Task.Error a -> ( Ids, Result Task.Error a )
evalTask results task =
    evalWith
        { maxDepth = 100000000
        , results = results
        , task = task
        , ids = Ids.init
        }


type alias Eval a =
    { maxDepth : Int
    , results : List ( Int, Encode.Value )
    , task : Task Task.Error a
    , ids : Ids
    }


evalWith : Eval a -> ( Ids, Result Task.Error a )
evalWith options =
    let
        results : Task.Results
        results =
            options.results
                |> List.head
                |> Maybe.withDefault ( -1, Encode.null )
                |> Tuple.mapFirst String.fromInt
                |> List.singleton
                |> Dict.fromList
    in
    case stepTask results ( options.ids, options.task ) of
        ( ids, Task.Done a ) ->
            ( ids, a )

        ( ids, Task.Pending _ next ) ->
            if options.maxDepth > 0 then
                evalWith
                    { options
                        | maxDepth = options.maxDepth - 1
                        , results = List.drop 1 options.results
                        , task = next
                        , ids = ids
                    }

            else
                ( ids, Err (Task.UnknownError "timeout") )


stepTask : Task.Results -> ( Ids, Task x a ) -> ( Ids, Task.Task_ x a )
stepTask res ( ids, Task.Task run ) =
    run res ids



-- Helpers


success : Encode.Value -> Encode.Value
success v =
    Encode.object
        [ ( "status", Encode.string "success" )
        , ( "value", v )
        ]


error : String -> String -> Encode.Value
error reason message =
    Encode.object
        [ ( "status", Encode.string "error" )
        , ( "error"
          , Encode.object
                [ ( "reason", Encode.string reason )
                , ( "message", Encode.string message )
                ]
          )
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


join4 : String -> String -> String -> String -> String
join4 a b c d =
    a ++ b ++ c ++ d


join3 : String -> String -> String -> String
join3 a b c =
    a ++ b ++ c


join2 : String -> String -> String
join2 a b =
    a ++ b



-- Runner
