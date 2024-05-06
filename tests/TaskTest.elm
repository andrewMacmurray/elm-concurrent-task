module TaskTest exposing (suite)

import ConcurrentTask
import ConcurrentTask.Internal as Internal exposing (ConcurrentTask)
import ConcurrentTask.Internal.Ids as Ids exposing (Ids)
import Dict
import Expect
import Fuzz exposing (int, intRange, string)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)
import Utils.Expect as Expect
import Utils.Test as Test



-- Task Tests


suite : Test
suite =
    describe "Tasks"
        [ hardcoded
        , successResponses
        , errors
        , helpers
        ]



-- Success Responses


successResponses : Test
successResponses =
    describe "Tasks with Success Responses"
        [ fuzz3 string string string "successful responses combine" <|
            \a b c ->
                ConcurrentTask.map3 join3
                    createTask
                    createTask
                    createTask
                    |> runTask
                        [ ( 0, Encode.string a )
                        , ( 1, Encode.string b )
                        , ( 2, Encode.string c )
                        ]
                    |> Expect.equal (Internal.Success (join3 a b c))
        , fuzz3 string string string "successful responses chain" <|
            \a b c ->
                ConcurrentTask.map2 (++)
                    createTask
                    createTask
                    |> ConcurrentTask.andThen
                        (\ab ->
                            ConcurrentTask.map2 (join3 ab)
                                createTask
                                createTask
                        )
                    |> runTask
                        [ ( 0, Encode.string a )
                        , ( 1, Encode.string b )
                        , ( 2, Encode.string c )
                        , ( 3, Encode.string c )
                        ]
                    |> Expect.equal (Internal.Success (a ++ b ++ c ++ c))
        , test "responses can arrive out of order" <|
            \_ ->
                ConcurrentTask.map2 join2
                    createTask
                    createTask
                    |> ConcurrentTask.andThen
                        (\ab ->
                            ConcurrentTask.map2 (join3 ab)
                                createTask
                                createTask
                        )
                    |> runTask
                        [ ( 1, Encode.string "b" )
                        , ( 0, Encode.string "a" )
                        , ( 2, Encode.string "c" )
                        , ( 3, Encode.string "d" )
                        ]
                    |> Expect.equal (Internal.Success "abcd")
        , test "can handle nested chains" <|
            \_ ->
                ConcurrentTask.map4 join4
                    (createTask |> ConcurrentTask.andThen withAnother)
                    (createTask |> ConcurrentTask.andThen withAnother)
                    (createTask |> ConcurrentTask.andThen withAnother)
                    (createTask |> ConcurrentTask.andThen withAnother)
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
                    |> Expect.equal (Internal.Success "12345678")
        , test "can handle deeply nested chains" <|
            \_ ->
                ConcurrentTask.map2 join2
                    (createTask
                        |> ConcurrentTask.andThen
                            (\x ->
                                withAnother x
                                    |> ConcurrentTask.andThen
                                        (\y ->
                                            withAnother y
                                                |> ConcurrentTask.andThen
                                                    (\z ->
                                                        withAnother z
                                                    )
                                        )
                            )
                    )
                    (createTask
                        |> ConcurrentTask.andThen
                            (\x ->
                                withAnother x
                                    |> ConcurrentTask.andThen
                                        (\y ->
                                            withAnother y
                                        )
                            )
                    )
                    |> ConcurrentTask.andThen withAnother
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
                    |> Expect.equal (Internal.Success "01234567")
        , fuzz3 int string int "can handle mixed response types" <|
            \a b c ->
                ConcurrentTask.map3 (\x y z -> ( x, y, z ))
                    (create Decode.int)
                    (create Decode.string)
                    (create Decode.int)
                    |> runTask
                        [ ( 0, Encode.int a )
                        , ( 1, Encode.string b )
                        , ( 2, Encode.int c )
                        ]
                    |> Expect.equal (Internal.Success ( a, b, c ))
        , fuzz (intRange 1 1000) "the id always increments in step with the number of tasks" <|
            \n ->
                List.repeat n createTask
                    |> ConcurrentTask.sequence
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
                    n : Int
                    n =
                        100000
                in
                List.repeat n (create Decode.int)
                    |> ConcurrentTask.sequence
                    |> ConcurrentTask.map List.sum
                    |> runTask
                        (List.range 0 n
                            |> List.map
                                (\i ->
                                    ( i
                                    , Encode.int 1
                                    )
                                )
                        )
                    |> Expect.equal (Internal.Success n)
        , test "handles large batches" <|
            \_ ->
                let
                    -- For some reason in elm-test large batches of Task.batch are very slow.
                    -- In a real program they are much faster.
                    n : Int
                    n =
                        1000
                in
                List.repeat n (create Decode.int)
                    |> ConcurrentTask.batch
                    |> ConcurrentTask.map List.sum
                    |> runTask
                        (List.range 0 n
                            |> List.map
                                (\i ->
                                    ( i
                                    , Encode.int 1
                                    )
                                )
                        )
                    |> Expect.equal (Internal.Success n)
        ]



-- Task with Errors


errors : Test
errors =
    describe "Task Errors"
        (let
            customErrorTask : ConcurrentTask Error String
            customErrorTask =
                ConcurrentTask.define
                    { function = "custom"
                    , expect = ConcurrentTask.expectString
                    , errors = ConcurrentTask.expectErrors (Decode.field "error" Decode.string)
                    , args = Encode.null
                    }

            catchTask : ConcurrentTask Error String
            catchTask =
                ConcurrentTask.define
                    { function = "catch"
                    , expect = ConcurrentTask.expectString
                    , errors = ConcurrentTask.expectThrows identity
                    , args = Encode.null
                    }

            noErrorsTask : ConcurrentTask x String
            noErrorsTask =
                ConcurrentTask.define
                    { function = "noErrors"
                    , expect = ConcurrentTask.expectString
                    , errors = ConcurrentTask.expectNoErrors
                    , args = Encode.null
                    }
         in
         [ describe "JS Exceptions"
            [ Test.forEach
                [ ( "noErrors", noErrorsTask )
                , ( "custom", customErrorTask )
                ]
                "Errors with UnhandledJsException if a task throws and does not catch exception"
                (\( fnName, task ) ->
                    ConcurrentTask.map2 join2
                        task
                        task
                        |> runTaskWith
                            [ ( 0, success (Encode.string "a") )
                            , ( 1, jsException "f threw an exception" (Encode.string "error data") )
                            ]
                        |> Expect.equal
                            (Internal.UnexpectedError
                                (Internal.UnhandledJsException
                                    { function = fnName
                                    , message = "f threw an exception"
                                    , raw = Encode.string "error data"
                                    }
                                )
                            )
                )
            , test "Errors with caught exception if task throws and has expectThrows handler" <|
                \_ ->
                    ConcurrentTask.map3 join3
                        catchTask
                        customErrorTask
                        catchTask
                        |> runTaskWith
                            [ ( 0, success (Encode.string "a") )
                            , ( 1, success (Encode.string "b") )
                            , ( 2, jsException "f threw an exception" (Encode.string "error data") )
                            ]
                        |> Expect.equal (Internal.Error "f threw an exception")
            , Test.forEach
                [ customErrorTask
                , noErrorsTask
                ]
                "JSExceptions can be caught and returned in regular task flow"
                (\task ->
                    task
                        |> ConcurrentTask.onJsException (.message >> ConcurrentTask.succeed)
                        |> runTaskWith [ ( 0, jsException "f threw an exception" (Encode.string "error data") ) ]
                        |> Expect.equal (Internal.Success "f threw an exception")
                )
            ]
         , describe "Tasks with Missing functions"
            [ Test.forEach
                [ catchTask
                , customErrorTask
                , noErrorsTask
                ]
                "Always errors with a RunnerError regardless of error handling strategy"
                (\task ->
                    ConcurrentTask.map3 join3 task task task
                        |> runTaskWith
                            [ ( 0, success (Encode.string "a") )
                            , ( 1, success (Encode.string "b") )
                            , ( 2, missingFunctionError "f is missing" )
                            ]
                        |> Expect.equal (Internal.UnexpectedError (Internal.MissingFunction "f is missing"))
                )
            ]
         , describe "Tasks with unexpected responses"
            [ Test.forEach
                [ ( "custom", customErrorTask )
                , ( "catch", catchTask )
                , ( "noErrors", noErrorsTask )
                ]
                "Errors with a ResponseDecoderFailure if a task returns an unexpected value"
                (\( fName, task ) ->
                    task
                        |> runTaskWith [ ( 0, success (Encode.int 1) ) ]
                        |> Expect.responseDecoderFailureFor fName "Expecting a STRING"
                )
            , Test.forEach
                [ customErrorTask
                , catchTask
                , noErrorsTask
                ]
                "ResponseDecoderFailures can be caught and returned in regular task flow"
                (\task ->
                    task
                        |> ConcurrentTask.onResponseDecoderFailure
                            (Decode.errorToString
                                >> String.right 18
                                >> ConcurrentTask.succeed
                            )
                        |> runTaskWith [ ( 0, success (Encode.int 1) ) ]
                        |> Expect.equal (Internal.Success "Expecting a STRING")
                )
            ]
         , describe "Tasks with custom errors"
            (let
                decodeError : Decoder CustomError
                decodeError =
                    Decode.string
                        |> Decode.andThen
                            (\e ->
                                case e of
                                    "ERR_1" ->
                                        Decode.succeed Error1

                                    "ERR_2" ->
                                        Decode.succeed Error2

                                    _ ->
                                        Decode.fail ("Unrecognized Error: " ++ e)
                            )

                task : ConcurrentTask CustomError String
                task =
                    ConcurrentTask.define
                        { function = "custom"
                        , expect = ConcurrentTask.expectString
                        , errors = ConcurrentTask.expectErrors decodeError
                        , args = Encode.null
                        }
             in
             [ test "Tasks can specify a custom error decoder" <|
                \_ ->
                    ConcurrentTask.map2 join2
                        task
                        task
                        |> runTaskWith
                            [ ( 0, success (Encode.string "1") )
                            , ( 1, success (Encode.object [ ( "error", Encode.string "ERR_1" ) ]) )
                            ]
                        |> Expect.equal (Internal.Error Error1)
             , test "Errors with ErrorDecoderFailure if the task returns an error key with an unexpected value on it" <|
                \_ ->
                    ConcurrentTask.map2 join2
                        task
                        task
                        |> runTaskWith
                            [ ( 0, success (Encode.string "1") )
                            , ( 1, success (Encode.object [ ( "error", Encode.string "ERR_3" ) ]) )
                            ]
                        |> Expect.errorDecoderFailureFor "custom" "Unrecognized Error: ERR_3"
             ]
            )
         , test "task with an unknown error reason" <|
            \_ ->
                ConcurrentTask.map2 join2
                    createTask
                    createTask
                    |> runTaskWith
                        [ ( 0, success (Encode.string "a") )
                        , ( 1, error "other_error" "..." Encode.null )
                        ]
                    |> Expect.equal (Internal.UnexpectedError (Internal.InternalError "Unknown runner error reason: other_error"))
         ]
        )


type CustomError
    = Error1
    | Error2



-- Hardcoded Tasks


hardcoded : Test
hardcoded =
    describe "Hardcoded Tasks"
        [ fuzz3 string string string "tasks can combine" <|
            \a b c ->
                ConcurrentTask.map3 join3
                    (ConcurrentTask.succeed a)
                    (ConcurrentTask.succeed b)
                    (ConcurrentTask.succeed c)
                    |> runTask []
                    |> Expect.equal (Internal.Success (join3 a b c))
        , fuzz3 string string string "tasks can chain" <|
            \a b c ->
                ConcurrentTask.map2 (++)
                    (ConcurrentTask.succeed a)
                    (ConcurrentTask.succeed b)
                    |> ConcurrentTask.andThen (\ab -> ConcurrentTask.map (join2 ab) (ConcurrentTask.succeed c))
                    |> runTask []
                    |> Expect.equal (Internal.Success (a ++ b ++ c))
        , test "tasks can short circuit" <|
            \_ ->
                ConcurrentTask.succeed 1
                    |> ConcurrentTask.andThenDo (ConcurrentTask.succeed 2)
                    |> ConcurrentTask.andThen (\_ -> ConcurrentTask.fail "hardcoded error")
                    |> runTask []
                    |> Expect.equal (Internal.Error "hardcoded error")
        , fuzz2 int int "tasks can recover from an error" <|
            \a b ->
                ConcurrentTask.succeed a
                    |> ConcurrentTask.andThen (\_ -> ConcurrentTask.fail "error")
                    |> ConcurrentTask.onError (\_ -> ConcurrentTask.succeed b)
                    |> runTask []
                    |> Expect.equal (Internal.Success b)
        ]


helpers : Test
helpers =
    describe "Task Helpers"
        [ describe "finallyDo"
            [ test "Runs the followup task when the first task succeeds" <|
                \_ ->
                    createTask
                        |> ConcurrentTask.finallyDo createTask
                        |> runTask
                            [ ( 0, Encode.string "first" )
                            , ( 1, Encode.string "followup" )
                            ]
                        |> Expect.equal (Internal.Success "followup")
            , test "Runs the followup task when the first task fails" <|
                \_ ->
                    createTask
                        |> ConcurrentTask.finallyDo (ConcurrentTask.succeed "followup")
                        |> evalTask
                            [ ( 0, jsException "error" Encode.null )
                            , ( 1, Encode.string "followup" )
                            ]
                        |> Expect.all
                            [ Tuple.second >> Expect.equal (Internal.Error "error")
                            , Tuple.first >> Ids.get >> Expect.equal "1"
                            ]
            ]
        ]



-- Task Runner


runTask : List ( Int, Encode.Value ) -> ConcurrentTask x a -> Internal.Response x a
runTask results =
    runTaskWith (List.map (Tuple.mapSecond success) results)


runTaskWith : List ( Int, Encode.Value ) -> ConcurrentTask x a -> Internal.Response x a
runTaskWith results task =
    Tuple.second (evalTask results task)


evalTask : List ( Int, Encode.Value ) -> ConcurrentTask x a -> ( Ids, Internal.Response x a )
evalTask results task =
    evalWith
        { maxDepth = 100000000
        , results = results
        , task = task
        , ids = Ids.init
        }


type alias Eval x a =
    { maxDepth : Int
    , results : List ( Int, Encode.Value )
    , task : ConcurrentTask x a
    , ids : Ids
    }


evalWith : Eval x a -> ( Ids, Internal.Response x a )
evalWith options =
    let
        results : Internal.Results
        results =
            options.results
                |> List.head
                |> Maybe.withDefault ( -1, Encode.null )
                |> Tuple.mapFirst String.fromInt
                |> List.singleton
                |> Dict.fromList
    in
    case stepTask results ( options.ids, options.task ) of
        ( ids, Internal.Done a ) ->
            ( ids, a )

        ( ids, Internal.Pending _ next ) ->
            if options.maxDepth > 0 then
                evalWith
                    { options
                        | maxDepth = options.maxDepth - 1
                        , results = List.drop 1 options.results
                        , task = next
                        , ids = ids
                    }

            else
                ( ids, Internal.UnexpectedError (Internal.InternalError "timeout") )


stepTask : Internal.Results -> ( Ids, ConcurrentTask x a ) -> ( Ids, Internal.Task_ x a )
stepTask res ( ids, Internal.Task run ) =
    run res ids



-- Helpers


type alias Error =
    String


success : Encode.Value -> Encode.Value
success v =
    Encode.object
        [ ( "status", Encode.string "success" )
        , ( "value", v )
        ]


jsException : String -> Encode.Value -> Encode.Value
jsException =
    error "js_exception"


missingFunctionError : String -> Encode.Value
missingFunctionError message =
    error "missing_function" message Encode.null


error : String -> String -> Encode.Value -> Encode.Value
error reason message raw =
    Encode.object
        [ ( "status", Encode.string "error" )
        , ( "error"
          , Encode.object
                [ ( "reason", Encode.string reason )
                , ( "message", Encode.string message )
                , ( "raw", raw )
                ]
          )
        ]


withAnother : String -> ConcurrentTask Error String
withAnother x =
    ConcurrentTask.map (join2 x) createTask


createTask : ConcurrentTask Error String
createTask =
    create Decode.string


create : Decoder a -> ConcurrentTask Error a
create decoder =
    ConcurrentTask.define
        { function = "aTask"
        , expect = ConcurrentTask.expectJson decoder
        , errors = ConcurrentTask.expectThrows identity
        , args = Encode.null
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
