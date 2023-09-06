module Integration exposing (specs)

import ConcurrentTask as Task exposing (ConcurrentTask, UnexpectedError(..))
import ConcurrentTask.Http as Http
import ConcurrentTask.Process
import Integration.Spec as Spec exposing (Spec)
import Json.Decode as Decode
import Json.Encode as Encode



-- All Specs


specs : List Spec
specs =
    [ batchAndSequenceSpeedTest
    , responseTest
    , largeBatchSpec
    , httpTimeoutSpec
    , missingFunctionSpec
    , httpMalformedSpec
    ]



-- Integration Specs


missingFunctionSpec : Spec
missingFunctionSpec =
    Spec.describeUnexpected
        "missing functions"
        "task should abort immediately if a function is not registered"
        (Task.define
            { function = "fire_ze_missiles"
            , expect = Task.expectWhatever
            , errors = Task.expectThrows identity
            , args = Encode.null
            }
            |> Task.andThenDo (ConcurrentTask.Process.sleep 500)
            |> Task.return "Completed"
        )
        (Spec.shouldEqual
            (MissingFunction "fire_ze_missiles is not registered")
        )


httpMalformedSpec : Spec
httpMalformedSpec =
    Spec.describe
        "malformed http response"
        "should return a BadBody Error for non JSON responses when expecting JSON"
        (Http.get
            { url = baseUrl ++ "/malformed"
            , headers = []
            , expect = Http.expectJson (Decode.field "invalid" Decode.string)
            , timeout = Nothing
            }
        )
        (Spec.assertError assertMalformedResponse)


assertMalformedResponse : Http.Error -> Spec.Expect
assertMalformedResponse err =
    case err of
        Http.BadBody _ _ e ->
            if String.contains "This is not valid JSON!" (Decode.errorToString e) then
                Spec.pass

            else
                Spec.failWith "Got BadBody but expected invalid JSON Error" e

        _ ->
            Spec.failWith "Expected BadBody, got" err


httpTimeoutSpec : Spec
httpTimeoutSpec =
    Spec.describe
        "http timeout"
        "http requests should abort if request takes longer than given timeout"
        (Spec.timeExecution
            (Http.get
                { url = waitThenRespond 10000
                , headers = []
                , expect = Http.expectWhatever
                , timeout = Just 100
                }
            )
        )
        (Spec.assertError
            (\err ->
                Spec.assertAll
                    [ Spec.shouldEqual Http.Timeout err.result
                    , err |> Spec.shouldHaveDurationLessThan 1000 -- account for test flake
                    ]
            )
        )


largeBatchSpec : Spec
largeBatchSpec =
    let
        batchSize : Int
        batchSize =
            100000
    in
    Spec.describe
        "large batches"
        "large batches should complete in reasonable time"
        (Spec.timeExecution
            (ConcurrentTask.Process.sleep 100
                |> List.repeat batchSize
                |> Task.batch
            )
        )
        (Spec.assertSuccess
            (\res ->
                Spec.assertAll
                    [ Spec.shouldHaveDurationLessThan 3000 res
                    , res.result |> Spec.shouldEqual (List.repeat batchSize ())
                    ]
            )
        )


batchAndSequenceSpeedTest : Spec
batchAndSequenceSpeedTest =
    Spec.describe
        "batch and sequence speed"
        "the batched branch should be faster than the sequential branch"
        (Task.map2 Tuple.pair
            (Spec.timeExecution
                (Task.batch
                    [ longRequest 100
                    , longRequest 100
                    , longRequest 100
                    , longRequest 100
                    ]
                )
            )
            (Spec.timeExecution
                (Task.sequence
                    [ longRequest 100
                    , longRequest 100
                    , longRequest 100
                    , longRequest 100
                    ]
                )
            )
        )
        (Spec.assertSuccess
            (\( batched, sequential ) ->
                Spec.assertAll
                    [ batched |> Spec.shouldBeFasterThan sequential
                    , batched.result |> Spec.shouldEqual sequential.result
                    ]
            )
        )


responseTest : Spec
responseTest =
    Spec.describe
        "complex responses"
        "task should decode and combine responses"
        (Task.map5 join5
            (longRequest 100)
            (longRequest 50
                |> Task.andThen
                    (\x ->
                        joinWith x
                            (Task.map2 join2
                                (longRequest 40)
                                (longRequest 60)
                            )
                    )
            )
            (longRequest 30
                |> Task.andThen
                    (\x ->
                        longRequest 10
                            |> joinWith x
                            |> Task.andThen
                                (\y ->
                                    [ longRequest 42
                                    , longRequest 20
                                    , longRequest 14
                                    , longRequest 86
                                    ]
                                        |> Task.batch
                                        |> Task.map (String.join ",")
                                        |> joinWith y
                                )
                    )
            )
            (longRequest 40)
            (longRequest 120)
        )
        (Spec.assertSuccess
            (Spec.shouldEqual
                (String.join ","
                    [ "done:100"
                    , "done:50"
                    , "done:40"
                    , "done:60"
                    , "done:30"
                    , "done:10"
                    , "done:42"
                    , "done:20"
                    , "done:14"
                    , "done:86"
                    , "done:40"
                    , "done:120"
                    ]
                )
            )
        )



-- Helpers


joinWith : String -> ConcurrentTask x String -> ConcurrentTask x String
joinWith x task =
    Task.map (join2 x) task


join2 : String -> String -> String
join2 a b =
    String.join "," [ a, b ]


join5 : String -> String -> String -> String -> String -> String
join5 a b c d e =
    String.join "," [ a, b, c, d, e ]


longRequest : Int -> ConcurrentTask Http.Error String
longRequest ms =
    Http.get
        { url = waitThenRespond ms
        , headers = []
        , expect = Http.expectJson (Decode.field "message" Decode.string)
        , timeout = Nothing
        }


waitThenRespond : Int -> String
waitThenRespond ms =
    baseUrl ++ "/wait-then-respond/" ++ String.fromInt ms


baseUrl : String
baseUrl =
    "http://localhost:4999"
