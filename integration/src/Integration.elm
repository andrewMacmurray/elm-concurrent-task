module Integration exposing (specs)

import ConcurrentTask as Task exposing (ConcurrentTask)
import ConcurrentTask.Http as Http
import ConcurrentTask.Process
import Integration.Spec as Spec exposing (Spec)
import Json.Decode as Decode


specs : List Spec
specs =
    [ batchAndSequenceSpeedTest
    , responseTest
    , largeBatchSpec
    ]


largeBatchSpec : Spec
largeBatchSpec =
    let
        batchSize : Int
        batchSize =
            100000
    in
    Spec.spec
        "large batch test"
        "can handle large batches"
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
    Spec.spec
        "batch and sequence speed test"
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
    Spec.spec
        "complex responses"
        "task should decode and combine responses correctly"
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
        { url = "http://localhost:4000/wait-then-respond/" ++ String.fromInt ms
        , headers = []
        , expect = Http.expectJson (Decode.field "message" Decode.string)
        , timeout = Nothing
        }
