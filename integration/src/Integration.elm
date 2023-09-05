module Integration exposing (specs)

import ConcurrentTask as Task exposing (ConcurrentTask)
import ConcurrentTask.Http as Http
import Integration.Spec as Spec exposing (Spec)


specs : List Spec
specs =
    [ batchSpeedTest
    , batchAndSequenceSpeedTest
    ]


batchSpeedTest : Spec
batchSpeedTest =
    Spec.spec
        "Batch Speed Test"
        "Batch should start all tasks at once"
        (Spec.timeExecution
            (Task.batch
                [ longRequest 100
                , longRequest 100
                , longRequest 100
                , longRequest 100
                ]
            )
        )
        (Spec.assertSuccess (\res -> Spec.shouldHaveDurationLessThan 150 res))


batchAndSequenceSpeedTest : Spec
batchAndSequenceSpeedTest =
    Spec.spec
        "Batch and Sequence speed test"
        "The batched branch should be faster than the sequential branch"
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


longRequest : Int -> ConcurrentTask Http.Error ()
longRequest ms =
    Http.get
        { url = "http://localhost:4000/wait-then-respond/" ++ String.fromInt ms
        , headers = []
        , expect = Http.expectWhatever
        , timeout = Nothing
        }
