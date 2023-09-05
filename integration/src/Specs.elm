module Specs exposing (all)

import ConcurrentTask as Task exposing (ConcurrentTask)
import ConcurrentTask.Http as Http
import Integration.Spec as Spec exposing (Spec)


all : List Spec
all =
    [ batchAndSequenceSpeedTest
    ]


batchAndSequenceSpeedTest : Spec
batchAndSequenceSpeedTest =
    Spec.taskSpec
        "Batch and Sequence speed test"
        "The batched branch should be faster than the sequential branch"
        (Task.map2 Tuple.pair
            (Spec.timeExecution
                (Task.batch
                    [ simpleGet
                    , simpleGet
                    , simpleGet
                    , simpleGet
                    ]
                )
            )
            (Spec.timeExecution
                (Task.sequence
                    [ simpleGet
                    , simpleGet
                    , simpleGet
                    , simpleGet
                    ]
                )
            )
        )
        (Spec.assertSuccess
            (\( batched, sequential ) ->
                Spec.assertAll
                    [ batched |> Spec.shouldBeFasterThan sequential
                    , batched.result |> Spec.assertEquals sequential.result
                    ]
            )
        )


simpleGet : ConcurrentTask Http.Error ()
simpleGet =
    Http.get
        { url = "http://localhost:4000/wait-then-respond/10"
        , headers = []
        , expect = Http.expectWhatever
        , timeout = Nothing
        }
