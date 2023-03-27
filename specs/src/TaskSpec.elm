port module TaskSpec exposing (main)

import Concurrent.Task2 as Task exposing (Task)
import Json.Decode as Decode
import Json.Encode as Encode
import Runner
import Spec exposing (describe, given, it, scenario, when)
import Spec.Claim
import Spec.Observer exposing (observeModel)
import Spec.Port as Ports
import Spec.Setup as Spec
import Spec.Step as Step



-- Program


type alias Model =
    { task : Task Task.Error String
    , result : Maybe (Result Task.Error String)
    }


type Msg
    = OnProgress ( Task Task.Error String, Cmd Msg )
    | OnResult (Result Task.Error String)


init : Task Task.Error String -> ( Model, Cmd Msg )
init t =
    let
        ( task, cmd ) =
            Task.attempt
                { send = send
                , onResult = OnResult
                }
                t
    in
    ( { task = task, result = Nothing }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnProgress ( task, cmd ) ->
            ( { model | task = task }, cmd )

        OnResult result ->
            ( { model | result = Just result }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Task.onProgress
        { onResult = OnResult
        , onProgress = OnProgress
        , receive = receive
        , send = send
        }
        model.task


getInt : Task Task.Error Int
getInt =
    Task.ffi
        { function = "slowInt"
        , args = Encode.null
        , expect = Decode.int
        }


getString : Task Task.Error String
getString =
    Task.ffi
        { function = "getString"
        , args = Encode.null
        , expect = Decode.string
        }


port send : Encode.Value -> Cmd msg


port receive : (Decode.Value -> msg) -> Sub msg



-- Spec


pendingSpec : Spec.Spec Model Msg
pendingSpec =
    describe "Pending Tasks"
        [ scenario "Single Task"
            (getString
                |> givenATask
                |> when "a single task is run"
                    [ sendProgress
                        [ Encode.string "42"
                        ]
                    ]
                |> it "decodes is results successfully" (expectResult (Ok "42"))
            )
        , scenario "Two Tasks"
            ((Task.map2 (+)
                getInt
                getInt
                |> Task.map String.fromInt
             )
                |> givenATask
                |> when "two concurrent tasks are run"
                    [ sendProgress
                        [ Encode.int 1
                        , Encode.int 3
                        ]
                    ]
                |> it "decodes the results successfully" (expectResult (Ok "4"))
            )
        , scenario "Three Tasks"
            (Task.map3 (\a b c -> a ++ b ++ c)
                getString
                getString
                getString
                |> givenATask
                |> when "three concurrent tasks are run"
                    [ sendProgress
                        [ Encode.string "1"
                        , Encode.string "3"
                        , Encode.string "5"
                        ]
                    ]
                |> it "decodes the results successfully" (expectResult (Ok "135"))
            )
        ]


hardcodedSpec : Spec.Spec Model Msg
hardcodedSpec =
    describe "Hardcoded Tasks"
        [ scenario "Single Task"
            (Task.succeed "42"
                |> givenATask
                |> when "a single hardcoded task is run" []
                |> it "returns immediately" (expectResult (Ok "42"))
            )
        , scenario "Multiple Tasks"
            (Task.map3 (\a b c -> a ++ b ++ c)
                (Task.succeed "1")
                (Task.succeed "1")
                (Task.succeed "1")
                |> givenATask
                |> when "a combined hardcoded task is run" []
                |> it "combines the two tasks" (expectResult (Ok "111"))
            )
        , scenario "Chaining Tasks"
            ((Task.succeed "1"
                |> Task.andThen
                    (\a ->
                        Task.succeed "2"
                            |> Task.map (\b -> a ++ b)
                    )
             )
                |> givenATask
                |> when "a chained hardcoded task is run" []
                |> it "combines all the tasks" (expectResult (Ok "12"))
            )
        , scenario "Failing Tasks"
            ((Task.succeed "1"
                |> Task.andThenDo (Task.succeed "2")
                |> Task.andThenDo (Task.fail Task.PendingError)
                |> Task.andThenDo (Task.succeed "3")
             )
                |> givenATask
                |> when "a failing task is run" []
                |> it "returns an error" (expectResult (Err Task.PendingError))
            )
        ]


sendProgress : List Encode.Value -> Step.Context model -> Step.Command msg
sendProgress results =
    Ports.send "receive" (Encode.list identity results)


expectResult : Result Task.Error String -> Spec.Expectation Model
expectResult expected =
    observeModel .result
        |> Spec.expect (Spec.Claim.isEqual Debug.toString (Just expected))


givenATask : Task Task.Error String -> Spec.Script Model Msg
givenATask t =
    given
        (Spec.init (init t)
            |> Spec.withUpdate update
            |> Spec.withSubscriptions subscriptions
        )


main : Program Spec.Flags (Spec.Model Model Msg) (Spec.Msg Msg)
main =
    Runner.program
        [ hardcodedSpec
        , pendingSpec
        ]
