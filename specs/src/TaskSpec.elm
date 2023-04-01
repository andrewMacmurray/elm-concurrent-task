port module TaskSpec exposing (main)

import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Runner
import Spec exposing (Spec, describe, given, it, scenario, when)
import Spec.Extra exposing (equals)
import Spec.Observer exposing (observeModel)
import Spec.Port
import Spec.Setup as Spec
import Spec.Step as Step



-- Program


type alias Model =
    { task : Task.Progress Task.Error String
    , result : Maybe (Result Task.Error String)
    }


type Msg
    = OnProgress ( Task.Progress Task.Error String, Cmd Msg )
    | OnResult (Result Task.Error String)


init : Task Task.Error String -> ( Model, Cmd Msg )
init task =
    let
        ( progress, cmd ) =
            Task.attempt
                { send = send
                , onResult = OnResult
                }
                task
    in
    ( { task = progress, result = Nothing }, cmd )


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


getInt : Int -> Task Task.Error Int
getInt i =
    Task.ffi
        { function = "getInt"
        , args = Encode.int i
        , expect = Decode.int
        }


getString : String -> Task Task.Error String
getString s =
    Task.ffi
        { function = "getString"
        , args = Encode.string s
        , expect = Decode.string
        }


port send : Encode.Value -> Cmd msg


port receive : (Decode.Value -> msg) -> Sub msg



-- Spec


main : Program Spec.Flags (Spec.Model Model Msg) (Spec.Msg Msg)
main =
    Runner.program
        [ hardcodedSpec
        , pendingSpec
        ]


pendingSpec : Spec Model Msg
pendingSpec =
    describe "Pending Tasks"
        [ scenario "Single Task"
            (getString "42"
                |> givenATask
                |> when "a single task is run" [ runBatch ]
                |> it "decodes its results successfully" (expectResult (Ok "42"))
            )
        , scenario "Two Tasks"
            (Task.map String.fromInt
                (Task.map2 (+)
                    (getInt 1)
                    (getInt 3)
                )
                |> givenATask
                |> when "two concurrent tasks are run" [ runBatch ]
                |> it "combines the results" (expectResult (Ok "4"))
            )
        , scenario "Three Tasks"
            (Task.map3 (\a b c -> a ++ b ++ c)
                (getString "1")
                (getString "3")
                (getString "5")
                |> givenATask
                |> when "three concurrent tasks are run" [ runBatch ]
                |> it "combines the results" (expectResult (Ok "135"))
            )
        , scenario "Chained Tasks"
            (Task.map2 (\a b -> a ++ b)
                (getString "A")
                (getString "B")
                |> Task.andThen
                    (\ab ->
                        Task.map2 (\c d -> ab ++ c ++ d)
                            (getString "C")
                            (getString "D")
                    )
                |> givenATask
                |> when "chained tasks are run" [ runBatch, runBatch ]
                |> it "chains and combines the results" (expectResult (Ok "ABCD"))
            )
        ]


hardcodedSpec : Spec Model Msg
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
        , scenario "Chained Tasks"
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



-- Helpers


type alias TaskDefinition =
    { id : String
    , function : String
    , args : Encode.Value
    }


runBatch : Step.Context model -> Step.Command msg
runBatch =
    Spec.Port.respond "send" decodeTaskDefinitions sendProgress


decodeTaskDefinitions : Decoder (List TaskDefinition)
decodeTaskDefinitions =
    Decode.list decodeTaskDefinition


decodeTaskDefinition : Decoder TaskDefinition
decodeTaskDefinition =
    Decode.map3
        TaskDefinition
        (Decode.field "id" Decode.string)
        (Decode.field "function" Decode.string)
        (Decode.field "args" Decode.value)


sendProgress : List TaskDefinition -> Step.Context model -> Step.Command msg
sendProgress defs =
    Spec.Port.send "receive" (encodeProgress defs)


encodeProgress : List TaskDefinition -> Encode.Value
encodeProgress defs =
    Encode.object (List.map (\def -> ( def.id, def.args )) defs)


expectResult : Result Task.Error String -> Spec.Expectation Model
expectResult expected =
    observeModel .result
        |> Spec.expect (equals (Just expected))


givenATask : Task Task.Error String -> Spec.Script Model Msg
givenATask t =
    given
        (Spec.init (init t)
            |> Spec.withUpdate update
            |> Spec.withSubscriptions subscriptions
        )
