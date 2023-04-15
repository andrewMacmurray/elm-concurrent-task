port module TaskSpec exposing (main)

import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Runner
import Spec exposing (Spec, describe, given, it, scenario, when)
import Spec.Claim as Claim
import Spec.Extra exposing (equals)
import Spec.Observer exposing (observeModel)
import Spec.Port
import Spec.Report as Report
import Spec.Setup as Spec
import Spec.Step as Step



-- Program


type alias Model =
    { task : Task.Progress Task.Error String
    , result : Maybe (Result Task.Error String)
    }


type Msg
    = OnProgress ( Task.Progress Task.Error String, Cmd Msg )
    | OnComplete (Result Task.Error String)


init : Task Task.Error String -> ( Model, Cmd Msg )
init task =
    let
        ( progress, cmd ) =
            Task.attempt
                { send = send
                , onComplete = OnComplete
                }
                task
    in
    ( { task = progress
      , result = Nothing
      }
    , cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnProgress ( task, cmd ) ->
            ( { model | task = task }, cmd )

        OnComplete result ->
            ( { model | result = Just result }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Task.onProgress
        { send = send
        , receive = receive
        , onComplete = OnComplete
        , onProgress = OnProgress
        }
        model.task


getInt : Int -> Task Task.Error Int
getInt i =
    Task.task
        { function = "getInt"
        , args = Encode.int i
        , expect = Decode.int
        }


getString : String -> Task Task.Error String
getString s =
    Task.task
        { function = "getString"
        , args = Encode.string s
        , expect = Decode.string
        }


port send : Encode.Value -> Cmd msg


port receive : (List Task.RawResult -> msg) -> Sub msg



-- Spec


main : Program Spec.Flags (Spec.Model Model Msg) (Spec.Msg Msg)
main =
    Runner.program
        [ pendingSpec
        , errorSpec
        , hardcodedSpec
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


errorSpec : Spec Model Msg
errorSpec =
    describe "Error spec"
        [ scenario "JS Error"
            (getString "42"
                |> givenATask
                |> when "a JS error is thrown by the function" [ sendError "js_exception" "something went wrong" ]
                |> it "returns an error" (expectResult (Err (Task.JsException "something went wrong")))
            )
        , scenario "Function missing error"
            (Task.map2 (++)
                (getString "42")
                (getString "42")
                |> givenATask
                |> when "a missing function error is thrown" [ sendError "missing_function" "getString function is missing" ]
                |> it "returns an error" (expectResult (Err (Task.MissingFunction "getString function is missing")))
            )
        , scenario "Decode response error"
            (getString "42"
                |> givenATask
                |> when "a function returns the wrong type"
                    [ sendProgress
                        [ { id = "0"
                          , function = "getString"
                          , args = Encode.int 42
                          }
                        ]
                    ]
                |> it "returns an error"
                    (expectResultIs
                        (\res ->
                            case res of
                                Err (Task.DecodeResponseError _) ->
                                    True

                                _ ->
                                    False
                        )
                    )
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
                |> Task.andThenDo (Task.fail (Task.JsException "something went wrong"))
                |> Task.andThenDo (Task.succeed "3")
             )
                |> givenATask
                |> when "a failing task is run" []
                |> it "returns an error" (expectResult (Err (Task.JsException "something went wrong")))
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


sendError : String -> String -> Step.Context model -> Step.Command msg
sendError error reason =
    Spec.Port.respond "send" decodeTaskDefinitions (sendError_ error reason)


sendError_ : String -> String -> List TaskDefinition -> Step.Context model -> Step.Command msg
sendError_ error reason defs =
    Spec.Port.send "receive" (Encode.list (encodeError error reason) defs)


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
    Spec.Port.send "receive" (Encode.list encodeSuccess defs)


encodeSuccess : TaskDefinition -> Encode.Value
encodeSuccess def =
    Encode.object
        [ ( "id", Encode.string def.id )
        , ( "result"
          , Encode.object
                [ ( "status", Encode.string "success" )
                , ( "value", def.args )
                ]
          )
        ]


encodeError : String -> String -> TaskDefinition -> Encode.Value
encodeError error message def =
    Encode.object
        [ ( "id", Encode.string def.id )
        , ( "result"
          , Encode.object
                [ ( "status", Encode.string "error" )
                , ( "error"
                  , Encode.object
                        [ ( "reason", Encode.string error )
                        , ( "message", Encode.string message )
                        ]
                  )
                ]
          )
        ]


expectResultIs : (Result Task.Error String -> Bool) -> Spec.Expectation Model
expectResultIs toExpected =
    observeModel .result
        |> Spec.expect
            (\res ->
                case res of
                    Just r ->
                        if toExpected r then
                            Claim.Accept

                        else
                            Claim.Reject (Report.note ("Expected result does not match predicate, got: " ++ Debug.toString r))

                    Nothing ->
                        Claim.Reject (Report.note "No Result present")
            )


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
