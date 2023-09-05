port module Integration.Runner exposing (main)

import ConcurrentTask as Task
import Integration
import Integration.Spec as Spec exposing (Assertion, Expect(..))
import Json.Decode as Decode



-- Model


type alias Flags =
    {}


type alias Model =
    { tasks : Pool
    , unexpectedErrors : List Task.UnexpectedError
    , completedAssertions : List Assertion
    }


type Msg
    = OnProgress ( Pool, Cmd Msg )
    | OnComplete (Task.Response Error Output)



-- Task Types


type alias Pool =
    Task.Pool Msg Error Output


type alias Error =
    Assertion


type alias Output =
    Assertion



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    startAllSpecs
        ( { tasks = Task.pool
          , unexpectedErrors = []
          , completedAssertions = []
          }
        , Cmd.none
        )


startAllSpecs : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
startAllSpecs ( model, cmd ) =
    List.foldl runSpec ( model, cmd ) Integration.specs


runSpec : Task.ConcurrentTask Error Output -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
runSpec spec ( model, cmd ) =
    let
        ( tasks, cmd_ ) =
            Task.attempt
                { send = send
                , onComplete = OnComplete
                , pool = model.tasks
                }
                spec
    in
    ( { model | tasks = tasks }
    , Cmd.batch [ cmd, cmd_ ]
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnComplete result ->
            result
                |> handleResult model
                |> handleComplete

        OnProgress ( tasks, cmd ) ->
            ( { model | tasks = tasks }, cmd )


handleResult : Model -> Task.Response Error Output -> Model
handleResult model result =
    case result of
        Task.Success assertion ->
            { model | completedAssertions = assertion :: model.completedAssertions }

        Task.Error assertion ->
            { model | completedAssertions = assertion :: model.completedAssertions }

        Task.UnexpectedError err ->
            { model | unexpectedErrors = err :: model.unexpectedErrors }


handleComplete : Model -> ( Model, Cmd Msg )
handleComplete model =
    if allSpecsHaveRun model then
        ( model
        , report
            { assertions = Spec.report model.completedAssertions
            , errors = Spec.reportErrors model.unexpectedErrors
            }
        )

    else
        ( model, Cmd.none )


allSpecsHaveRun : Model -> Bool
allSpecsHaveRun model =
    List.length model.completedAssertions + List.length model.unexpectedErrors == List.length Integration.specs



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Task.onProgress
        { send = send
        , receive = receive
        , onProgress = OnProgress
        }
        model.tasks



-- Ports


port send : Decode.Value -> Cmd msg


port receive : (Decode.Value -> msg) -> Sub msg


port report : { assertions : String, errors : String } -> Cmd msg



-- Program


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
