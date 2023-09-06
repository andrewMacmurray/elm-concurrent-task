port module Integration.Runner exposing (main)

import ConcurrentTask as Task exposing (UnexpectedError)
import Integration
import Integration.Spec as Spec exposing (Assertion, Spec(..))
import Json.Decode as Decode



-- Model


type alias Flags =
    {}


type alias Model =
    { tasks : Pool
    , completed : List Assertion
    }


type Msg
    = OnProgress ( Pool, Cmd Msg )
    | OnComplete (UnexpectedError -> Assertion) (Task.Response Error Output)



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
    startAllSpecs Integration.specs
        ( { tasks = Task.pool
          , completed = []
          }
        , Cmd.none
        )


startAllSpecs : List Spec -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
startAllSpecs specs ( model, cmd ) =
    List.foldl runSpec ( model, cmd ) specs


runSpec : Spec -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
runSpec (Spec unexpected task) ( model, cmd ) =
    let
        ( tasks, cmd_ ) =
            Task.attempt
                { send = send
                , onComplete = OnComplete unexpected
                , pool = model.tasks
                }
                task
    in
    ( { model | tasks = tasks }
    , Cmd.batch [ cmd, cmd_ ]
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnComplete handleUnexpected result ->
            result
                |> handleResult model handleUnexpected
                |> handleComplete

        OnProgress ( tasks, cmd ) ->
            ( { model | tasks = tasks }, cmd )


handleResult : Model -> (UnexpectedError -> Assertion) -> Task.Response Error Output -> Model
handleResult model handleUnexpected result =
    case result of
        Task.Success assertion ->
            { model | completed = assertion :: model.completed }

        Task.Error assertion ->
            { model | completed = assertion :: model.completed }

        Task.UnexpectedError err ->
            { model | completed = handleUnexpected err :: model.completed }


handleComplete : Model -> ( Model, Cmd Msg )
handleComplete model =
    if allSpecsHaveRun model then
        ( model
        , report (Spec.report model.completed)
        )

    else
        ( model, Cmd.none )


allSpecsHaveRun : Model -> Bool
allSpecsHaveRun model =
    List.length model.completed == List.length Integration.specs



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


port report : { message : String, passed : Bool } -> Cmd msg



-- Program


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
