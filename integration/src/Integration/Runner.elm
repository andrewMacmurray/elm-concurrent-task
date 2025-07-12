module Integration.Runner exposing
    ( Flags
    , Model
    , Msg
    , RunnerProgram
    , program
    )

import ConcurrentTask as Task exposing (UnexpectedError)
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
    Task.Pool Msg


type alias Error =
    Assertion


type alias Output =
    Assertion



-- Init


init : Options Msg -> Flags -> ( Model, Cmd Msg )
init options _ =
    startAllSpecs options
        ( { tasks = Task.pool
          , completed = []
          }
        , Cmd.none
        )


startAllSpecs : Options Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
startAllSpecs options ( model, cmd ) =
    List.foldl (runSpec options) ( model, cmd ) options.specs


runSpec : Options Msg -> Spec -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
runSpec options (Spec unexpected task) ( model, cmd ) =
    let
        ( tasks, cmd_ ) =
            Task.attempt
                { send = options.send
                , onComplete = OnComplete unexpected
                , pool = model.tasks
                }
                task
    in
    ( { model | tasks = tasks }
    , Cmd.batch [ cmd, cmd_ ]
    )



-- Update


update : Options Msg -> Msg -> Model -> ( Model, Cmd Msg )
update options msg model =
    case msg of
        OnComplete handleUnexpected result ->
            result
                |> handleResult model handleUnexpected
                |> handleComplete options

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


handleComplete : Options Msg -> Model -> ( Model, Cmd Msg )
handleComplete options model =
    if allSpecsHaveRun options model then
        ( model
        , options.report (Spec.report model.completed)
        )

    else
        ( model, Cmd.none )


allSpecsHaveRun : Options Msg -> Model -> Bool
allSpecsHaveRun options model =
    List.length model.completed == List.length options.specs



-- Subscriptions


subscriptions : Options Msg -> Model -> Sub Msg
subscriptions options model =
    Task.onProgress
        { send = options.send
        , receive = options.receive
        , onProgress = OnProgress
        }
        model.tasks



-- Program


type alias Options msg =
    { specs : List Spec
    , send : Decode.Value -> Cmd msg
    , receive : (Decode.Value -> msg) -> Sub msg
    , report : { message : String, passed : Bool } -> Cmd msg
    }


type alias RunnerProgram =
    Program Flags Model Msg


program : Options Msg -> RunnerProgram
program options =
    Platform.worker
        { init = init options
        , update = update options
        , subscriptions = subscriptions options
        }
