port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import ConcurrentTask
import ConcurrentTask.Browser.Dom
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Json.Decode as Decode



-- Program


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type alias Model =
    { tasks : Pool
    }


type Msg
    = FocusClicked
    | BlurClicked
    | OnComplete (ConcurrentTask.Response Error Output)
    | OnProgress ( Pool, Cmd Msg )


type alias Flags =
    ()



-- Task Types


type alias Pool =
    ConcurrentTask.Pool Msg Error Output


type alias Error =
    Dom.Error


type alias Output =
    ()



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { tasks = ConcurrentTask.pool }
    , Cmd.none
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BlurClicked ->
            let
                ( tasks, cmd ) =
                    ConcurrentTask.attempt
                        { send = send
                        , pool = model.tasks
                        , onComplete = OnComplete
                        }
                        (ConcurrentTask.Browser.Dom.blur "input")
            in
            ( { model | tasks = tasks }, cmd )

        FocusClicked ->
            let
                ( tasks, cmd ) =
                    ConcurrentTask.attempt
                        { send = send
                        , pool = model.tasks
                        , onComplete = OnComplete
                        }
                        (ConcurrentTask.Browser.Dom.focus "input")
            in
            ( { model | tasks = tasks }, cmd )

        OnComplete res ->
            let
                _ =
                    Debug.log "response" res
            in
            ( model, Cmd.none )

        OnProgress ( pool, cmd ) ->
            ( { model | tasks = pool }, cmd )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    ConcurrentTask.onProgress
        { send = send
        , receive = receive
        , onProgress = OnProgress
        }
        model.tasks



-- Ports


port send : Decode.Value -> Cmd msg


port receive : (Decode.Value -> msg) -> Sub msg



-- View


view : Model -> Html Msg
view model =
    div []
        [ input [ id "input" ] []
        , button [ onClick FocusClicked ] [ text "Focus Me!" ]
        , button [ onClick BlurClicked ] [ text "Blur Me!" ]
        ]
