port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import ConcurrentTask
import ConcurrentTask.Browser.Dom
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, id)
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
    , foundViewport : Maybe Dom.Viewport
    }


type Msg
    = FocusClicked
    | BlurClicked
    | FindTheSizeClicked
    | OnComplete (ConcurrentTask.Response Error Output)
    | OnProgress ( Pool, Cmd Msg )


type alias Flags =
    ()



-- Task Types


type alias Pool =
    ConcurrentTask.Pool Msg Error Output


type alias Error =
    Dom.Error


type Output
    = DomNodeOperation ()
    | ViewportOperation Dom.Viewport



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { tasks = ConcurrentTask.pool
      , foundViewport = Nothing
      }
    , Cmd.none
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FindTheSizeClicked ->
            let
                ( tasks, cmd ) =
                    ConcurrentTask.attempt
                        { send = send
                        , pool = model.tasks
                        , onComplete = OnComplete
                        }
                        (ConcurrentTask.Browser.Dom.getViewportOf "box"
                            |> ConcurrentTask.map ViewportOperation
                        )
            in
            ( { model | tasks = tasks }, cmd )

        BlurClicked ->
            let
                ( tasks, cmd ) =
                    ConcurrentTask.attempt
                        { send = send
                        , pool = model.tasks
                        , onComplete = OnComplete
                        }
                        (ConcurrentTask.Browser.Dom.blur "input"
                            |> ConcurrentTask.map DomNodeOperation
                        )
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
                        (ConcurrentTask.Browser.Dom.focus "input"
                            |> ConcurrentTask.map DomNodeOperation
                        )
            in
            ( { model | tasks = tasks }, cmd )

        OnComplete (ConcurrentTask.Success (ViewportOperation vp)) ->
            ( { model | foundViewport = Just vp }, Cmd.none )

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
    div [ class "row" ]
        [ input [ id "input" ] []
        , div
            [ class "box"
            , id "box"
            , onClick FindTheSizeClicked
            ]
            [ text "What size am i?" ]
        , showViewport model
        , div []
            [ button [ onClick FocusClicked ] [ text "Focus the Input!" ]
            , button [ onClick BlurClicked ] [ text "Blur the Input!" ]
            ]
        ]


showViewport : Model -> Html msg
showViewport model =
    case model.foundViewport of
        Nothing ->
            text ""

        Just vp ->
            div []
                [ div [] [ text "Found viewport!" ]
                , div [] [ text (Debug.toString vp) ]
                ]
