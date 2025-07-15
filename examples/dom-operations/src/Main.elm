port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Browser.Dom
import Html exposing (Html, button, div, input, p, text)
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
    , foundElement : Maybe Dom.Element
    , foundWindowViewport : Maybe Dom.Viewport
    }


type Msg
    = FocusClicked
    | BlurClicked
    | GetBoxViewportClicked
    | GetElementClicked
    | SetViewportOfElementClicked
    | ScrollTopClicked
    | GetWindowViewportClicked
    | OnComplete (ConcurrentTask.Response Error Output)
    | OnProgress ( Pool, Cmd Msg )


type alias Flags =
    ()



-- Task Types


type alias Pool =
    ConcurrentTask.Pool Msg


type alias Error =
    Dom.Error


type Output
    = DomNodeOperation ()
    | ElementViewportOperation Dom.Viewport
    | WindowViewportOperation Dom.Viewport
    | ElementOperation Dom.Element



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { tasks = ConcurrentTask.pool
      , foundViewport = Nothing
      , foundElement = Nothing
      , foundWindowViewport = Nothing
      }
    , Cmd.none
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetWindowViewportClicked ->
            let
                ( tasks, cmd ) =
                    startTask model.tasks
                        (ConcurrentTask.Browser.Dom.getViewport
                            |> ConcurrentTask.map WindowViewportOperation
                        )
            in
            ( { model | tasks = tasks }, cmd )

        ScrollTopClicked ->
            let
                ( tasks, cmd ) =
                    startTask model.tasks
                        (ConcurrentTask.Browser.Dom.setViewport 0 0
                            |> ConcurrentTask.map DomNodeOperation
                        )
            in
            ( { model | tasks = tasks }, cmd )

        SetViewportOfElementClicked ->
            let
                ( tasks, cmd ) =
                    startTask model.tasks
                        (ConcurrentTask.Browser.Dom.setViewportOf "scrolly-box" 0 0
                            |> ConcurrentTask.map DomNodeOperation
                        )
            in
            ( { model | tasks = tasks }, cmd )

        GetElementClicked ->
            let
                ( tasks, cmd ) =
                    startTask model.tasks
                        (ConcurrentTask.Browser.Dom.getElement "box"
                            |> ConcurrentTask.map ElementOperation
                        )
            in
            ( { model | tasks = tasks }, cmd )

        GetBoxViewportClicked ->
            let
                ( tasks, cmd ) =
                    startTask model.tasks
                        (ConcurrentTask.Browser.Dom.getViewportOf "box"
                            |> ConcurrentTask.map ElementViewportOperation
                        )
            in
            ( { model | tasks = tasks }, cmd )

        BlurClicked ->
            let
                ( tasks, cmd ) =
                    startTask model.tasks
                        (ConcurrentTask.Browser.Dom.blur "input"
                            |> ConcurrentTask.map DomNodeOperation
                        )
            in
            ( { model | tasks = tasks }, cmd )

        FocusClicked ->
            let
                ( tasks, cmd ) =
                    startTask model.tasks
                        (ConcurrentTask.Browser.Dom.focus "input"
                            |> ConcurrentTask.map DomNodeOperation
                        )
            in
            ( { model | tasks = tasks }, cmd )

        OnComplete (ConcurrentTask.Success (ElementViewportOperation vp)) ->
            ( { model | foundViewport = Just vp }, Cmd.none )

        OnComplete (ConcurrentTask.Success (WindowViewportOperation vp)) ->
            ( { model | foundWindowViewport = Just vp }, Cmd.none )

        OnComplete (ConcurrentTask.Success (ElementOperation el)) ->
            ( { model | foundElement = Just el }, Cmd.none )

        OnComplete res ->
            let
                _ =
                    Debug.log "response" res
            in
            ( model, Cmd.none )

        OnProgress ( pool, cmd ) ->
            ( { model | tasks = pool }, cmd )


startTask : Pool -> ConcurrentTask Error Output -> ( Pool, Cmd Msg )
startTask pool =
    ConcurrentTask.attempt
        { send = send
        , onComplete = OnComplete
        , pool = pool
        }



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
        , div [ class "box", id "box" ] [ text "What size am i?" ]
        , div [ class "scrolly-box", id "scrolly-box" ]
            [ text "Scroll mee!!"
            , p [] [ text "..." ]
            , p [] [ text "..." ]
            , p [] [ text "..." ]
            , p [] [ text "..." ]
            , p [] [ text "..." ]
            , p [] [ text "..." ]
            ]
        , showViewport model
        , showWindowViewport model
        , showElement model
        , div [ class "buttons" ]
            [ button [ onClick FocusClicked ] [ text "Focus the Input" ]
            , button [ onClick BlurClicked ] [ text "Blur the Input" ]
            ]
        , div [ class "buttons" ]
            [ button [ onClick SetViewportOfElementClicked ] [ text "Scroll top of the scroll box" ]
            , button [ onClick ScrollTopClicked ] [ text "Scroll Window to Top" ]
            ]
        , div [ class "buttons" ]
            [ button [ onClick GetBoxViewportClicked ] [ text "Get Viewport of the blue box" ]
            , button [ onClick GetWindowViewportClicked ] [ text "Get Window Viewport" ]
            , button [ onClick GetElementClicked ] [ text "Get Element size of the blue box" ]
            ]
        ]


showViewport : Model -> Html msg
showViewport model =
    case model.foundViewport of
        Nothing ->
            text ""

        Just vp ->
            div []
                [ div [] [ text "Found blue box viewport!" ]
                , div [] [ text (Debug.toString vp) ]
                ]


showWindowViewport : Model -> Html msg
showWindowViewport model =
    case model.foundWindowViewport of
        Nothing ->
            text ""

        Just vp ->
            div []
                [ div [] [ text "Found window viewport!" ]
                , div [] [ text (Debug.toString vp) ]
                ]


showElement : Model -> Html msg
showElement model =
    case model.foundElement of
        Nothing ->
            text ""

        Just vp ->
            div []
                [ div [] [ text "Found blue box element size!" ]
                , div [] [ text (Debug.toString vp) ]
                ]
