port module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Cmd.Extra
import ConcurrentTask exposing (ConcurrentTask)
import Effect exposing (Effect)
import Html as H exposing (Html)
import Html.Attributes as A
import Json.Decode as Decode exposing (Decoder)
import Pages.Posts as Posts
import Pages.Todos as Todos
import Route exposing (Route)
import Url exposing (Url)



-- Program


main : Program {} Model Msg
main =
    Browser.application
        { init = always init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }



-- Model


type alias Model =
    { navKey : Nav.Key
    , globalError : Maybe String
    , page : Page
    , tasks : ConcurrentTask.Pool Msg
    }


type Msg
    = OnProgress ( ConcurrentTask.Pool Msg, Cmd Msg )
    | OnComplete (ConcurrentTask.Response Msg Msg)
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | PageMsg PageMsg_



-- Page


type PageMsg_
    = TodosMsg Todos.Msg
    | PostsMsg Posts.Msg


type Page
    = NotFound
    | TodosPage Todos.Model
    | PostsPage Posts.Model



-- Init


init : Url -> Nav.Key -> ( Model, Cmd Msg )
init url navKey =
    initPage (Route.fromUrl url)
        { navKey = navKey
        , globalError = Nothing
        , page = NotFound
        , tasks = ConcurrentTask.pool
        }


initPage : Route -> Model -> ( Model, Cmd Msg )
initPage route model =
    case route of
        Route.NotFound ->
            ( { model | page = NotFound }, Cmd.none )

        Route.Todos ->
            Todos.init
                |> liftPage model TodosPage TodosMsg
                |> handleEffect

        Route.Posts ->
            Posts.init
                |> liftPage model PostsPage PostsMsg
                |> handleEffect



-- Effects


handleEffect : ( Model, Effect Msg ) -> ( Model, Cmd Msg )
handleEffect ( model, effect ) =
    case effect of
        Effect.None ->
            ( model, Cmd.none )

        Effect.AttemptTask task ->
            let
                ( tasks, cmd ) =
                    ConcurrentTask.attempt
                        { send = send
                        , pool = model.tasks
                        , onComplete = OnComplete
                        }
                        task
            in
            ( { model | tasks = tasks }, cmd )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnProgress ( pool, cmd ) ->
            ( { model | tasks = pool }, cmd )

        OnComplete res ->
            case res of
                ConcurrentTask.Success msg_ ->
                    ( model, Cmd.Extra.perform msg_ )

                ConcurrentTask.Error msg_ ->
                    ( model, Cmd.Extra.perform msg_ )

                ConcurrentTask.UnexpectedError err ->
                    ( { model | globalError = Just (Debug.toString err) }, Cmd.none )

        PageMsg pageMsg ->
            case ( model.page, pageMsg ) of
                ( TodosPage page, TodosMsg msg_ ) ->
                    Todos.update msg_ page
                        |> liftPage model TodosPage TodosMsg
                        |> handleEffect

                ( PostsPage page, PostsMsg msg_ ) ->
                    Posts.update msg_ page
                        |> liftPage model PostsPage PostsMsg
                        |> handleEffect

                ( _, _ ) ->
                    ( model, Cmd.none )

        OnUrlRequest req ->
            case req of
                Browser.Internal u ->
                    ( model, Nav.pushUrl model.navKey (Url.toString u) )

                Browser.External u ->
                    ( model, Nav.load u )

        OnUrlChange url ->
            { model | tasks = ConcurrentTask.cancelAll model.tasks, globalError = Nothing }
                |> initPage (Route.fromUrl url)


liftPage : Model -> (page -> Page) -> (msg -> PageMsg_) -> ( page, Effect msg ) -> ( Model, Effect Msg )
liftPage model toModel toMsg ( page, eff ) =
    ( { model | page = toModel page }
    , Effect.map (toMsg >> PageMsg) eff
    )



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


view : Model -> Browser.Document Msg
view model =
    let
        navBar : Html msg
        navBar =
            H.nav [ A.class "row gap-xs" ]
                [ navLink "todos" "/"
                , navLink "posts" "/posts"
                ]

        navLink : String -> String -> Html msg
        navLink name url =
            H.a [ A.href url, A.class "f3", A.style "color" "black" ] [ H.text name ]

        viewGlobalError : Html msg
        viewGlobalError =
            model.globalError
                |> Maybe.map (\e -> H.p [ A.class "error" ] [ H.text e ])
                |> Maybe.withDefault (H.text "")

        viewLayout : Html msg -> Html msg
        viewLayout body =
            H.div [ A.class "pa-s col gap-s" ]
                [ navBar
                , body
                , viewGlobalError
                ]

        toHtml : String -> (msg -> PageMsg_) -> Html msg -> Browser.Document Msg
        toHtml title pageMsg body =
            { title = title
            , body = [ viewLayout (H.map (pageMsg >> PageMsg) body) ]
            }
    in
    case model.page of
        NotFound ->
            { title = "not found"
            , body = [ H.text "Not found" ]
            }

        TodosPage page ->
            Todos.view page
                |> toHtml "todos" TodosMsg

        PostsPage page ->
            Posts.view page
                |> toHtml "posts" PostsMsg
