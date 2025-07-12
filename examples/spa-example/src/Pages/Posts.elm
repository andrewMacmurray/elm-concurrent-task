module Pages.Posts exposing (Model, Msg, page)

import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Http as Http
import ConcurrentTask.Process as Process
import Effect exposing (Effect)
import Element exposing (..)
import Json.Decode as Decode
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Spa.ConcurrentTask
import Ui.Spacing as Spacing
import Ui.Text as Text
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { tasks : Pool
    , posts : Posts
    }


type Posts
    = Loading
    | Error String
    | Loaded (List String)


init : () -> ( Model, Effect Msg )
init () =
    let
        ( tasks, cmd ) =
            Spa.ConcurrentTask.attempt
                { task = getAllPosts
                , pool = ConcurrentTask.pool
                , onComplete = OnComplete
                }
    in
    ( { tasks = tasks
      , posts = Loading
      }
    , Effect.sendCmd cmd
    )


getAllPosts : ConcurrentTask Http.Error (List String)
getAllPosts =
    Process.sleep 1000
        |> ConcurrentTask.andThenDo
            (ConcurrentTask.map3 (\a b c -> [ a, b, c ])
                (getPost 1)
                (getPost 2)
                (getPost 3)
            )


getPost : Int -> ConcurrentTask Http.Error String
getPost id =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/posts/" ++ String.fromInt id
        , headers = []
        , expect =
            Http.expectJson
                (Decode.map2 (\_ x -> x)
                    (Decode.field "body" Decode.string)
                    (Decode.field "title" Decode.string)
                )
        , timeout = Nothing
        }



-- Task Types


type alias Pool =
    ConcurrentTask.Pool Msg


type alias Error =
    Http.Error


type alias Output =
    List String



-- UPDATE


type Msg
    = OnComplete (ConcurrentTask.Response Error Output)
    | OnProgress ( Pool, Cmd Msg )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        OnComplete (ConcurrentTask.Success todos) ->
            ( { model | posts = Loaded todos }, Effect.none )

        OnComplete (ConcurrentTask.Error err) ->
            ( { model | posts = Error (Debug.toString err) }, Effect.none )

        OnComplete (ConcurrentTask.UnexpectedError err) ->
            ( { model | posts = Error (Debug.toString err) }, Effect.none )

        OnProgress progress ->
            Spa.ConcurrentTask.progress model progress



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    Spa.ConcurrentTask.subscriptions OnProgress



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Posts"
    , body =
        column [ spacing Spacing.s ]
            [ Text.text [ Text.f1 ] "Posts"
            , viewTodos model
            ]
    }


viewTodos : Model -> Element msg
viewTodos model =
    case model.posts of
        Loading ->
            Text.text [] "Loading..."

        Error err ->
            Text.error [] err

        Loaded todos ->
            column [ spacing Spacing.xxs ]
                [ Text.text [] "Loaded"
                , column [ spacing Spacing.xxs ] (List.map viewTodo todos)
                ]


viewTodo : String -> Element msg
viewTodo t =
    Text.success [] ("- " ++ t)
