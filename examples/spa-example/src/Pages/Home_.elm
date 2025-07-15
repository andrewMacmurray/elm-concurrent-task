module Pages.Home_ exposing (Model, Msg, page)

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
    , todos : Todos
    }


type Todos
    = Loading
    | Error String
    | Loaded (List String)


init : () -> ( Model, Effect Msg )
init () =
    let
        ( tasks, cmd ) =
            Spa.ConcurrentTask.attempt
                { task = getAllTodos
                , pool = ConcurrentTask.pool
                , onComplete = OnComplete
                }
    in
    ( { tasks = tasks
      , todos = Loading
      }
    , Effect.sendCmd cmd
    )


getAllTodos : ConcurrentTask Http.Error (List String)
getAllTodos =
    Process.sleep 1000
        |> ConcurrentTask.andThenDo
            (ConcurrentTask.map3 (\a b c -> [ a, b, c ])
                (getTodo 1)
                (getTodo 2)
                (getTodo 3)
            )


getTodo : Int -> ConcurrentTask Http.Error String
getTodo id =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/todos/" ++ String.fromInt id
        , headers = []
        , expect = Http.expectJson (Decode.field "title" Decode.string)
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
            ( { model | todos = Loaded todos }, Effect.none )

        OnComplete (ConcurrentTask.Error err) ->
            ( { model | todos = Error (Debug.toString err) }, Effect.none )

        OnComplete (ConcurrentTask.UnexpectedError err) ->
            ( { model | todos = Error (Debug.toString err) }, Effect.none )

        OnProgress progress ->
            Spa.ConcurrentTask.progress model progress



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    Spa.ConcurrentTask.subscriptions OnProgress



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Home"
    , body =
        column [ spacing Spacing.s ]
            [ Text.text [ Text.f1 ] "Home"
            , column [ spacing Spacing.s ]
                [ Text.text [ Text.f4 ] "TODOs:"
                , viewTodos model
                ]
            ]
    }


viewTodos : Model -> Element msg
viewTodos model =
    case model.todos of
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
