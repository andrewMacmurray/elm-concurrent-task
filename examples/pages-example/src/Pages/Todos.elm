module Pages.Todos exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Http as Http
import ConcurrentTask.Process as Process
import Effect exposing (Effect)
import Html as H exposing (Html)
import Html.Attributes as A
import Json.Decode as Decode



-- INIT


type alias Model =
    { todos : Todos
    }


type Todos
    = Loading
    | Error String
    | Loaded (List String)


init : ( Model, Effect Msg )
init =
    ( { todos = Loading }
    , Effect.attemptTask GotTodos getAllTodos
    )


getAllTodos : ConcurrentTask Http.Error (List String)
getAllTodos =
    Process.sleep 1000
        |> ConcurrentTask.andThenDo
            (ConcurrentTask.batch
                [ getTodo 1
                , getTodo 2
                , getTodo 3
                ]
            )


getTodo : Int -> ConcurrentTask Http.Error String
getTodo id =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/todos/" ++ String.fromInt id
        , headers = []
        , expect = Http.expectJson (Decode.field "title" Decode.string)
        , timeout = Nothing
        }



-- UPDATE


type Msg
    = GotTodos (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotTodos (Ok todos) ->
            ( { model | todos = Loaded todos }, Effect.None )

        GotTodos (Err err) ->
            ( { model | todos = Error (Debug.toString err) }, Effect.None )



-- VIEW


view : Model -> Html Msg
view model =
    H.div [ A.class "col gap-s" ]
        [ H.p [ A.class "f1" ] [ H.text "Todos" ]
        , viewTodos model
        ]


viewTodos : Model -> Html msg
viewTodos model =
    case model.todos of
        Loading ->
            H.p [] [ H.text "Loading..." ]

        Error err ->
            H.p [ A.class "error" ] [ H.text err ]

        Loaded todos ->
            H.div [ A.class "col gap-xxs" ]
                [ H.p [] [ H.text "Loaded" ]
                , H.ul [ A.class "col gap-xxs" ] (List.map viewTodo todos)
                ]


viewTodo : String -> Html msg
viewTodo t =
    H.li [ A.class "success" ] [ H.text t ]
