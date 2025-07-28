module Pages.Posts exposing
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
    { posts : Posts
    }


type Posts
    = Loading
    | Error String
    | Loaded (List String)


init : ( Model, Effect Msg )
init =
    ( { posts = Loading }
    , Effect.attemptTask GotPosts getAllPosts
    )


getAllPosts : ConcurrentTask Http.Error (List String)
getAllPosts =
    Process.sleep 1000
        |> ConcurrentTask.andThenDo
            (ConcurrentTask.batch
                [ getPost 1
                , getPost 2
                , getPost 3
                ]
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



-- UPDATE


type Msg
    = GotPosts (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotPosts (Ok todos) ->
            ( { model | posts = Loaded todos }, Effect.None )

        GotPosts (Err err) ->
            ( { model | posts = Error (Debug.toString err) }, Effect.None )



-- VIEW


view : Model -> Html Msg
view model =
    H.div [ A.class "col gap-s" ]
        [ H.p [ A.class "f1" ] [ H.text "Posts" ]
        , viewTodos model
        ]


viewTodos : Model -> Html msg
viewTodos model =
    case model.posts of
        Loading ->
            H.p [] [ H.text "Loading..." ]

        Error err ->
            H.p [ A.class "error" ] [ H.text err ]

        Loaded todos ->
            H.div [ A.class "col gap-xxs" ]
                [ H.p [] [ H.text "Loaded" ]
                , H.ul [ A.class "col gap-xxs" ] (List.map viewPost todos)
                ]


viewPost : String -> Html msg
viewPost t =
    H.li [ A.class "success" ] [ H.text t ]
