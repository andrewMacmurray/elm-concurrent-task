port module Requests exposing (main)

import Concurrent.Task as Task exposing (Task)
import Concurrent.Task.Http as Http
import Json.Decode as Decode exposing (Decoder)



-- Model


type alias Flags =
    {}


type alias Model =
    { tasks : Pool
    }


type Msg
    = OnProgress ( Pool, Cmd Msg )
    | OnComplete (Task.Response Error Output)



-- Task Types


type alias Pool =
    Task.Pool Msg Error Output


type alias Error =
    Http.Error


type alias Output =
    String



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        ( tasks, cmd ) =
            Task.attempt
                { send = send
                , onComplete = OnComplete
                , pool = Task.pool
                }
                longRequestChain
    in
    ( { tasks = tasks }, cmd )



-- Requests


longRequestChain : Task Error Output
longRequestChain =
    Task.map3 join3
        (longRequest 100)
        (longRequest 100)
        (httpError
            |> Task.onError (\_ -> longRequest 100)
            |> Task.andThenDo (longRequest 100)
        )
        |> Task.andThenDo (longRequest 100)
        |> Task.andThenDo (longRequest 100)
        |> Task.andThenDo (longRequest 100)
        |> Task.andThenDo httpError
        |> Task.onError (\_ -> httpError)
        |> Task.onError (\_ -> httpError)
        |> Task.onError (\_ -> httpError)
        |> Task.onError (\_ -> httpError)
        |> Task.onError (\_ -> httpError)
        |> Task.onError (\_ -> longRequest 100)
        |> Task.andThenDo (longRequest 100)
        |> Task.andThenDo (longRequest 100)
        |> Task.onError (\_ -> httpError)
        |> Task.onError (\_ -> httpError)
        |> Task.onError (\_ -> longRequest 1000)
        |> Task.andThenDo
            (httpError
                |> Task.onError (\_ -> httpError)
                |> Task.onError (\_ -> httpError)
                |> Task.onError (\_ -> longRequest 500)
                |> Task.andThenDo (longRequest 500)
            )
        |> Task.andThenDo (longRequest 300)
        |> Task.return "Completed Http Requests"


longRequest : Int -> Task Http.Error String
longRequest ms =
    Http.request
        { url = "http://localhost:4000/wait-then-respond/" ++ String.fromInt ms
        , method = "GET"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.field "message" Decode.string)
        , timeout = Nothing
        }


httpError : Task Http.Error String
httpError =
    Http.request
        { url = "http://localhost:4000/boom"
        , method = "GET"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.field "message" Decode.string)
        , timeout = Nothing
        }


join3 : String -> String -> String -> String
join3 a b c =
    String.join "," [ a, b, c ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnComplete result ->
            case result of
                Task.TaskError Http.NetworkError ->
                    ( model, printResult "NetworkError: make sure the local dev server is running - `npm run server`" )

                _ ->
                    ( model, printResult ("Result: " ++ Debug.toString result) )

        OnProgress ( tasks, cmd ) ->
            ( { model | tasks = tasks }, cmd )



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


port printResult : String -> Cmd msg



-- Program


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
