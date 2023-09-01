port module Main exposing (main)

import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Http as Http
import Json.Decode as Decode exposing (Decoder)


{-| Many Requests

  - This example fires off a long chain of http requests to a local server.
  - Shows examples of batching requests and recovering from http errors.

-}



-- Model


type alias Flags =
    {}


type alias Model =
    { tasks : Pool
    }


type Msg
    = OnProgress ( Pool, Cmd Msg )
    | OnComplete (ConcurrentTask.Response Error Output)



-- Task Types


type alias Pool =
    ConcurrentTask.Pool Msg Error Output


type alias Error =
    Http.Error


type alias Output =
    String



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        ( tasks, cmd ) =
            ConcurrentTask.attempt
                { send = send
                , onComplete = OnComplete
                , pool = ConcurrentTask.pool
                }
                longRequestChain
    in
    ( { tasks = tasks }, cmd )



-- Requests


longRequestChain : ConcurrentTask Error Output
longRequestChain =
    ConcurrentTask.map3 join3
        (longRequest 100)
        (longRequest 100)
        (httpError
            |> ConcurrentTask.onError (\_ -> longRequest 100)
            |> ConcurrentTask.andThenDo (longRequest 100)
        )
        |> ConcurrentTask.andThenDo (longRequest 100)
        |> ConcurrentTask.andThenDo (longRequest 100)
        |> ConcurrentTask.andThenDo (longRequest 100)
        |> ConcurrentTask.andThenDo httpError
        |> ConcurrentTask.onError (\_ -> httpError)
        |> ConcurrentTask.onError (\_ -> httpError)
        |> ConcurrentTask.onError (\_ -> httpError)
        |> ConcurrentTask.onError (\_ -> httpError)
        |> ConcurrentTask.onError (\_ -> httpError)
        |> ConcurrentTask.onError (\_ -> longRequest 100)
        |> ConcurrentTask.andThenDo (longRequest 100)
        |> ConcurrentTask.andThenDo (longRequest 100)
        |> ConcurrentTask.onError (\_ -> httpError)
        |> ConcurrentTask.onError (\_ -> httpError)
        |> ConcurrentTask.onError (\_ -> longRequest 1000)
        |> ConcurrentTask.andThenDo
            (httpError
                |> ConcurrentTask.onError (\_ -> httpError)
                |> ConcurrentTask.onError (\_ -> httpError)
                |> ConcurrentTask.onError (\_ -> longRequest 500)
                |> ConcurrentTask.andThenDo (longRequest 500)
            )
        |> ConcurrentTask.andThenDo (longRequest 300)
        |> ConcurrentTask.return "Completed Http Requests"


longRequest : Int -> ConcurrentTask Http.Error String
longRequest ms =
    Http.request
        { url = "http://localhost:4000/wait-then-respond/" ++ String.fromInt ms
        , method = "GET"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.field "message" Decode.string)
        , timeout = Nothing
        }


httpError : ConcurrentTask Http.Error String
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
                ConcurrentTask.Error Http.NetworkError ->
                    ( model, printResult "NetworkError: make sure the local dev server is running - `npm run server`" )

                _ ->
                    ( model, printResult ("Result: " ++ Debug.toString result) )

        OnProgress ( tasks, cmd ) ->
            ( { model | tasks = tasks }, cmd )



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


port printResult : String -> Cmd msg



-- Program


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
