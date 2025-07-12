port module Main exposing (main)

import ConcurrentTask as Task exposing (ConcurrentTask)
import ConcurrentTask.Http as Http
import ConcurrentTask.Process
import Json.Decode as Decode


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
    | OnComplete (Task.Response Error Output)



-- Task Types


type alias Pool =
    Task.Pool Msg


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


longRequestChain : ConcurrentTask Error Output
longRequestChain =
    Task.map3 join3
        (longRequest 100)
        (longRequest 100)
        (httpError
            |> Task.onError (\_ -> longRequest 100)
            |> Task.andThenDo (longRequest 100)
        )
        |> Task.andThenDo (longRequest 100)
        |> Task.andThenDo (retry 20 sometimesFails)
        |> Task.andThenDo (retry 20 sometimesFails)
        |> Task.andThenDo (retry 20 sometimesFails)
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


longRequest : Int -> ConcurrentTask Http.Error String
longRequest ms =
    Http.get
        { url = baseUrl ++ "/wait-then-respond/" ++ String.fromInt ms
        , headers = []
        , expect = Http.expectJson (Decode.field "message" Decode.string)
        , timeout = Nothing
        }
        |> Task.debug Debug.toString Debug.toString


sometimesFails : ConcurrentTask Http.Error String
sometimesFails =
    Http.get
        { url = baseUrl ++ "/flaky"
        , headers = []
        , expect = Http.expectJson (Decode.field "message" Decode.string)
        , timeout = Nothing
        }
        |> Task.debug Debug.toString Debug.toString


httpError : ConcurrentTask Http.Error String
httpError =
    Http.get
        { url = baseUrl ++ "/boom"
        , headers = []
        , expect = Http.expectJson (Decode.field "message" Decode.string)
        , timeout = Nothing
        }
        |> Task.debug Debug.toString Debug.toString


{-| Simple retry mechanism with backoff
-}
retry : Int -> ConcurrentTask x a -> ConcurrentTask x a
retry n task =
    retry_ n (Task.mapError (Tuple.pair n) task)
        |> Task.mapError Tuple.second


retry_ : Int -> ConcurrentTask ( Int, x ) a -> ConcurrentTask ( Int, x ) a
retry_ max task =
    task
        |> Task.onError
            (\( n, err ) ->
                if n > 0 then
                    ConcurrentTask.Process.sleep ((max - n) * 50)
                        |> Task.andThenDo task
                        |> Task.mapError (Tuple.mapFirst (\n_ -> n_ - 1))
                        |> retry_ max

                else
                    Task.fail ( n, err )
            )


join3 : String -> String -> String -> String
join3 a b c =
    String.join "," [ a, b, c ]


baseUrl : String
baseUrl =
    "http://localhost:4000"



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnComplete result ->
            case result of
                Task.Error Http.NetworkError ->
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
