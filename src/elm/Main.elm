port module Main exposing (main)

import Concurrent.Task as Task exposing (Task)
import Concurrent.Task.Http as Http
import Concurrent.Task.Random
import Concurrent.Task.Time
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Time



-- Program


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Flags =
    {}


type alias Model =
    { task : Task.Progress Task.Error String
    }


type Msg
    = OnResult (Result Task.Error String)
    | OnProgress ( Task.Progress Task.Error String, Cmd Msg )



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        ( progress, cmd ) =
            Task.attempt
                { send = send
                , onResult = OnResult
                }
                myCombo
    in
    ( { task = progress }
    , cmd
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnResult result ->
            let
                _ =
                    Debug.log "result" result
            in
            ( model, Cmd.none )

        OnProgress ( task, cmd ) ->
            ( { model | task = task }, cmd )



-- Task


randomTask : Task x Int
randomTask =
    Concurrent.Task.Random.generate (Random.int 0 100000000)


timeNowTask : Task x Time.Posix
timeNowTask =
    Concurrent.Task.Time.now


myHttpTask : Task Task.Error String
myHttpTask =
    Http.request
        { url = "https://jsonplaceholder.typicode.com/todos/1"
        , method = "GET"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.field "title" Decode.string)
        }


manyEnvs : Task Task.Error String
manyEnvs =
    Task.map3 join3
        (getEnv "ONE" |> Task.andThenDo (getEnv "TWO"))
        (getEnv "THREE")
        getHome
        |> Task.andThenDo (getEnv "USER")


myCombo : Task Task.Error String
myCombo =
    Task.map3 join3
        (waitThenDone 500
            |> Task.andThenDo (waitThenDone 500)
            |> Task.andThenDo (waitThenDone 1000)
        )
        (waitThenDone 1000
            |> Task.andThenDo (waitThenDone 750)
            |> Task.andThenDo (waitThenDone 100)
            |> Task.andThenDo (waitThenDone 100)
            |> Task.andThenDo (waitThenDone 100)
        )
        (waitThenDone 100)
        |> Task.andThenDo
            (Task.map2 join2
                (waitThenDone 100)
                (waitThenDone 200)
            )


waitThenDone : Int -> Task Task.Error String
waitThenDone ms =
    Http.request
        { url = "http://localhost:4000/wait-then-respond/" ++ String.fromInt ms
        , method = "GET"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.field "message" Decode.string)
        }


getHome : Task Task.Error String
getHome =
    getEnv "HOME"


getEnv : String -> Task Task.Error String
getEnv var =
    Task.ffi
        { function = "getEnv"
        , args = Encode.string var
        , expect = Decode.string
        }


slowInts : Task Task.Error String
slowInts =
    Task.map3 join3
        (doubleSlowInt 1)
        (doubleSlowInt 3)
        (doubleSlowInt 5)


doubleSlowInt : Int -> Task Task.Error String
doubleSlowInt i =
    Task.map2 join2
        (slowInt i)
        (slowInt (i + 1))


slowInt : Int -> Task Task.Error String
slowInt id =
    Task.ffi
        { function = "slowInt"
        , args = Encode.int id
        , expect = Decode.map String.fromInt Decode.int
        }


join3 : String -> String -> String -> String
join3 a b c =
    a ++ ", " ++ b ++ ", " ++ c


join2 : String -> String -> String
join2 a b =
    a ++ ", " ++ b



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Task.onProgress
        { send = send
        , receive = receive
        , onResult = OnResult
        , onProgress = OnProgress
        }
        model.task



-- Ports


port send : Decode.Value -> Cmd msg


port receive : ({ id : String, result : Decode.Value } -> msg) -> Sub msg
