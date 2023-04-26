port module Main exposing (main)

import Concurrent.Task as Task exposing (Task)
import Concurrent.Task.Http as Http
import Concurrent.Task.Process
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
    { tasks : Task.Pool Error String
    }


type Msg
    = OnFireMany Int
    | OnManualEnter String
    | OnComplete String (Result Error String)
    | OnProgress ( Task.Pool Error String, Cmd Msg )


type Error
    = HttpError Http.Error
    | TaskError Task.Error



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { tasks = Task.pool }
    , Cmd.none
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnManualEnter id ->
            let
                ( tasks, cmd ) =
                    Task.attempt
                        { send = send
                        , onComplete = OnComplete
                        , id = id
                        , pool = model.tasks
                        }
                        (getProfile |> Task.andThenDo clientRequest)
            in
            ( { tasks = tasks }, cmd )

        OnFireMany id ->
            let
                ( tasks, cmd ) =
                    Task.attempt
                        { send = send
                        , onComplete = OnComplete
                        , id = String.fromInt id
                        , pool = model.tasks
                        }
                        (slowSequence id)
            in
            ( { tasks = tasks }, cmd )

        OnComplete id result ->
            let
                _ =
                    Debug.log ("result for " ++ id) result
            in
            ( model, Cmd.none )

        OnProgress ( task, cmd ) ->
            ( { model | tasks = task }, cmd )



-- Test
-- Load
-- (getProfile: (config
-- |> andThen (getAccessToken: (oauthRequest: (httpRequest: (task |> mapError |> fromResult) |> mapError)))
-- |> andThen (getProfileData: (httpRequest |> mapError)
-- (config: (map2 f (load) (oauthCredentialsFor)))
--
-- (load: (loadEnv: task |> mapError) |> andThen |> fromResult |> mapError)
-- (loadWith: (load |> andThen)
-- (oauth2CredentialsFor: (loadWith |> (failOnMissingOAuth2: succeed || fail |> mapError))


clientRequest =
    Task.andThen identity
        (Task.map2 (\a b -> getEnv "USER")
            (Task.map identity (getEnv "HOME"))
            load
        )


getProfile =
    config
        |> Task.andThen getAccessToken
        |> Task.andThen getProfileData


getAccessToken _ =
    --longRequest 10
    getEnv "HOME"
        |> Task.mapError identity


getProfileData _ =
    --longRequest 10
    getEnv "HOME"
        |> Task.mapError identity


config =
    Task.map2 join2
        load
        oauth2CredentialsFor


oauth2CredentialsFor : Task Error String
oauth2CredentialsFor =
    loadWith
        (\_ ->
            failOnNothing
                (TaskError (Task.InternalError "Waa"))
                (Just "hello")
        )


failOnNothing e =
    Maybe.map Task.succeed >> Maybe.withDefault (Task.fail e)


loadWith f =
    load |> Task.andThen f


load =
    getEnv "HOME"
        |> Task.andThenDo (Task.fromResult (Ok "hello"))
        |> Task.mapError identity



-- Task


randomTask : Task x Int
randomTask =
    Concurrent.Task.Random.generate (Random.int 0 100000000)


timeNowTask : Task x Time.Posix
timeNowTask =
    Concurrent.Task.Time.now


sleep : Int -> Task x ()
sleep =
    Concurrent.Task.Process.sleep


myHttpTask : Task Error String
myHttpTask =
    request
        { url = "https://jsonplaceholder.typicode.com/todos/1"
        , method = "GET"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.field "title" Decode.string)
        }


manyEnvs : Task Error String
manyEnvs =
    Task.map3 join3
        (getEnv "ONE" |> Task.andThenDo (getEnv "TWO"))
        (getEnv "THREE")
        getHome
        |> Task.andThenDo getUser


slowSequence : Int -> Task Error String
slowSequence i =
    longRequest 1000
        |> Task.andThenDo (longRequest 1000)
        |> Task.andThenDo (longRequest 1000)
        |> Task.andThenDo (longRequest 1000)
        |> Task.andThenDo (longRequest i)


httpCombo : Task Error String
httpCombo =
    Task.map3 join3
        (longRequest 500
            |> Task.andThenDo (longRequest 500)
            |> Task.andThenDo (longRequest 50)
            |> Task.andThenDo (longRequest 50)
            |> Task.andThenDo (longRequest 20)
        )
        (longRequest 100
            |> Task.andThenDo (longRequest 100)
            |> Task.andThenDo (longRequest 500)
        )
        (Task.map2 join2
            (longRequest 70)
            (longRequest 80)
        )
        |> Task.andThen
            (\res ->
                Task.map (join2 res)
                    (Task.map3 join3
                        (longRequest 50)
                        (longRequest 100)
                        (longRequest 200)
                    )
            )


longRequest : Int -> Task Error String
longRequest ms =
    request
        { url = "http://localhost:4000/wait-then-respond/" ++ String.fromInt ms
        , method = "GET"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.field "message" Decode.string)
        }


getBigFile : Task Error String
getBigFile =
    request
        { url = "http://localhost:4000/big-file"
        , method = "GET"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson Decode.string
        }


httpError : Task Error String
httpError =
    request
        { url = "http://localhost:4000/boom"
        , method = "GET"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.succeed "whatever")
        }


request : Http.Request a -> Task Error a
request =
    Http.request >> Task.mapError HttpError


getHome : Task Error String
getHome =
    getEnv "HOME"


getUser : Task Error String
getUser =
    getEnv "USER"


getEnv : String -> Task Error String
getEnv var =
    Task.mapError TaskError
        (Task.task
            { function = "getEnv"
            , args = Encode.string var
            , expect = Task.expectJson Decode.string
            }
        )


slowInts : Task Error String
slowInts =
    Task.map3 join3
        (doubleSlowInt 1)
        (doubleSlowInt 3)
        (doubleSlowInt 5)


doubleSlowInt : Int -> Task Error String
doubleSlowInt i =
    Task.map2 join2
        (slowInt i)
        (slowInt (i + 1))


slowInt : Int -> Task Error String
slowInt id =
    Task.mapError TaskError
        (Task.task
            { function = "slowInt"
            , args = Encode.int id
            , expect = Task.expectJson (Decode.map String.fromInt Decode.int)
            }
        )



-- Retry


retry : Int -> Task b a -> Task b a
retry n task =
    retry_ (Task.mapError (Tuple.pair n) task)
        |> Task.mapError Tuple.second


retry_ : Task ( Int, x ) a -> Task ( Int, x ) a
retry_ task =
    task
        |> Task.onError
            (\( n, err ) ->
                if n > 0 then
                    task
                        |> Task.mapError (Tuple.mapFirst (\n_ -> n_ - 1))
                        |> retry_

                else
                    Task.fail ( n, err )
            )



-- Helpers


join3 : String -> String -> String -> String
join3 a b c =
    a ++ ", " ++ b ++ ", " ++ c


join2 : String -> String -> String
join2 a b =
    a ++ ", " ++ b



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ manualEnter OnManualEnter
        , fireMany OnFireMany
        , Task.onProgress
            { send = send
            , receive = receive
            , onComplete = OnComplete
            , onProgress = OnProgress
            }
            model.tasks
        ]



-- Ports


port send : Decode.Value -> Cmd msg


port receive : (Task.RawResults -> msg) -> Sub msg


port manualEnter : (String -> msg) -> Sub msg


port fireMany : (Int -> msg) -> Sub msg
