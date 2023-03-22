module Main exposing (main)

import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode
import Json.Encode as Encode



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
    {}


type Msg
    = OnResult (Result Task.Error Int)



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( {}
    , Task.attempt OnResult myTask
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnResult result ->
            let
                _ =
                    Debug.log "res" result
            in
            ( model, Cmd.none )



-- Task


myTask : Task Decode.Error Int
myTask =
    Task.map3 (\a b c -> a + b + c)
        doubleSlowInt
        doubleSlowInt
        slowInt


doubleSlowInt : Task Decode.Error Int
doubleSlowInt =
    slowInt |> Task.andThenDo (Task.map2 (+) slowInt slowInt)


slowInt : Task Decode.Error Int
slowInt =
    Task.ffi
        { function = "slowInt"
        , args = Encode.null
        , expect = Decode.int
        }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
