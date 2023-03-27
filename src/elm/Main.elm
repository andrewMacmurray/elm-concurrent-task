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
    = OnResult (Result Task.Error String)



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


myTask : Task Decode.Error String
myTask =
    Task.map4 (\a b c d -> a ++ ", " ++ b ++ ", " ++ c ++ ", " ++ d)
        (doubleSlowInt 1)
        (doubleSlowInt 3)
        (doubleSlowInt 5)
        (doubleSlowInt 7)


doubleSlowInt : Int -> Task Decode.Error String
doubleSlowInt i =
    Task.map2 (\a b -> a ++ ", " ++ b) (slowInt i) (slowInt (i + 1))


slowInt : Int -> Task Decode.Error String
slowInt id =
    Task.ffi
        { function = "slowInt"
        , args = Encode.int id
        , expect = Decode.int |> Decode.map String.fromInt
        }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
