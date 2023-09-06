port module Main exposing (main)

import Browser
import ConcurrentTask exposing (ConcurrentTask)
import Element exposing (..)
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as Decode
import Ui.Spacing as Spacing
import Ui.Text as Text



-- Program


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type alias Model =
    { tasks : Pool
    }


type Msg
    = OnComplete (ConcurrentTask.Response Error Output)
    | OnProgress ( Pool, Cmd Msg )


type alias Flags =
    ()



-- Task Types


type alias Pool =
    ConcurrentTask.Pool Msg Error Output


type alias Error =
    ()


type alias Output =
    ()



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { tasks = ConcurrentTask.pool
      }
    , Cmd.none
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnComplete res ->
            ( model, Cmd.none )

        OnProgress ( pool, cmd ) ->
            ( { model | tasks = pool }, cmd )



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



-- View


view : Model -> Html Msg
view model =
    Element.layout [ Text.fonts ]
        (column
            [ width (fill |> maximum 800)
            , paddingXY Spacing.s Spacing.s
            , spacing Spacing.m
            , centerX
            ]
            [ Text.text [ Text.f1 ] "Fruit Basket"
            , row [ spacing Spacing.s ]
                [ fruit { text = "Apples 🍎", color = Element.rgb255 255 0 0 }
                , fruit { text = "Oranges 🍊", color = Element.rgb255 217 133 7 }
                , fruit { text = "Peaches 🍑", color = Element.rgb255 252 26 89 }
                , fruit { text = "Pears 🍐", color = Element.rgb255 70 163 115 }
                ]
            ]
        )


fruit : { text : String, color : Element.Color } -> Element msg
fruit options =
    column [ spacing Spacing.xs ]
        [ Text.text [ Text.f4, Font.color options.color ] options.text
        , Text.text [ Text.f2, Font.color options.color ] "0"
        ]
