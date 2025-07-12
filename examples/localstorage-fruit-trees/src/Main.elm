port module Main exposing (main)

import Browser
import ConcurrentTask exposing (ConcurrentTask)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Localstorage
import Ui.Palette as Palette
import Ui.Spacing as Spacing exposing (edges)
import Ui.Text as Text



-- Model


type alias Model =
    { tasks : Pool
    , basket : Fruits
    , trees : Fruits
    , error : Maybe TransactionError
    }


type Msg
    = OnComplete (ConcurrentTask.Response Error Output)
    | OnProgress ( Pool, Cmd Msg )
    | AddApplesClicked Int
    | AddOrangesClicked Int
    | AddPeachesClicked Int
    | AddPearsClicked Int


type alias Flags =
    { basket : Fruits
    , trees : Fruits
    }


type alias Fruits =
    { apples : Int
    , oranges : Int
    , peaches : Int
    , pears : Int
    }


updateApples : Int -> Fruits -> Fruits
updateApples n fruits =
    { fruits | apples = n }


updateOranges : Int -> Fruits -> Fruits
updateOranges n fruits =
    { fruits | oranges = n }


updatePeaches : Int -> Fruits -> Fruits
updatePeaches n fruits =
    { fruits | peaches = n }


updatePears : Int -> Fruits -> Fruits
updatePears n fruits =
    { fruits | pears = n }



-- Task Types


type alias Pool =
    ConcurrentTask.Pool Msg


type alias Error =
    TransactionError


type alias Output =
    TransactionSuccess



-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( tasks, cmd ) =
            startTask
                { pool = ConcurrentTask.pool
                , task = growMoreFruits
                }
    in
    ( { tasks = tasks
      , basket = flags.basket
      , trees = flags.trees
      , error = Nothing
      }
    , cmd
    )


startTask : { pool : Pool, task : ConcurrentTask Error Output } -> ( Pool, Cmd Msg )
startTask options =
    ConcurrentTask.attempt
        { send = send
        , pool = options.pool
        , onComplete = OnComplete
        }
        options.task



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddApplesClicked n ->
            let
                ( tasks, cmd ) =
                    startTask
                        { pool = model.tasks
                        , task = addFruit "apples" ApplesAdded n
                        }
            in
            ( { model | tasks = tasks }, cmd )

        AddOrangesClicked n ->
            let
                ( tasks, cmd ) =
                    startTask
                        { pool = model.tasks
                        , task = addFruit "oranges" OrangesAdded n
                        }
            in
            ( { model | tasks = tasks }, cmd )

        AddPeachesClicked n ->
            let
                ( tasks, cmd ) =
                    startTask
                        { pool = model.tasks
                        , task = addFruit "peaches" PeachesAdded n
                        }
            in
            ( { model | tasks = tasks }, cmd )

        AddPearsClicked n ->
            let
                ( tasks, cmd ) =
                    startTask
                        { pool = model.tasks
                        , task = addFruit "pears" PearsAdded n
                        }
            in
            ( { model | tasks = tasks }, cmd )

        OnComplete (ConcurrentTask.Success transaction) ->
            case transaction of
                FruitsGrown trees ->
                    ( { model | trees = trees }
                    , Cmd.none
                    )

                ApplesAdded ( basket, tree ) ->
                    ( { model
                        | basket = updateApples basket model.basket
                        , trees = updateApples tree model.trees
                      }
                    , Cmd.none
                    )

                OrangesAdded ( basket, tree ) ->
                    ( { model
                        | basket = updateOranges basket model.basket
                        , trees = updateOranges tree model.trees
                      }
                    , Cmd.none
                    )

                PeachesAdded ( basket, tree ) ->
                    ( { model
                        | basket = updatePeaches basket model.basket
                        , trees = updatePeaches tree model.trees
                      }
                    , Cmd.none
                    )

                PearsAdded ( basket, tree ) ->
                    ( { model
                        | basket = updatePears basket model.basket
                        , trees = updatePears tree model.trees
                      }
                    , Cmd.none
                    )

        OnComplete (ConcurrentTask.Error e) ->
            ( { model | error = Just e }, Cmd.none )

        OnComplete (ConcurrentTask.UnexpectedError e) ->
            ( { model | error = Just (UnexpectedError e) }, Cmd.none )

        OnProgress ( pool, cmd ) ->
            ( { model | tasks = pool }, cmd )



-- Fruits Transaction


type TransactionSuccess
    = ApplesAdded ( Int, Int )
    | OrangesAdded ( Int, Int )
    | PeachesAdded ( Int, Int )
    | PearsAdded ( Int, Int )
    | FruitsGrown Fruits


type TransactionError
    = ReadError Localstorage.ReadError
    | WriteError Localstorage.WriteError
    | TreeRunOutOfFruits
    | UnexpectedError ConcurrentTask.UnexpectedError


growMoreFruits : ConcurrentTask TransactionError TransactionSuccess
growMoreFruits =
    ConcurrentTask.map4 Fruits
        (getTree "apples")
        (getTree "oranges")
        (getTree "peaches")
        (getTree "pears")
        |> ConcurrentTask.map
            (\tree ->
                { tree
                    | apples = growFruit tree.apples
                    , oranges = growFruit tree.oranges
                    , peaches = growFruit tree.peaches
                    , pears = growFruit tree.pears
                }
            )
        |> ConcurrentTask.andThen
            (\tree ->
                ConcurrentTask.batch
                    [ setTree "apples" tree.apples
                    , setTree "oranges" tree.oranges
                    , setTree "peaches" tree.peaches
                    , setTree "pears" tree.pears
                    ]
                    |> ConcurrentTask.return (FruitsGrown tree)
            )


growFruit : Int -> Int
growFruit fruits =
    if fruits < 1 then
        fruits + 10

    else
        fruits


addFruit :
    String
    -> (( Int, Int ) -> TransactionSuccess)
    -> Int
    -> ConcurrentTask TransactionError TransactionSuccess
addFruit key onSuccess quantity =
    ConcurrentTask.map2 Tuple.pair
        (getBasket key)
        (getTree key)
        |> ConcurrentTask.andThen
            (\( basket, tree ) ->
                if tree - quantity < 0 then
                    ConcurrentTask.fail TreeRunOutOfFruits

                else
                    ConcurrentTask.succeed
                        ( basket + quantity
                        , tree - quantity
                        )
            )
        |> ConcurrentTask.andThen
            (\( basket, tree ) ->
                ConcurrentTask.batch
                    [ setBasket key basket
                    , setTree key tree
                    ]
                    |> ConcurrentTask.return (onSuccess ( basket, tree ))
            )


getBasket : String -> ConcurrentTask TransactionError Int
getBasket key =
    getFruit ("basket:" ++ key)


setBasket : String -> Int -> ConcurrentTask TransactionError ()
setBasket key =
    setFruit ("basket:" ++ key)


getTree : String -> ConcurrentTask TransactionError Int
getTree key =
    getFruit ("tree:" ++ key)


setTree : String -> Int -> ConcurrentTask TransactionError ()
setTree key =
    setFruit ("tree:" ++ key)


getFruit : String -> ConcurrentTask TransactionError Int
getFruit key =
    Localstorage.getItem key Decode.int
        |> ConcurrentTask.onError
            (\e ->
                case e of
                    Localstorage.NoValue ->
                        ConcurrentTask.succeed 0

                    _ ->
                        ConcurrentTask.fail (ReadError e)
            )


setFruit : String -> Int -> ConcurrentTask TransactionError ()
setFruit key n =
    Localstorage.setItem key (Encode.int n)
        |> ConcurrentTask.mapError WriteError



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
            , spacing Spacing.l
            , centerX
            ]
            [ column [ width fill, spacing Spacing.s ]
                [ Text.text [ Text.f1 ] "Fruit Basket"
                , Text.text [] "Add fruits to your basket!"
                , row [ spacing Spacing.m ]
                    [ basketFruit
                        { icon = "🍎"
                        , text = "Apples"
                        , color = Palette.red
                        , addClicked = AddApplesClicked
                        , value = model.basket.apples
                        }
                    , basketFruit
                        { icon = "🍊"
                        , text = "Oranges"
                        , color = Palette.orange
                        , addClicked = AddOrangesClicked
                        , value = model.basket.oranges
                        }
                    , basketFruit
                        { icon = "🍑"
                        , text = "Peaches"
                        , color = Palette.peach
                        , addClicked = AddPeachesClicked
                        , value = model.basket.peaches
                        }
                    , basketFruit
                        { icon = "🍐"
                        , text = "Pears"
                        , color = Palette.green
                        , addClicked = AddPearsClicked
                        , value = model.basket.pears
                        }
                    ]
                ]
            , column [ width fill, spacing Spacing.m ]
                [ Text.text [ Text.f1 ] "Fruit Trees"
                , row [ spacing Spacing.m ]
                    [ fruitTree
                        { icon = "🌳🍎"
                        , color = Palette.red
                        , value = model.trees.apples
                        }
                    , fruitTree
                        { icon = "🌳🍊"
                        , color = Palette.orange
                        , value = model.trees.oranges
                        }
                    , fruitTree
                        { icon = "🌳🍑"
                        , color = Palette.peach
                        , value = model.trees.peaches
                        }
                    , fruitTree
                        { icon = "🌳🍐"
                        , color = Palette.green
                        , value = model.trees.pears
                        }
                    ]
                , viewMaybe viewError model.error
                ]
            ]
        )


viewError : TransactionError -> Element msg
viewError error =
    case error of
        TreeRunOutOfFruits ->
            Text.error [] "Tree ran out of fruits, try refreshing the page"

        ReadError readError ->
            Text.error [] ("Localstorage Read Error: " ++ Debug.toString readError)

        WriteError writeError ->
            Text.error [] ("Localstorage Write Error: " ++ Debug.toString writeError)

        UnexpectedError unexpectedError ->
            Text.error [] ("Something went wrong: " ++ Debug.toString unexpectedError)


viewMaybe : (a -> Element msg) -> Maybe a -> Element msg
viewMaybe f =
    Maybe.map f >> Maybe.withDefault Element.none


basketFruit :
    { icon : String
    , text : String
    , color : Element.Color
    , addClicked : Int -> Msg
    , value : Int
    }
    -> Element Msg
basketFruit options =
    column [ spacing Spacing.xs, width (px 75) ]
        [ Text.text [ Text.f1 ] options.icon
        , Text.text [ Text.f6, Font.color options.color ] options.text
        , Text.text [ Text.f1, Font.color options.color ] (String.fromInt options.value)
        , Input.button
            [ Border.color options.color
            , Border.width 2
            , Border.rounded 5
            , paddingEach
                { top = 3
                , bottom = 5
                , left = Spacing.xs
                , right = Spacing.xs
                }
            ]
            { onPress = Just (options.addClicked 1)
            , label =
                Text.text
                    [ Text.f3
                    , centerX
                    , centerY
                    , Text.color options.color
                    ]
                    "+"
            }
        ]


fruitTree : { icon : String, color : Color, value : Int } -> Element msg
fruitTree options =
    column [ spacing Spacing.xs, width (px 75) ]
        [ Text.text [ Text.f1 ] options.icon
        , Text.text
            [ paddingEach { edges | left = 7 }
            , Text.f1
            , Font.color options.color
            ]
            (String.fromInt options.value)
        ]



-- Program


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
