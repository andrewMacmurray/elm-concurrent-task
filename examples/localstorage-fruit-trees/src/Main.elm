port module Main exposing (main)

import Browser
import ConcurrentTask exposing (ConcurrentTask)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Localstorage



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
    H.div
        [ A.class "col gap-l pa-s center"
        , A.style "max-width" "800px"
        ]
        [ H.div [ A.class "col gap-s" ]
            [ H.p [ A.class "f1" ] [ H.text "Fruit Basket" ]
            , H.p [ A.class "f3" ] [ H.text "Add fruits to your basket" ]
            , H.div [ A.class "row gap-m" ]
                [ basketFruit
                    { icon = "🍎"
                    , text = "Apples"
                    , color = "var(--red)"
                    , addClicked = AddApplesClicked
                    , value = model.basket.apples
                    }
                , basketFruit
                    { icon = "🍊"
                    , text = "Oranges"
                    , color = "var(--orange)"
                    , addClicked = AddOrangesClicked
                    , value = model.basket.oranges
                    }
                , basketFruit
                    { icon = "🍑"
                    , text = "Peaches"
                    , color = "var(--peach)"
                    , addClicked = AddPeachesClicked
                    , value = model.basket.peaches
                    }
                , basketFruit
                    { icon = "🍐"
                    , text = "Pears"
                    , color = "var(--green)"
                    , addClicked = AddPearsClicked
                    , value = model.basket.pears
                    }
                ]
            , H.div [ A.class "col gap-m" ]
                [ H.p [ A.class "f1" ] [ H.text "Fruit Trees" ]
                , H.div [ A.class "row gap-m" ]
                    [ fruitTree
                        { icon = "🌳🍎"
                        , color = "var(--red)"
                        , value = model.trees.apples
                        }
                    , fruitTree
                        { icon = "🌳🍊"
                        , color = "var(--orange)"
                        , value = model.trees.oranges
                        }
                    , fruitTree
                        { icon = "🌳🍑"
                        , color = "var(--peach)"
                        , value = model.trees.peaches
                        }
                    , fruitTree
                        { icon = "🌳🍐"
                        , color = "var(--green)"
                        , value = model.trees.pears
                        }
                    ]
                ]
            , viewMaybe viewError model.error
            ]
        ]


viewError : TransactionError -> Html msg
viewError error =
    case error of
        TreeRunOutOfFruits ->
            H.p [ A.class "error" ] [ H.text "Tree ran out of fruits, try refreshing the page" ]

        ReadError readError ->
            H.p [ A.class "error" ] [ H.text ("Localstorage Read Error: " ++ Debug.toString readError) ]

        WriteError writeError ->
            H.p [ A.class "error" ] [ H.text ("Localstorage Write Error: " ++ Debug.toString writeError) ]

        UnexpectedError unexpectedError ->
            H.p [ A.class "error" ] [ H.text ("Something went wrong: " ++ Debug.toString unexpectedError) ]


viewMaybe : (a -> Html msg) -> Maybe a -> Html msg
viewMaybe f =
    Maybe.map f >> Maybe.withDefault (H.text "")


basketFruit :
    { icon : String
    , text : String
    , color : String
    , addClicked : Int -> Msg
    , value : Int
    }
    -> Html Msg
basketFruit options =
    H.div [ A.class "col gap-xs", A.style "width" "75px" ]
        [ H.p [ A.class "f1" ] [ H.text options.icon ]
        , H.p [ A.class "f3", A.style "color" options.color ] [ H.text options.text ]
        , H.p [ A.class "f1", A.style "color" options.color ] [ H.text (String.fromInt options.value) ]
        , H.button
            [ A.style "border" "solid 2px"
            , A.style "border-color" options.color
            , A.style "border-radius" "5px"
            , A.style "background-color" "white"
            , A.class "ph-xs pv-xxs"
            , E.onClick (options.addClicked 1)
            ]
            [ H.p [ A.class "f3", A.style "color" options.color ] [ H.text "+" ]
            ]
        ]


fruitTree : { icon : String, color : String, value : Int } -> Html msg
fruitTree options =
    H.div [ A.class "col gap-xs", A.style "width" "75px" ]
        [ H.p [ A.class "f1" ] [ H.text options.icon ]
        , H.p [ A.class "f1 ph-xxs", A.style "color" options.color ] [ H.text (String.fromInt options.value) ]
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
