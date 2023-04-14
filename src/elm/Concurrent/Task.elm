module Concurrent.Task exposing
    ( Definition
    , Error(..)
    , Progress
    , Response
    , Task
    , andMap
    , andThen
    , andThenDo
    , attempt
    , fail
    , fromResult
    , map
    , map2
    , map3
    , mapError
    , onError
    , onProgress
    , succeed
    , task
    )

import Concurrent.Ids as Ids
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)
import Task as CoreTask



-- Task


type Task x a
    = Task (Model -> Progress x a)


type alias Progress x a =
    ( Task_ x a, Model )


type alias Model =
    { ids : Ids.Sequence
    , started : Set Ids.Id
    , results : Results
    }


type alias Results =
    Dict Ids.Id Decode.Value


type Task_ x a
    = Pending (List Definition) (Results -> Task_ x a)
    | Done (Result x a)


type alias Definition =
    { id : Ids.Id
    , function : String
    , args : Encode.Value
    }


type alias Expect a =
    Decoder a


type alias Response =
    { id : Ids.Id
    , result : Decode.Value
    }


type Error
    = DecodeResponseError Decode.Error
    | JsException String
    | MissingFunction String
    | UnknownStatus String



-- Model


init : Model
init =
    { ids = Ids.init
    , started = Set.empty
    , results = Dict.empty
    }


nextId : Model -> Model
nextId model =
    { model | ids = Ids.next model.ids }


combineSequences : Ids.Sequence -> Model -> Model
combineSequences ids model =
    { model | ids = Ids.combine ids model.ids }


recordSent : List Definition -> Model -> Model
recordSent defs model =
    { model | started = Set.union model.started (toSentIds defs) }


withResponses : Results -> Model -> Model
withResponses responses model =
    { model | results = responses }


toSentIds : List Definition -> Set Ids.Id
toSentIds defs =
    Set.fromList (List.map .id defs)



-- Create


type alias Ffi a =
    { function : String
    , args : Encode.Value
    , expect : Expect a
    }


task : Ffi a -> Task Error a
task options =
    Task
        (\model ->
            let
                id : Ids.Id
                id =
                    Ids.get model.ids
            in
            ( Pending
                [ { id = id
                  , function = options.function
                  , args = options.args
                  }
                ]
                (\results ->
                    case Dict.get id results of
                        Just res ->
                            res
                                |> Decode.decodeValue (decodeResponse options.expect)
                                |> Result.mapError DecodeResponseError
                                |> Result.andThen identity
                                |> fromResult_

                        Nothing ->
                            unwrap (task options) model
                )
            , nextId model
            )
        )


decodeResponse : Decoder value -> Decoder (Result Error value)
decodeResponse expect =
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\status ->
                case status of
                    "success" ->
                        Decode.field "value" (Decode.map Ok expect)

                    "error" ->
                        Decode.field "error" (Decode.map Err errorDecoder)

                    _ ->
                        Decode.succeed (Err (UnknownStatus status))
            )


errorDecoder : Decoder Error
errorDecoder =
    Decode.field "reason" Decode.string
        |> Decode.andThen
            (\reason ->
                case reason of
                    "js_exception" ->
                        Decode.field "message" (Decode.map JsException Decode.string)

                    "missing_function" ->
                        Decode.field "message" (Decode.map MissingFunction Decode.string)

                    _ ->
                        Decode.succeed (UnknownStatus ("Unknown error reason: " ++ reason))
            )


encodeDefinition : Definition -> Encode.Value
encodeDefinition definition =
    Encode.object
        [ ( "id", Encode.string definition.id )
        , ( "function", Encode.string definition.function )
        , ( "args", definition.args )
        ]


fromResult : Result x a -> Task x a
fromResult res =
    case res of
        Ok a ->
            succeed a

        Err e ->
            fail e


fromResult_ : Result x a -> Task_ x a
fromResult_ res =
    case res of
        Ok a ->
            succeed_ a

        Err e ->
            fail_ e



-- Maps


map : (a -> b) -> Task x a -> Task x b
map f (Task toTask) =
    Task
        (\model ->
            let
                ( task_, model1 ) =
                    toTask model
            in
            ( map_ f task_, model1 )
        )


map_ : (a -> b) -> Task_ x a -> Task_ x b
map_ f task_ =
    case task_ of
        Done res ->
            Done (Result.map f res)

        Pending defs next ->
            Pending defs (next >> map_ f)


map2 : (a -> b -> c) -> Task x a -> Task x b -> Task x c
map2 f (Task toTask1) (Task toTask2) =
    Task
        (\model ->
            let
                ( task1_, model1 ) =
                    toTask1 model

                ( task2_, model2 ) =
                    toTask2 model1
            in
            ( map2_ f task1_ task2_
            , combineSequences model1.ids model2
            )
        )


map2_ : (a -> b -> c) -> Task_ x a -> Task_ x b -> Task_ x c
map2_ f task1 task2 =
    case ( task1, task2 ) of
        ( Done res1, Done res2 ) ->
            Done (Result.map2 f res1 res2)

        ( Done res1, Pending defs next ) ->
            Pending defs (\res -> map2_ f (Done res1) (next res))

        ( Pending defs next, Done res2 ) ->
            Pending defs (\res -> map2_ f (next res) (Done res2))

        ( Pending defs1 next1, Pending defs2 next2 ) ->
            Pending (defs1 ++ defs2) (\res -> map2_ f (next1 res) (next2 res))


andMap : Task x a -> Task x (a -> b) -> Task x b
andMap =
    map2 (|>)


map3 : (a -> b -> c -> d) -> Task x a -> Task x b -> Task x c -> Task x d
map3 f task1 task2 task3 =
    succeed f
        |> andMap task1
        |> andMap task2
        |> andMap task3



-- Chains


andThen : (a -> Task x b) -> Task x a -> Task x b
andThen f (Task toTask) =
    Task
        (\model ->
            let
                ( task_, model1 ) =
                    toTask model

                next a =
                    unwrap (f a) model1
            in
            ( andThen_ next task_
            , nextId model1
            )
        )


andThen_ : (a -> Task_ x b) -> Task_ x a -> Task_ x b
andThen_ f task_ =
    case task_ of
        Done res ->
            case res of
                Ok a ->
                    f a

                Err e ->
                    fail_ e

        Pending defs next ->
            Pending defs (next >> andThen_ f)


andThenDo : Task x b -> Task x a -> Task x b
andThenDo task2 task1 =
    task1 |> andThen (\_ -> task2)


fail : a -> Task a b
fail e =
    Task (\_ -> ( fail_ e, init ))


fail_ : x -> Task_ x a
fail_ e =
    Done (Err e)


succeed : a -> Task x a
succeed a =
    Task (\_ -> ( succeed_ a, init ))


succeed_ : a -> Task_ x a
succeed_ a =
    Done (Ok a)



-- Errors


onError : (x -> Task y a) -> Task x a -> Task y a
onError f (Task toTask) =
    Task
        (\model ->
            let
                ( task_, _ ) =
                    toTask model

                next x =
                    unwrap (f x) model
            in
            ( onError_ next task_
            , model
            )
        )


onError_ : (x -> Task_ y a) -> Task_ x a -> Task_ y a
onError_ f task_ =
    case task_ of
        Done res ->
            case res of
                Ok a ->
                    Done (Ok a)

                Err e ->
                    f e

        Pending defs next ->
            Pending defs (next >> onError_ f)


mapError : (x -> y) -> Task x a -> Task y a
mapError f (Task toTask) =
    Task
        (\model ->
            let
                ( task_, _ ) =
                    toTask model
            in
            ( mapError_ f task_
            , model
            )
        )


mapError_ : (x -> y) -> Task_ x a -> Task_ y a
mapError_ f task_ =
    case task_ of
        Done res ->
            Done (Result.mapError f res)

        Pending defs next ->
            Pending defs (next >> mapError_ f)


unwrap : Task x a -> Model -> Task_ x a
unwrap (Task toTask) model =
    Tuple.first (toTask model)



-- Attempt


type alias Attempt msg x a =
    { send : Encode.Value -> Cmd msg
    , onComplete : Result x a -> msg
    }


type alias OnProgress msg x a =
    { send : Encode.Value -> Cmd msg
    , receive : (List Response -> msg) -> Sub msg
    , onComplete : Result x a -> msg
    , onProgress : ( Progress x a, Cmd msg ) -> msg
    }


attempt : Attempt msg x a -> Task x a -> ( Progress x a, Cmd msg )
attempt options (Task toTask) =
    case toTask init of
        ( Done res, model ) ->
            ( ( Done res, model )
            , CoreTask.succeed res
                |> CoreTask.perform options.onComplete
            )

        ( Pending defs next, model ) ->
            ( ( Pending defs next
              , recordSent defs model
              )
            , options.send (Encode.list encodeDefinition defs)
            )


onProgress : OnProgress msg x a -> Progress x a -> Sub msg
onProgress options ( task_, model ) =
    case task_ of
        Done res ->
            options.receive (\_ -> options.onComplete res)

        Pending defs_ next ->
            options.receive
                (\results ->
                    let
                        updatedResponses =
                            List.foldl addResponse model.results results
                    in
                    case next updatedResponses of
                        Done res ->
                            options.onComplete res

                        Pending defs next_ ->
                            options.onProgress
                                ( ( Pending defs next_
                                  , model
                                        |> withResponses (clearDoneResponses defs_ updatedResponses)
                                        |> recordSent defs
                                        |> nextId
                                  )
                                , defs
                                    |> List.filter (\d -> not (Set.member d.id model.started))
                                    |> Encode.list encodeDefinition
                                    |> options.send
                                )
                )


clearDoneResponses : List Definition -> Results -> Results
clearDoneResponses defs =
    Dict.filter (\id _ -> not (List.member id (List.map .id defs)))


addResponse : Response -> Results -> Results
addResponse r results =
    Dict.insert r.id r.result results
