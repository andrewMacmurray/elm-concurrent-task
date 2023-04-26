module Concurrent.Task exposing
    ( Attempt
    , Definition
    , Error(..)
    , OnProgress
    , Pool
    , Progress
    , RawResults
    , Task
    , andMap
    , andThen
    , andThenDo
    , attempt
    , errorToString
    , expectJson
    , expectWhatever
    , fail
    , fromResult
    , isRunning
    , map
    , map2
    , map3
    , mapError
    , onError
    , onProgress
    , pool
    , succeed
    , task
    )

import Concurrent.Internal.Id as Id exposing (Id)
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
    { sequence : Id.Sequence
    , started : Set Id
    }


type alias Results =
    Dict Id Decode.Value


type Task_ x a
    = Pending (List Definition_) (Results -> Task_ x a)
    | Done (Result x a)


type alias Definition_ =
    { id : Id
    , function : String
    , args : Encode.Value
    }


type Expect a
    = ExpectJson (Decoder a)


type alias RawResults =
    { attempt : Id
    , results : List RawResult
    }


type alias RawResult =
    { id : Id
    , result : Decode.Value
    }


type Error
    = DecodeResponseError Decode.Error
    | JsException String
    | MissingFunction String
    | InternalError String



-- Model


init : Model
init =
    { sequence = Id.init
    , started = Set.empty
    }


nextId : Model -> Model
nextId model =
    { model | sequence = Id.next model.sequence }


combineSequences : Id.Sequence -> Model -> Model
combineSequences sequence model =
    { model | sequence = Id.combine sequence model.sequence }


recordSent : List Definition_ -> Model -> Model
recordSent defs model =
    { model | started = Set.union model.started (toSentIds defs) }


toSentIds : List Definition_ -> Set Id
toSentIds defs =
    Set.fromList (List.map .id defs)



-- Expect


expectJson : Decoder a -> Expect a
expectJson =
    ExpectJson


expectWhatever : Expect ()
expectWhatever =
    ExpectJson (Decode.succeed ())



-- Create


type alias Definition a =
    { function : String
    , args : Encode.Value
    , expect : Expect a
    }


task : Definition a -> Task Error a
task options =
    Task
        (\model ->
            let
                id : Id
                id =
                    Id.get model.sequence
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


decodeResponse : Expect value -> Decoder (Result Error value)
decodeResponse (ExpectJson expect) =
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\status ->
                case status of
                    "success" ->
                        Decode.field "value" (Decode.map Ok expect)

                    "error" ->
                        Decode.field "error" (Decode.map Err errorDecoder)

                    _ ->
                        Decode.succeed (Err (InternalError ("Unknown response status: " ++ status)))
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
                        Decode.succeed (InternalError ("Unknown error reason: " ++ reason))
            )


encodeDefinition : Id -> Definition_ -> Encode.Value
encodeDefinition attemptId def =
    Encode.object
        [ ( "id", Encode.string def.id )
        , ( "attempt", Encode.string attemptId )
        , ( "function", Encode.string def.function )
        , ( "args", def.args )
        ]


fromResult : Result x a -> Task x a
fromResult res =
    Task (\model -> ( Done res, model ))


fromResult_ : Result x a -> Task_ x a
fromResult_ =
    Done



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
            , combineSequences model1.sequence model2
            )
        )


map2_ : (a -> b -> c) -> Task_ x a -> Task_ x b -> Task_ x c
map2_ f task1 task2 =
    case ( task1, task2 ) of
        ( Done res1, Done res2 ) ->
            Done (Result.map2 f res1 res2)

        ( Done res1, Pending defs next ) ->
            haltOnError res1 (Pending defs (\res -> map2_ f (Done res1) (next res)))

        ( Pending defs next, Done res2 ) ->
            haltOnError res2 (Pending defs (\res -> map2_ f (next res) (Done res2)))

        ( Pending defs1 next1, Pending defs2 next2 ) ->
            Pending (defs1 ++ defs2) (\res -> map2_ f (next1 res) (next2 res))


haltOnError : Result x a -> Task_ x b -> Task_ x b
haltOnError res task_ =
    case res of
        Ok _ ->
            task_

        Err e ->
            fail_ e


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
    Task (\model -> ( fail_ e, model ))


fail_ : x -> Task_ x a
fail_ e =
    Done (Err e)


succeed : a -> Task x a
succeed a =
    Task (\model -> ( succeed_ a, model ))


succeed_ : a -> Task_ x a
succeed_ a =
    Done (Ok a)



-- Errors


onError : (x -> Task y a) -> Task x a -> Task y a
onError f (Task toTask) =
    Task
        (\model ->
            let
                ( task_, model1 ) =
                    toTask model

                next x =
                    unwrap (f x) model1
            in
            ( onError_ next task_
            , model1
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
                ( task_, model1 ) =
                    toTask model
            in
            ( mapError_ f task_
            , model1
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


errorToString : Error -> String
errorToString err =
    case err of
        DecodeResponseError error ->
            "DecodeResponseError: " ++ Decode.errorToString error

        JsException string ->
            "JsException: " ++ string

        MissingFunction string ->
            "MissingFunction: " ++ string

        InternalError string ->
            "InternalError: " ++ string



-- Execute a Task


type Pool x a
    = Pool (Pool_ x a)


type alias Pool_ x a =
    Dict Id (Progress x a)


type alias Attempt msg x a =
    { send : Encode.Value -> Cmd msg
    , id : Id
    , pool : Pool x a
    , onComplete : Id -> Result x a -> msg
    }


type alias OnProgress msg x a =
    { send : Encode.Value -> Cmd msg
    , receive : (RawResults -> msg) -> Sub msg
    , onComplete : Id -> Result x a -> msg
    , onProgress : ( Pool x a, Cmd msg ) -> msg
    }


attempt : Attempt msg x a -> Task x a -> ( Pool x a, Cmd msg )
attempt attempt_ (Task toTask) =
    case toTask init of
        ( Done res, _ ) ->
            ( attempt_.pool
            , sendResult attempt_.onComplete attempt_.id res
            )

        ( Pending defs next, model ) ->
            ( startAttempt attempt_.id
                ( Pending defs next
                , recordSent defs model
                )
                attempt_.pool
            , attempt_.send (Encode.list (encodeDefinition attempt_.id) defs)
            )


onProgress : OnProgress msg x a -> Pool x a -> Sub msg
onProgress options pool_ =
    options.receive
        (\result ->
            case findAttempt result.attempt pool_ of
                Just ( Pending _ next, model ) ->
                    case next (toResults result) of
                        Done res ->
                            case res of
                                Ok a ->
                                    options.onProgress
                                        ( removeFromPool result.attempt pool_
                                        , sendResult options.onComplete result.attempt (Ok a)
                                        )

                                Err e ->
                                    options.onProgress
                                        ( removeFromPool result.attempt pool_
                                        , sendResult options.onComplete result.attempt (Err e)
                                        )

                        Pending defs next_ ->
                            options.onProgress
                                ( updateProgressFor result.attempt
                                    ( Pending defs next_
                                    , model
                                        |> recordSent defs
                                        |> nextId
                                    )
                                    pool_
                                , defs
                                    |> List.filter (notStarted model)
                                    |> Encode.list (encodeDefinition result.attempt)
                                    |> options.send
                                )

                _ ->
                    options.onProgress ( pool_, Cmd.none )
        )


sendResult : (Id -> Result x a -> msg) -> Id -> Result x a -> Cmd msg
sendResult onComplete id res =
    CoreTask.succeed res |> CoreTask.perform (onComplete id)


notStarted : Model -> Definition_ -> Bool
notStarted model def =
    not (Set.member def.id model.started)


toResults : RawResults -> Results
toResults result =
    List.foldl addResponse Dict.empty result.results


addResponse : RawResult -> Results -> Results
addResponse r =
    Dict.insert r.id r.result



-- Pool


pool : Pool x a
pool =
    Pool Dict.empty


isRunning : Id -> Pool x a -> Bool
isRunning execution (Pool p) =
    Dict.member execution p


startAttempt : Id -> Progress x a -> Pool x a -> Pool x a
startAttempt execution progress =
    mapPool (Dict.insert execution progress)


updateProgressFor : Id -> Progress x a -> Pool x a -> Pool x a
updateProgressFor execution progress_ =
    mapPool (Dict.update execution (Maybe.map (always progress_)))


removeFromPool : Id -> Pool x a -> Pool x a
removeFromPool execution =
    mapPool (Dict.remove execution)


findAttempt : Id -> Pool x a -> Maybe (Progress x a)
findAttempt attemptId (Pool p) =
    Dict.get attemptId p


mapPool : (Pool_ x a -> Pool_ x a) -> Pool x a -> Pool x a
mapPool f (Pool p) =
    Pool (f p)
