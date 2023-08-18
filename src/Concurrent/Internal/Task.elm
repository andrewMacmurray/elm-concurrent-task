module Concurrent.Internal.Task exposing
    ( Attempt
    , Definition
    , Error(..)
    , Expect
    , OnProgress
    , Pool
    , Results
    , Task(..)
    , Task_(..)
    , andMap
    , andThen
    , andThenDo
    , attempt
    , batch
    , define
    , doBatch
    , errorToString
    , expectJson
    , expectString
    , expectWhatever
    , fail
    , fromResult
    , map
    , map2
    , map3
    , map4
    , map5
    , mapError
    , onError
    , onProgress
    , pool
    , return
    , sequence
    , succeed
    )

import Array exposing (Array)
import Concurrent.Internal.Ids as Ids exposing (Ids)
import Concurrent.Internal.Utils.List
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)
import Task as CoreTask



-- Task


type Task x a
    = Task (Results -> Ids -> ( Ids, Task_ x a ))


type Task_ x a
    = Pending (Array Todo) (Task x a)
    | Done (Result x a)


type alias TaskId =
    Ids.Id


type alias Todo =
    { taskId : TaskId
    , function : String
    , args : Encode.Value
    }


type alias Results =
    Dict TaskId Decode.Value


type Expect a
    = ExpectJson (Decoder a)


type Error
    = ResponseError Decode.Error
    | JsException String
    | MissingFunction String
    | UnknownError String



-- Expect


expectJson : Decoder a -> Expect a
expectJson =
    ExpectJson


expectString : Expect String
expectString =
    ExpectJson Decode.string


expectWhatever : Expect ()
expectWhatever =
    ExpectJson (Decode.succeed ())



-- Define a Task


type alias Definition a =
    { function : String
    , args : Encode.Value
    , expect : Expect a
    }


define : Definition a -> Task Error a
define def =
    Task
        (\results ids ->
            let
                taskId =
                    Ids.get ids
            in
            ( Ids.next ids
            , Pending
                (Array.fromList
                    [ { taskId = taskId
                      , function = def.function
                      , args = def.args
                      }
                    ]
                )
                (case Dict.get taskId results of
                    Just result ->
                        result
                            |> Decode.decodeValue (decodeResult def.expect)
                            |> Result.mapError ResponseError
                            |> Result.andThen identity
                            |> fromResult

                    Nothing ->
                        runWith ids (define def)
                )
            )
        )


runWith : Ids -> Task x a -> Task x a
runWith s (Task run) =
    Task (\res _ -> run res s)



-- Maps


map : (a -> b) -> Task x a -> Task x b
map f (Task run) =
    Task
        (\result ids ->
            let
                ( ids_, task ) =
                    run result ids
            in
            ( ids_
            , case task of
                Pending defs next ->
                    Pending defs (map f next)

                Done a ->
                    Done (Result.map f a)
            )
        )


andMap : Task x a -> Task x (a -> b) -> Task x b
andMap (Task run1) (Task run2) =
    Task
        (\res ids ->
            let
                ( ids_, task2 ) =
                    run2 res ids

                ( ids__, task1 ) =
                    run1 res ids_
            in
            ( Ids.combine ids_ ids__
            , case ( task1, task2 ) of
                ( Pending defs1 next1, Pending defs2 next2 ) ->
                    Pending (Array.append defs1 defs2) (andMap next1 next2)

                ( Pending defs next1, Done b ) ->
                    haltOnError b (Pending defs (andMap next1 (fromResult b)))

                ( Done a, Pending defs next2 ) ->
                    haltOnError a (Pending defs (andMap (fromResult a) next2))

                ( Done a, Done b ) ->
                    Done (Result.map2 (|>) a b)
            )
        )


haltOnError : Result x a -> Task_ x b -> Task_ x b
haltOnError res task =
    case res of
        Err e ->
            Done (Err e)

        Ok _ ->
            task


map2 : (a -> b -> c) -> Task x a -> Task x b -> Task x c
map2 f t1 t2 =
    succeed f
        |> andMap t1
        |> andMap t2


map3 : (a -> b -> c -> d) -> Task x a -> Task x b -> Task x c -> Task x d
map3 f t1 t2 t3 =
    succeed f
        |> andMap t1
        |> andMap t2
        |> andMap t3


map4 : (a -> b -> c -> d -> e) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e
map4 f t1 t2 t3 t4 =
    succeed f
        |> andMap t1
        |> andMap t2
        |> andMap t3
        |> andMap t4


map5 : (a -> b -> c -> d -> e -> f) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x f
map5 f t1 t2 t3 t4 t5 =
    succeed f
        |> andMap t1
        |> andMap t2
        |> andMap t3
        |> andMap t4
        |> andMap t5



-- Sequence


sequence : List (Task x a) -> Task x (List a)
sequence tasks =
    sequenceHelp tasks (succeed []) |> map List.reverse


sequenceHelp : List (Task x a) -> Task x (List a) -> Task x (List a)
sequenceHelp tasks combined =
    case tasks of
        task :: rest ->
            combined |> andThen (\xs -> sequenceHelp rest (map (\x -> x :: xs) task))

        [] ->
            combined



-- Batch


batch : List (Task x a) -> Task x (List a)
batch tasks =
    tasks
        |> miniBatchesOf 10
        |> miniBatchesOf 10
        |> miniBatchesOf 10
        |> miniBatchesOf 10
        |> miniBatchesOf 10
        |> miniBatchesOf 10
        |> doBatch
        |> map
            (List.concat
                >> List.concat
                >> List.concat
                >> List.concat
                >> List.concat
                >> List.concat
            )


miniBatchesOf : Int -> List (Task x a) -> List (Task x (List a))
miniBatchesOf n =
    Concurrent.Internal.Utils.List.chunk n >> List.map doBatch


doBatch : List (Task x a) -> Task x (List a)
doBatch =
    List.foldr (map2 (::)) (succeed [])



-- Chain Tasks


succeed : a -> Task x a
succeed a =
    fromResult (Ok a)


fail : x -> Task x a
fail x =
    fromResult (Err x)


fromResult : Result x a -> Task x a
fromResult res =
    Task (\_ ids -> ( ids, Done res ))


andThen : (a -> Task x b) -> Task x a -> Task x b
andThen f (Task run) =
    Task
        (\res ids ->
            let
                ( ids_, task ) =
                    run res ids
            in
            case task of
                Done a ->
                    case a of
                        Err e ->
                            ( ids_, Done (Err e) )

                        Ok a_ ->
                            unwrap res ids_ (f a_)

                Pending defs next ->
                    ( ids_, Pending defs (andThen f next) )
        )


unwrap : Results -> Ids -> Task x a -> ( Ids, Task_ x a )
unwrap res ids (Task run) =
    run res ids


andThenDo : Task x b -> Task x a -> Task x b
andThenDo t2 t1 =
    t1 |> andThen (\_ -> t2)


return : a -> Task x b -> Task x a
return a =
    map (\_ -> a)



-- Task Errors


onError : (x -> Task y a) -> Task x a -> Task y a
onError f (Task run) =
    Task
        (\res ids ->
            let
                ( ids_, task ) =
                    run res ids
            in
            case task of
                Done a ->
                    case a of
                        Err e ->
                            unwrap res ids_ (f e)

                        Ok a_ ->
                            ( ids_, Done (Ok a_) )

                Pending defs next ->
                    ( ids_, Pending defs (onError f next) )
        )


mapError : (x -> y) -> Task x a -> Task y a
mapError f (Task run) =
    Task
        (\res ids ->
            let
                ( ids_, task ) =
                    run res ids
            in
            ( ids_
            , case task of
                Pending defs next ->
                    Pending defs (mapError f next)

                Done a ->
                    Done (Result.mapError f a)
            )
        )


errorToString : Error -> String
errorToString err =
    case err of
        ResponseError e ->
            "DecodeResponseError: " ++ Decode.errorToString e

        JsException string ->
            "JsException: " ++ string

        MissingFunction string ->
            "MissingFunction: " ++ string

        UnknownError string ->
            "InternalError: " ++ string



-- Execute a Task


type Pool x a
    = Pool (Pool_ x a)


type alias Pool_ x a =
    Dict AttemptId (Progress x a)


type alias Progress x a =
    { inFlight : Set TaskId
    , task : ( Ids, Task x a )
    }


type alias BatchResults =
    Dict AttemptId Results


type alias RawResult =
    { attemptId : AttemptId
    , taskId : TaskId
    , result : Decode.Value
    }


type alias AttemptId =
    String


type alias Attempt msg x a =
    { id : AttemptId
    , pool : Pool x a
    , send : Decode.Value -> Cmd msg
    , onComplete : AttemptId -> Result x a -> msg
    }


type alias OnProgress msg x a =
    { send : Decode.Value -> Cmd msg
    , receive : (Decode.Value -> msg) -> Sub msg
    , onComplete : AttemptId -> Result x a -> msg
    , onProgress : ( Pool x a, Cmd msg ) -> msg
    }


attempt : Attempt msg x a -> Task x a -> ( Pool x a, Cmd msg )
attempt attempt_ task =
    case stepTask Dict.empty ( Ids.init, task ) of
        ( _, Done res ) ->
            ( attempt_.pool
            , sendResult attempt_.onComplete attempt_.id res
            )

        ( _, Pending defs _ ) ->
            ( startAttempt attempt_.id
                { task = ( Ids.init, task )
                , inFlight = recordSent defs Set.empty
                }
                attempt_.pool
            , attempt_.send (encodeDefinitions attempt_.id defs)
            )


onProgress : OnProgress msg x a -> Pool x a -> Sub msg
onProgress options pool_ =
    options.receive
        (\rawResults ->
            toBatchResults rawResults
                |> Dict.toList
                |> List.foldl
                    (\( attempt_, results ) ( p, cmd ) ->
                        case findAttempt attempt_ p of
                            Nothing ->
                                ( p, cmd )

                            Just progress ->
                                progress
                                    |> updateAttempt options p ( attempt_, results )
                                    |> Tuple.mapSecond (\c -> Cmd.batch [ c, cmd ])
                    )
                    ( pool_, Cmd.none )
                |> options.onProgress
        )


updateAttempt : OnProgress msg x a -> Pool x a -> ( AttemptId, Results ) -> Progress x a -> ( Pool x a, Cmd msg )
updateAttempt options pool_ ( attemptId, results ) progress =
    case stepTask results progress.task of
        ( ids_, Pending _ next_ ) ->
            let
                nextProgress =
                    ( ids_, next_ )
            in
            case stepTask results nextProgress of
                ( _, Done res ) ->
                    case res of
                        Ok a ->
                            ( removeFromPool attemptId pool_
                            , sendResult options.onComplete attemptId (Ok a)
                            )

                        Err e ->
                            ( removeFromPool attemptId pool_
                            , sendResult options.onComplete attemptId (Err e)
                            )

                ( _, Pending defs _ ) ->
                    ( updateProgressFor attemptId
                        { task = nextProgress
                        , inFlight =
                            progress.inFlight
                                |> recordSent defs
                                |> removeCompleted results
                        }
                        pool_
                    , defs
                        |> Array.filter (notStarted progress)
                        |> encodeDefinitions attemptId
                        |> options.send
                    )

        ( _, _ ) ->
            ( pool_, Cmd.none )


stepTask : Results -> ( Ids, Task x a ) -> ( Ids, Task_ x a )
stepTask res ( ids, Task run ) =
    run res ids


recordSent : Array Todo -> Set TaskId -> Set TaskId
recordSent defs inFlight =
    Set.union inFlight (toSentIds defs)


removeCompleted : Results -> Set TaskId -> Set TaskId
removeCompleted res inFlight =
    Set.diff inFlight (Set.fromList (Dict.keys res))


toSentIds : Array Todo -> Set TaskId
toSentIds defs =
    Array.map .taskId defs
        |> Array.toList
        |> Set.fromList


sendResult : (AttemptId -> Result x a -> msg) -> AttemptId -> Result x a -> Cmd msg
sendResult onComplete attemptId res =
    CoreTask.succeed res |> CoreTask.perform (onComplete attemptId)


notStarted : Progress x a -> Todo -> Bool
notStarted model def =
    not (Set.member def.taskId model.inFlight)


toBatchResults : Decode.Value -> BatchResults
toBatchResults =
    Decode.decodeValue (Decode.list decodeRawResult)
        >> Result.map toBatchResults_
        >> Result.withDefault Dict.empty


toBatchResults_ : List RawResult -> BatchResults
toBatchResults_ =
    List.foldl
        (\result batch_ ->
            Dict.update result.attemptId
                (\attempt_ ->
                    case attempt_ of
                        Nothing ->
                            Just (Dict.singleton result.taskId result.result)

                        Just attempt__ ->
                            Just (Dict.insert result.taskId result.result attempt__)
                )
                batch_
        )
        Dict.empty



-- Encode / Decode


decodeRawResult : Decoder RawResult
decodeRawResult =
    Decode.map3 RawResult
        (Decode.field "attemptId" Decode.string)
        (Decode.field "taskId" Decode.string)
        (Decode.field "result" Decode.value)


decodeResult : Expect value -> Decoder (Result Error value)
decodeResult (ExpectJson expect) =
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\status ->
                case status of
                    "success" ->
                        Decode.field "value" (Decode.map Ok expect)

                    "error" ->
                        Decode.field "error" (Decode.map Err decodeError)

                    _ ->
                        Decode.succeed (Err (UnknownError ("Unknown response status: " ++ status)))
            )


decodeError : Decoder Error
decodeError =
    Decode.field "reason" Decode.string
        |> Decode.andThen
            (\reason ->
                case reason of
                    "js_exception" ->
                        Decode.field "message" (Decode.map JsException Decode.string)

                    "missing_function" ->
                        Decode.field "message" (Decode.map MissingFunction Decode.string)

                    _ ->
                        Decode.succeed (UnknownError ("Unknown error reason: " ++ reason))
            )


encodeDefinitions : AttemptId -> Array Todo -> Encode.Value
encodeDefinitions attemptId =
    Encode.array (encodeDefinition attemptId)


encodeDefinition : AttemptId -> Todo -> Encode.Value
encodeDefinition attemptId def =
    Encode.object
        [ ( "attemptId", Encode.string attemptId )
        , ( "taskId", Encode.string def.taskId )
        , ( "function", Encode.string def.function )
        , ( "args", def.args )
        ]



-- Pool


pool : Pool x a
pool =
    Pool Dict.empty


startAttempt : AttemptId -> Progress x a -> Pool x a -> Pool x a
startAttempt attemptId progress =
    mapPool (Dict.insert attemptId progress)


updateProgressFor : AttemptId -> Progress x a -> Pool x a -> Pool x a
updateProgressFor attemptId progress_ =
    mapPool (Dict.update attemptId (Maybe.map (always progress_)))


removeFromPool : AttemptId -> Pool x a -> Pool x a
removeFromPool attemptId =
    mapPool (Dict.remove attemptId)


findAttempt : AttemptId -> Pool x a -> Maybe (Progress x a)
findAttempt attemptId (Pool p) =
    Dict.get attemptId p


mapPool : (Pool_ x a -> Pool_ x a) -> Pool x a -> Pool x a
mapPool f (Pool p) =
    Pool (f p)
