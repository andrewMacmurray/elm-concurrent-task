module Concurrent.Task exposing
    ( Attempt
    , Definition
    , Error(..)
    , OnProgress
    , Pool
    , RawResult
    , RawResults
    , Task
    , andMap
    , andThen
    , andThenDo
    , attempt
    , batch
    , define
    , errorToString
    , expectJson
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
    , sequence
    , succeed
    , testEval
    )

import Concurrent.Internal.Id as Id exposing (Id)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)
import Task as CoreTask



-- Task


type Task x a
    = State (TaskResults -> Ids -> ( Ids, Task_ x a ))


type Task_ x a
    = Pending (List Definition_) (Task x a)
    | Done (Result x a)


type alias Ids =
    Id.Sequence


type alias TaskId =
    Id


type alias Definition_ =
    { taskId : TaskId
    , function : String
    , args : Encode.Value
    }


type alias TaskResults =
    Dict TaskId Decode.Value


type Expect a
    = ExpectJson (Decoder a)


type Error
    = DecodeResponseError Decode.Error
    | JsException String
    | MissingFunction String
    | InternalError String



-- Expect


expectJson : Decoder a -> Expect a
expectJson =
    ExpectJson


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
define a =
    State
        (\results ids ->
            let
                taskId =
                    Id.get ids
            in
            ( Id.next ids
            , Pending
                [ { taskId = taskId
                  , function = a.function
                  , args = a.args
                  }
                ]
                (case Dict.get taskId results of
                    Just result ->
                        result
                            |> Decode.decodeValue (decodeResponse a.expect)
                            |> Result.mapError DecodeResponseError
                            |> Result.andThen identity
                            |> fromResult

                    Nothing ->
                        runWith ids (define a)
                )
            )
        )


runWith : Ids -> Task x a -> Task x a
runWith s (State run) =
    State (\res _ -> run res s)



-- Maps


map : (a -> b) -> Task x a -> Task x b
map f (State run) =
    State
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


map2Internal : (a -> b -> c) -> Task x a -> Task x b -> Task x c
map2Internal f (State run1) (State run2) =
    State
        (\res ids ->
            let
                ( ids_, task2 ) =
                    run2 res ids

                ( ids__, task1 ) =
                    run1 res ids_
            in
            ( Id.combine ids_ ids__
            , case ( task1, task2 ) of
                ( Pending defs1 next1, Pending defs2 next2 ) ->
                    Pending (defs1 ++ defs2) (map2Internal f next1 next2)

                ( Pending defs next1, Done b ) ->
                    Pending defs (map2Internal f next1 (fromResult b))

                ( Done a, Pending defs next2 ) ->
                    Pending defs (map2Internal f (fromResult a) next2)

                ( Done a, Done b ) ->
                    Done (Result.map2 f a b)
            )
        )


andMap : Task x a -> Task x (a -> b) -> Task x b
andMap =
    map2Internal (|>)


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
sequenceHelp tasks task =
    case tasks of
        todo :: rest ->
            task |> andThen (\xs -> sequenceHelp rest (map (\x -> x :: xs) todo))

        [] ->
            task



-- Batch


batch : List (Task x a) -> Task x (List a)
batch tasks =
    batchHelp tasks (succeed []) |> map List.reverse


batchHelp : List (Task x a) -> Task x (List a) -> Task x (List a)
batchHelp tasks task =
    case tasks of
        todo :: rest ->
            batchHelp rest (map2 (::) todo task)

        [] ->
            task



-- Chain Tasks


succeed : a -> Task x a
succeed a =
    fromResult (Ok a)


fail : x -> Task x a
fail x =
    fromResult (Err x)


fromResult : Result x a -> Task x a
fromResult res =
    State (\_ ids -> ( ids, Done res ))


andThen : (a -> Task x b) -> Task x a -> Task x b
andThen f (State run) =
    State
        (\res ids ->
            let
                ( ids_, a ) =
                    run res ids

                (State run_) =
                    case a of
                        Done a_ ->
                            case a_ of
                                Ok a__ ->
                                    f a__

                                Err e ->
                                    fail e

                        Pending defs next ->
                            State (\_ ids__ -> ( ids__, Pending defs (andThen f next) ))
            in
            run_ res ids_
        )


andThenDo : Task x b -> Task x a -> Task x b
andThenDo s2 s1 =
    s1 |> andThen (\_ -> s2)



-- Task Errors


onError : (x -> Task y a) -> Task x a -> Task y a
onError f (State run) =
    State
        (\res ids ->
            let
                ( ids_, a ) =
                    run res ids

                (State run_) =
                    case a of
                        Done a_ ->
                            case a_ of
                                Ok a__ ->
                                    succeed a__

                                Err e ->
                                    f e

                        Pending defs next ->
                            State (\_ ids__ -> ( ids__, Pending defs (onError f next) ))
            in
            run_ res ids_
        )


mapError : (x -> y) -> Task x a -> Task y a
mapError f (State run) =
    State
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
        DecodeResponseError e ->
            "DecodeResponseError: " ++ Decode.errorToString e

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
    Dict AttemptId (Progress x a)


type alias Progress x a =
    { inFlight : Set TaskId
    , task : ( Ids, Task x a )
    }


type alias BatchResults =
    Dict AttemptId TaskResults


type alias RawResults =
    List RawResult


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
    , send : Encode.Value -> Cmd msg
    , onComplete : AttemptId -> Result x a -> msg
    }


type alias OnProgress msg x a =
    { send : Encode.Value -> Cmd msg
    , receive : (RawResults -> msg) -> Sub msg
    , onComplete : AttemptId -> Result x a -> msg
    , onProgress : ( Pool x a, Cmd msg ) -> msg
    }


attempt : Attempt msg x a -> Task x a -> ( Pool x a, Cmd msg )
attempt attempt_ task =
    case runTask Dict.empty ( Id.init, task ) of
        ( _, Done res ) ->
            ( attempt_.pool
            , sendResult attempt_.onComplete attempt_.id res
            )

        ( _, Pending defs _ ) ->
            ( startAttempt attempt_.id
                { task = ( Id.init, task )
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
                                updateAttempt options p ( attempt_, results ) progress
                                    |> Tuple.mapSecond (\c -> Cmd.batch [ c, cmd ])
                    )
                    ( pool_, Cmd.none )
                |> options.onProgress
        )


updateAttempt : OnProgress msg x a -> Pool x a -> ( AttemptId, TaskResults ) -> Progress x a -> ( Pool x a, Cmd msg )
updateAttempt options pool_ ( attemptId, results ) progress =
    case runTask results progress.task of
        ( ids_, Pending _ next_ ) ->
            let
                nextProgress =
                    ( ids_, next_ )
            in
            case runTask results nextProgress of
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
                        |> List.filter (notStarted progress)
                        |> encodeDefinitions attemptId
                        |> options.send
                    )

        ( _, _ ) ->
            ( pool_, Cmd.none )


runTask : TaskResults -> ( Ids, Task x a ) -> ( Ids, Task_ x a )
runTask res ( ids, State run ) =
    run res ids


recordSent : List Definition_ -> Set Id -> Set Id
recordSent defs inFlight =
    Set.union inFlight (toSentIds defs)


removeCompleted : TaskResults -> Set Id -> Set Id
removeCompleted res inFlight =
    Set.diff inFlight (Set.fromList (Dict.keys res))


toSentIds : List Definition_ -> Set Id
toSentIds defs =
    Set.fromList (List.map .taskId defs)


sendResult : (Id -> Result x a -> msg) -> Id -> Result x a -> Cmd msg
sendResult onComplete id res =
    CoreTask.succeed res |> CoreTask.perform (onComplete id)


notStarted : Progress x a -> Definition_ -> Bool
notStarted model def =
    not (Set.member def.taskId model.inFlight)


toBatchResults : RawResults -> BatchResults
toBatchResults =
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


decodeResponse : Expect value -> Decoder (Result Error value)
decodeResponse (ExpectJson expect) =
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\status ->
                case status of
                    "success" ->
                        Decode.field "value" (Decode.map Ok expect)

                    "error" ->
                        Decode.field "error" (Decode.map Err decodeError)

                    _ ->
                        Decode.succeed (Err (InternalError ("Unknown response status: " ++ status)))
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
                        Decode.succeed (InternalError ("Unknown error reason: " ++ reason))
            )


encodeDefinitions : Id -> List Definition_ -> Encode.Value
encodeDefinitions attemptId =
    Encode.list (encodeDefinition attemptId)


encodeDefinition : Id -> Definition_ -> Encode.Value
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


startAttempt : Id -> Progress x a -> Pool x a -> Pool x a
startAttempt attemptId progress =
    mapPool (Dict.insert attemptId progress)


updateProgressFor : Id -> Progress x a -> Pool x a -> Pool x a
updateProgressFor attemptId progress_ =
    mapPool (Dict.update attemptId (Maybe.map (always progress_)))


removeFromPool : Id -> Pool x a -> Pool x a
removeFromPool attemptId =
    mapPool (Dict.remove attemptId)


findAttempt : Id -> Pool x a -> Maybe (Progress x a)
findAttempt attemptId (Pool p) =
    Dict.get attemptId p


mapPool : (Pool_ x a -> Pool_ x a) -> Pool x a -> Pool x a
mapPool f (Pool p) =
    Pool (f p)



-- Test Eval


type alias TestEval a =
    { maxDepth : Int
    , results : List ( Int, Encode.Value )
    , task : Task Error a
    , ids : Ids
    }


testEval : TestEval a -> ( Ids, Result Error a )
testEval options =
    let
        results : TaskResults
        results =
            options.results
                |> List.head
                |> Maybe.withDefault ( 100, Encode.null )
                |> Tuple.mapFirst String.fromInt
                |> List.singleton
                |> Dict.fromList
    in
    case runTask results ( options.ids, options.task ) of
        ( ids, Done a ) ->
            ( ids, a )

        ( ids, Pending _ next ) ->
            if options.maxDepth > 0 then
                testEval
                    { options
                        | maxDepth = options.maxDepth - 1
                        , results = List.drop 1 options.results
                        , task = next
                        , ids = ids
                    }

            else
                ( ids, Err (InternalError "timeout") )
