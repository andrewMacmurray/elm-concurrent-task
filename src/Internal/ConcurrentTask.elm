module Internal.ConcurrentTask exposing
    ( Attempt
    , ConcurrentTask(..)
    , ConcurrentTask_(..)
    , Errors
    , Expect
    , OnProgress
    , Pool
    , Response(..)
    , Results
    , UnexpectedError(..)
    , andMap
    , andThen
    , andThenDo
    , attempt
    , batch
    , catchAll
    , define
    , expectErrors
    , expectJson
    , expectString
    , expectThrows
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
    , onResponseDecoderFailure
    , pool
    , return
    , sequence
    , succeed
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Internal.Ids as Ids exposing (Ids)
import Internal.List as List
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)
import Task as CoreTask



-- Concurrent Task


type ConcurrentTask x a
    = ConcurrentTask (Results -> Ids -> ( Ids, ConcurrentTask_ x a ))


type ConcurrentTask_ x a
    = Pending (Array Todo) (ConcurrentTask x a)
    | Done (Response x a)


type Response x a
    = Success a
    | Error x
    | UnexpectedError UnexpectedError


type UnexpectedError
    = UnhandledJsException { function : String, message : String }
    | ResponseDecoderFailure { function : String, error : Decode.Error }
    | ErrorsDecoderFailure { function : String, error : Decode.Error }
    | MissingFunction String
    | InternalError String


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


type Errors x a
    = CatchAll a
    | ExpectThrows (String -> x)
    | ExpectErrors (Decoder x)



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



-- Errors


catchAll : a -> Errors x a
catchAll =
    CatchAll


expectThrows : (String -> x) -> Errors x a
expectThrows =
    ExpectThrows


expectErrors : Decoder x -> Errors x a
expectErrors =
    ExpectErrors



-- Define a Task


type alias Definition x a =
    { function : String
    , expect : Expect a
    , errors : Errors x a
    , args : Encode.Value
    }


define : Definition x a -> ConcurrentTask x a
define def =
    ConcurrentTask
        (\results ids ->
            let
                taskId : TaskId
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
                        wrap (decodeResponse def result)

                    Nothing ->
                        runWith ids (define def)
                )
            )
        )


runWith : Ids -> ConcurrentTask x a -> ConcurrentTask x a
runWith s (ConcurrentTask run) =
    ConcurrentTask (\res _ -> run res s)


wrap : Response x a -> ConcurrentTask x a
wrap res =
    ConcurrentTask (\_ ids -> ( ids, Done res ))



-- Maps


map : (a -> b) -> ConcurrentTask x a -> ConcurrentTask x b
map f (ConcurrentTask run) =
    ConcurrentTask
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
                    Done (mapResponse f a)
            )
        )


andMap : ConcurrentTask x a -> ConcurrentTask x (a -> b) -> ConcurrentTask x b
andMap ((ConcurrentTask run1) as task1) ((ConcurrentTask run2) as task2) =
    ConcurrentTask
        (\res ids ->
            let
                ( ids_, task2_ ) =
                    run2 res ids

                ( ids__, task1_ ) =
                    run1 res ids_
            in
            ( Ids.combine ids_ ids__
            , case ( task1_, task2_ ) of
                ( Pending defs1 next1, Pending defs2 next2 ) ->
                    Pending (Array.append defs1 defs2) (andMap next1 next2)

                ( Pending defs next1, Done b ) ->
                    haltOnError b (Pending defs (andMap next1 task2))

                ( Done a, Pending defs next2 ) ->
                    haltOnError a (Pending defs (andMap task1 next2))

                ( Done a, Done b ) ->
                    Done (map2Response (|>) a b)
            )
        )


haltOnError : Response x a -> ConcurrentTask_ x b -> ConcurrentTask_ x b
haltOnError res task =
    case res of
        Success _ ->
            task

        Error e ->
            Done (Error e)

        UnexpectedError e ->
            Done (UnexpectedError e)


map2 : (a -> b -> c) -> ConcurrentTask x a -> ConcurrentTask x b -> ConcurrentTask x c
map2 f t1 t2 =
    succeed f
        |> andMap t1
        |> andMap t2


map3 : (a -> b -> c -> d) -> ConcurrentTask x a -> ConcurrentTask x b -> ConcurrentTask x c -> ConcurrentTask x d
map3 f t1 t2 t3 =
    succeed f
        |> andMap t1
        |> andMap t2
        |> andMap t3


map4 : (a -> b -> c -> d -> e) -> ConcurrentTask x a -> ConcurrentTask x b -> ConcurrentTask x c -> ConcurrentTask x d -> ConcurrentTask x e
map4 f t1 t2 t3 t4 =
    succeed f
        |> andMap t1
        |> andMap t2
        |> andMap t3
        |> andMap t4


map5 : (a -> b -> c -> d -> e -> f) -> ConcurrentTask x a -> ConcurrentTask x b -> ConcurrentTask x c -> ConcurrentTask x d -> ConcurrentTask x e -> ConcurrentTask x f
map5 f t1 t2 t3 t4 t5 =
    succeed f
        |> andMap t1
        |> andMap t2
        |> andMap t3
        |> andMap t4
        |> andMap t5



-- Sequence


sequence : List (ConcurrentTask x a) -> ConcurrentTask x (List a)
sequence tasks =
    sequenceHelp tasks (succeed []) |> map List.reverse


sequenceHelp : List (ConcurrentTask x a) -> ConcurrentTask x (List a) -> ConcurrentTask x (List a)
sequenceHelp tasks combined =
    case tasks of
        task :: rest ->
            combined |> andThen (\xs -> sequenceHelp rest (map (\x -> x :: xs) task))

        [] ->
            combined



-- Batch


batch : List (ConcurrentTask x a) -> ConcurrentTask x (List a)
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


miniBatchesOf : Int -> List (ConcurrentTask x a) -> List (ConcurrentTask x (List a))
miniBatchesOf n =
    List.chunk n >> List.map doBatch


doBatch : List (ConcurrentTask x a) -> ConcurrentTask x (List a)
doBatch =
    List.foldr (map2 (::)) (succeed [])



-- Chain Tasks


succeed : a -> ConcurrentTask x a
succeed a =
    wrap (Success a)


fail : x -> ConcurrentTask x a
fail x =
    wrap (Error x)


fromResult : Result x a -> ConcurrentTask x a
fromResult res =
    ConcurrentTask
        (\_ ids ->
            ( ids
            , Done
                (case res of
                    Ok a ->
                        Success a

                    Err e ->
                        Error e
                )
            )
        )


andThen : (a -> ConcurrentTask x b) -> ConcurrentTask x a -> ConcurrentTask x b
andThen f (ConcurrentTask run) =
    ConcurrentTask
        (\res ids ->
            let
                ( ids_, task ) =
                    run res ids
            in
            case task of
                Done a ->
                    case a of
                        Success a_ ->
                            unwrap res ids_ (f a_)

                        Error e ->
                            ( ids_, Done (Error e) )

                        UnexpectedError e ->
                            ( ids, Done (UnexpectedError e) )

                Pending defs next ->
                    ( ids_, Pending defs (andThen f next) )
        )


unwrap : Results -> Ids -> ConcurrentTask x a -> ( Ids, ConcurrentTask_ x a )
unwrap res ids (ConcurrentTask run) =
    run res ids


andThenDo : ConcurrentTask x b -> ConcurrentTask x a -> ConcurrentTask x b
andThenDo t2 t1 =
    t1 |> andThen (\_ -> t2)


return : a -> ConcurrentTask x b -> ConcurrentTask x a
return a =
    map (\_ -> a)



-- Task Errors


onError : (x -> ConcurrentTask y a) -> ConcurrentTask x a -> ConcurrentTask y a
onError f (ConcurrentTask run) =
    ConcurrentTask
        (\res ids ->
            let
                ( ids_, task ) =
                    run res ids
            in
            case task of
                Done a ->
                    case a of
                        Success a_ ->
                            ( ids_, Done (Success a_) )

                        Error e ->
                            unwrap res ids_ (f e)

                        UnexpectedError e ->
                            ( ids_, Done (UnexpectedError e) )

                Pending defs next ->
                    ( ids_, Pending defs (onError f next) )
        )


mapError : (x -> y) -> ConcurrentTask x a -> ConcurrentTask y a
mapError f (ConcurrentTask run) =
    ConcurrentTask
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
                    Done (mapResponseError f a)
            )
        )


onResponseDecoderFailure : (Decode.Error -> ConcurrentTask x a) -> ConcurrentTask x a -> ConcurrentTask x a
onResponseDecoderFailure f (ConcurrentTask run) =
    ConcurrentTask
        (\res ids ->
            let
                ( ids_, task ) =
                    run res ids
            in
            case task of
                Done (UnexpectedError (ResponseDecoderFailure e_)) ->
                    unwrap res ids_ (f e_.error)

                Done _ ->
                    ( ids, task )

                Pending defs next ->
                    ( ids_, Pending defs (onResponseDecoderFailure f next) )
        )



-- Responses


mapResponse : (a -> b) -> Response x a -> Response x b
mapResponse f res =
    case res of
        Success a ->
            Success (f a)

        Error e ->
            Error e

        UnexpectedError e ->
            UnexpectedError e


map2Response : (a -> b -> c) -> Response x a -> Response x b -> Response x c
map2Response f res1 res2 =
    case ( res1, res2 ) of
        ( Success a, Success b ) ->
            Success (f a b)

        ( UnexpectedError e, _ ) ->
            UnexpectedError e

        ( _, UnexpectedError e ) ->
            UnexpectedError e

        ( Error e, _ ) ->
            Error e

        ( _, Error e ) ->
            Error e


mapResponseError : (x -> y) -> Response x a -> Response y a
mapResponseError f res =
    case res of
        Success a ->
            Success a

        Error e ->
            Error (f e)

        UnexpectedError e ->
            UnexpectedError e



-- Execute a Task


type Pool msg x a
    = Pool (Pool_ msg x a)


type alias Pool_ msg x a =
    { attempts : Dict AttemptId (Progress msg x a)
    , attemptIds : Ids
    }


type alias Progress msg x a =
    { inFlight : Set TaskId
    , task : ( Ids, ConcurrentTask x a )
    , onComplete : Response x a -> msg
    }


type alias BatchResults =
    Dict AttemptId Results


type alias RawResult =
    { attemptId : AttemptId
    , taskId : TaskId
    , result : Decode.Value
    }


type alias AttemptId =
    Ids.Id


type alias Attempt msg x a =
    { pool : Pool msg x a
    , send : Decode.Value -> Cmd msg
    , onComplete : Response x a -> msg
    }


type alias OnProgress msg x a =
    { send : Decode.Value -> Cmd msg
    , receive : (Decode.Value -> msg) -> Sub msg
    , onProgress : ( Pool msg x a, Cmd msg ) -> msg
    }


attempt : Attempt msg x a -> ConcurrentTask x a -> ( Pool msg x a, Cmd msg )
attempt attempt_ task =
    case stepTask Dict.empty ( Ids.init, task ) of
        ( _, Done res ) ->
            ( attempt_.pool
            , sendResult attempt_.onComplete res
            )

        ( _, Pending defs _ ) ->
            ( startAttempt
                { task = ( Ids.init, task )
                , inFlight = recordSent defs Set.empty
                , onComplete = attempt_.onComplete
                }
                attempt_.pool
            , attempt_.send (encodeDefinitions (currentAttemptId attempt_.pool) defs)
            )


currentAttemptId : Pool msg x a -> AttemptId
currentAttemptId (Pool pool_) =
    Ids.get pool_.attemptIds


onProgress : OnProgress msg x a -> Pool msg x a -> Sub msg
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


updateAttempt : OnProgress msg x a -> Pool msg x a -> ( AttemptId, Results ) -> Progress msg x a -> ( Pool msg x a, Cmd msg )
updateAttempt options pool_ ( attemptId, results ) progress =
    case stepTask results progress.task of
        ( ids_, Pending _ next_ ) ->
            let
                nextProgress : ( Ids, ConcurrentTask x a )
                nextProgress =
                    ( ids_, next_ )
            in
            case stepTask results nextProgress of
                ( _, Done res ) ->
                    ( removeFromPool attemptId pool_
                    , sendResult progress.onComplete res
                    )

                ( _, Pending defs _ ) ->
                    ( updateProgressFor attemptId
                        { progress
                            | task = nextProgress
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

        _ ->
            ( pool_, Cmd.none )


stepTask : Results -> ( Ids, ConcurrentTask x a ) -> ( Ids, ConcurrentTask_ x a )
stepTask res ( ids, ConcurrentTask run ) =
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


sendResult : (Response x a -> msg) -> Response x a -> Cmd msg
sendResult onComplete res =
    CoreTask.succeed res |> CoreTask.perform onComplete


notStarted : Progress msg x a -> Todo -> Bool
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


decodeResponse : Definition x a -> Decode.Value -> Response x a
decodeResponse def val =
    case def.errors of
        CatchAll fallback ->
            decodeCatchAll fallback def val

        ExpectThrows catch ->
            decodeExpectThrows catch def val

        ExpectErrors expect ->
            decodeExpectErrors expect def val


decodeCatchAll : a -> Definition x a -> Decode.Value -> Response b a
decodeCatchAll fallback def val =
    case Decode.decodeValue (decodeRunnerError def) val of
        Ok err ->
            case err of
                UnhandledJsException _ ->
                    Success fallback

                ResponseDecoderFailure _ ->
                    Success fallback

                _ ->
                    UnexpectedError err

        Err _ ->
            case Decode.decodeValue (decodeRunnerSuccess def) val of
                Ok a ->
                    Success a

                Err _ ->
                    Success fallback


decodeExpectThrows : (String -> x) -> Definition a b -> Decode.Value -> Response x b
decodeExpectThrows catch def val =
    case Decode.decodeValue (decodeRunnerError def) val of
        Ok err ->
            case err of
                UnhandledJsException e ->
                    Error (catch e.message)

                _ ->
                    UnexpectedError err

        Err _ ->
            case Decode.decodeValue (decodeRunnerSuccess def) val of
                Ok a ->
                    Success a

                Err e ->
                    UnexpectedError
                        (ResponseDecoderFailure
                            { function = def.function
                            , error = e
                            }
                        )


decodeExpectErrors : Decoder x -> Definition a b -> Decode.Value -> Response x b
decodeExpectErrors expect def val =
    case Decode.decodeValue (decodeRunnerError def) val of
        Ok err ->
            UnexpectedError err

        Err _ ->
            case Decode.decodeValue (decodeExpectErrorField Decode.value) val of
                Ok _ ->
                    case Decode.decodeValue (decodeExpectErrorField expect) val of
                        Ok err_ ->
                            Error err_

                        Err e_ ->
                            UnexpectedError
                                (ErrorsDecoderFailure
                                    { function = def.function
                                    , error = e_
                                    }
                                )

                Err _ ->
                    case Decode.decodeValue (decodeRunnerSuccess def) val of
                        Ok a ->
                            Success a

                        Err e_ ->
                            UnexpectedError
                                (ResponseDecoderFailure
                                    { function = def.function
                                    , error = e_
                                    }
                                )


decodeExpectErrorField : Decoder a -> Decoder a
decodeExpectErrorField decoder =
    Decode.field "value" (Decode.field "error" decoder)


decodeRunnerSuccess : Definition x a -> Decoder a
decodeRunnerSuccess def =
    case def.expect of
        ExpectJson expect ->
            Decode.field "value" expect


decodeRunnerError : Definition x a -> Decoder UnexpectedError
decodeRunnerError def =
    Decode.field "error"
        (Decode.field "reason" Decode.string
            |> Decode.andThen
                (\reason ->
                    case reason of
                        "js_exception" ->
                            Decode.field "message"
                                (Decode.map
                                    (\msg ->
                                        UnhandledJsException
                                            { function = def.function
                                            , message = msg
                                            }
                                    )
                                    Decode.string
                                )

                        "missing_function" ->
                            Decode.field "message" (Decode.map MissingFunction Decode.string)

                        _ ->
                            Decode.succeed (InternalError ("Unknown runner error reason: " ++ reason))
                )
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


pool : Pool msg x a
pool =
    Pool
        { attempts = Dict.empty
        , attemptIds = Ids.init
        }


startAttempt : Progress msg x a -> Pool msg x a -> Pool msg x a
startAttempt progress =
    mapPool
        (\pool_ ->
            { pool_
                | attempts = Dict.insert (Ids.get pool_.attemptIds) progress pool_.attempts
                , attemptIds = Ids.next pool_.attemptIds
            }
        )


updateProgressFor : AttemptId -> Progress msg x a -> Pool msg x a -> Pool msg x a
updateProgressFor attemptId progress_ =
    mapPool (\pool_ -> { pool_ | attempts = Dict.update attemptId (Maybe.map (always progress_)) pool_.attempts })


removeFromPool : AttemptId -> Pool msg x a -> Pool msg x a
removeFromPool attemptId =
    mapPool (\pool_ -> { pool_ | attempts = Dict.remove attemptId pool_.attempts })


findAttempt : AttemptId -> Pool msg x a -> Maybe (Progress msg x a)
findAttempt attemptId (Pool p) =
    Dict.get attemptId p.attempts


mapPool : (Pool_ msg x a -> Pool_ msg x a) -> Pool msg x a -> Pool msg x a
mapPool f (Pool p) =
    Pool (f p)
