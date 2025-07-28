module ConcurrentTask.Internal exposing
    ( Attempt
    , ConcurrentTask(..)
    , Errors(..)
    , Expect(..)
    , OnProgress
    , Pool
    , Response(..)
    , Results
    , Task_(..)
    , UnexpectedError(..)
    , andMap
    , andThen
    , attempt
    , define
    , fail
    , fromResult
    , map
    , mapError
    , onError
    , onJsException
    , onProgress
    , onResponseDecoderFailure
    , pool
    , succeed
    , withPoolId
    )

import Array exposing (Array)
import ConcurrentTask.Internal.Ids as Ids exposing (Ids)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)
import Task as CoreTask



-- Concurrent Task


type ConcurrentTask x a
    = Task (Results -> Ids -> ( Ids, Task_ x a ))


type Task_ x a
    = Pending (Array Todo) (ConcurrentTask x a)
    | Done (Response x a)


type Response x a
    = Success a
    | Error x
    | UnexpectedError UnexpectedError


type UnexpectedError
    = UnhandledJsException { function : String, message : String, raw : Decode.Value }
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


type Errors x
    = ExpectNoErrors
    | ExpectThrows (String -> x)
    | ExpectErrors (Decoder x)



-- Define a Task


type alias Definition x a =
    { function : String
    , expect : Expect a
    , errors : Errors x
    , args : Encode.Value
    }


define : Definition x a -> ConcurrentTask x a
define def =
    Task
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
runWith s (Task run) =
    Task (\res _ -> run res s)


wrap : Response x a -> ConcurrentTask x a
wrap res =
    Task (\_ ids -> ( ids, Done res ))



-- Maps


map : (a -> b) -> ConcurrentTask x a -> ConcurrentTask x b
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
                    Done (mapResponse f a)
            )
        )


andMap : ConcurrentTask x a -> ConcurrentTask x (a -> b) -> ConcurrentTask x b
andMap ((Task run1) as task1) ((Task run2) as task2) =
    Task
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


haltOnError : Response x a -> Task_ x b -> Task_ x b
haltOnError res task =
    case res of
        Success _ ->
            task

        Error e ->
            Done (Error e)

        UnexpectedError e ->
            Done (UnexpectedError e)



-- Chain Tasks


succeed : a -> ConcurrentTask x a
succeed a =
    wrap (Success a)


fail : x -> ConcurrentTask x a
fail x =
    wrap (Error x)


fromResult : Result x a -> ConcurrentTask x a
fromResult res =
    Task
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
                        Success a_ ->
                            stepTask res ( ids_, f a_ )

                        Error e ->
                            ( ids_, Done (Error e) )

                        UnexpectedError e ->
                            ( ids, Done (UnexpectedError e) )

                Pending defs next ->
                    ( ids_, Pending defs (andThen f next) )
        )



-- Task Errors


onError : (x -> ConcurrentTask y a) -> ConcurrentTask x a -> ConcurrentTask y a
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
                        Success a_ ->
                            ( ids_, Done (Success a_) )

                        Error e ->
                            stepTask res ( ids_, f e )

                        UnexpectedError e ->
                            ( ids_, Done (UnexpectedError e) )

                Pending defs next ->
                    ( ids_, Pending defs (onError f next) )
        )


mapError : (x -> y) -> ConcurrentTask x a -> ConcurrentTask y a
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
                    Done (mapResponseError f a)
            )
        )


onResponseDecoderFailure : (Decode.Error -> ConcurrentTask x a) -> ConcurrentTask x a -> ConcurrentTask x a
onResponseDecoderFailure f (Task run) =
    Task
        (\res ids ->
            let
                ( ids_, task ) =
                    run res ids
            in
            case task of
                Done (UnexpectedError (ResponseDecoderFailure e_)) ->
                    stepTask res ( ids_, f e_.error )

                Done _ ->
                    ( ids, task )

                Pending defs next ->
                    ( ids_, Pending defs (onResponseDecoderFailure f next) )
        )


onJsException : ({ message : String, raw : Decode.Value } -> ConcurrentTask x a) -> ConcurrentTask x a -> ConcurrentTask x a
onJsException f (Task run) =
    Task
        (\res ids ->
            let
                ( ids_, task ) =
                    run res ids
            in
            case task of
                Done (UnexpectedError (UnhandledJsException e_)) ->
                    stepTask res ( ids_, f { message = e_.message, raw = e_.raw } )

                Done _ ->
                    ( ids, task )

                Pending defs next ->
                    ( ids_, Pending defs (onJsException f next) )
        )


stepTask : Results -> ( Ids, ConcurrentTask x a ) -> ( Ids, Task_ x a )
stepTask res ( ids, Task run ) =
    run res ids



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


type Pool msg
    = Pool (Pool_ msg)


type alias Pool_ msg =
    { poolId : Maybe Int
    , attempts : Dict AttemptId (Progress msg)
    , attemptIds : Ids
    }


type alias Progress msg =
    { inFlight : Set TaskId
    , task : ( Ids, ConcurrentTask msg msg )
    , onComplete : Response msg msg -> msg
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


type alias Attempt msg =
    { pool : Pool msg
    , send : Decode.Value -> Cmd msg
    , onComplete : Response msg msg -> msg
    }


type alias OnProgress msg =
    { send : Decode.Value -> Cmd msg
    , receive : (Decode.Value -> msg) -> Sub msg
    , onProgress : ( Pool msg, Cmd msg ) -> msg
    }


attempt : Attempt msg -> ConcurrentTask msg msg -> ( Pool msg, Cmd msg )
attempt attempt_ task =
    case stepTask Dict.empty ( Ids.init, task ) of
        ( _, Done res ) ->
            ( attempt_.pool
            , sendResult attempt_.onComplete res
            )

        ( _, Pending defs _ ) ->
            let
                progress : Progress msg
                progress =
                    { task = ( Ids.init, task )
                    , inFlight = recordSent defs Set.empty
                    , onComplete = attempt_.onComplete
                    }
            in
            ( startAttempt progress attempt_.pool
            , attempt_.send (encodeDefinitions (currentAttemptId attempt_.pool) defs)
            )


onProgress : OnProgress msg -> Pool msg -> Sub msg
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
                                    |> withCmd cmd
                    )
                    ( pool_, Cmd.none )
                |> options.onProgress
        )


updateAttempt : OnProgress msg -> Pool msg -> ( AttemptId, Results ) -> Progress msg -> ( Pool msg, Cmd msg )
updateAttempt options pool_ ( attemptId, results ) progress =
    case stepTask results progress.task of
        ( ids_, Pending _ next_ ) ->
            let
                nextProgress : ( Ids, ConcurrentTask msg msg )
                nextProgress =
                    ( ids_, next_ )

                notStarted : Todo -> Bool
                notStarted def =
                    not (Set.member def.taskId progress.inFlight)
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
                        |> Array.filter notStarted
                        |> encodeDefinitions attemptId
                        |> options.send
                    )

        _ ->
            ( pool_, Cmd.none )


recordSent : Array Todo -> Set TaskId -> Set TaskId
recordSent defs inFlight =
    let
        sentIds : Set TaskId
        sentIds =
            Array.map .taskId defs
                |> Array.toList
                |> Set.fromList
    in
    Set.union inFlight sentIds


removeCompleted : Results -> Set TaskId -> Set TaskId
removeCompleted res inFlight =
    Set.diff inFlight (Set.fromList (Dict.keys res))


sendResult : (Response x a -> msg) -> Response x a -> Cmd msg
sendResult onComplete res =
    CoreTask.succeed res |> CoreTask.perform onComplete


toBatchResults : Decode.Value -> BatchResults
toBatchResults =
    let
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
    in
    Decode.decodeValue (Decode.list decodeRawResult)
        >> Result.map toBatchResults_
        >> Result.withDefault Dict.empty



-- Encode / Decode


decodeRawResult : Decoder RawResult
decodeRawResult =
    Decode.map3 RawResult
        (Decode.field "attemptId" Decode.string)
        (Decode.field "taskId" Decode.string)
        (Decode.field "result" Decode.value)


decodeResponse : Definition x a -> Decode.Value -> Response x a
decodeResponse def val =
    let
        decodeRunnerSuccess : Decoder a
        decodeRunnerSuccess =
            case def.expect of
                ExpectJson expect ->
                    Decode.field "value" expect

        decodeRunnerError : Decoder UnexpectedError
        decodeRunnerError =
            Decode.field "error"
                (Decode.field "reason" Decode.string
                    |> Decode.andThen
                        (\reason ->
                            case reason of
                                "js_exception" ->
                                    Decode.map2
                                        (\msg raw ->
                                            UnhandledJsException
                                                { function = def.function
                                                , message = msg
                                                , raw = raw
                                                }
                                        )
                                        (Decode.field "message" Decode.string)
                                        (Decode.field "raw" Decode.value)

                                "missing_function" ->
                                    Decode.field "message" (Decode.map MissingFunction Decode.string)

                                _ ->
                                    Decode.succeed (InternalError ("Unknown runner error reason: " ++ reason))
                        )
                )

        decodeExpectErrors : Decoder x -> Response x a
        decodeExpectErrors expect =
            let
                decodeExpectErrorField : Decoder val -> Decoder val
                decodeExpectErrorField decoder =
                    Decode.field "value" (Decode.field "error" decoder)
            in
            case Decode.decodeValue decodeRunnerError val of
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
                            case Decode.decodeValue decodeRunnerSuccess val of
                                Ok a ->
                                    Success a

                                Err e_ ->
                                    UnexpectedError
                                        (ResponseDecoderFailure
                                            { function = def.function
                                            , error = e_
                                            }
                                        )

        decodeExpectThrows : (String -> x) -> Response x a
        decodeExpectThrows catch =
            case Decode.decodeValue decodeRunnerError val of
                Ok err ->
                    case err of
                        UnhandledJsException e ->
                            Error (catch e.message)

                        _ ->
                            UnexpectedError err

                Err _ ->
                    case Decode.decodeValue decodeRunnerSuccess val of
                        Ok a ->
                            Success a

                        Err e ->
                            UnexpectedError
                                (ResponseDecoderFailure
                                    { function = def.function
                                    , error = e
                                    }
                                )

        decodeExpectNoErrors : () -> Response x a
        decodeExpectNoErrors _ =
            case Decode.decodeValue decodeRunnerError val of
                Ok err ->
                    UnexpectedError err

                Err _ ->
                    case Decode.decodeValue decodeRunnerSuccess val of
                        Ok a ->
                            Success a

                        Err e ->
                            UnexpectedError
                                (ResponseDecoderFailure
                                    { function = def.function
                                    , error = e
                                    }
                                )
    in
    case def.errors of
        ExpectThrows catch ->
            decodeExpectThrows catch

        ExpectErrors expect ->
            decodeExpectErrors expect

        ExpectNoErrors ->
            decodeExpectNoErrors ()


encodeDefinitions : AttemptId -> Array Todo -> Encode.Value
encodeDefinitions attemptId =
    Encode.array
        (\def ->
            Encode.object
                [ ( "attemptId", Encode.string attemptId )
                , ( "taskId", Encode.string def.taskId )
                , ( "function", Encode.string def.function )
                , ( "args", def.args )
                ]
        )



-- Pool


pool : Pool msg
pool =
    Pool
        { poolId = Nothing
        , attempts = Dict.empty
        , attemptIds = Ids.init
        }


withPoolId : Int -> Pool msg -> Pool msg
withPoolId id =
    mapPool (\pool_ -> { pool_ | poolId = Just id })


startAttempt : Progress msg -> Pool msg -> Pool msg
startAttempt progress p =
    mapPool
        (\pool_ ->
            { pool_
                | attempts = Dict.insert (currentAttemptId p) progress pool_.attempts
                , attemptIds = Ids.next pool_.attemptIds
            }
        )
        p


currentAttemptId : Pool msg -> AttemptId
currentAttemptId (Pool pool_) =
    case pool_.poolId of
        Just id ->
            String.fromInt id ++ ":" ++ Ids.get pool_.attemptIds

        Nothing ->
            Ids.get pool_.attemptIds


updateProgressFor : AttemptId -> Progress msg -> Pool msg -> Pool msg
updateProgressFor attemptId progress_ =
    mapPool (\pool_ -> { pool_ | attempts = Dict.update attemptId (Maybe.map (always progress_)) pool_.attempts })


removeFromPool : AttemptId -> Pool msg -> Pool msg
removeFromPool attemptId =
    mapPool (\pool_ -> { pool_ | attempts = Dict.remove attemptId pool_.attempts })


findAttempt : AttemptId -> Pool msg -> Maybe (Progress msg)
findAttempt attemptId (Pool p) =
    Dict.get attemptId p.attempts


mapPool : (Pool_ msg -> Pool_ msg) -> Pool msg -> Pool msg
mapPool f (Pool p) =
    Pool (f p)



-- Utils


withCmd : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
withCmd cmd =
    Tuple.mapSecond (\c -> Cmd.batch [ c, cmd ])
