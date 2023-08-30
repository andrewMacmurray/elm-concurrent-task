port module Main exposing (main)

import Aws.S3 as S3
import Aws.SNS as SNS
import Aws.SQS as SQS
import Common.Decode as Decode
import Common.Encode as Encode
import Common.Logger as Logger
import Common.Uuid as Uuid exposing (Uuid)
import Concurrent.Task as Task exposing (Task)
import Concurrent.Task.Time
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time


{-| Fruit Picking Worker 🍓🍑🍐

This is an example pipeline worker that talks to AWS services.

  - It listens for messages on an SQS queue (using SQS long polling).
  - Each message batch is processed concurrently and the results stored for later in an S3 bucket.
  - An SNS message is sent for each message processed.

See the `processOrchards` function for more details.

-}



-- Model


type alias Flags =
    {}


type alias Model =
    { tasks : Pool
    }


type Msg
    = OnProgress ( Pool, Cmd Msg )
    | OnComplete (Task.Response Error Output)



-- Task Types


type alias Pool =
    Task.Pool Msg Error Output


type Error
    = SQSPollingError SQS.Error
    | ProcessingError ProcessError


type alias Output =
    ()



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        ( tasks, cmd ) =
            startTask
                { pool = Task.pool
                , task = processOrchards
                }
    in
    ( { tasks = tasks }
    , cmd
    )


startTask : { pool : Pool, task : SQS.Message -> Task ProcessError Output } -> ( Pool, Cmd Msg )
startTask { pool, task } =
    Task.attempt
        { send = send
        , onComplete = OnComplete
        , pool = pool
        }
        (processMessagesWith task)



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnComplete result ->
            case result of
                Task.RunnerError err ->
                    let
                        _ =
                            Debug.log "runner error" err
                    in
                    ( model, Cmd.none )

                _ ->
                    let
                        ( tasks, cmd ) =
                            startTask
                                { pool = model.tasks
                                , task = processOrchards
                                }
                    in
                    ( { model | tasks = tasks }, cmd )

        OnProgress ( tasks, cmd ) ->
            ( { model | tasks = tasks }, cmd )



-- Process Fn


type alias Orchard =
    { fruit : String
    , trees : Int
    , yield : Yield
    }


type alias Harvest =
    { fruit : String
    , harvested : Int
    , seeds : Int
    , time : Time.Posix
    }


type Yield
    = High
    | Medium
    | Low


type alias MessageBody =
    { s3Key : String
    , s3Bucket : String
    }


type ProcessError
    = DecodeMessageFailed Decode.Error
    | GetFromBucketFailed S3.Error
    | DecodeOrchardsFailed (List Decode.Error)
    | SaveHarvestFailed S3.Error
    | NotifyHarvestsFailed SNS.Error


{-| This is the main pipeline logic - the fruit picking process!

  - Extracts a list of fruit tree orchards stored in an S3 bucket.
  - Harvests the trees and saves the results in an output S3 bucket.
  - Notifies saved harvests via SNS.

-}
processOrchards : SQS.Message -> Task ProcessError ()
processOrchards message =
    decodeMessage message.body
        |> Task.andThen (Logger.withInfo "getting orchards from bucket.." << getFromBucket)
        |> Task.andThen (Logger.withInfo "decoding orchards.." << decodeOrchards)
        |> Task.andThen (Logger.withInfo "harvesting orchards.." << harvestOrchards)
        |> Task.andThen (Logger.withInfo "saving harvests.." << saveHarvests)
        |> Task.andThen (Logger.withInfo "notifying saved harvests.." << notifySavedHarvests)
        |> Task.andThen (Logger.info << successMessage)


successMessage : List a -> String
successMessage harvests =
    "success! harvested " ++ numberOf harvests ++ " orchards."


notifySavedHarvests : List String -> Task ProcessError (List String)
notifySavedHarvests harvests =
    SNS.publish
        { topicName = "tasks-out"
        , message = encodeSavedHarvestMessage harvests
        }
        |> Task.mapError NotifyHarvestsFailed
        |> Task.return harvests


encodeSavedHarvestMessage : List String -> String
encodeSavedHarvestMessage harvests =
    Encode.encode 0
        (Encode.object
            [ ( "message", Encode.string "harvests saved" )
            , ( "harvests", Encode.list Encode.string harvests )
            ]
        )


saveHarvests : ( Time.Posix, List Harvest ) -> Task ProcessError (List String)
saveHarvests ( time, harvests ) =
    harvests
        |> List.map (saveHarvest time)
        |> Task.batch


saveHarvest : Time.Posix -> Harvest -> Task ProcessError String
saveHarvest time harvest =
    Uuid.generate
        |> Task.andThen (saveHarvest_ time harvest)
        |> Task.mapError SaveHarvestFailed


saveHarvest_ : Time.Posix -> Harvest -> Uuid -> Task S3.Error String
saveHarvest_ time harvest id =
    let
        key : String
        key =
            harvestFilePath id time harvest ++ ".json"
    in
    Encode.encode 0 (encodeHarvest harvest)
        |> S3.putObject { bucket = "out-bucket", key = key }
        |> Task.return key


harvestFilePath : Uuid -> Time.Posix -> Harvest -> String
harvestFilePath id time harvest =
    String.join "_"
        [ "harvest"
        , String.fromInt (Time.posixToMillis time)
        , harvest.fruit
        , id
        ]


harvestOrchards : List Orchard -> Task x ( Time.Posix, List Harvest )
harvestOrchards orchards =
    Concurrent.Task.Time.now
        |> Task.map (toHarvests orchards)


toHarvests : List Orchard -> Time.Posix -> ( Time.Posix, List Harvest )
toHarvests orchards time =
    ( time, List.map (harvestOrchard time) orchards )


harvestOrchard : Time.Posix -> Orchard -> Harvest
harvestOrchard time orchard =
    case orchard.yield of
        High ->
            { fruit = orchard.fruit
            , harvested = orchard.trees * 10
            , seeds = orchard.trees * 30
            , time = time
            }

        Medium ->
            { fruit = orchard.fruit
            , harvested = orchard.trees * 5
            , seeds = orchard.trees * 15
            , time = time
            }

        Low ->
            { fruit = orchard.fruit
            , harvested = orchard.trees * 3
            , seeds = orchard.trees * 10
            , time = time
            }


decodeOrchards : String -> Task ProcessError (List Orchard)
decodeOrchards =
    Decode.decodeJsonl decodeTodo
        >> Result.mapError DecodeOrchardsFailed
        >> Task.fromResult


getFromBucket : MessageBody -> Task ProcessError String
getFromBucket body =
    Task.mapError GetFromBucketFailed
        (S3.getObject
            { bucket = body.s3Bucket
            , key = body.s3Key
            }
        )


decodeMessage : String -> Task ProcessError MessageBody
decodeMessage =
    Decode.decodeString decodeMessageBody
        >> Result.mapError DecodeMessageFailed
        >> Task.fromResult


encodeHarvest : Harvest -> Encode.Value
encodeHarvest harvest =
    Encode.object
        [ ( "fruit", Encode.string harvest.fruit )
        , ( "harvested", Encode.int harvest.harvested )
        , ( "seeds", Encode.int harvest.seeds )
        , ( "time", Encode.time harvest.time )
        ]


decodeMessageBody : Decoder MessageBody
decodeMessageBody =
    Decode.map2 MessageBody
        (Decode.field "s3_key" Decode.string)
        (Decode.field "s3_bucket" Decode.string)


decodeTodo : Decoder Orchard
decodeTodo =
    Decode.map3 Orchard
        (Decode.field "fruit" Decode.string)
        (Decode.field "trees" Decode.int)
        (Decode.field "yield" decodeYield)


decodeYield : Decoder Yield
decodeYield =
    Decode.string
        |> Decode.andThen
            (\yield ->
                case String.toUpper yield of
                    "HIGH" ->
                        Decode.succeed High

                    "MEDIUM" ->
                        Decode.succeed Medium

                    "LOW" ->
                        Decode.succeed Low

                    _ ->
                        Decode.fail "Unrecognized Yield"
            )



-- SQS


processMessagesWith : (SQS.Message -> Task ProcessError a) -> Task Error Output
processMessagesWith fn =
    Logger.info "polling for new messages.."
        |> Task.andThenDo
            (sqsReceiveMessages
                { queueName = inQueueName
                , waitTimeSeconds = 20
                , visibilityTimeout = 2
                , maxMessages = 10
                }
            )
        |> Task.andThen (List.map (processMessage fn) >> Task.batch)
        |> Logger.inspect Debug.toString processedMessage
        |> Task.return ()


processMessage : (SQS.Message -> Task ProcessError a) -> SQS.Message -> Task Error ()
processMessage processFn msg =
    processFn msg
        |> Task.mapError ProcessingError
        |> Task.andThenDo
            (sqsDeleteMessage
                { queueName = inQueueName
                , receiptHandle = msg.receiptHandle
                }
            )


processedMessage : List a -> String
processedMessage xs =
    "poll complete - processed " ++ numberOf xs ++ " messages"


inQueueName : String
inQueueName =
    "tasks-in"


sqsReceiveMessages : SQS.ReceiveMessage -> Task Error (List SQS.Message)
sqsReceiveMessages =
    SQS.receiveMessage >> Task.mapError SQSPollingError


sqsDeleteMessage : SQS.DeleteMessage -> Task Error ()
sqsDeleteMessage =
    SQS.deleteMessage >> Task.mapError SQSPollingError


numberOf : List a -> String
numberOf =
    List.length >> String.fromInt



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Task.onProgress
        { send = send
        , receive = receive
        , onProgress = OnProgress
        }
        model.tasks



-- Ports


port send : Decode.Value -> Cmd msg


port receive : (Decode.Value -> msg) -> Sub msg



-- Program


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
