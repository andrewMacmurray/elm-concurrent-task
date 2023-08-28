port module Worker exposing (main)

import Aws.S3 as S3
import Aws.SQS as SQS
import Common.Logger as Logger
import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode



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


type alias WorkerTask =
    Task Error Output


type Error
    = S3Error S3.Error
    | SQSError SQS.Error


type alias Output =
    String



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    startTask pollForMessages
        ( { tasks = Task.pool }
        , Cmd.none
        )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnComplete result ->
            startTask pollForMessages ( model, Cmd.none )

        OnProgress progress ->
            updateProgress progress ( model, Cmd.none )


startTask : WorkerTask -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
startTask task ( model, cmd ) =
    updateProgress (startTask_ model task) ( model, cmd )


updateProgress : ( Pool, Cmd Msg ) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateProgress ( tasks, taskCmd ) ( model, cmd ) =
    ( { model | tasks = tasks }
    , Cmd.batch [ taskCmd, cmd ]
    )


startTask_ : Model -> WorkerTask -> ( Pool, Cmd Msg )
startTask_ model =
    Task.attempt
        { send = send
        , onComplete = OnComplete
        , pool = model.tasks
        }



-- SQS


pollForMessages : WorkerTask
pollForMessages =
    Logger.info "Polling for new messages..."
        |> Task.andThenDo
            (sqsReceiveMessages
                { queueName = inQueueName
                , waitTimeSeconds = 20
                , visibilityTimeout = 2
                , maxMessages = 10
                }
            )
        |> Task.andThen (deleteAll >> Task.batch)
        |> Task.map processedMessage
        |> Logger.inspect Debug.toString identity


processedMessage : List a -> String
processedMessage xs =
    "Poll Complete - processed " ++ String.fromInt (List.length xs) ++ " messages"


deleteAll : List SQS.Message -> List (Task Error ())
deleteAll =
    List.map
        (\msg ->
            sqsDeleteMessage
                { queueName = inQueueName
                , receiptHandle = msg.receiptHandle
                }
        )


inQueueName : String
inQueueName =
    "tasks-in"


sqsReceiveMessages : SQS.ReceiveMessage -> Task Error (List SQS.Message)
sqsReceiveMessages =
    SQS.receiveMessage >> Task.mapError SQSError


sqsDeleteMessage : SQS.DeleteMessage -> Task Error ()
sqsDeleteMessage =
    SQS.deleteMessage >> Task.mapError SQSError



-- S3


putAndGet : WorkerTask
putAndGet =
    s3PutObject
        { bucket = "in-bucket"
        , key = "hello.txt"
        }
        "Hello World"
        |> Task.andThenDo
            (s3GetObject
                { bucket = "in-bucket"
                , key = "hello.txt"
                }
            )
        |> Logger.inspect Debug.toString identity


s3GetObject : S3.GetObject -> Task Error String
s3GetObject =
    S3.getObject >> Task.mapError S3Error


s3PutObject : S3.PutObject -> String -> Task Error ()
s3PutObject options =
    S3.putObject options >> Task.mapError S3Error



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
