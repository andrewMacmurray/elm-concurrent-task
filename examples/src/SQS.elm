module SQS exposing (..)

import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Message =
    { messageId : String
    , receiptHandle : String
    , body : String
    }


type Error
    = ReceiveError String
    | DeleteError String


type alias ReceiveMessages =
    { queueName : String
    , visibilityTimeout : Int
    , maxMessages : Int
    }


type alias DeleteMessage =
    { queueName : String
    , receiptHandle : String
    }


deleteMessage : DeleteMessage -> Task Error ()
deleteMessage options =
    Task.define
        { function = "sqs:deleteMessage"
        , expect = Task.expectWhatever
        , errors = Task.expectThrows DeleteError
        , args =
            Encode.object
                [ ( "receiptHandle", Encode.string options.receiptHandle )
                , ( "queueName", Encode.string options.queueName )
                ]
        }


receiveMessages : ReceiveMessages -> Task Error (List Message)
receiveMessages options =
    Task.define
        { function = "sqs:receiveMessage"
        , expect = Task.expectJson decodeMessages
        , errors = Task.expectThrows ReceiveError
        , args =
            Encode.object
                [ ( "queueName", Encode.string options.queueName )
                , ( "visibilityTimeout", Encode.int options.visibilityTimeout )
                , ( "maxMessages", Encode.int options.maxMessages )
                ]
        }


decodeMessages : Decoder (List Message)
decodeMessages =
    Decode.list decodeMessage


decodeMessage : Decoder Message
decodeMessage =
    Decode.map3 Message
        (Decode.field "MessageId" Decode.string)
        (Decode.field "ReceiptHandle" Decode.string)
        (Decode.field "Body" Decode.string)
