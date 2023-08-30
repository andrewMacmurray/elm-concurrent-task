module Aws.SQS exposing
    ( DeleteMessage
    , Error(..)
    , Message
    , ReceiveMessage
    , deleteMessage
    , receiveMessage
    )

import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- SQS


type alias Message =
    { messageId : String
    , receiptHandle : String
    , body : String
    }


type Error
    = ReceiveError String
    | DeleteError String



-- Delete Message


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
                [ ( "queueName", Encode.string options.queueName )
                , ( "receiptHandle", Encode.string options.receiptHandle )
                ]
        }



-- Receive Message


type alias ReceiveMessage =
    { queueName : String
    , maxMessages : Int
    , visibilityTimeout : Int
    , waitTimeSeconds : Int
    }


receiveMessage : ReceiveMessage -> Task Error (List Message)
receiveMessage options =
    Task.define
        { function = "sqs:receiveMessage"
        , expect = Task.expectJson (Decode.list decodeMessage)
        , errors = Task.expectThrows ReceiveError
        , args =
            Encode.object
                [ ( "queueName", Encode.string options.queueName )
                , ( "visibilityTimeout", Encode.int options.visibilityTimeout )
                , ( "maxMessages", Encode.int options.maxMessages )
                , ( "waitTimeSeconds", Encode.int options.waitTimeSeconds )
                ]
        }


decodeMessage : Decoder Message
decodeMessage =
    Decode.map3 Message
        (Decode.field "MessageId" Decode.string)
        (Decode.field "ReceiptHandle" Decode.string)
        (Decode.field "Body" Decode.string)
