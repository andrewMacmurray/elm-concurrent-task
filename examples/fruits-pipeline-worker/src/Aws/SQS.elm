module Aws.SQS exposing
    ( DeleteMessage
    , Error(..)
    , Message
    , ReceiveMessage
    , deleteMessage
    , receiveMessage
    )

import ConcurrentTask exposing (ConcurrentTask)
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


deleteMessage : DeleteMessage -> ConcurrentTask Error ()
deleteMessage options =
    ConcurrentTask.define
        { function = "sqs:deleteMessage"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectThrows DeleteError
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


receiveMessage : ReceiveMessage -> ConcurrentTask Error (List Message)
receiveMessage options =
    ConcurrentTask.define
        { function = "sqs:receiveMessage"
        , expect = ConcurrentTask.expectJson (Decode.list decodeMessage)
        , errors = ConcurrentTask.expectThrows ReceiveError
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
