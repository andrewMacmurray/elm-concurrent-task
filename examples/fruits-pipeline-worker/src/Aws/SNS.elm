module Aws.SNS exposing
    ( Error(..)
    , Publish
    , publish
    )

import ConcurrentTask exposing (ConcurrentTask)
import Json.Encode as Encode



-- SNS


type Error
    = PublishError String



-- Publish


type alias Publish =
    { topicName : String
    , message : String
    }


publish : Publish -> ConcurrentTask Error ()
publish options =
    ConcurrentTask.define
        { function = "sns:publish"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectThrows PublishError
        , args =
            Encode.object
                [ ( "topicName", Encode.string options.topicName )
                , ( "message", Encode.string options.message )
                ]
        }
