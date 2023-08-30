module Aws.SNS exposing
    ( Error(..)
    , Publish
    , publish
    )

import Concurrent.Task as Task exposing (Task)
import Json.Encode as Encode



-- SNS


type Error
    = PublishError String



-- Publish


type alias Publish =
    { topicName : String
    , message : String
    }


publish : Publish -> Task Error ()
publish options =
    Task.define
        { function = "sns:publish"
        , expect = Task.expectWhatever
        , errors = Task.expectThrows PublishError
        , args =
            Encode.object
                [ ( "topicName", Encode.string options.topicName )
                , ( "message", Encode.string options.message )
                ]
        }
