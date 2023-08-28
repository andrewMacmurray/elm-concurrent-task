module Aws.S3 exposing
    ( Error(..)
    , GetObject
    , PutObject
    , getObject
    , putObject
    )

import Concurrent.Task as Task exposing (Task)
import Json.Encode as Encode



-- S3


type Error
    = GetError String
    | PutError String



-- Get Object


type alias GetObject =
    { bucket : String
    , key : String
    }


getObject : GetObject -> Task Error String
getObject options =
    Task.define
        { function = "s3:getObject"
        , expect = Task.expectString
        , errors = Task.expectThrows GetError
        , args =
            Encode.object
                [ ( "bucket", Encode.string options.bucket )
                , ( "key", Encode.string options.key )
                ]
        }



-- Put Object


type alias PutObject =
    { bucket : String
    , key : String
    }


putObject : PutObject -> String -> Task Error ()
putObject options contents =
    Task.define
        { function = "s3:putObject"
        , expect = Task.expectWhatever
        , errors = Task.expectThrows PutError
        , args =
            Encode.object
                [ ( "bucket", Encode.string options.bucket )
                , ( "key", Encode.string options.key )
                , ( "contents", Encode.string contents )
                ]
        }
