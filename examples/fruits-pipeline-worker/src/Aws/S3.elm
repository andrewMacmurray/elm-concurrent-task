module Aws.S3 exposing
    ( Error(..)
    , GetObject
    , PutObject
    , getObject
    , putObject
    )

import ConcurrentTask exposing (ConcurrentTask)
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


getObject : GetObject -> ConcurrentTask Error String
getObject options =
    ConcurrentTask.define
        { function = "s3:getObject"
        , expect = ConcurrentTask.expectString
        , errors = ConcurrentTask.expectThrows GetError
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


putObject : PutObject -> String -> ConcurrentTask Error ()
putObject options contents =
    ConcurrentTask.define
        { function = "s3:putObject"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectThrows PutError
        , args =
            Encode.object
                [ ( "bucket", Encode.string options.bucket )
                , ( "key", Encode.string options.key )
                , ( "contents", Encode.string contents )
                ]
        }
