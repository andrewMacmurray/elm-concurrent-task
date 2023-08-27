module S3 exposing
    ( Error
    , getObject
    , putObject
    )

import Concurrent.Task as Task exposing (Task)
import Json.Encode as Encode


type Error
    = GetError String
    | PutError String


getObject : { bucket : String, key : String } -> Task Error String
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


putObject : { bucket : String, key : String } -> String -> Task Error ()
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
