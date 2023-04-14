module Concurrent.Task.Http exposing
    ( Body
    , Expect
    , Header
    , Request
    , emptyBody
    , expectJson
    , header
    , request
    )

import Concurrent.Task as Task exposing (Task)
import Json.Decode exposing (Decoder)
import Json.Encode as Encode



-- Http Task


type alias Request a =
    { url : String
    , method : String
    , headers : List Header
    , body : Body
    , expect : Expect a
    }


type Body
    = Json Encode.Value


type Expect a
    = ExpectJson (Decoder a)


type Header
    = Header String String



-- Header


header : String -> String -> Header
header =
    Header



-- Expect


expectJson : Decoder a -> Expect a
expectJson =
    ExpectJson



-- Body


emptyBody : Body
emptyBody =
    Json Encode.null



-- Send Request


request : Request a -> Task Task.Error a
request r =
    Task.ffi
        { function = "httpRequest"
        , args = encode r
        , expect = toTaskExpect r.expect
        }


toTaskExpect : Expect a -> Decoder a
toTaskExpect (ExpectJson decoder) =
    decoder



-- Encode Request


encode : Request a -> Encode.Value
encode r =
    Encode.object
        [ ( "url", Encode.string r.url )
        , ( "method", Encode.string r.method )
        , ( "headers", Encode.list encodeHeader r.headers )
        , ( "body", encodeBody r.body )
        ]


encodeHeader : Header -> Encode.Value
encodeHeader (Header name value) =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "value", Encode.string value )
        ]


encodeBody : Body -> Encode.Value
encodeBody body =
    case body of
        Json value ->
            Encode.object
                [ ( "type", Encode.string "json" )
                , ( "value", value )
                ]
