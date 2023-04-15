module Concurrent.Task.Http exposing
    ( Body
    , Error(..)
    , Expect
    , Header
    , Request
    , emptyBody
    , expectJson
    , header
    , request
    )

import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode exposing (Decoder)
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


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus StatusDetails
    | BadBody String
    | TaskError Task.Error


type alias StatusDetails =
    { code : Int
    , text : String
    , body : Decode.Value
    }



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


request : Request a -> Task Error a
request r =
    Task.task
        { function = "httpRequest"
        , args = encode r
        , expect = decodeResponse r
        }
        |> Task.onError wrapError
        |> Task.andThen Task.fromResult


wrapError : Task.Error -> Task Error a
wrapError err =
    case err of
        Task.DecodeResponseError e ->
            Task.fail (BadBody (Decode.errorToString e))

        _ ->
            Task.fail (TaskError err)


decodeResponse : Request a -> Decoder (Result Error a)
decodeResponse r =
    Decode.oneOf
        [ decodeExpect r.expect
        , decodeError r
        ]


decodeError : Request a -> Decoder (Result Error value)
decodeError r =
    Decode.field "errorCode" Decode.string
        |> Decode.andThen
            (\code ->
                case code of
                    "ECONNABORTED" ->
                        Decode.succeed (Err Timeout)

                    "ERR_NETWORK" ->
                        Decode.succeed (Err NetworkError)

                    "ERR_INVALID_URL" ->
                        Decode.succeed (Err (BadUrl r.url))

                    _ ->
                        Decode.succeed (Err (TaskError (Task.InternalError ("Unknown error code: " ++ code))))
            )


decodeExpect : Expect a -> Decoder (Result Error a)
decodeExpect (ExpectJson decoder) =
    Decode.field "status" Decode.int
        |> Decode.andThen
            (\code ->
                if code >= 200 && code < 300 then
                    Decode.field "data" (Decode.map Ok decoder)

                else
                    Decode.map2
                        (\text body ->
                            Err
                                (BadStatus
                                    { code = code
                                    , text = text
                                    , body = body
                                    }
                                )
                        )
                        (Decode.field "statusText" Decode.string)
                        (Decode.field "data" Decode.value)
            )



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
