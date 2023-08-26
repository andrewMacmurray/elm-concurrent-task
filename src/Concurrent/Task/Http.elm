module Concurrent.Task.Http exposing
    ( Request, request
    , Expect, expectJson, expectString, expectWhatever
    , Error(..), StatusDetails
    , Header, header
    , Body, emptyBody, stringBody, jsonBody
    )

{-| Make concurrent http requests.

The JavaScript runner has this task builtin by default.
Internally It uses the [fetch api](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch) which is widely supported in the Browser and (as of Node 18) in NodeJS.

If needed you can supply a custom implementation like so:

    Tasks.register({
      tasks: {},
      ports: app.ports,
      builtins: {
        http: (request) => customHttp(request),
      },
    });

See the [typescript definitions](https://github.com/andrewMacmurray/elm-concurrent-task/blob/main/src/runner/http/index.ts) and the [fetch adapter](https://github.com/andrewMacmurray/elm-concurrent-task/blob/main/src/runner/http/fetch.ts) to see how to create your own.


# Request

@docs Request, request


# Expect

@docs Expect, expectJson, expectString, expectWhatever


# Error

@docs Error, StatusDetails


# Header

@docs Header, header


# Body

@docs Body, emptyBody, stringBody, jsonBody

-}

import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Http Task


{-| -}
type alias Request a =
    { url : String
    , method : String
    , headers : List Header
    , body : Body
    , expect : Expect a
    , timeout : Maybe Int
    }


{-| -}
type Body
    = EmptyBody
    | StringBody String String


{-| -}
type Expect a
    = ExpectJson (Decoder a)
    | ExpectString (Decoder a)
    | ExpectWhatever (Decoder a)


{-| -}
type alias Header =
    ( String, String )


{-| -}
type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus StatusDetails
    | BadBody String


{-| -}
type alias StatusDetails =
    { code : Int
    , text : String
    , body : Decode.Value
    }



-- Header


{-| -}
header : String -> String -> Header
header =
    Tuple.pair



-- Expect


{-| -}
expectJson : Decoder a -> Expect a
expectJson =
    ExpectJson


{-| -}
expectString : Expect String
expectString =
    ExpectString Decode.string


{-| -}
expectWhatever : Expect ()
expectWhatever =
    ExpectWhatever (Decode.succeed ())



-- Body


{-| -}
emptyBody : Body
emptyBody =
    EmptyBody


{-| -}
jsonBody : Encode.Value -> Body
jsonBody value =
    StringBody "application/json" (Encode.encode 0 value)


{-| -}
stringBody : String -> String -> Body
stringBody =
    StringBody



-- Send Request


{-| -}
request : Request a -> Task Error a
request r =
    Task.define
        { function = "builtin:http"
        , expect = Task.expectJson (decodeExpect r.expect)
        , errors = Task.expectErrors (decodeError r)
        , args = encode r
        }
        |> Task.andThen Task.fromResult
        |> Task.onResponseDecoderFailure wrapError


wrapError : Decode.Error -> Task Error a
wrapError =
    Decode.errorToString
        >> BadBody
        >> Task.fail


decodeError : Request a -> Decoder Error
decodeError r =
    Decode.field "reason" Decode.string
        |> Decode.andThen
            (\code ->
                case code of
                    "TIMEOUT" ->
                        Decode.succeed Timeout

                    "NETWORK_ERROR" ->
                        Decode.succeed NetworkError

                    "BAD_URL" ->
                        Decode.succeed (BadUrl r.url)

                    "BAD_BODY" ->
                        Decode.field "message" Decode.string
                            |> Decode.map BadBody

                    _ ->
                        Decode.fail ("Unknown error code: " ++ code)
            )


decodeExpect : Expect a -> Decoder (Result Error a)
decodeExpect expect =
    Decode.field "status" Decode.int
        |> Decode.andThen
            (\code ->
                if code >= 200 && code < 300 then
                    case expect of
                        ExpectJson decoder ->
                            Decode.field "body" (Decode.map Ok decoder)

                        ExpectString decoder ->
                            Decode.field "body" (Decode.map Ok decoder)

                        ExpectWhatever decoder ->
                            Decode.field "body" (Decode.map Ok decoder)

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
                        (Decode.field "body" Decode.value)
            )



-- Encode Request


encode : Request a -> Encode.Value
encode r =
    Encode.object
        [ ( "url", Encode.string r.url )
        , ( "method", Encode.string r.method )
        , ( "headers", encodeHeaders r.body r.headers )
        , ( "expect", encodeExpect r.expect )
        , ( "body", encodeBody r.body )
        , ( "timeout", encodeTimeout r.timeout )
        ]


encodeTimeout : Maybe Int -> Encode.Value
encodeTimeout =
    Maybe.map Encode.int >> Maybe.withDefault Encode.null


encodeExpect : Expect a -> Encode.Value
encodeExpect expect =
    case expect of
        ExpectString _ ->
            Encode.string "STRING"

        ExpectJson _ ->
            Encode.string "JSON"

        ExpectWhatever _ ->
            Encode.string "WHATEVER"


encodeHeaders : Body -> List Header -> Encode.Value
encodeHeaders body headers =
    headers
        |> addContentTypeForBody body
        |> Encode.list encodeHeader


addContentTypeForBody : Body -> List Header -> List Header
addContentTypeForBody body headers =
    case body of
        EmptyBody ->
            headers

        StringBody contentType _ ->
            header "Content-Type" contentType :: headers


encodeHeader : Header -> Encode.Value
encodeHeader ( name, value ) =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "value", Encode.string value )
        ]


encodeBody : Body -> Encode.Value
encodeBody body =
    case body of
        StringBody _ value ->
            Encode.string value

        EmptyBody ->
            Encode.null
