module ConcurrentTask.Http exposing
    ( request, get, post
    , Body, emptyBody, stringBody, jsonBody, bytesBody
    , Expect, expectJson, expectString, expectBytes, expectWhatever, withMetadata
    , Header, header
    , Error(..), Metadata
    )

{-| Make concurrent http requests.

The JavaScript runner has this task builtin by default.
Internally It uses the [fetch api](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch) which is widely supported in the Browser and (as of Node 18) in NodeJS.

If needed you can supply a custom implementation like so:

    import * as ConcurrentTask from "@andrewmacmurray/elm-concurrent-task"
    import { HttpRequest, HttpResponse } from "@andrewmacmurray/elm-concurrent-task"

    ConcurrentTask.register({
      tasks: {},
      ports: app.ports,
      builtins: {
        http: customRequest
      },
    });

    function customRequest(req: HttpRequest): Promise<HttpResponse> {
      return ...<Your Custom Http Request>
    }

See the [typescript definitions](https://github.com/andrewMacmurray/elm-concurrent-task/blob/main/runner/http/index.ts) and the [fetch adapter](https://github.com/andrewMacmurray/elm-concurrent-task/blob/main/runner/http/fetch.ts) to see how to create your own.

**Note:**

You're not required to use this module for http requests in `ConcurrentTask`, it's here for convenience.
You could create entirely your own from scratch - maybe you want an http package with request caching or special retry logic built in on the JS side.


# Requests

@docs request, get, post


# Body

@docs Body, emptyBody, stringBody, jsonBody, bytesBody


# Expect

@docs Expect, expectJson, expectString, expectBytes, expectWhatever, withMetadata


# Headers

@docs Header, header


# Error

@docs Error, Metadata

-}

import Base64
import Bytes exposing (Bytes)
import Bytes.Decode
import ConcurrentTask exposing (ConcurrentTask)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Http Task


{-| Send data in your http request.
-}
type Body
    = EmptyBody
    | StringBody String String
    | BytesBody String Bytes


{-| Describe what you expect to be returned in an http response body.
-}
type Expect a
    = ExpectJson (Decoder a)
    | ExpectString (Decoder a)
    | ExpectBytes (Bytes.Decode.Decoder a)
    | ExpectWhatever (Decoder a)
    | ExpectMetadata (Metadata -> Expect a)


{-| An Http header for configuring a request.
-}
type alias Header =
    ( String, String )


{-| A Request can fail in a couple ways:

  - `BadUrl` means you did not provide a valid URL.
  - `Timeout` means it took too long to get a response.
  - `NetworkError` means the user turned off their wifi, went in a cave, etc.
  - `BadStatus` means you got a response back, but the status code indicates failure. Contains:
      - The response `Metadata`.
      - The raw response body as a `Json.Decode.Value`.
  - `BadBody` means you got a response back with a nice status code, but the body of the response was something unexpected. Contains:
      - The response `Metadata`.
      - The raw response body as a `Json.Decode.Value`.
      - The `Json.Decode.Error` that caused the error.

-}
type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Metadata Decode.Value
    | BadBody Metadata Decode.Value Decode.Error


{-| Extra information about the response:

  - url of the server that actually responded (so you can detect redirects)
  - statusCode like 200 or 404
  - statusText describing what the statusCode means a little
  - headers like Content-Length and Expires

**Note:**

It is possible for a response to have the same header multiple times.
In that case, all the values end up in a single entry in the headers dictionary.
The values are separated by commas, following the rules outlined [here](https://stackoverflow.com/questions/4371328/are-duplicate-http-response-headers-acceptable).

-}
type alias Metadata =
    { url : String
    , statusCode : Int
    , statusText : String
    , headers : Dict String String
    }



-- Header


{-| Create a `Header`. e.g.:

    header "X-Requested-With" "Fetch"

-}
header : String -> String -> Header
header =
    Tuple.pair



-- Body


{-| Create an empty body for your request.
This is useful for `GET` requests and `POST` requests where you are not sending any data.
-}
emptyBody : Body
emptyBody =
    EmptyBody


{-| Put a `String` in the body of your request. Defining `jsonBody` looks like this:

    import Json.Encode as Encode

    jsonBody : Encode.Value -> Body
    jsonBody value =
        stringBody "application/json" (Encode.encode 0 value)

The first argument is a [MIME type](https://en.wikipedia.org/wiki/Media_type) of the body.

-}
stringBody : String -> String -> Body
stringBody =
    StringBody


{-| Put some JSON value in the body of your request. This will automatically add the `Content-Type: application/json` header.
-}
jsonBody : Encode.Value -> Body
jsonBody value =
    stringBody "application/json" (Encode.encode 0 value)


{-| Put some `Bytes` in the body of your `Request`. This allows you to use
[`elm/bytes`](https://package.elm-lang.org/packages/elm/bytes/latest/) to have full control over the binary
representation of the data you are sending. For example, you could create an
`archive.zip` file and send it along like this:

    import Bytes exposing (Bytes)

    zipBody : Bytes -> Body
    zipBody bytes =
        bytesBody "application/zip" bytes

The first argument is a [MIME type](https://en.wikipedia.org/wiki/Media_type)
of the body. In other scenarios you may want to use MIME types like `image/png`
or `image/jpeg` instead.

**NOTE**: Because `Bytes` can't be sent out of a port (internally `ConcurrentTask` sends all its arguments out of a port), they are serialised to a base64 encoded `String`.

Unfortunately some of the performance benefits of `Bytes` are lost at this point.

-}
bytesBody : String -> Bytes -> Body
bytesBody =
    BytesBody



-- Expect


{-| Expect the response body to be `JSON`, decode it using the supplied decoder.
-}
expectJson : Decoder a -> Expect a
expectJson =
    ExpectJson


{-| Expect the response body to be a `String`.
-}
expectString : Expect String
expectString =
    ExpectString Decode.string


{-| Expect the response body to be `Bytes`, decode it using the supplied decoder.
-}
expectBytes : Bytes.Decode.Decoder a -> Expect a
expectBytes =
    ExpectBytes


{-| Discard the response body.
-}
expectWhatever : Expect ()
expectWhatever =
    ExpectWhatever (Decode.succeed ())


{-| Include Http metadata in a successful response.
-}
withMetadata : (Metadata -> a -> b) -> Expect a -> Expect b
withMetadata toMeta expect =
    case expect of
        ExpectJson decoder ->
            ExpectMetadata (\meta -> ExpectJson (Decode.map (toMeta meta) decoder))

        ExpectString decoder ->
            ExpectMetadata (\meta -> ExpectString (Decode.map (toMeta meta) decoder))

        ExpectBytes decoder ->
            ExpectMetadata (\meta -> ExpectBytes (Bytes.Decode.map (toMeta meta) decoder))

        ExpectWhatever decoder ->
            ExpectMetadata (\meta -> ExpectWhatever (Decode.map (toMeta meta) decoder))

        ExpectMetadata f ->
            ExpectMetadata (\meta -> withMetadata toMeta (f meta))



-- Send Request


type alias Request a =
    { url : String
    , method : String
    , headers : List Header
    , body : Body
    , expect : Expect a
    , timeout : Maybe Int
    }


{-| Send an Http request - similar to `elm/http`'s [`Http.Task`](https://package.elm-lang.org/packages/elm/http/latest/Http#task)
-}
request :
    { url : String
    , method : String
    , headers : List Header
    , body : Body
    , expect : Expect a
    , timeout : Maybe Int
    }
    -> ConcurrentTask Error a
request r =
    ConcurrentTask.define
        { function = "builtin:http"
        , expect = ConcurrentTask.expectJson (decodeExpect r.expect)
        , errors = ConcurrentTask.expectErrors (decodeError r)
        , args = encode r
        }
        |> ConcurrentTask.andThen ConcurrentTask.fromResult


{-| Send an Http `GET` request
-}
get :
    { url : String
    , headers : List Header
    , expect : Expect a
    , timeout : Maybe Int
    }
    -> ConcurrentTask Error a
get options =
    request
        { url = options.url
        , method = "GET"
        , headers = options.headers
        , body = emptyBody
        , expect = options.expect
        , timeout = options.timeout
        }


{-| Send an Http `POST` request
-}
post :
    { url : String
    , headers : List Header
    , body : Body
    , expect : Expect a
    , timeout : Maybe Int
    }
    -> ConcurrentTask Error a
post options =
    request
        { url = options.url
        , method = "POST"
        , headers = options.headers
        , body = options.body
        , expect = options.expect
        , timeout = options.timeout
        }


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

                    _ ->
                        Decode.fail ("Unknown error code: " ++ code)
            )


decodeExpect : Expect a -> Decoder (Result Error a)
decodeExpect expect =
    decodeMetadata
        |> Decode.andThen
            (\meta ->
                if meta.statusCode >= 200 && meta.statusCode < 300 then
                    decodeSuccess meta expect

                else
                    withBodyValue (\body -> Err (BadStatus meta body))
            )


decodeSuccess : Metadata -> Expect a -> Decoder (Result Error a)
decodeSuccess meta expect =
    case expect of
        ExpectJson decoder ->
            Decode.field "body" (decodeJsonBody decoder meta)

        ExpectString decoder ->
            Decode.field "body" (Decode.map Ok decoder)

        ExpectBytes decoder ->
            Decode.field "body" (decodeBytesBody decoder meta)

        ExpectWhatever decoder ->
            Decode.field "body" (Decode.map Ok decoder)

        ExpectMetadata toMeta ->
            decodeSuccess meta (toMeta meta)


decodeMetadata : Decoder Metadata
decodeMetadata =
    Decode.map4 Metadata
        (Decode.field "url" Decode.string)
        (Decode.field "statusCode" Decode.int)
        (Decode.field "statusText" Decode.string)
        (Decode.field "headers" (Decode.dict Decode.string))


decodeJsonBody : Decoder a -> Metadata -> Decoder (Result Error a)
decodeJsonBody decoder meta =
    Decode.string
        |> Decode.andThen
            (\res ->
                case Decode.decodeString decoder res of
                    Ok a ->
                        Decode.succeed (Ok a)

                    Err e ->
                        Decode.succeed (Err (BadBody meta (Encode.string res) e))
            )


decodeBytesBody : Bytes.Decode.Decoder a -> Metadata -> Decoder (Result Error a)
decodeBytesBody decoder meta =
    Decode.string
        |> Decode.andThen
            (\res ->
                case Base64.toBytes res of
                    Just bytes ->
                        case Bytes.Decode.decode decoder bytes of
                            Just a ->
                                Decode.succeed (Ok a)

                            Nothing ->
                                Decode.succeed (Err (BadBody meta (Encode.string res) (Decode.Failure "Could not decode Bytes" Encode.null)))

                    Nothing ->
                        Decode.succeed (Err (BadBody meta (Encode.string res) (Decode.Failure "Invalid Bytes body" Encode.null)))
            )


withBodyValue : (Decode.Value -> a) -> Decoder a
withBodyValue decode =
    Decode.map decode (Decode.field "body" Decode.value)



-- Encode Request


encode : Request a -> Encode.Value
encode r =
    Encode.object
        [ ( "url", Encode.string r.url )
        , ( "method", Encode.string (String.toUpper r.method) )
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

        ExpectBytes _ ->
            Encode.string "BYTES"

        ExpectWhatever _ ->
            Encode.string "WHATEVER"

        ExpectMetadata toExpect ->
            let
                -- It's safe to use fake metadata here to get at the expect kind `String`, as `withMetadata` never changes the underlying kind of expect.
                -- But it's important not to expose the `ExpectMetadata` constructor as this would make it unsafe.
                fake : Metadata
                fake =
                    { url = ""
                    , statusCode = 123
                    , statusText = ""
                    , headers = Dict.empty
                    }
            in
            encodeExpect (toExpect fake)


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

        BytesBody contentType _ ->
            header "Content-Type" contentType :: headers


encodeHeader : Header -> Encode.Value
encodeHeader ( name, value ) =
    Encode.list identity
        [ Encode.string name
        , Encode.string value
        ]


encodeBody : Body -> Encode.Value
encodeBody body =
    case body of
        StringBody _ value ->
            Encode.string value

        EmptyBody ->
            Encode.null

        BytesBody _ bytes ->
            Base64.fromBytes bytes
                |> Maybe.withDefault ""
                |> Encode.string
