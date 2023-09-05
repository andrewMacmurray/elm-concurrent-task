module ConcurrentTask.Http exposing
    ( request, get, post
    , Body, emptyBody, stringBody, jsonBody
    , Expect, expectJson, expectString, expectWhatever
    , Header, header
    , Error(..), Metadata
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

**Note:**

You're not required to use this module for http requests in `ConcurrentTask`, it's here for convenience.
You could create entirely your own from scratch - maybe you want an http package with request caching or special retry logic built in on the JS side.


# Requests

@docs request, get, post


# Body

@docs Body, emptyBody, stringBody, jsonBody


# Expect

@docs Expect, expectJson, expectString, expectWhatever


# Headers

@docs Header, header


# Error

@docs Error, Metadata

-}

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


{-| Describe what you expect to be returned in an http response body.
-}
type Expect a
    = ExpectJson (Decoder a)
    | ExpectString (Decoder a)
    | ExpectWhatever (Decoder a)


{-| An Http header for configuring a request.
-}
type alias Header =
    ( String, String )


{-| A Request can fail in a couple ways:

  - `BadUrl` means you did not provide a valid URL.
  - `Timeout` means it took too long to get a response.
  - `NetworkError` means the user turned off their wifi, went in a cave, etc.
  - `BadStatus` means you got a response back, but the status code indicates failure.
  - `BadBody` means you got a response back with a nice status code, but the body of the response was something unexpected.
    The String in this case is a debugging message that explains what went wrong with your JSON decoder or whatever.

-}
type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Metadata Decode.Value
    | BadBody String


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


{-| Discard the response body.
-}
expectWhatever : Expect ()
expectWhatever =
    ExpectWhatever (Decode.succeed ())



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
        |> ConcurrentTask.onResponseDecoderFailure wrapError


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


wrapError : Decode.Error -> ConcurrentTask Error a
wrapError =
    Decode.errorToString
        >> BadBody
        >> ConcurrentTask.fail


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
                            Decode.field "body" (decodeJsonBody decoder)

                        ExpectString decoder ->
                            Decode.field "body" (Decode.map Ok decoder)

                        ExpectWhatever decoder ->
                            Decode.field "body" (Decode.map Ok decoder)

                else
                    Decode.map4
                        (\url text body headers ->
                            Err
                                (BadStatus
                                    { url = url
                                    , statusCode = code
                                    , statusText = text
                                    , headers = headers
                                    }
                                    body
                                )
                        )
                        (Decode.field "url" Decode.string)
                        (Decode.field "statusText" Decode.string)
                        (Decode.field "body" Decode.value)
                        (Decode.field "headers" (Decode.dict Decode.string))
            )


decodeJsonBody : Decoder a -> Decoder (Result Error a)
decodeJsonBody decoder =
    Decode.string
        |> Decode.andThen
            (\res ->
                case Decode.decodeString decoder res of
                    Ok a ->
                        Decode.succeed (Ok a)

                    Err e ->
                        Decode.succeed (Err (BadBody (Decode.errorToString e)))
            )



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
