module Localstorage exposing
    ( ReadError(..)
    , WriteError(..)
    , getItem
    , setItem
    )

import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Localstorage getItem


type ReadError
    = NoValue
    | ReadBlocked
    | DecodeError Decode.Error


getItem : String -> Decoder a -> ConcurrentTask ReadError a
getItem key decoder =
    ConcurrentTask.define
        { function = "localstorage:getItem"
        , expect = ConcurrentTask.expectString
        , errors = ConcurrentTask.expectErrors decodeReadErrors
        , args = Encode.object [ ( "key", Encode.string key ) ]
        }
        |> ConcurrentTask.map (Decode.decodeString decoder >> Result.mapError DecodeError)
        |> ConcurrentTask.andThen ConcurrentTask.fromResult


decodeReadErrors : Decoder ReadError
decodeReadErrors =
    Decode.string
        |> Decode.andThen
            (\reason ->
                case reason of
                    "NO_VALUE" ->
                        Decode.succeed NoValue

                    "READ_BLOCKED" ->
                        Decode.succeed ReadBlocked

                    _ ->
                        Decode.fail ("Unrecognized ReadError " ++ reason)
            )



-- Localstorage setItem


type WriteError
    = QuotaExceeded
    | WriteBlocked


setItem : String -> Encode.Value -> ConcurrentTask WriteError ()
setItem key value =
    ConcurrentTask.define
        { function = "localstorage:setItem"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectErrors decodeWriteErrors
        , args =
            Encode.object
                [ ( "key", Encode.string key )
                , ( "value", Encode.string (Encode.encode 0 value) )
                ]
        }


decodeWriteErrors : Decoder WriteError
decodeWriteErrors =
    Decode.string
        |> Decode.andThen
            (\reason ->
                case reason of
                    "QUOTA_EXCEEDED" ->
                        Decode.succeed QuotaExceeded

                    "WRITE_BLOCKED" ->
                        Decode.succeed WriteBlocked

                    _ ->
                        Decode.fail ("Unrecognized WriteError " ++ reason)
            )
