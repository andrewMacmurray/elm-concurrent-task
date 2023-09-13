module Utils.Expect exposing
    ( errorDecoderFailureFor
    , responseDecoderFailureFor
    )

import ConcurrentTask.Internal.ConcurrentTask as ConcurrentTask
import Expect exposing (Expectation)
import Json.Decode as Decode


responseDecoderFailureFor : String -> String -> ConcurrentTask.Response x a -> Expectation
responseDecoderFailureFor function message res =
    case res of
        ConcurrentTask.UnexpectedError (ConcurrentTask.ResponseDecoderFailure err) ->
            if err.function == function && String.contains message (Decode.errorToString err.error) then
                Expect.pass

            else
                Expect.fail
                    (String.concat
                        [ "Got a ResponseDecoderFailure but with the wrong config \n\nexpected: function - "
                        , function
                        , ", message - "
                        , message
                        , "\n\ngot: function - "
                        , err.function
                        , ", message - "
                        , Decode.errorToString err.error
                        ]
                    )

        _ ->
            Expect.fail ("Expected an ResponseDecoderFailure, got instead " ++ Debug.toString res)


errorDecoderFailureFor : String -> String -> ConcurrentTask.Response x a -> Expectation
errorDecoderFailureFor function message res =
    case res of
        ConcurrentTask.UnexpectedError (ConcurrentTask.ErrorsDecoderFailure err) ->
            if err.function == function && String.contains message (Decode.errorToString err.error) then
                Expect.pass

            else
                Expect.fail
                    (String.concat
                        [ "Got a ErrorDecoderFailure but with the wrong config \n\nexpected: function - "
                        , function
                        , ", message - "
                        , message
                        , "\n\ngot: function - "
                        , err.function
                        , ", message - "
                        , Decode.errorToString err.error
                        ]
                    )

        _ ->
            Expect.fail ("Expected an ErrorDecoderFailure, got instead " ++ Debug.toString res)
