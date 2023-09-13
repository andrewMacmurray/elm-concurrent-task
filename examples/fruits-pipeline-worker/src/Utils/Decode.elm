module Utils.Decode exposing (decodeJsonl)

import Json.Decode as Decode exposing (Decoder)


decodeJsonl : Decoder a -> String -> Result (List Decode.Error) (List a)
decodeJsonl decoder input =
    String.split "\n" input
        |> List.map (Decode.decodeString decoder)
        |> List.foldr collectJsonlResult (Ok [])


collectJsonlResult : Result x a -> Result (List x) (List a) -> Result (List x) (List a)
collectJsonlResult val acc =
    case ( acc, val ) of
        ( Ok xs, Ok a ) ->
            Ok (a :: xs)

        ( Ok _, Err e ) ->
            Err [ e ]

        ( Err ex, Ok _ ) ->
            Err ex

        ( Err ex, Err e ) ->
            Err (e :: ex)
