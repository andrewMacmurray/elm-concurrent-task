module DecodeNested exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Task a
    = Hardcoded a
    | Task (Decoder (Val a))


type alias Val a =
    { current : a -- can I decode into a NOT RESOLVED type so it can skip it?
    , next : Encode.Value
    }


map2 : (a -> b -> c) -> Task a -> Task b -> Task c
map2 f da db =
    case ( da, db ) of
        ( Task da_, Task db_ ) ->
            Task
                (da_
                    |> Decode.andThen
                        (\resA ->
                            case Decode.decodeValue db_ resA.next of
                                Ok resB ->
                                    Decode.succeed
                                        { current = f resA.current resB.current
                                        , next = resB.next
                                        }

                                Err e ->
                                    Decode.fail (Decode.errorToString e)
                        )
                )

        ( Task da_, Hardcoded b ) ->
            Task
                (Decode.map
                    (\res ->
                        { current = f res.current b
                        , next = res.next
                        }
                    )
                    da_
                )

        ( Hardcoded a, Task db_ ) ->
            Task
                (Decode.map
                    (\res ->
                        { current = f a res.current
                        , next = res.next
                        }
                    )
                    db_
                )

        ( Hardcoded a, Hardcoded b ) ->
            Hardcoded (f a b)


create : Decoder a -> Task a
create d =
    Task
        (Decode.map2
            (\current next ->
                { current = current
                , next = next
                }
            )
            (Decode.field "current" d)
            (Decode.field "next" Decode.value)
        )


succeed : a -> Task a
succeed =
    Hardcoded


combined : Task String
combined =
    map2 (++)
        (create Decode.string)
        (create Decode.string)


combinedAgain : Task String
combinedAgain =
    map2 (++)
        combined
        (map2
            (++)
            combined
            combined
        )


andMap : Task a -> Task (a -> b) -> Task b
andMap =
    map2 (|>)


encodeEls : List Encode.Value -> Encode.Value
encodeEls xs =
    case xs of
        x :: rest ->
            List.foldl
                (\val acc ->
                    Encode.object
                        [ ( "current", val )
                        , ( "next", acc )
                        ]
                )
                (Encode.object
                    [ ( "current", x )
                    , ( "next", Encode.null )
                    ]
                )
                rest

        [] ->
            Encode.null


run_ : Task a -> Result Decode.Error { current : a, next : Encode.Value }
run_ task =
    case task of
        Hardcoded a ->
            Ok { current = a, next = Encode.null }

        Task decoder ->
            Decode.decodeValue
                decoder
                (encodeEls
                    (List.reverse
                        [ Encode.string "1"
                        , Encode.string "2"
                        , Encode.string "3"
                        , Encode.string "4"
                        , Encode.string "5"
                        , Encode.string "6"
                        ]
                    )
                )


run : Result Decode.Error { current : String, next : Encode.Value }
run =
    run_
        (map2 (++)
            (map2 (++)
                (create Decode.string)
                (create Decode.string)
            )
            (succeed (++)
                |> andMap (create Decode.string)
                |> andMap (create Decode.string)
            )
        )
