module Tree2 exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- A task needs some way of associating a definition with a result producing function
-- How can I combine many of these Exec2s?
-- Is there a List form I can associate across function calls?
-- Can I have a list of Maybe Decode.Values and associate the index with the defs?


type Task a
    = Task (List Def) (List (Maybe Decode.Value) -> Response a)


type Response a
    = AwaitingNext
    | InternalError
    | Complete (Result String a)
    | Continue (Task a)


type Def
    = Def String


create : String -> Decoder a -> Task a
create name decode =
    Task [ Def name ]
        (\response ->
            case response of
                Nothing :: _ ->
                    AwaitingNext

                (Just res) :: _ ->
                    res
                        |> Decode.decodeValue decode
                        |> Result.mapError Decode.errorToString
                        |> Complete

                [] ->
                    InternalError
        )


map : (a -> b) -> Task a -> Task b
map f task =
    case task of
        Task defs next ->
            Task defs
                (\res ->
                    case next res of
                        Complete a ->
                            Complete (Result.map f a)

                        AwaitingNext ->
                            AwaitingNext

                        InternalError ->
                            InternalError

                        Continue task_ ->
                            Continue (map f task_)
                )


map2Response : (a -> b -> c) -> Response a -> Response b -> Response c
map2Response f resA resB =
    case ( resA, resB ) of
        ( InternalError, _ ) ->
            InternalError

        ( _, InternalError ) ->
            InternalError

        ( Complete (Err e), _ ) ->
            Complete (Err e)

        ( _, Complete (Err e) ) ->
            Complete (Err e)

        ( AwaitingNext, _ ) ->
            AwaitingNext

        ( _, AwaitingNext ) ->
            AwaitingNext

        ( Complete (Ok a), Complete (Ok b) ) ->
            Complete (Ok (f a b))

        ( Continue ta, Continue tb ) ->
            Continue (map2 f ta tb)

        ( Continue ta, Complete (Ok b) ) ->
            Continue (map2 f ta (succeed b))

        ( Complete (Ok a), Continue tb ) ->
            Continue (map2 f (succeed a) tb)


map2 : (a -> b -> c) -> Task a -> Task b -> Task c
map2 f taskA taskB =
    case ( taskA, taskB ) of
        ( Task defs1 f1, Task defs2 f2 ) ->
            Task (defs1 ++ defs2)
                (\resX ->
                    let
                        resXA =
                            List.take (List.length defs1) resX

                        resXB =
                            List.drop (List.length defs1) resX
                    in
                    if List.length resXA /= List.length defs1 || List.length resXB /= List.length defs2 then
                        InternalError

                    else
                        map2Response f (f1 resXA) (f2 resXB)
                )


andThen : (a -> Task b) -> Task a -> Task b
andThen f task =
    case task of
        Task defs next ->
            Task defs
                (\res ->
                    case next res of
                        InternalError ->
                            InternalError

                        AwaitingNext ->
                            AwaitingNext

                        Complete (Err e) ->
                            Complete (Err e)

                        Complete (Ok a) ->
                            Continue (f a)

                        Continue a ->
                            Continue (andThen f a)
                )


succeed : a -> Task a
succeed a =
    Task [] (\_ -> Complete (Ok a))


andMap : Task a -> Task (a -> b) -> Task b
andMap =
    map2 (|>)


example : Response String
example =
    --run (map2 (++) aTask aTask)
    --    [ Just (Encode.int 1)
    --    , Just (Encode.string "foo")
    --    , Just (Encode.int 2)
    --    , Just (Encode.string "bar")
    --    ]
    run
        (andMapped
            |> andThen
                (\_ ->
                    andMapped
                        |> andThen
                            (\_ ->
                                andMapped
                            )
                )
            |> andThen (\_ -> andMapped)
        )
        (List.reverse
            [ Just (Encode.string "1")
            , Just (Encode.string "2")
            , Just (Encode.string "3")
            ]
        )


andMapped : Task String
andMapped =
    succeed join3
        |> andMap getString
        |> andMap getString
        |> andMap getString


join3 : String -> String -> String -> String
join3 a b c =
    String.join "," [ a, b, c ]


aTask : Task String
aTask =
    map2 (\a b -> String.fromInt a ++ b)
        getInt
        getString


getString : Task String
getString =
    create "getString" Decode.string


getInt : Task Int
getInt =
    create "getInt" Decode.int


run : Task a -> List (Maybe Decode.Value) -> Response a
run task resX =
    case task of
        Task defs next ->
            let
                _ =
                    Debug.log "defs" defs
            in
            case next resX of
                Continue task_ ->
                    let
                        _ =
                            Debug.log "continuing" ()
                    in
                    run task_ resX

                x ->
                    x
