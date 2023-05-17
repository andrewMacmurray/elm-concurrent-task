module Concurrent.Fake exposing (runExample)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Task


type Task x a
    = State (Int -> ( Int, Task_ x a ))


type Task_ x a
    = Pending (List Definition_) (AResult -> Task x a)
    | Done (Result x a)


type alias Definition_ =
    { id : Int
    , function : String
    }


type alias AResult =
    ( Int, Decode.Value )



-- Create


type alias Definition a =
    { function : String
    , expect : Decoder a
    }


define : Definition a -> Task String a
define a =
    State
        (\s ->
            ( s + 1
            , Pending [ { id = s, function = a.function } ]
                (\( id, resx ) ->
                    if id == s then
                        Decode.decodeValue a.expect resx
                            |> Result.mapError Decode.errorToString
                            |> fromResult

                    else
                        let
                            (State run) =
                                define a
                        in
                        State (\_ -> run s)
                )
            )
        )



-- Ops


map : (a -> b) -> Task x a -> Task x b
map f (State run) =
    State
        (\s ->
            let
                ( s_, a ) =
                    run s
            in
            ( s_, mapTask f a )
        )


mapTask : (a -> b) -> Task_ x a -> Task_ x b
mapTask f task =
    case task of
        Pending defs next ->
            Pending defs (next >> map f)

        Done a ->
            Done (Result.map f a)


map2 : (a -> b -> c) -> Task x a -> Task x b -> Task x c
map2 f (State run1) (State run2) =
    State
        (\s ->
            let
                ( s1, a ) =
                    run1 s

                ( s2, b ) =
                    run2 s1
            in
            ( s2, mapTask2 f a b )
        )


map3 : (a -> b -> c -> d) -> Task x a -> Task x b -> Task x c -> Task x d
map3 f t1 t2 t3 =
    succeed f
        |> andMap t1
        |> andMap t2
        |> andMap t3


andMap : Task x a -> Task x (a -> b) -> Task x b
andMap =
    map2 (|>)


mapTask2 : (a -> b -> c) -> Task_ x a -> Task_ x b -> Task_ x c
mapTask2 f task1 task2 =
    case ( task1, task2 ) of
        ( Pending defs1 next1, Pending defs2 next2 ) ->
            Pending (defs1 ++ defs2) (\res -> map2 f (next1 res) (next2 res))

        ( Pending defs next1, Done b ) ->
            Pending defs (\res -> map2 f (next1 res) (fromResult b))

        ( Done a, Pending defs next2 ) ->
            Pending defs (\res -> map2 f (fromResult a) (next2 res))

        ( Done a, Done b ) ->
            Done (Result.map2 f a b)


succeed : a -> Task x a
succeed a =
    fromResult (Ok a)


fail : x -> Task x a
fail x =
    fromResult (Err x)


fromResult : Result x a -> Task x a
fromResult res =
    State (\s -> ( s, Done res ))


andThen : (a -> Task x b) -> Task x a -> Task x b
andThen f (State run) =
    State
        (\s ->
            let
                ( s_, a ) =
                    run s

                (State run_) =
                    case a of
                        Done a_ ->
                            case a_ of
                                Ok a__ ->
                                    f a__

                                Err e ->
                                    fail e

                        Pending defs next ->
                            State (\s__ -> ( s__, Pending defs (next >> andThen f) ))
            in
            run_ s_
        )


andThenDo : Task x b -> Task x a -> Task x b
andThenDo s2 s1 =
    s1 |> andThen (\_ -> s2)


onError : (x -> Task y a) -> Task x a -> Task y a
onError f (State run) =
    State
        (\s ->
            let
                ( s_, a ) =
                    run s

                (State run_) =
                    case a of
                        Done a_ ->
                            case a_ of
                                Ok a__ ->
                                    succeed a__

                                Err e ->
                                    f e

                        Pending defs next ->
                            State (\s__ -> ( s__, Pending defs (next >> onError f) ))
            in
            run_ s_
        )



-- Eval


eval : Int -> List ( Int, Encode.Value ) -> Task String a -> Int -> ( Int, Result String a )
eval attempts results (State run) n =
    case run n of
        ( n_, Done a ) ->
            ( n_, a )

        ( n_, Pending defs next ) ->
            let
                _ =
                    Debug.log "(state, defs, resultn)"
                        ( n_
                        , defs
                        , results
                            |> List.head
                            |> Maybe.map Tuple.first
                        )
            in
            if attempts > 0 then
                eval (attempts - 1)
                    (List.drop 1 results)
                    (results
                        |> List.head
                        |> Maybe.withDefault ( 100, Encode.null )
                        |> next
                    )
                    n_

            else
                ( n_, Err "timeout" )



-- Example


create a =
    define
        { function = a
        , expect = Decode.string
        }


error =
    define
        { function = "error"
        , expect = Decode.fail "error"
        }


join3 : appendable -> appendable -> appendable -> appendable
join3 a b c =
    a ++ b ++ c


example =
    map2 (++)
        (create "hello")
        (create "world")
        |> andThenDo (create "foo")
        |> andThenDo
            (create "foo"
                |> andThenDo
                    (error
                        |> onError (\_ -> error)
                        |> onError (\_ -> error)
                        |> onError (\_ -> create "bar")
                    )
                |> andThenDo
                    (create "bar"
                        |> andThenDo (create "bar")
                        |> andThenDo (create "bar")
                        |> andThenDo
                            (create "bar"
                                |> andThenDo (create "bar")
                                |> andThenDo error
                                |> onError (\_ -> create "bar")
                            )
                        |> andThenDo (create "bar")
                    )
            )
        |> andThenDo (create "baz")
        |> andThenDo (create "baz")
        |> andThenDo (create "baz")


runExample =
    eval
        20
        [ ( 0, Encode.string "zero" )
        , ( 1, Encode.string "one" )
        , ( 2, Encode.string "two" )
        , ( 3, Encode.string "three" )
        , ( 4, Encode.string "four" )
        , ( 5, Encode.string "five" )
        , ( 6, Encode.string "six" )
        , ( 7, Encode.string "seven" )
        , ( 8, Encode.string "eight" )
        , ( 9, Encode.string "nine" )
        , ( 10, Encode.string "ten" )
        , ( 11, Encode.string "eleven" )
        , ( 12, Encode.string "twelve" )
        , ( 13, Encode.string "thirteen" )
        , ( 14, Encode.string "fourteen" )
        , ( 15, Encode.string "fifteen" )
        , ( 16, Encode.string "sixteen" )
        , ( 17, Encode.string "seventeen" )
        , ( 18, Encode.string "eighteen" )
        , ( 19, Encode.string "nineteen" )
        , ( 20, Encode.string "twenty" )
        ]
        example
        0
