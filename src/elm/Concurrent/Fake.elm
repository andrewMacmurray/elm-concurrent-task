module Concurrent.Fake exposing (runExample)

-- Task


type Task x a
    = State (Int -> ( Int, Task_ x a ))


type Task_ x a
    = Pending (List Definition_) (Task x a)
    | Done (Result x a)


type alias Definition_ =
    { id : Int
    , function : String
    }



-- Create


type alias Definition a =
    { function : String
    , expect : Result String a
    }


define : Definition a -> Task String a
define a =
    State
        (\s ->
            ( s + 1
            , Pending [ { id = s, function = a.function } ] (fromResult a.expect)
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
            Pending defs (map f next)

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


mapTask2 : (a -> b -> c) -> Task_ x a -> Task_ x b -> Task_ x c
mapTask2 f task1 task2 =
    case ( task1, task2 ) of
        ( Pending defs1 next1, Pending defs2 next2 ) ->
            Pending (defs1 ++ defs2) (map2 f next1 next2)

        ( Pending defs next1, Done b ) ->
            Pending defs (map2 f next1 (fromResult b))

        ( Done a, Pending defs next2 ) ->
            Pending defs (map2 f (fromResult a) next2)

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
                            State (\s__ -> ( s__, Pending defs (next |> andThen f) ))
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
                            State (\s__ -> ( s__, Pending defs (next |> onError f) ))
            in
            run_ s_
        )



-- Eval


eval : Task x a -> Int -> ( Int, Result x a )
eval (State run) n =
    case run n of
        ( n_, Done a ) ->
            ( n_, a )

        ( n_, Pending defs next ) ->
            let
                _ =
                    Debug.log "defs" ( n_, defs )
            in
            eval next n_



-- Example


create a =
    define
        { function = "create"
        , expect = Ok a
        }


error =
    define
        { function = "error"
        , expect = Err "error"
        }


example =
    map2 (++)
        (map2 (++)
            (create "hello")
            (create "world")
        )
        (map2 (++)
            (create "hello")
            (create "world")
        )
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
    eval example 0
