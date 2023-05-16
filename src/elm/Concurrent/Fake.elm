module Concurrent.Fake exposing (runExample)

-- Task


type Task a
    = State (Int -> ( Int, Task_ a ))


type Task_ a
    = Pending (List Definition_) (Task a)
    | Done a


type alias Definition_ =
    { id : Int
    , function : String
    }



-- Create


type alias Definition a =
    { function : String
    , expect : a
    }


define : Definition a -> Task a
define a =
    State
        (\s ->
            ( s + 1
            , Pending [ { id = s, function = a.function } ] (succeed a.expect)
            )
        )



-- Ops


map : (a -> b) -> Task a -> Task b
map f (State run) =
    State
        (\s ->
            let
                ( s_, a ) =
                    run s
            in
            ( s_, mapTask f a )
        )


mapTask : (a -> b) -> Task_ a -> Task_ b
mapTask f task =
    case task of
        Pending defs next ->
            Pending defs (map f next)

        Done a ->
            Done (f a)


map2 : (a -> b -> c) -> Task a -> Task b -> Task c
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


mapTask2 : (a -> b -> c) -> Task_ a -> Task_ b -> Task_ c
mapTask2 f task1 task2 =
    case ( task1, task2 ) of
        ( Pending defs1 next1, Pending defs2 next2 ) ->
            Pending (defs1 ++ defs2) (map2 f next1 next2)

        ( Pending defs next1, Done b ) ->
            Pending defs (map2 f next1 (succeed b))

        ( Done a, Pending defs next2 ) ->
            Pending defs (map2 f (succeed a) next2)

        ( Done a, Done b ) ->
            Done (f a b)


succeed : a -> Task a
succeed a =
    State (\s -> ( s, Done a ))


andThen : (a -> Task b) -> Task a -> Task b
andThen f (State run) =
    State
        (\s ->
            let
                ( s_, a ) =
                    run s

                (State run_) =
                    case a of
                        Done a_ ->
                            f a_

                        Pending defs next ->
                            State (\s__ -> ( s__, Pending defs (next |> andThen f) ))
            in
            run_ s_
        )


addDefs : List Definition_ -> Task a -> Task a
addDefs defs (State toTask) =
    State
        (\model ->
            let
                ( model1, task_ ) =
                    toTask model
            in
            ( model1
            , case task_ of
                Done a ->
                    Done a

                Pending defs1 next ->
                    Pending (defs ++ defs1) next
            )
        )


andThenDo : Task b -> Task a -> Task b
andThenDo s2 s1 =
    s1 |> andThen (\_ -> s2)



-- Eval


eval : Task a -> Int -> ( Int, a )
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
        , expect = a
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
                |> andThenDo (create "bar")
                |> andThenDo
                    (create "bar"
                        |> andThenDo (create "bar")
                        |> andThenDo (create "bar")
                        |> andThenDo (create "bar" |> andThenDo (create "bar"))
                        |> andThenDo (create "bar")
                    )
            )
        |> andThenDo (create "baz")
        |> andThenDo (create "baz")
        |> andThenDo (create "baz")


runExample =
    eval example 0
