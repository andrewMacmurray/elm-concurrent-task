module Fake5 exposing (..)

-- Module to test out optimizations


type Task a
    = Pending (Int -> ( List Def, Int, Task a ))
    | Done a


type Def
    = Def Int


map : (a -> b) -> Task a -> Task b
map f task =
    case task of
        Done a ->
            Done (f a)

        Pending next ->
            Pending
                (\s ->
                    let
                        ( e, s_, a ) =
                            next s
                    in
                    ( e, s_, map f a )
                )


log : String -> a -> a
log label val =
    val


map2 : (a -> b -> c) -> Task a -> Task b -> Task c
map2 f taskA taskB =
    case ( taskA, taskB ) of
        ( Done a, Done b ) ->
            Done (f a b)

        ( Done a, Pending next ) ->
            Pending
                (\s ->
                    let
                        ( def, s_, task ) =
                            next s
                    in
                    ( def
                    , s_
                    , map2 f (Done a) task
                    )
                )

        ( Pending next, Done b ) ->
            Pending
                (\s ->
                    let
                        ( def, s_, task ) =
                            next s
                    in
                    ( def
                    , s_
                    , map2 f task (Done b)
                    )
                )

        ( Pending next1, Pending next2 ) ->
            Pending
                (\s ->
                    let
                        ( defs1, s1, task2 ) =
                            next2 s

                        ( defs2, s2, task1 ) =
                            next1 s1
                    in
                    ( defs1 ++ defs2
                    , max s1 s2
                    , map2 f task1 task2
                    )
                )


andThen : (a -> Task b) -> Task a -> Task b
andThen f task =
    case task of
        Done a ->
            f a

        Pending next ->
            Pending
                (\s ->
                    let
                        ( e, s_, task_ ) =
                            next s
                    in
                    case task_ of
                        Done a ->
                            ( e, s_, f a )

                        Pending _ ->
                            ( e, s_, andThen f task_ )
                )


succeed : a -> Task a
succeed a =
    Done a


create : Task String
create =
    Pending
        (\n ->
            ( [ Def n ]
            , n + 1
            , Done (String.fromInt n)
            )
        )



-- Sequence


sequence : List (Task a) -> Task (List a)
sequence tasks =
    sequenceHelp tasks (succeed [])


sequenceHelp : List (Task a) -> Task (List a) -> Task (List a)
sequenceHelp tasks combined =
    case tasks of
        task :: rest ->
            combined |> andThen (\xs -> sequenceHelp rest (map (\x -> x :: xs) task))

        [] ->
            combined



-- Batch


batch : List (Task a) -> Task (List a)
batch tasks =
    batchHelp tasks (succeed [])


batchHelp : List (Task a) -> Task (List a) -> Task (List a)
batchHelp tasks combined =
    case tasks of
        task :: rest ->
            batchHelp rest (map2 (::) task combined)

        [] ->
            combined


run : Int -> Task a -> a
run n task =
    case task of
        Done a ->
            a

        Pending next ->
            let
                ( todos, n_, tsk ) =
                    next n

                _ =
                    Debug.log "todos" ( n, todos )
            in
            run n_ tsk
