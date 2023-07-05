module Fake3 exposing (..)

-- Module to test out optimizations


type Task a
    = Pending Exec (Int -> ( Def, Int, Task a ))
    | Done a


type Exec
    = Sequential
    | Batch


type Def
    = Def Int



-- Map


map : (a -> b) -> Task a -> Task b
map f task =
    case task of
        Done a ->
            Done (f a)

        Pending exec next ->
            Pending exec
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

        ( Done a, Pending exec next ) ->
            Pending exec
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

        ( Pending exec next, Done b ) ->
            Pending exec
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

        ( Pending _ next1, Pending _ next2 ) ->
            Pending Batch
                (\s ->
                    let
                        ( def1, s1, task1 ) =
                            next1 s
                    in
                    ( def1
                    , s1
                    , Pending Batch
                        (\s2 ->
                            let
                                ( def2, s3, task2 ) =
                                    next2 s2
                            in
                            ( def2
                            , s3
                            , map2 f task1 task2
                            )
                        )
                    )
                )


andThen : (a -> Task b) -> Task a -> Task b
andThen f task =
    case task of
        Done a ->
            f a

        Pending exec next ->
            Pending exec
                (\s ->
                    let
                        ( e, s_, task_ ) =
                            next s
                    in
                    case task_ of
                        Done a ->
                            ( e, s_, f a )

                        Pending _ _ ->
                            ( e, s_, andThen f task_ )
                )


succeed : a -> Task a
succeed a =
    Done a


create : Task String
create =
    Pending Sequential
        (\n ->
            ( Def n
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


run : Int -> List Def -> Task a -> a
run n todos task =
    case task of
        Done a ->
            let
                _ =
                    Debug.log "final todos" todos
            in
            a

        Pending exec next ->
            case exec of
                Sequential ->
                    let
                        ( e, n_, tsk ) =
                            next n

                        _ =
                            Debug.log "doing todos" todos
                    in
                    run n_ [ e ] tsk

                Batch ->
                    let
                        ( e, n_, tsk ) =
                            next n
                    in
                    run n_ (e :: todos) tsk
