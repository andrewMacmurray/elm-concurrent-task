module Fake4 exposing (..)

-- Module to test out optimizations

import Recursion


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
                    Recursion.runRecursion
                        (\next_ ->
                            let
                                ( defs, s_, task ) =
                                    next_ s
                            in
                            case task of
                                Done b ->
                                    Recursion.base ( defs, s_, Done (f a b) )

                                Pending next__ ->
                                    Recursion.recurse next__
                        )
                        next
                )

        ( Pending next, Done b ) ->
            Pending
                (\s ->
                    Recursion.runRecursion
                        (\next_ ->
                            let
                                ( defs, s_, task ) =
                                    next_ s
                            in
                            case task of
                                Done a ->
                                    Recursion.base ( defs, s_, Done (f a b) )

                                Pending next__ ->
                                    Recursion.recurse next__
                        )
                        next
                )

        ( Pending next1, Pending next2 ) ->
            Pending
                (\s ->
                    Recursion.runRecursion
                        (\( next1_, next2_ ) ->
                            let
                                ( defs1, s_, task1 ) =
                                    next1_ s

                                ( defs2, s__, task2 ) =
                                    next2_ s_
                            in
                            case ( task1, task2 ) of
                                ( Done a, Done b ) ->
                                    Recursion.base ( defs1 ++ defs2, max s_ s__, Done (f a b) )

                                ( Done a, Pending next__ ) ->
                                    Recursion.recurse ( Debug.todo "", next__ )

                                ( Pending next__, Done b ) ->
                                    Recursion.recurse ( next__, Debug.todo "" )

                                ( Pending next1__, Pending next2__ ) ->
                                    Recursion.recurse ( next1__, next2__ )
                        )
                        ( next1, next2 )
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


run : Int -> List Def -> Task a -> a
run n todos task =
    case task of
        Done a ->
            let
                _ =
                    Debug.log "final todos" todos
            in
            a

        Pending next ->
            let
                ( e, n_, tsk ) =
                    next n
            in
            run n_ e tsk
