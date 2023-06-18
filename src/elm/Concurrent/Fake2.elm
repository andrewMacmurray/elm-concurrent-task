module Concurrent.Fake2 exposing (..)

import Recursion


type Task a
    = Pending (Int -> ( Int, Task a ))
    | Done a


map : (a -> b) -> Task a -> Task b
map f task =
    case task of
        Done a ->
            Done (f a)

        Pending next ->
            Pending
                (\s ->
                    Recursion.runRecursion
                        (\next_ ->
                            let
                                ( s_, task_ ) =
                                    next_ s
                            in
                            case task_ of
                                Done a ->
                                    Recursion.base ( s_, Done (f a) )

                                Pending next__ ->
                                    Recursion.recurse next__
                        )
                        next
                )


andThen : (a -> Task b) -> Task a -> Task b
andThen f task =
    case task of
        Done a ->
            f a

        Pending next ->
            Pending
                (\s ->
                    Recursion.runRecursion
                        (\next_ ->
                            let
                                ( s_, task_ ) =
                                    next_ s
                            in
                            case task_ of
                                Done a ->
                                    Recursion.base ( s_, f a )

                                Pending next__ ->
                                    Recursion.recurse next__
                        )
                        next
                )


create : Task String
create =
    Pending
        (\n ->
            ( n + 1
            , Done ("some value " ++ String.fromInt n)
            )
        )


run : Int -> Task a -> a
run n task =
    case task of
        Done a ->
            a

        Pending next ->
            let
                ( n_, tsk ) =
                    next n
            in
            run n_ tsk
