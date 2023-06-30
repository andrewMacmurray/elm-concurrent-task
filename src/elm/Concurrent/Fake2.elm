module Concurrent.Fake2 exposing (..)


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
                    let
                        ( s_, a ) =
                            next s
                    in
                    ( s_, map f a )
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
                        ( s_, task_ ) =
                            next s
                    in
                    case task_ of
                        Done a ->
                            ( s_, f a )

                        Pending _ ->
                            ( s_, andThen f task_ )
                )


succeed : a -> Task a
succeed a =
    Pending
        (\n ->
            ( n
            , Done a
            )
        )


create : Task String
create =
    Pending
        (\n ->
            ( n + 1
            , Done (String.fromInt n)
            )
        )



-- Sequence and Batch


sequence : List (Task a) -> Task (List a)
sequence tasks =
    sequenceHelp tasks (succeed [])


sequenceHelp : List (Task a) -> Task (List a) -> Task (List a)
sequenceHelp tasks task =
    case tasks of
        todo :: rest ->
            task |> andThen (\xs -> sequenceHelp rest (map (\a -> a :: xs) todo))

        [] ->
            task


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
