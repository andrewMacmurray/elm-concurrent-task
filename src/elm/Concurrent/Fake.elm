module Concurrent.Fake exposing (..)

-- Module to test out optimizations


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


map2 : (a -> b -> c) -> Task a -> Task b -> Task c
map2 f taskA taskB =
    case ( taskA, taskB ) of
        ( Done a, Done b ) ->
            Done (f a b)

        ( Done _, Pending next ) ->
            Pending
                (\s ->
                    let
                        ( s_, taskB_ ) =
                            next s
                    in
                    ( s_, map2 f taskA taskB_ )
                )

        ( Pending next, Done _ ) ->
            Pending
                (\s ->
                    let
                        ( s_, taskA_ ) =
                            next s
                    in
                    ( s_, map2 f taskA_ taskB )
                )

        ( Pending nextA, Pending nextB ) ->
            Pending
                (\s ->
                    let
                        ( s_, taskA_ ) =
                            nextA s

                        ( s__, taskB_ ) =
                            nextB s_
                    in
                    ( s__, map2 f taskA_ taskB_ )
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



-- Sequence


sequence : List (Task a) -> Task (List a)
sequence tasks =
    sequenceHelp tasks (succeed [])


sequenceHelp : List (Task a) -> Task (List a) -> Task (List a)
sequenceHelp tasks combined =
    case tasks of
        task :: rest ->
            combined |> andThen (\xs -> sequenceHelp rest (map (\a -> a :: xs) task))

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
            map2 (::) task (batchHelp rest combined)

        [] ->
            combined


addMany : Task Int -> List Int -> Task Int
addMany task xs =
    case xs of
        n :: rest ->
            addMany (map ((+) n) task) rest

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
