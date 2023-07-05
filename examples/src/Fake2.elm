module Fake2 exposing (..)

-- Module to test out optimizations


type Task a
    = Pending (Int -> ( Int, Def, Task a ))
    | Batch (Int -> ( Int, Def, Task a ))
    | Done a


type Def
    = Exec Int


map : (a -> b) -> Task a -> Task b
map f task =
    case task of
        Done a ->
            Done (f a)

        Batch next ->
            Batch
                (\s ->
                    let
                        ( s_, def, a ) =
                            next s
                    in
                    ( s_, def, map f a )
                )

        Pending next ->
            Pending
                (\s ->
                    let
                        ( s_, def, a ) =
                            next s
                    in
                    ( s_, def, map f a )
                )


log : String -> a -> a
log label val =
    val


andThen : (a -> Task b) -> Task a -> Task b
andThen f task =
    case task of
        Done a ->
            f a

        Batch next ->
            Batch
                (\s ->
                    let
                        ( s_, def, task_ ) =
                            next s
                    in
                    case task_ of
                        Done a ->
                            ( s_, def, f a )

                        Batch _ ->
                            ( s_, def, andThen f task_ )

                        Pending _ ->
                            ( s_, def, andThen f task_ )
                )

        Pending next ->
            Pending
                (\s ->
                    let
                        ( s_, def, task_ ) =
                            next s
                    in
                    case task_ of
                        Done a ->
                            ( s_, def, f a )

                        Batch _ ->
                            ( s_, def, andThen f task_ )

                        Pending _ ->
                            ( s_, def, andThen f task_ )
                )


map2 : (a -> b -> c) -> Task a -> Task b -> Task c
map2 f ta tb =
    Debug.todo ""


succeed : a -> Task a
succeed a =
    Done a


create : Task String
create =
    Pending
        (\n ->
            ( n + 1
            , Exec n
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
            combined |> andThen (\xs -> batchHelp rest (map (\x -> x :: xs) task))

        [] ->
            combined


addMany : Task Int -> List Int -> Task Int
addMany task xs =
    case xs of
        n :: rest ->
            addMany (map ((+) n) task) rest

        [] ->
            task


run : Int -> List Def -> Task a -> a
run n todos task =
    case task of
        Done a ->
            let
                _ =
                    Debug.log "final todos" todos
            in
            a

        Batch next ->
            let
                ( n_, def, tsk ) =
                    next n
            in
            run n_ (def :: todos) tsk

        Pending next ->
            let
                ( n_, def, tsk ) =
                    next n

                _ =
                    Debug.log "doing todos" todos
            in
            run n_ [ def ] tsk
