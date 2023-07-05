module Fake9 exposing (..)

import Array exposing (Array)



-- Module to test out optimizations


type Task a
    = Pending (Array String) (Task a)
    | Done a


map : (a -> b) -> Task a -> Task b
map f task =
    case task of
        Done a ->
            Done (f a)

        Pending xs next ->
            Pending xs (map f next)


map2 : (a -> b -> c) -> Task a -> Task b -> Task c
map2 f taskA taskB =
    case ( taskA, taskB ) of
        ( Done a, Done b ) ->
            Done (f a b)

        ( Done _, Pending xs next ) ->
            Pending xs (map2 f taskA next)

        ( Pending xs next, Done _ ) ->
            Pending xs (map2 f next taskB)

        ( Pending xsA nextA, Pending xsB nextB ) ->
            Pending (Array.append xsA xsB) (map2 f nextA nextB)


succeed : a -> Task a
succeed a =
    Done a


create : Task String
create =
    Pending (Array.fromList [ "a" ]) (Done "1")



-- Batch


batch : List (Task a) -> Task (List a)
batch tasks =
    batchHelp tasks (succeed [])


batchHelp : List (Task a) -> Task (List a) -> Task (List a)
batchHelp tasks combined =
    case tasks of
        task :: rest ->
            batchHelp rest (map2 (\xs x -> x :: xs) combined task)

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
run runs task =
    case task of
        Done a ->
            a

        Pending _ next ->
            run (runs + 1) next
