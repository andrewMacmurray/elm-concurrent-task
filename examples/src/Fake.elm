module Fake exposing (..)

import Array exposing (Array)



-- Module to test out optimizations


type Task a
    = Pending (Array String) (Int -> ( Int, Task a ))
    | More (Task a)
    | Done a


map : (a -> b) -> Task a -> Task b
map f task =
    case task of
        Done a ->
            Done (f a)

        More next ->
            more (map f next)

        Pending xs next ->
            Pending xs
                (\s ->
                    let
                        ( s_, a ) =
                            next s
                    in
                    ( s_, map f a )
                )


more : Task a -> Task a
more t =
    More t


map2 : (a -> b -> c) -> Task a -> Task b -> Task c
map2 f taskA taskB =
    case ( taskA, taskB ) of
        ( Done a, Done b ) ->
            let
                _ =
                    log "1: (d, d)" ()
            in
            Done (f a b)

        ( Done _, More next ) ->
            let
                _ =
                    log "2: (d, m)" ()
            in
            map2 f (more taskA) next

        ( More next, Done _ ) ->
            let
                _ =
                    log "3: (m, d)" ()
            in
            map2 f next taskB

        ( More nextA, More nextB ) ->
            let
                _ =
                    log "4: (m, m)" ()
            in
            more (map2 f nextA nextB)

        ( Done _, Pending xs next ) ->
            let
                _ =
                    log "5: (d, p)" (Array.length xs)
            in
            Pending xs
                (\s ->
                    let
                        ( s_, taskB_ ) =
                            next s
                    in
                    ( s_, more (map2 f taskA taskB_) )
                )

        ( Pending xs next, Done _ ) ->
            let
                _ =
                    log "6: (p, d)" (Array.length xs)
            in
            Pending xs
                (\s ->
                    let
                        ( s_, taskA_ ) =
                            next s
                    in
                    ( s_, map2 f taskA_ taskB )
                )

        ( Pending xs nextA, More nextB ) ->
            let
                _ =
                    log "7: (p, m)" (Array.length xs)
            in
            more
                (Pending xs
                    (\s ->
                        let
                            ( s_, taskA_ ) =
                                nextA s
                        in
                        ( s_, map2 f taskA_ nextB )
                    )
                )

        ( More nextA, Pending xs nextB ) ->
            let
                _ =
                    log "8: (m, m)" (Array.length xs)
            in
            more
                (Pending xs
                    (\s ->
                        let
                            ( s_, taskB_ ) =
                                nextB s
                        in
                        ( s_, map2 f nextA taskB_ )
                    )
                )

        ( Pending xsA nextA, Pending xsB nextB ) ->
            let
                _ =
                    log "9: (p, p)" ( Array.length xsA, Array.length xsB )
            in
            more
                (Pending (Array.append xsA xsB)
                    (\s ->
                        let
                            ( s_, taskA_ ) =
                                nextA s

                            ( s__, taskB_ ) =
                                nextB s_
                        in
                        ( s__, map2 f taskA_ taskB_ )
                    )
                )


log : String -> a -> a
log label val =
    val


succeed : a -> Task a
succeed a =
    Done a


create : Task String
create =
    Pending (Array.fromList [ "a" ])
        (\n ->
            ( n + 1
            , Done (String.fromInt n)
            )
        )



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


addMany : Task Int -> List Int -> Task Int
addMany task xs =
    case xs of
        n :: rest ->
            addMany (map ((+) n) task) rest

        [] ->
            task


run : ( Int, Int ) -> Task a -> a
run ( n, runs ) task =
    case task of
        Done a ->
            a

        More next ->
            --let
            --    _ =
            --        Debug.log "more" ( n, runs )
            --in
            run ( n, runs + 1 ) next

        Pending _ next ->
            let
                ( n_, tsk ) =
                    next n

                --_ =
                --    Debug.log "pending" ( n_, runs )
            in
            run ( n_, runs + 1 ) tsk
