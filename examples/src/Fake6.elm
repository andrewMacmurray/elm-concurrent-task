module Fake6 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Task


type Task x a
    = Pending (Results -> Int -> ( Int, Array Definition, Task x a ))
    | Done (Result x a)


type Definition
    = Definition String Int


type alias Results =
    Dict Int Decode.Value



-- Define a Task


define : Decoder a -> String -> Task Decode.Error a
define decode def =
    Pending
        (\results id ->
            case Dict.get id results of
                Just result ->
                    ( id + 1
                    , Array.fromList [ Definition def id ]
                    , fromResult (Decode.decodeValue decode result)
                    )

                Nothing ->
                    ( id + 1
                    , Array.empty
                    , runWith id (define decode def)
                    )
        )


runWith : Int -> Task x a -> Task x a
runWith id task =
    case task of
        Done a ->
            Done a

        Pending next ->
            Pending (\res _ -> next res id)



-- Maps


map : (a -> b) -> Task x a -> Task x b
map f task =
    case task of
        Done a ->
            Done (Result.map f a)

        Pending next ->
            Pending
                (\results id ->
                    let
                        ( id_, xs, t ) =
                            next results id
                    in
                    ( id_, xs, map f t )
                )


map2 : (a -> b -> c) -> Task x a -> Task x b -> Task x c
map2 f t1 t2 =
    case ( t1, t2 ) of
        ( Done a, Done b ) ->
            Done (Result.map2 f a b)

        ( Done _, Pending next ) ->
            Pending
                (\res id ->
                    let
                        ( id_, xs, t ) =
                            next res id
                    in
                    ( id_, xs, map2 f t1 t )
                )

        ( Pending next, Done _ ) ->
            Pending
                (\res id ->
                    let
                        ( id_, xs, t ) =
                            next res id
                    in
                    ( id_, xs, map2 f t t2 )
                )

        ( Pending next1, Pending next2 ) ->
            Pending
                (\res id ->
                    let
                        ( id_, xs2, t2_ ) =
                            next2 res id

                        ( id__, xs1, t1_ ) =
                            next1 res id_
                    in
                    ( max id_ id__
                    , Array.append xs1 xs2
                    , map2 f t1_ t2_
                    )
                )



-- AndThen


andThen : (a -> Task x b) -> Task x a -> Task x b
andThen f task =
    case task of
        Done (Ok a) ->
            f a

        Done (Err e) ->
            fail e

        Pending next ->
            Pending
                (\res id ->
                    let
                        ( id_, xs, t ) =
                            next res id
                    in
                    ( id_, xs, andThen f t )
                )


succeed : a -> Task x a
succeed a =
    fromResult (Ok a)


fail : a -> Task a value
fail e =
    fromResult (Err e)


fromResult : Result x a -> Task x a
fromResult =
    Done



-- Sequence


sequence : List (Task x a) -> Task x (List a)
sequence tasks =
    -- This works with 1M plus tasks
    sequenceHelp tasks (succeed []) |> map List.reverse


sequenceHelp : List (Task x a) -> Task x (List a) -> Task x (List a)
sequenceHelp tasks combined =
    case tasks of
        task :: rest ->
            andThen (\xs -> sequenceHelp rest (map (\x -> x :: xs) task)) combined

        [] ->
            combined



-- Batch


batch : List (Task x a) -> Task x (List a)
batch tasks =
    -- This blows the stack ~10K tasks
    batchHelp tasks (succeed []) |> map List.reverse


batchHelp : List (Task x a) -> Task x (List a) -> Task x (List a)
batchHelp tasks combined =
    case tasks of
        task :: rest ->
            batchHelp rest (map2 (::) task combined)

        [] ->
            combined



-- Run a Task


attempt : Results -> Task x a -> Result x a
attempt results task =
    doAttempt 0 results task


doAttempt : Int -> Results -> Task x a -> Result x a
doAttempt id results task =
    case task of
        Done res ->
            res

        Pending next ->
            let
                ( nextId, defs, task_ ) =
                    next results id

                _ =
                    Debug.log "doing tasks" defs
            in
            doAttempt nextId results task_



-- Example


doIt : Result Decode.Error (List String)
doIt =
    (stringTask
        |> batchOf 10
        |> batchOf 10
        |> batchOf 10
        |> batchOf 10
        |> batchOf 10
        |> batchOf 10
    )
        |> map
            (List.concat
                >> List.concat
                >> List.concat
                >> List.concat
                >> List.concat
            )
        |> attempt bagOfResults


doItNested : Result Decode.Error (List String)
doItNested =
    (batch
        (List.repeat 10
            (batch
                (List.repeat 10
                    (batch
                        (List.repeat 10
                            (batch
                                (List.repeat 10
                                    (batch
                                        (List.repeat 10
                                            (batch
                                                (List.repeat 10 stringTask)
                                            )
                                        )
                                        |> map List.concat
                                    )
                                )
                                |> map List.concat
                            )
                        )
                        |> map List.concat
                    )
                )
                |> map List.concat
            )
        )
        |> map List.concat
    )
        |> attempt bagOfResults


stringTask : Task Decode.Error String
stringTask =
    define Decode.string "stringTask"


batchOf : Int -> Task x a -> Task x (List a)
batchOf n task =
    batch (List.repeat n task)


bagOfResults : Dict Int Encode.Value
bagOfResults =
    List.range 0 1000000
        |> List.map (\i -> ( i, Encode.string (String.fromInt i) ))
        |> Dict.fromList
