module Internal.Utils.List exposing (chunk)

import Array exposing (Array)



-- Trampoline


type Trampoline a
    = Done a
    | Jump (() -> Trampoline a)


evaluate : Trampoline a -> a
evaluate trampoline =
    case trampoline of
        Done value ->
            value

        Jump f ->
            evaluate (f ())


{-| Split a list into chunks of length `n`.

Be aware that the last sub-list may be smaller than `n`-items long.

For example `chunk 3 [1..10] => [[1,2,3], [4,5,6], [7,8,9], [10]]`

-}
chunk : Int -> List a -> List (List a)
chunk n xs =
    if n < 1 then
        List.singleton xs

    else
        evaluate (chunkInternal n xs Array.empty)


chunkInternal : Int -> List a -> Array (List a) -> Trampoline (List (List a))
chunkInternal n xs acc =
    if List.isEmpty xs then
        Done (Array.toList acc)

    else
        Jump
            (\_ ->
                chunkInternal n
                    (List.drop n xs)
                    (Array.push (List.take n xs) acc)
            )
