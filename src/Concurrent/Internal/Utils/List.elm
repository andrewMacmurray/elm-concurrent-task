module Concurrent.Internal.Utils.List exposing (chunk)

import Array exposing (Array)


{-| Adapted from <https://package.elm-lang.org/packages/krisajenkins/elm-exts/latest/Exts-List#chunk>

Split a list into chunks of length `n`.

Be aware that the last sub-list may be smaller than `n`-items long.

For example `chunk 3 [1..10] => [[1,2,3], [4,5,6], [7,8,9], [10]]`

-}
chunk : Int -> List a -> List (List a)
chunk n xs =
    if n < 1 then
        List.singleton xs

    else
        eval (chunkHelp n xs Array.empty)


chunkHelp : Int -> List a -> Array (List a) -> Trampoline (List (List a))
chunkHelp n xs acc =
    if List.isEmpty xs then
        Done (Array.toList acc)

    else
        Jump
            (\_ ->
                chunkHelp n
                    (List.drop n xs)
                    (Array.push (List.take n xs) acc)
            )



-- Trampoline


type Trampoline a
    = Done a
    | Jump (() -> Trampoline a)


eval : Trampoline a -> a
eval trampoline =
    case trampoline of
        Done value ->
            value

        Jump f ->
            eval (f ())
