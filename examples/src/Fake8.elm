module Fake8 exposing (..)


type Trampoline a
    = More (() -> Trampoline a)
    | Done a


wrapMany : Int -> Trampoline a -> Trampoline a
wrapMany n trampoline =
    if n > 0 then
        wrapMany (n - 1) (more trampoline)

    else
        trampoline


more : Trampoline a -> Trampoline a
more a =
    More (\_ -> a)


run : Trampoline a -> a
run trampoline =
    case trampoline of
        More next ->
            run (next ())

        Done a ->
            a


example : String
example =
    Done "finished"
        |> wrapMany 1000000
        |> run
