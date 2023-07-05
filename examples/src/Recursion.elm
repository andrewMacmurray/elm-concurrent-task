module Recursion exposing (..)


type Rec r t
    = Base t
    | Recurse r (t -> Rec r t)


{-| The base case of a recursion. The value is injected directly into the `Rec` type.
-}
base : t -> Rec r t
base =
    Base


{-| Recurse on a value.

When the recursion is complete the `Rec` will contain a value of type `t`.

-}
recurse : r -> (t -> Rec r t) -> Rec r t
recurse =
    Recurse


{-| Run a recursion given a function to run one layer and an initial value.
-}
runRecursion : (r -> Rec r t) -> r -> t
runRecursion project init =
    let
        go : Rec r t -> List (t -> Rec r t) -> t
        go step stack =
            case step of
                Base t ->
                    case stack of
                        [] ->
                            t

                        next :: rest ->
                            go (next t) rest

                Recurse r after ->
                    go (project r) (after :: stack)
    in
    go (project init) []
