module RecExample exposing (..)

import Recursion


type Tree a
    = Leaf a
    | Node String (Tree a)


map : (a -> b) -> Tree a -> Tree b
map f =
    Recursion.runRecursion
        (\tree ->
            case tree of
                Leaf a ->
                    Recursion.base (Leaf (f a))

                Node x t ->
                    Recursion.recurse t (\t_ -> Recursion.base (Node x t_))
        )


map2 : (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 f ta tb =
    Recursion.runRecursion
        (\( ta_, tb_ ) ->
            case ( ta_, tb_ ) of
                ( Leaf a, Leaf b ) ->
                    Recursion.base (Leaf (f a b))

                ( Leaf a, Node x b ) ->
                    Recursion.recurse ( Leaf a, b ) (\c -> Recursion.base (Node x c))

                ( Node x a, Leaf b ) ->
                    Recursion.recurse ( a, Leaf b ) (\c -> Recursion.base (Node x c))

                ( Node x1 a, Node x2 b ) ->
                    Recursion.recurse ( a, b ) (\c -> Recursion.base (Node x1 (Node x2 c)))
        )
        ( ta, tb )


deep : Int -> Tree a -> Tree a
deep n tree =
    if n == 0 then
        tree

    else
        deep (n - 1) (Node (String.fromInt n) tree)


printTree : Tree Int -> String
printTree =
    printTree_ ""


printTree_ : String -> Tree Int -> String
printTree_ acc tree =
    case tree of
        Leaf n ->
            acc ++ "Leaf(-- " ++ String.fromInt n ++ " --)"

        Node x tree_ ->
            printTree_ ("Node:" ++ x ++ "," ++ acc) tree_


aTree : Tree Int
aTree =
    deep 10000 (Leaf 4)


example : String
example =
    map2 (+) aTree aTree
        |> printTree
