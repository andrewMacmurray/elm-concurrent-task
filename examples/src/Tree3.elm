module Tree3 exposing (..)


type Tree a
    = Tree (List String) (Tree a)
    | Done a


example =
    List.repeat 100000 (create "one" 1)
        |> batch
        |> run


run : Tree a -> a
run tree =
    case tree of
        Tree xs next ->
            let
                _ =
                    Debug.log "xs" xs
            in
            run next

        Done a ->
            a


batch : List (Tree a) -> Tree (List a)
batch =
    batchHelp (Done [])


batchHelp : Tree (List a) -> List (Tree a) -> Tree (List a)
batchHelp combined trees =
    case trees of
        tree :: rest ->
            batchHelp (map2 (::) tree combined) rest

        [] ->
            combined


create : String -> a -> Tree a
create name val =
    Tree [ name ] (Done val)


map2 : (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 f ta tb =
    map2Help f ta tb []


map2Help : (a -> b -> c) -> Tree a -> Tree b -> List (List String) -> Tree c
map2Help f ta tb stack =
    case ( ta, tb ) of
        ( Done a, Done b ) ->
            List.foldl (\xs acc -> Tree xs acc) (Done (f a b)) stack

        ( Done _, Tree x tb_ ) ->
            map2Help f ta tb_ (x :: stack)

        ( Tree x ta_, Done _ ) ->
            map2Help f ta_ tb (x :: stack)

        ( Tree xa ta_, Tree xb tb_ ) ->
            map2Help f ta_ tb_ ((xa ++ xb) :: stack)
