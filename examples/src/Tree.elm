module Tree exposing (..)

-- Tree


type Tree a
    = Tree ( Int, Tree_ a )


type Tree_ a
    = Tree_ (List Def) (Int -> ( Int, Tree_ a ))
    | Done a


type Def
    = Def String Int



-- Create


create : String -> a -> Tree a
create def a =
    Tree
        ( 0
        , Tree_ []
            (\i ->
                ( i + 1
                , Tree_ [ Def def i ]
                    (\i_ ->
                        ( i_
                        , Done a
                        )
                    )
                )
            )
        )


succeed : a -> Tree a
succeed a =
    Tree ( 0, Done a )



-- Example


example3 : String
example3 =
    let
        tree =
            List.repeat 10000 example2_
                |> batch
                |> map (String.join ",")
    in
    viewTree tree


example2 : String
example2 =
    viewTree example2_


example2_ =
    map2 (++)
        (map2 (++) example_ example_)
        (map2 (++) example_ example_)


batch : List (Tree a) -> Tree (List a)
batch =
    batchHelp (succeed [])


batchHelp : Tree (List a) -> List (Tree a) -> Tree (List a)
batchHelp trees xs =
    case xs of
        x :: rest ->
            batchHelp (map2 (::) x trees) rest

        [] ->
            trees


example_ : Tree String
example_ =
    map2 (++) aTree aTree


aTree : Tree String
aTree =
    create "root" "one"


viewTree : Tree String -> String
viewTree =
    viewTreeHelp ""


viewTreeHelp : String -> Tree String -> String
viewTreeHelp acc (Tree ( n, t )) =
    case t of
        Tree_ xs t_ ->
            let
                acc_ =
                    List.foldl
                        (\(Def name id) acc__ -> name ++ ":" ++ String.fromInt id ++ "," ++ acc__)
                        acc
                        xs

                ( state_, t__ ) =
                    t_ n
            in
            viewTreeHelp acc_ (Tree ( state_, t__ ))

        Done a ->
            acc ++ "Done(-- " ++ a ++ " --)"



-- Maps


map2 : (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 f (Tree ( ia, a )) (Tree ( ib, b )) =
    mapAccumulate2Help f a b ( [], [] ) ( ia, ib )


mapAccumulate2Help :
    (a -> b -> c)
    -> Tree_ a
    -> Tree_ b
    ->
        ( List
            ( Int
            , (Int -> ( Int, Tree_ c )) -> Tree_ c
            )
        , List
            ( Int
            , (Int -> ( Int, Tree_ c )) -> Tree_ c
            )
        )
    -> ( Int, Int )
    -> Tree c
mapAccumulate2Help f tA tB ( aStack, bStack ) ( aState, bState ) =
    case tA of
        Done a ->
            case tB of
                Done b ->
                    let
                        apply : ( Int, (Int -> ( Int, Tree_ c )) -> Tree_ c ) -> Tree_ c -> Tree_ c
                        apply ( i_, f_ ) acc =
                            f_ (\x -> ( x, acc ))

                        final : Tree_ c
                        final =
                            Done (f a b)

                        --_ =
                        --    Debug.log "final"
                        --        ( List.map Tuple.first aStack
                        --        , List.map Tuple.first bStack
                        --        )
                    in
                    Tree
                        ( aState + bState
                        , List.foldl apply final (aStack ++ bStack)
                        )

                Tree_ defs next ->
                    let
                        ( bState_, b ) =
                            next bState
                    in
                    mapAccumulate2Help f
                        tA
                        b
                        ( aStack
                        , ( bState_, Tree_ defs ) :: bStack
                        )
                        ( aState
                        , bState_
                        )

        Tree_ defs next ->
            let
                ( aState_, a ) =
                    next aState
            in
            mapAccumulate2Help f
                a
                tB
                ( ( aState_, Tree_ defs ) :: aStack
                , bStack
                )
                ( aState_
                , bState
                )


map : (a -> b) -> Tree a -> Tree b
map f (Tree ( i, a )) =
    mapAccumulateHelp f a [] i


mapAccumulateHelp : (a -> b) -> Tree_ a -> List ( Int, (Int -> ( Int, Tree_ b )) -> Tree_ b ) -> Int -> Tree b
mapAccumulateHelp f t stack state =
    case t of
        Done a ->
            let
                apply : ( Int, (Int -> ( Int, Tree_ b )) -> Tree_ b ) -> Tree_ b -> Tree_ b
                apply ( i_, f_ ) t_ =
                    f_ (\_ -> ( i_, t_ ))
            in
            Tree ( state, List.foldl apply (Done (f a)) stack )

        Tree_ ids a ->
            let
                ( state_, a_ ) =
                    a state
            in
            mapAccumulateHelp f a_ (( state_, Tree_ ids ) :: stack) state_
