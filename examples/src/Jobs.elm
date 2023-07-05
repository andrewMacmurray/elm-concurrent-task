module Jobs exposing (..)


type Job a
    = Done a
    | Sequence (Int -> Def) (Job a)
    | Parallel (Int -> Def) (Job a)


type Def
    = Def String Int


aJob : Job String
aJob =
    Parallel (Def "a")
        (Parallel (Def "b")
            (Parallel (Def "c")
                (Sequence (Def "d")
                    (Sequence (Def "e")
                        (Done "done")
                    )
                )
            )
        )


map2 : (a -> b -> c) -> Job a -> Job b -> Job c
map2 f jobA jobB =
    case ( jobA, jobB ) of
        ( Done a, Done b ) ->
            Done (f a b)

        ( Done a, Parallel def b ) ->
            Parallel def (map2 f (Done a) b)

        ( Done a, Sequence def b ) ->
            Parallel def (map2 f (Done a) b)

        ( Sequence def a, Done b ) ->
            Parallel def (map2 f a (Done b))

        ( Parallel def a, Done b ) ->
            Parallel def (map2 f a (Done b))

        ( Parallel defA a, Parallel defB b ) ->
            Parallel defA (Parallel defB (map2 f a b))

        ( Parallel defA a, Sequence defB b ) ->
            Parallel defA (Sequence defB (map2 f a b))

        ( Sequence defA a, Sequence defB b ) ->
            Parallel defA (Sequence defB (map2 f a b))

        ( Sequence defA a, Parallel defB b ) ->
            Parallel defA (Parallel defB (map2 f a b))


create : a -> Job a
create a =
    Sequence (Def "create") (Done a)


combined : Job String
combined =
    map2 (++)
        (create "foo")
        (create "bar")
