module Jobs2 exposing (..)


type Job a
    = Leaf a
    | Sequence String (Job a)
    | Parallel String (Job a)


type Step
    = InSequence String Int
    | InParallel String Int


values : Job a -> ( a, List Step )
values root =
    let
        go : Int -> ( Job a, List Step ) -> ( a, List Step )
        go i ( job, steps ) =
            case job of
                Leaf a ->
                    ( a, steps )

                Sequence def j ->
                    go (i + 1) ( j, InSequence def i :: steps )

                Parallel def j ->
                    go (i + 1) ( j, InParallel def i :: steps )
    in
    go 0 ( root, [] )


create : Job String
create =
    nest 1000000 (Parallel "one" (Leaf "Done"))


nest : Int -> Job a -> Job a
nest x job =
    let
        go j xs =
            case xs of
                [] ->
                    j

                _ :: rest ->
                    go (Sequence "nest" j) rest
    in
    go job (List.repeat x ())
