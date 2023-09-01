module Internal.Ids exposing
    ( Id
    , Ids
    , combine
    , get
    , init
    , next
    )

-- Opaque Sequence of Ids


type Ids
    = Sequence Int


type alias Id =
    String


init : Ids
init =
    Sequence 0


next : Ids -> Ids
next (Sequence id) =
    Sequence (id + 1)


get : Ids -> Id
get (Sequence id) =
    String.fromInt id


combine : Ids -> Ids -> Ids
combine (Sequence a) (Sequence b) =
    Sequence (max a b)
