module Concurrent.Id exposing
    ( Id
    , Sequence
    , combine
    , get
    , init
    , next
    )

-- Opaque Sequence of Ids


type Sequence
    = Sequence Int


type alias Id =
    String


init : Sequence
init =
    Sequence 0


next : Sequence -> Sequence
next (Sequence id) =
    Sequence (id + 1)


get : Sequence -> Id
get (Sequence id) =
    String.fromInt id


combine : Sequence -> Sequence -> Sequence
combine (Sequence a) (Sequence b) =
    Sequence (max a b)
