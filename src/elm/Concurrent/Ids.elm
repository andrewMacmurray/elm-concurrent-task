module Concurrent.Ids exposing
    ( Ids
    , get
    , init
    , next
    )

-- Opaque Sequence of Ids


type Ids
    = Ids Int


init : Ids
init =
    Ids 0


next : Ids -> Ids
next (Ids id) =
    Ids (id + 1)


get : Ids -> String
get (Ids id) =
    String.fromInt id
