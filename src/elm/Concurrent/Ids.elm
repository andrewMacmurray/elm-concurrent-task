module Concurrent.Ids exposing
    ( Ids
    , init
    , next
    )

-- Opaque Sequence of Ids


type Ids
    = Ids Int


init : Ids
init =
    Ids 0


next : Ids -> ( String, Ids )
next (Ids ids) =
    ( String.fromInt ids
    , Ids (ids + 1)
    )
