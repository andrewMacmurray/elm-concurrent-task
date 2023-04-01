module Concurrent.Ids exposing (Ids, init, next)


type alias Ids =
    Int


init : Ids
init =
    0


next : Ids -> ( String, Ids )
next ids =
    ( String.fromInt ids
    , ids + 1
    )
