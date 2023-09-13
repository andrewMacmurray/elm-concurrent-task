module Utils.Uuid exposing
    ( Uuid
    , generate
    )

import ConcurrentTask exposing (ConcurrentTask)
import Json.Encode as Encode


type alias Uuid =
    String


generate : ConcurrentTask x Uuid
generate =
    ConcurrentTask.define
        { function = "uuid:generate"
        , expect = ConcurrentTask.expectString
        , errors = ConcurrentTask.expectNoErrors
        , args = Encode.null
        }
