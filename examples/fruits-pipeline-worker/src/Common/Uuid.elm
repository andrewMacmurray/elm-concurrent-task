module Common.Uuid exposing
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
        , errors = ConcurrentTask.catchAll "THIS_SHOULD_NOT_HAPPEN"
        , args = Encode.null
        }
