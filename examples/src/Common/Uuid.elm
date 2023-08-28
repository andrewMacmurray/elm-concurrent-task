module Common.Uuid exposing
    ( Uuid
    , generate
    )

import Concurrent.Task as Task exposing (Task)
import Json.Encode as Encode


type alias Uuid =
    String


generate : Task x Uuid
generate =
    Task.define
        { function = "uuid:generate"
        , expect = Task.expectString
        , errors = Task.catchAll "THIS_SHOULD_NOT_HAPPEN"
        , args = Encode.null
        }
