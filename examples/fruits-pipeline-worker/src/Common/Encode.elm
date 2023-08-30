module Common.Encode exposing (time)

import Json.Encode as Encode
import Time


time : Time.Posix -> Encode.Value
time =
    Time.posixToMillis >> Encode.int
