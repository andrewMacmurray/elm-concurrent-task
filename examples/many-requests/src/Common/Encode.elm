module Common.Encode exposing (..)

import Json.Encode as Encode
import Time


encodeJsonl : (a -> Encode.Value) -> List a -> String
encodeJsonl encode_ =
    List.map (encode_ >> Encode.encode 0) >> String.join "\n"


time : Time.Posix -> Encode.Value
time =
    Time.posixToMillis >> Encode.int
