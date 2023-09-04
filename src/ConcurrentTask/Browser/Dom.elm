module ConcurrentTask.Browser.Dom exposing (focus, blur)

{-|

@docs focus, blur

-}

import Browser.Dom as Dom exposing (Error)
import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as Decode
import Json.Encode as Encode


{-| -}
focus : String -> ConcurrentTask Error ()
focus id =
    ConcurrentTask.define
        { function = "builtin:domFocus"
        , expect = ConcurrentTask.expectWhatever
        , errors = expectDomError id
        , args = Encode.string id
        }


{-| -}
blur : String -> ConcurrentTask Error ()
blur id =
    ConcurrentTask.define
        { function = "builtin:domBlur"
        , expect = ConcurrentTask.expectWhatever
        , errors = expectDomError id
        , args = Encode.string id
        }


expectDomError : String -> ConcurrentTask.Errors Error a
expectDomError id =
    ConcurrentTask.expectErrors (Decode.succeed (Dom.NotFound id))
