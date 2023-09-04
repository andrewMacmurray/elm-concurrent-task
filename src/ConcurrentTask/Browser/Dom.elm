module ConcurrentTask.Browser.Dom exposing (focus, blur, getViewportOf)

{-|

@docs focus, blur, getViewportOf

-}

import Browser.Dom as Dom exposing (Error, Viewport)
import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as Decode exposing (Decoder)
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


{-| -}
getViewportOf : String -> ConcurrentTask Error Viewport
getViewportOf id =
    ConcurrentTask.define
        { function = "builtin:domGetViewportOf"
        , expect = ConcurrentTask.expectJson decodeViewport
        , errors = expectDomError id
        , args = Encode.string id
        }



-- Decoders


decodeViewport : Decoder Viewport
decodeViewport =
    Decode.map2
        (\scene viewport ->
            { scene = scene
            , viewport = viewport
            }
        )
        decodeScene_
        decodeViewport_


decodeScene_ : Decoder { width : Float, height : Float }
decodeScene_ =
    Decode.map2
        (\w h ->
            { width = w
            , height = h
            }
        )
        (Decode.field "scene" (Decode.field "width" Decode.float))
        (Decode.field "scene" (Decode.field "height" Decode.float))


decodeViewport_ : Decoder { x : Float, y : Float, width : Float, height : Float }
decodeViewport_ =
    Decode.map4
        (\x y w h ->
            { x = x
            , y = y
            , width = w
            , height = h
            }
        )
        (Decode.field "viewport" (Decode.field "x" Decode.float))
        (Decode.field "viewport" (Decode.field "y" Decode.float))
        (Decode.field "viewport" (Decode.field "width" Decode.float))
        (Decode.field "viewport" (Decode.field "height" Decode.float))


expectDomError : String -> ConcurrentTask.Errors Error a
expectDomError id =
    ConcurrentTask.expectErrors (Decode.succeed (Dom.NotFound id))
