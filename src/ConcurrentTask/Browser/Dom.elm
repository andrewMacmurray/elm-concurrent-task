module ConcurrentTask.Browser.Dom exposing (focus, blur, getViewportOf, getElement)

{-|

@docs focus, blur, getViewportOf, getElement

-}

import Browser.Dom as Dom
import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| -}
focus : String -> ConcurrentTask Dom.Error ()
focus id =
    ConcurrentTask.define
        { function = "builtin:domFocus"
        , expect = ConcurrentTask.expectWhatever
        , errors = expectDomError id
        , args = Encode.string id
        }


{-| -}
blur : String -> ConcurrentTask Dom.Error ()
blur id =
    ConcurrentTask.define
        { function = "builtin:domBlur"
        , expect = ConcurrentTask.expectWhatever
        , errors = expectDomError id
        , args = Encode.string id
        }


{-| -}
getViewportOf : String -> ConcurrentTask Dom.Error Dom.Viewport
getViewportOf id =
    ConcurrentTask.define
        { function = "builtin:domGetViewportOf"
        , expect = ConcurrentTask.expectJson decodeViewport
        , errors = expectDomError id
        , args = Encode.string id
        }


{-| -}
getElement : String -> ConcurrentTask Dom.Error Dom.Element
getElement id =
    ConcurrentTask.define
        { function = "builtin:domGetElement"
        , expect = ConcurrentTask.expectJson decodeElement
        , errors = expectDomError id
        , args = Encode.string id
        }



-- Errors


expectDomError : String -> ConcurrentTask.Errors Dom.Error a
expectDomError id =
    ConcurrentTask.expectErrors (Decode.succeed (Dom.NotFound id))



-- Decoders


decodeElement : Decoder Dom.Element
decodeElement =
    Decode.map3
        (\scene viewport element ->
            { scene = scene
            , viewport = viewport
            , element = element
            }
        )
        decodeScene_
        decodeViewport_
        decodeElement_


decodeViewport : Decoder Dom.Viewport
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


decodeElement_ : Decoder { x : Float, y : Float, width : Float, height : Float }
decodeElement_ =
    Decode.map4
        (\x y w h ->
            { x = x
            , y = y
            , width = w
            , height = h
            }
        )
        (Decode.field "element" (Decode.field "x" Decode.float))
        (Decode.field "element" (Decode.field "y" Decode.float))
        (Decode.field "element" (Decode.field "width" Decode.float))
        (Decode.field "element" (Decode.field "height" Decode.float))
