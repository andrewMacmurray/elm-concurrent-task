module ConcurrentTask.Browser.Dom exposing
    ( focus, blur
    , getViewport, getViewportOf
    , setViewport, setViewportOf
    , getElement
    )

{-|

@docs focus, blur

@docs getViewport, getViewportOf

@docs setViewport, setViewportOf

@docs getElement

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
getViewport : ConcurrentTask x Dom.Viewport
getViewport =
    ConcurrentTask.define
        { function = "builtin:domGetViewport"
        , expect = ConcurrentTask.expectJson decodeViewport
        , errors = ConcurrentTask.catchAll fallbackViewport
        , args = Encode.null
        }


fallbackViewport : Dom.Viewport
fallbackViewport =
    { scene =
        { width = 0
        , height = 0
        }
    , viewport =
        { x = 0
        , y = 0
        , width = 0
        , height = 0
        }
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
setViewport : Float -> Float -> ConcurrentTask x ()
setViewport x y =
    ConcurrentTask.define
        { function = "builtin:domSetViewport"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.catchAll ()
        , args =
            Encode.object
                [ ( "x", Encode.float x )
                , ( "y", Encode.float y )
                ]
        }


{-| -}
setViewportOf : String -> Float -> Float -> ConcurrentTask Dom.Error ()
setViewportOf id x y =
    ConcurrentTask.define
        { function = "builtin:domSetViewportOf"
        , expect = ConcurrentTask.expectWhatever
        , errors = expectDomError id
        , args =
            Encode.object
                [ ( "id", Encode.string id )
                , ( "x", Encode.float x )
                , ( "y", Encode.float y )
                ]
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
        (Decode.at [ "scene", "width" ] Decode.float)
        (Decode.at [ "scene", "height" ] Decode.float)


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
        (Decode.at [ "viewport", "x" ] Decode.float)
        (Decode.at [ "viewport", "y" ] Decode.float)
        (Decode.at [ "viewport", "width" ] Decode.float)
        (Decode.at [ "viewport", "height" ] Decode.float)


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
        (Decode.at [ "element", "x" ] Decode.float)
        (Decode.at [ "element", "y" ] Decode.float)
        (Decode.at [ "element", "width" ] Decode.float)
        (Decode.at [ "element", "height" ] Decode.float)
