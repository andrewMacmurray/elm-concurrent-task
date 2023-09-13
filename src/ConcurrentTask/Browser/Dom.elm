module ConcurrentTask.Browser.Dom exposing
    ( focus, blur
    , getViewport, getViewportOf
    , setViewport, setViewportOf
    , getElement
    )

{-| Drop in replacements for [elm/browser's](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom)'s `Browser.Dom` `Task`s.

The JavaScript runner has these tasks builtin by default. There shouldn't be much need to do this, but they can be overridden like so:

**NOTE:** You can see the [built-in implementations here](https://github.com/andrewMacmurray/elm-concurrent-task/blob/main/runner/browser/dom.ts). They follow the [`Elm.Kernel`](https://github.com/elm/browser/blob/1.0.2/src/Elm/Kernel/Browser.js) implementations as closely as possible.

    import * as ConcurrentTask from "@andrewMacmurray/elm-concurrent-task"

    ConcurrentTask.register({
      tasks: {},
      ports: app.ports,
      builtins: {
        domFocus: customFocus,
        domBlur: customBlur,
        domGetViewport: customGetViewport,
        domGetViewportOf: customGetViewportOf,
        domSetViewport: customSetViewport,
        domSetViewportOf: customSetViewportOf,
        domGetElement: customGetElement,
      }
    });


# Focus Tasks

@docs focus, blur


# Get Viewport Tasks

@docs getViewport, getViewportOf


# Set Viewport Tasks

@docs setViewport, setViewportOf


# Position Tasks

@docs getElement

-}

import Browser.Dom as Dom
import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| A direct replacement for `elm/browser`'s [`Browser.Dom.focus`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#focus).
-}
focus : String -> ConcurrentTask Dom.Error ()
focus id =
    ConcurrentTask.define
        { function = "builtin:domFocus"
        , expect = ConcurrentTask.expectWhatever
        , errors = expectDomError id
        , args = Encode.string id
        }


{-| A direct replacement for `elm/browser`'s [`Browser.Dom.blur`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#blur).
-}
blur : String -> ConcurrentTask Dom.Error ()
blur id =
    ConcurrentTask.define
        { function = "builtin:domBlur"
        , expect = ConcurrentTask.expectWhatever
        , errors = expectDomError id
        , args = Encode.string id
        }


{-| A direct replacement for `elm/browser`'s [`Browser.Dom.getViewport`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#getViewport).
-}
getViewport : ConcurrentTask x Dom.Viewport
getViewport =
    ConcurrentTask.define
        { function = "builtin:domGetViewport"
        , expect = ConcurrentTask.expectJson decodeViewport
        , errors = ConcurrentTask.expectNoErrors
        , args = Encode.null
        }


{-| A direct replacement for `elm/browser`'s [`Browser.Dom.getViewportOf`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#getViewportOf).
-}
getViewportOf : String -> ConcurrentTask Dom.Error Dom.Viewport
getViewportOf id =
    ConcurrentTask.define
        { function = "builtin:domGetViewportOf"
        , expect = ConcurrentTask.expectJson decodeViewport
        , errors = expectDomError id
        , args = Encode.string id
        }


{-| A direct replacement for `elm/browser`'s [`Browser.Dom.setViewport`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#setViewport).
-}
setViewport : Float -> Float -> ConcurrentTask x ()
setViewport x y =
    ConcurrentTask.define
        { function = "builtin:domSetViewport"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectNoErrors
        , args =
            Encode.object
                [ ( "x", Encode.float x )
                , ( "y", Encode.float y )
                ]
        }


{-| A direct replacement for `elm/browser`'s [`Browser.Dom.setViewportOf`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#setViewportOf).
-}
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


{-| A direct replacement for `elm/browser`'s [`Browser.Dom.getElement`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#getElement).
-}
getElement : String -> ConcurrentTask Dom.Error Dom.Element
getElement id =
    ConcurrentTask.define
        { function = "builtin:domGetElement"
        , expect = ConcurrentTask.expectJson decodeElement
        , errors = expectDomError id
        , args = Encode.string id
        }



-- Errors


expectDomError : String -> ConcurrentTask.Errors Dom.Error
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
