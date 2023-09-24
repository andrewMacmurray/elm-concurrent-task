module View exposing
    ( View, map
    , none, fromString
    , toBrowserDocument
    )

{-|

@docs View, map
@docs none, fromString
@docs toBrowserDocument

-}

import Browser
import Element exposing (..)
import Element.Font as Font
import Route exposing (Route)
import Shared.Model
import Ui.Spacing as Spacing
import Ui.Text as Text


type alias View msg =
    { title : String
    , body : Element msg
    }


{-| Used internally by Elm Land to create your application
so it works with Elm's expected `Browser.Document msg` type.
-}
toBrowserDocument :
    { shared : Shared.Model.Model
    , route : Route ()
    , view : View msg
    }
    -> Browser.Document msg
toBrowserDocument { view } =
    { title = view.title
    , body =
        [ Element.layout
            [ Text.fonts
            , paddingXY Spacing.s Spacing.s
            ]
            (column [ spacing Spacing.s ]
                [ navBar
                , view.body
                ]
            )
        ]
    }


navBar : Element msg
navBar =
    row [ spacing Spacing.xs ]
        [ navLink "home" "/"
        , navLink "posts" "/posts"
        ]


navLink : String -> String -> Element msg
navLink name url =
    Element.link []
        { url = url
        , label = Text.text [ Text.f6, Font.underline ] name
        }


{-| Used internally by Elm Land to connect your pages together.
-}
map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn view =
    { title = view.title
    , body = Element.map fn view.body
    }


{-| Used internally by Elm Land whenever transitioning between
authenticated pages.
-}
none : View msg
none =
    { title = ""
    , body = Element.none
    }


{-| If you customize the `View` module, anytime you run `elm-land add page`,
the generated page will use this when adding your `view` function.

That way your app will compile after adding new pages, and you can see
the new page working in the web browser!

-}
fromString : String -> View msg
fromString moduleName =
    { title = moduleName
    , body = Element.text moduleName
    }
