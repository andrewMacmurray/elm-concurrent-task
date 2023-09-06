module Ui.Text exposing
    ( baseSize
    , bold
    , color
    , f1
    , f2
    , f3
    , f4
    , f5
    , f6
    , f7
    , f8
    , fonts
    , fxl
    , fxxl
    , paragraph
    , spaced
    , text
    )

import Element exposing (Attribute, Element)
import Element.Font as Font
import Ui.Spacing as Spacing



-- Config


fxxl : Element.Attr decorative msg
fxxl =
    Font.size 64


fxl : Element.Attr decorative msg
fxl =
    Font.size 48


f1 : Element.Attr decorative msg
f1 =
    Font.size 36


f2 : Element.Attr decorative msg
f2 =
    Font.size 30


f3 : Element.Attr decorative msg
f3 =
    Font.size 24


f4 : Element.Attr decorative msg
f4 =
    Font.size 20


f5 : Element.Attr decorative msg
f5 =
    Font.size 18


f6 : Element.Attr decorative msg
f6 =
    Font.size baseSize


f7 : Element.Attr decorative msg
f7 =
    Font.size 14


f8 : Element.Attr decorative msg
f8 =
    Font.size 12


baseSize : number
baseSize =
    16


fonts : Attribute msg
fonts =
    Font.family (List.map Font.typeface fontStack)


fontStack : List String
fontStack =
    [ "system-ui"
    , "Open Sans"
    , "Arial"
    , "sans-serif"
    ]



-- Text


text : List (Attribute msg) -> String -> Element msg
text attrs content =
    Element.el (f5 :: attrs) (Element.text content)


paragraph : List (Attribute msg) -> List (Element msg) -> Element msg
paragraph attrs =
    Element.paragraph (Element.spacing Spacing.s :: attrs)



-- Spacing


spaced : Attribute msg
spaced =
    Font.letterSpacing 1.3



-- Utils


color : Element.Color -> Element.Attr decorative msg
color =
    Font.color


bold : Attribute msg
bold =
    Font.bold
