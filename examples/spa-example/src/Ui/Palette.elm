module Ui.Palette exposing
    ( error
    , orange
    , peach
    , red
    , success
    , white
    )

import Element


white : Element.Color
white =
    Element.rgb255 255 255 255


error : Element.Color
error =
    Element.rgb255 147 8 8


red : Element.Color
red =
    Element.rgb255 255 0 0


orange : Element.Color
orange =
    Element.rgb255 217 133 7


peach : Element.Color
peach =
    Element.rgb255 252 26 89


success : Element.Color
success =
    Element.rgb255 70 163 115
