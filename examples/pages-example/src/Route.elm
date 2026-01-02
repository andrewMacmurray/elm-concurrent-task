module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing (s)


type Route
    = NotFound
    | Todos
    | Posts


fromUrl : Url -> Route
fromUrl =
    Parser.parse parser >> Maybe.withDefault NotFound


parser : Parser.Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Todos Parser.top
        , Parser.map Posts (s "posts")
        ]
