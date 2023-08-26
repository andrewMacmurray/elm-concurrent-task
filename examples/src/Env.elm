module Env exposing
    ( Error
    , Parser
    , andThen
    , bool
    , fail
    , float
    , int
    , load
    , map
    , optional
    , parse
    , printError
    , required
    , string
    , succeed
    , withDefault
    )

import Concurrent.Task as Task exposing (Task)
import Json.Decode as Decode
import Json.Encode as Encode



-- Load Env


load : Parser a -> Task Error a
load parser =
    Task.define
        { function = "env:load"
        , expect = Task.expectJson Decode.value
        , errors = Task.catchAll Encode.null
        , args = Encode.null
        }
        |> Task.map (parse parser)
        |> Task.andThen Task.fromResult



-- Env Parser


type Parser a
    = Parser (Decode.Value -> Result Error a)


type Error
    = Error (List Detail)


type alias Detail =
    { name : String
    , reason : Reason
    }


type Reason
    = MissingRequiredField
    | UnexpectedValue String
    | CustomError String


parse : Parser a -> Decode.Value -> Result Error a
parse (Parser parse_) value =
    parse_ value



-- Print Error


printError : Error -> String
printError (Error errors) =
    "There were the following problems parsing the given Env:\n\n"
        ++ String.join "\n" (List.map printDetail errors)


printDetail : Detail -> String
printDetail detail =
    detail.name ++ ": " ++ printReason detail.reason


printReason : Reason -> String
printReason reason =
    case reason of
        MissingRequiredField ->
            "Missing required field"

        UnexpectedValue help ->
            "Required field present but " ++ help

        CustomError help ->
            "Parsing logic failed - " ++ help



-- String


string : String -> Parser String
string name =
    Parser
        (decode
            { name = name
            , help = "expecting a STRING"
            , decoder = Decode.string
            }
        )



-- Int


int : String -> Parser Int
int name =
    Parser
        (decode
            { name = name
            , help = "expecting an INT"
            , decoder = int_
            }
        )


int_ : Decode.Decoder Int
int_ =
    Decode.string
        |> Decode.andThen
            (\s ->
                case String.toInt s of
                    Just i ->
                        Decode.succeed i

                    Nothing ->
                        Decode.fail "Unrecognized Int"
            )



-- Bool


bool : String -> Parser Bool
bool name =
    Parser
        (decode
            { name = name
            , help = "expecting a BOOL"
            , decoder = bool_
            }
        )


bool_ : Decode.Decoder Bool
bool_ =
    Decode.string
        |> Decode.andThen
            (\s ->
                case String.toUpper s of
                    "TRUE" ->
                        Decode.succeed True

                    "FALSE" ->
                        Decode.succeed False

                    _ ->
                        Decode.fail "Unrecognized Bool"
            )



-- Float


float : String -> Parser Float
float name =
    Parser
        (decode
            { name = name
            , help = "expecting a FLOAT"
            , decoder = float_
            }
        )


float_ : Decode.Decoder Float
float_ =
    Decode.string
        |> Decode.andThen
            (\s ->
                case String.toFloat s of
                    Just i ->
                        Decode.succeed i

                    Nothing ->
                        Decode.fail "Unrecognized Float"
            )



-- Combine Parsers


required : Parser a -> Parser (a -> b) -> Parser b
required =
    andMap


optional : Parser a -> Parser (Maybe a -> b) -> Parser b
optional parser =
    andMap
        (mapResult
            (\res ->
                case res of
                    Ok a ->
                        Ok (Just a)

                    Err _ ->
                        Ok Nothing
            )
            parser
        )


withDefault : a -> Parser a -> Parser (a -> b) -> Parser b
withDefault default parser =
    andMap
        (mapResult
            (\res ->
                case res of
                    Ok a ->
                        Ok a

                    Err _ ->
                        Ok default
            )
            parser
        )



-- Internal


decode :
    { name : String
    , help : String
    , decoder : Decode.Decoder a
    }
    -> Decode.Value
    -> Result Error a
decode options value =
    case Decode.decodeValue (Decode.field options.name Decode.value) value of
        Ok val_ ->
            val_
                |> Decode.decodeValue options.decoder
                |> Result.mapError (\_ -> toError options.name (UnexpectedValue options.help))

        Err _ ->
            Err (toError options.name MissingRequiredField)


toError : String -> Reason -> Error
toError name reason =
    Error
        [ { name = name
          , reason = reason
          }
        ]


mapResult : (Result Error a -> Result Error b) -> Parser a -> Parser b
mapResult f (Parser run) =
    Parser (run >> f)



-- Combine Parsers


succeed : a -> Parser a
succeed a =
    Parser (\_ -> Ok a)


fail : String -> String -> Parser a
fail name reason =
    Parser (\_ -> Err (toError name (CustomError reason)))


map : (a -> b) -> Parser a -> Parser b
map f (Parser run) =
    Parser (\val -> Result.map f (run val))


andMap : Parser a -> Parser (a -> b) -> Parser b
andMap (Parser runA) (Parser runB) =
    Parser
        (\val ->
            case ( runA val, runB val ) of
                ( Ok a, Ok b ) ->
                    Ok (b a)

                ( Err e, Ok _ ) ->
                    Err e

                ( Ok _, Err e ) ->
                    Err e

                ( Err (Error exA), Err (Error exB) ) ->
                    Err (Error (exA ++ exB))
        )


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen f (Parser run) =
    Parser
        (\val ->
            case run val of
                Ok a ->
                    parse (f a) val

                Err e ->
                    Err e
        )
