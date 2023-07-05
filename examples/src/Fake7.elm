module Fake7 exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)



-- Task


type Task x a
    = Task (Int -> ( Int, Task_ x a ))


type Task_ x a
    = Pending (List Definition) (Results -> Task x a)
    | Done (Result x a)


type Definition
    = Definition String Int


type alias Results =
    Dict Int Decode.Value



-- Define a Task


define : Decoder a -> String -> Task Decode.Error a
define decode def =
    Task
        (\id ->
            ( id + 1
            , Pending [ Definition def id ]
                (\results ->
                    case Dict.get id results of
                        Just result ->
                            fromResult (Decode.decodeValue decode result)

                        Nothing ->
                            runWith id (define decode def)
                )
            )
        )


runWith : Int -> Task x a -> Task x a
runWith id (Task run) =
    Task (\_ -> run id)


succeed : a -> Task x a
succeed a =
    fromResult (Ok a)


fromResult : Result x a -> Task x a
fromResult result =
    Task (\id -> ( id, Done result ))
