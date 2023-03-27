module Concurrent.Task2 exposing
    ( Definition
    , Error(..)
    , Task
    , andMap
    , andThen
    , andThenDo
    , attempt
    , fail
    , ffi
    , map
    , map2
    , map3
    , mapError
    , onError
    , onProgress
    , succeed
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task as CoreTask
import TaskPort



-- Task


type Task x a
    = Pending (List Definition) (RawResponses -> Task x a)
    | Done (Result x a)


type alias Definition =
    { function : String
    , args : Encode.Value
    }


type alias RawResponses =
    Decode.Value


type alias Expect a =
    Decoder a



-- Create


type alias Ffi a =
    { function : String
    , args : Encode.Value
    , expect : Expect a
    }


ffi : Ffi a -> Task Error a
ffi options =
    Pending [ toDefinition options ] (getResponseFor (toDefinition options) options.expect)
        |> mapError DecodeError


toDefinition : Ffi a -> Definition
toDefinition x =
    { function = x.function
    , args = x.args
    }


getResponseFor : Definition -> Expect a -> RawResponses -> Task Decode.Error a
getResponseFor definition expect results =
    results
        |> Decode.decodeValue (Decode.index 0 expect)
        |> fromResult


definitionHash : Definition -> String
definitionHash definition =
    encodeDefinition definition
        |> Encode.encode 0


encodeDefinition : Definition -> Encode.Value
encodeDefinition definition =
    Encode.object
        [ ( "function", Encode.string definition.function )
        , ( "args", definition.args )
        ]


fromResult : Result x a -> Task x a
fromResult res =
    case res of
        Ok a ->
            succeed a

        Err e ->
            fail e



-- Maps


map : (a -> b) -> Task x a -> Task x b
map f task =
    case task of
        Done res ->
            Done (Result.map f res)

        Pending defs next ->
            Pending defs (next >> map f)


map2 : (a -> b -> c) -> Task x a -> Task x b -> Task x c
map2 f task1 task2 =
    case ( task1, task2 ) of
        ( Done res1, Done res2 ) ->
            Done (Result.map2 f res1 res2)

        ( Done res1, Pending defs next ) ->
            Pending defs (\res -> map2 f (Done res1) (next res))

        ( Pending defs next, Done res2 ) ->
            Pending defs (\res -> map2 f (next res) (Done res2))

        ( Pending defs1 next1, Pending defs2 next2 ) ->
            Pending (defs1 ++ defs2) (\res -> map2 f (next1 res) (next2 res))


andMap : Task x a -> Task x (a -> b) -> Task x b
andMap =
    map2 (|>)


map3 : (a -> b -> c -> d) -> Task x a -> Task x b -> Task x c -> Task x d
map3 f task1 task2 task3 =
    succeed f
        |> andMap task1
        |> andMap task2
        |> andMap task3



-- Chains


andThen : (a -> Task x b) -> Task x a -> Task x b
andThen f task =
    case task of
        Done res ->
            case res of
                Ok a ->
                    f a

                Err e ->
                    fail e

        Pending defs next ->
            Pending defs (next >> andThen f)


andThenDo : Task x b -> Task x a -> Task x b
andThenDo task2 task1 =
    task1 |> andThen (\_ -> task2)


fail : x -> Task x a
fail e =
    Done (Err e)


succeed : a -> Task x a
succeed a =
    Done (Ok a)



-- Errors


onError : (x -> Task y a) -> Task x a -> Task y a
onError f task =
    case task of
        Done res ->
            case res of
                Ok a ->
                    Done (Ok a)

                Err e ->
                    f e

        Pending defs next ->
            Pending defs (next >> onError f)


mapError : (x -> y) -> Task x a -> Task y a
mapError f task =
    case task of
        Done res ->
            Done (Result.mapError f res)

        Pending defs next ->
            Pending defs (next >> mapError f)



-- Batches


type Error
    = TaskPortError TaskPort.Error
    | PendingError
    | DecodeError Decode.Error


type alias Attempt msg a =
    { send : Decode.Value -> Cmd msg
    , onResult : Result Error a -> msg
    }


type alias OnProgress msg a =
    { onResult : Result Error a -> msg
    , onProgress : ( Task Error a, Cmd msg ) -> msg
    , receive : (Decode.Value -> msg) -> Sub msg
    , send : Encode.Value -> Cmd msg
    }


attempt : Attempt msg a -> Task Error a -> ( Task Error a, Cmd msg )
attempt options task =
    case task of
        Done res ->
            ( task
            , CoreTask.succeed res
                |> CoreTask.perform options.onResult
            )

        Pending defs _ ->
            ( task
            , options.send (Encode.list encodeDefinition defs)
            )


onProgress : OnProgress msg a -> Task Error a -> Sub msg
onProgress options task =
    case task of
        Done res ->
            options.receive (\_ -> options.onResult res)

        Pending _ next ->
            options.receive
                (\results ->
                    case next results of
                        Done res ->
                            options.onResult res

                        Pending defs next_ ->
                            options.onProgress
                                ( Pending defs next_
                                , options.send (Encode.list encodeDefinition defs)
                                )
                )
