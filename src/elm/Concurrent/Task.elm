module Concurrent.Task exposing
    ( Definition
    , Error(..)
    , Progress
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

import Concurrent.Ids as Ids exposing (Ids)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task as CoreTask



-- Task


type Task x a
    = Task (Ids -> Progress x a)


type alias Progress x a =
    ( Task_ x a, Ids )


type Task_ x a
    = Pending (List Definition) (Responses -> Task_ x a)
    | Done (Result x a)


type alias Definition =
    { id : String
    , function : String
    , args : Encode.Value
    }


type alias Responses =
    Decode.Value


type alias Expect a =
    Decoder a


type Error
    = DecodeResponseError Decode.Error
    | JsException String
    | MissingFunction String
    | UnknownStatus String



-- Create


type alias Ffi a =
    { function : String
    , args : Encode.Value
    , expect : Expect a
    }


ffi : Ffi a -> Task Error a
ffi options =
    Task
        (\ids ->
            let
                id =
                    Ids.get ids
            in
            ( Pending
                [ { id = id
                  , function = options.function
                  , args = options.args
                  }
                ]
                (\results ->
                    results
                        |> Decode.decodeValue (decodeResponse id options.expect)
                        |> Result.mapError DecodeResponseError
                        |> Result.andThen identity
                        |> fromResult_
                )
            , Ids.next ids
            )
        )


decodeResponse : String -> Decoder value -> Decoder (Result Error value)
decodeResponse id expect =
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\status ->
                case status of
                    "success" ->
                        Decode.field "results" (Decode.field id (Decode.map Ok expect))

                    "error" ->
                        Decode.field "error" (Decode.map Err errorDecoder)

                    _ ->
                        Decode.succeed (Err (UnknownStatus status))
            )


errorDecoder : Decoder Error
errorDecoder =
    Decode.field "reason" Decode.string
        |> Decode.andThen
            (\reason ->
                case reason of
                    "js_exception" ->
                        Decode.field "message" (Decode.map JsException Decode.string)

                    "missing_function" ->
                        Decode.field "message" (Decode.map MissingFunction Decode.string)

                    _ ->
                        Decode.succeed (UnknownStatus ("Unknown error reason: " ++ reason))
            )


encodeDefinition : Definition -> Encode.Value
encodeDefinition definition =
    Encode.object
        [ ( "id", Encode.string definition.id )
        , ( "function", Encode.string definition.function )
        , ( "args", definition.args )
        ]


fromResult_ : Result x a -> Task_ x a
fromResult_ res =
    case res of
        Ok a ->
            succeed_ a

        Err e ->
            fail_ e



-- Maps


map : (a -> b) -> Task x a -> Task x b
map f (Task task) =
    Task
        (\ids ->
            let
                ( tsk, nextIds ) =
                    task ids
            in
            ( map_ f tsk, nextIds )
        )


map_ : (a -> b) -> Task_ x a -> Task_ x b
map_ f task =
    case task of
        Done res ->
            Done (Result.map f res)

        Pending defs next ->
            Pending defs (next >> map_ f)


map2 : (a -> b -> c) -> Task x a -> Task x b -> Task x c
map2 f (Task task1) (Task task2) =
    Task
        (\ids ->
            let
                ( task1_, ids1 ) =
                    task1 ids

                ( task2_, ids2 ) =
                    task2 ids1
            in
            ( map2_ f task1_ task2_
            , Ids.next ids2
            )
        )


map2_ : (a -> b -> c) -> Task_ x a -> Task_ x b -> Task_ x c
map2_ f task1 task2 =
    case ( task1, task2 ) of
        ( Done res1, Done res2 ) ->
            Done (Result.map2 f res1 res2)

        ( Done res1, Pending defs next ) ->
            Pending defs (\res -> map2_ f (Done res1) (next res))

        ( Pending defs next, Done res2 ) ->
            Pending defs (\res -> map2_ f (next res) (Done res2))

        ( Pending defs1 next1, Pending defs2 next2 ) ->
            Pending (defs1 ++ defs2) (\res -> map2_ f (next1 res) (next2 res))


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
andThen f (Task task) =
    Task
        (\ids ->
            let
                ( task_, ids1 ) =
                    task ids

                next a =
                    unwrap (f a) ids1
            in
            ( andThen_ next task_
            , Ids.next ids1
            )
        )


andThen_ : (a -> Task_ x b) -> Task_ x a -> Task_ x b
andThen_ f task =
    case task of
        Done res ->
            case res of
                Ok a ->
                    f a

                Err e ->
                    fail_ e

        Pending defs next ->
            Pending defs (next >> andThen_ f)


andThenDo : Task x b -> Task x a -> Task x b
andThenDo task2 task1 =
    task1 |> andThen (\_ -> task2)


fail : a -> Task a b
fail e =
    Task (\_ -> ( fail_ e, Ids.init ))


fail_ : x -> Task_ x a
fail_ e =
    Done (Err e)


succeed : a -> Task x a
succeed a =
    Task (\_ -> ( succeed_ a, Ids.init ))


succeed_ : a -> Task_ x a
succeed_ a =
    Done (Ok a)



-- Errors


onError : (x -> Task y a) -> Task x a -> Task y a
onError f (Task task) =
    Task
        (\ids ->
            let
                ( task_, nextIds ) =
                    task ids

                next x =
                    unwrap (f x) nextIds
            in
            ( onError_ next task_
            , nextIds
            )
        )


onError_ : (x -> Task_ y a) -> Task_ x a -> Task_ y a
onError_ f task =
    case task of
        Done res ->
            case res of
                Ok a ->
                    Done (Ok a)

                Err e ->
                    f e

        Pending defs next ->
            Pending defs (next >> onError_ f)


mapError : (x -> y) -> Task x a -> Task y a
mapError f (Task task) =
    Task
        (\ids ->
            let
                ( task_, nextIds ) =
                    task ids
            in
            ( mapError_ f task_
            , nextIds
            )
        )


mapError_ : (x -> y) -> Task_ x a -> Task_ y a
mapError_ f task =
    case task of
        Done res ->
            Done (Result.mapError f res)

        Pending defs next ->
            Pending defs (next >> mapError_ f)


unwrap : Task x a -> Ids -> Task_ x a
unwrap (Task task) ids =
    Tuple.first (task ids)



-- Batches


type alias Attempt msg a =
    { send : Decode.Value -> Cmd msg
    , onResult : Result Error a -> msg
    }


type alias OnProgress msg a =
    { send : Encode.Value -> Cmd msg
    , receive : (Decode.Value -> msg) -> Sub msg
    , onResult : Result Error a -> msg
    , onProgress : ( Progress Error a, Cmd msg ) -> msg
    }


attempt : Attempt msg a -> Task Error a -> ( Progress Error a, Cmd msg )
attempt options (Task toTask) =
    case toTask Ids.init of
        ( Done res, ids ) ->
            ( ( Done res, ids )
            , CoreTask.succeed res
                |> CoreTask.perform options.onResult
            )

        ( Pending defs next, ids ) ->
            ( ( Pending defs next, ids )
            , options.send (Encode.list encodeDefinition defs)
            )


onProgress : OnProgress msg a -> Progress Error a -> Sub msg
onProgress options ( task, ids ) =
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
                                ( ( Pending defs next_, Ids.next ids )
                                , options.send (Encode.list encodeDefinition defs)
                                )
                )
