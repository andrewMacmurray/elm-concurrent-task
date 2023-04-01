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
    = Exec (Ids -> Progress x a)


type alias Progress x a =
    ( Task_ x a, Ids )


type Task_ x a
    = Pending (List Definition) (RawResponses -> Task_ x a)
    | Done (Result x a)


type alias Definition =
    { id : String
    , function : String
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
    Exec
        (\ids ->
            let
                ( id, nextIds ) =
                    Ids.next ids
            in
            ( Pending
                [ { id = id
                  , function = options.function
                  , args = options.args
                  }
                ]
                (\results ->
                    results
                        |> Decode.decodeValue (Decode.field id options.expect)
                        |> fromResult_
                        |> mapError_ DecodeError
                )
            , nextIds
            )
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
map f (Exec task) =
    Exec
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
map2 f (Exec task1) (Exec task2) =
    Exec
        (\ids ->
            let
                ( task1_, ids1 ) =
                    task1 ids

                ( task2_, ids2 ) =
                    task2 ids1
            in
            ( map2_ f task1_ task2_
            , ids2
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
andThen f (Exec task) =
    Exec
        (\ids ->
            let
                ( task_, ids1 ) =
                    task ids

                next a =
                    unwrap (f a) ids1
            in
            ( andThen_ (next >> Tuple.first) task_
            , Tuple.second (Ids.next ids1)
            )
        )


unwrap : Task x a -> Ids -> ( Task_ x a, Ids )
unwrap (Exec task) =
    task


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
    Exec (\_ -> ( fail_ e, Ids.init ))


fail_ : x -> Task_ x a
fail_ e =
    Done (Err e)


succeed : a -> Task x a
succeed a =
    Exec (\_ -> ( succeed_ a, Ids.init ))


succeed_ : a -> Task_ x a
succeed_ a =
    Done (Ok a)



-- Errors


onError : (x -> Task y a) -> Task x a -> Task y a
onError f (Exec task) =
    Exec
        (\ids ->
            let
                ( task_, nextIds ) =
                    task ids

                next x =
                    unwrap (f x) nextIds
            in
            ( onError_ (next >> Tuple.first) task_
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
mapError f (Exec task) =
    Exec
        (\ids ->
            let
                ( tsk, ids1 ) =
                    task ids
            in
            ( mapError_ f tsk
            , ids1
            )
        )


mapError_ : (x -> y) -> Task_ x a -> Task_ y a
mapError_ f task =
    case task of
        Done res ->
            Done (Result.mapError f res)

        Pending defs next ->
            Pending defs (next >> mapError_ f)



-- Batches


type Error
    = PendingError
    | DecodeError Decode.Error


type alias Attempt msg a =
    { send : Decode.Value -> Cmd msg
    , onResult : Result Error a -> msg
    }


type alias OnProgress msg a =
    { onResult : Result Error a -> msg
    , onProgress : ( Progress Error a, Cmd msg ) -> msg
    , receive : (Decode.Value -> msg) -> Sub msg
    , send : Encode.Value -> Cmd msg
    }


attempt : Attempt msg a -> Task Error a -> ( Progress Error a, Cmd msg )
attempt options ((Exec toTask) as t) =
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
                                ( ( Pending defs next_, Tuple.second (Ids.next ids) )
                                , options.send (Encode.list encodeDefinition defs)
                                )
                )
