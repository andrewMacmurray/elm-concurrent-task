module Concurrent.Fake exposing (runExample)

import Concurrent.Internal.Id as Id exposing (Id)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Task


type Task x a
    = State (Id.Sequence -> ( Id.Sequence, Task_ x a ))


type Task_ x a
    = Pending (List Definition_) (AResult -> Task x a)
    | Done (Result x a)


type alias Definition_ =
    { id : Id
    , function : String
    , args : Encode.Value
    }


type alias AResult =
    ( Id, Decode.Value )


type Expect a
    = ExpectJson (Decoder a)


type Error
    = DecodeResponseError Decode.Error
    | JsException String
    | MissingFunction String
    | InternalError String



-- Expect


expectJson : Decoder a -> Expect a
expectJson =
    ExpectJson


expectWhatever : Expect ()
expectWhatever =
    ExpectJson (Decode.succeed ())



-- Create


type alias Definition a =
    { function : String
    , args : Encode.Value
    , expect : Expect a
    }


define : Definition a -> Task Error a
define a =
    State
        (\ids ->
            let
                id =
                    Id.get ids
            in
            ( Id.next ids
            , Pending
                [ { id = id
                  , function = a.function
                  , args = a.args
                  }
                ]
                (\( id_, resx ) ->
                    if id == id_ then
                        resx
                            |> Decode.decodeValue (decodeResponse a.expect)
                            |> Result.mapError DecodeResponseError
                            |> Result.andThen identity
                            |> fromResult

                    else
                        runWith ids (define a)
                )
            )
        )


runWith : Id.Sequence -> Task x a -> Task x a
runWith s (State run) =
    State (\_ -> run s)


decodeResponse : Expect value -> Decoder (Result Error value)
decodeResponse (ExpectJson expect) =
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\status ->
                case status of
                    "success" ->
                        Decode.field "value" (Decode.map Ok expect)

                    "error" ->
                        Decode.field "error" (Decode.map Err errorDecoder)

                    _ ->
                        Decode.succeed (Err (InternalError ("Unknown response status: " ++ status)))
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
                        Decode.succeed (InternalError ("Unknown error reason: " ++ reason))
            )



-- Ops


map : (a -> b) -> Task x a -> Task x b
map f (State run) =
    State
        (\ids ->
            let
                ( ids_, a ) =
                    run ids
            in
            ( ids_, mapTask f a )
        )


mapTask : (a -> b) -> Task_ x a -> Task_ x b
mapTask f task =
    case task of
        Pending defs next ->
            Pending defs (next >> map f)

        Done a ->
            Done (Result.map f a)


map2 : (a -> b -> c) -> Task x a -> Task x b -> Task x c
map2 f (State run1) (State run2) =
    State
        (\ids ->
            let
                ( ids_, a ) =
                    run1 ids

                ( ids__, b ) =
                    run2 ids_
            in
            ( ids__, mapTask2 f a b )
        )


mapTask2 : (a -> b -> c) -> Task_ x a -> Task_ x b -> Task_ x c
mapTask2 f task1 task2 =
    case ( task1, task2 ) of
        ( Pending defs1 next1, Pending defs2 next2 ) ->
            Pending (defs1 ++ defs2) (\res -> map2 f (next1 res) (next2 res))

        ( Pending defs next1, Done b ) ->
            Pending defs (\res -> map2 f (next1 res) (fromResult b))

        ( Done a, Pending defs next2 ) ->
            Pending defs (\res -> map2 f (fromResult a) (next2 res))

        ( Done a, Done b ) ->
            Done (Result.map2 f a b)


andMap : Task x a -> Task x (a -> b) -> Task x b
andMap =
    map2 (|>)


map3 : (a -> b -> c -> d) -> Task x a -> Task x b -> Task x c -> Task x d
map3 f t1 t2 t3 =
    succeed f
        |> andMap t1
        |> andMap t2
        |> andMap t3


succeed : a -> Task x a
succeed a =
    fromResult (Ok a)


fail : x -> Task x a
fail x =
    fromResult (Err x)


fromResult : Result x a -> Task x a
fromResult res =
    State (\ids -> ( ids, Done res ))


andThen : (a -> Task x b) -> Task x a -> Task x b
andThen f (State run) =
    State
        (\ids ->
            let
                ( ids_, a ) =
                    run ids

                (State run_) =
                    case a of
                        Done a_ ->
                            case a_ of
                                Ok a__ ->
                                    f a__

                                Err e ->
                                    fail e

                        Pending defs next ->
                            State (\ids__ -> ( ids__, Pending defs (next >> andThen f) ))
            in
            run_ ids_
        )


andThenDo : Task x b -> Task x a -> Task x b
andThenDo s2 s1 =
    s1 |> andThen (\_ -> s2)


onError : (x -> Task y a) -> Task x a -> Task y a
onError f (State run) =
    State
        (\ids ->
            let
                ( ids_, a ) =
                    run ids

                (State run_) =
                    case a of
                        Done a_ ->
                            case a_ of
                                Ok a__ ->
                                    succeed a__

                                Err e ->
                                    f e

                        Pending defs next ->
                            State (\ids__ -> ( ids__, Pending defs (next >> onError f) ))
            in
            run_ ids_
        )


mapError : (x -> y) -> Task x a -> Task y a
mapError f (State run) =
    State
        (\ids ->
            let
                ( ids_, a ) =
                    run ids
            in
            ( ids_, mapTaskError f a )
        )


mapTaskError : (x -> y) -> Task_ x a -> Task_ y a
mapTaskError f task =
    case task of
        Pending defs next ->
            Pending defs (next >> mapError f)

        Done a ->
            Done (Result.mapError f a)


errorToString : Error -> String
errorToString err =
    case err of
        DecodeResponseError e ->
            "DecodeResponseError: " ++ Decode.errorToString e

        JsException string ->
            "JsException: " ++ string

        MissingFunction string ->
            "MissingFunction: " ++ string

        InternalError string ->
            "InternalError: " ++ string



-- Eval


eval : Int -> List ( Id, Encode.Value ) -> Task Error a -> Id.Sequence -> ( Id.Sequence, Result Error a )
eval attempts results (State run) n =
    case run n of
        ( n_, Done a ) ->
            ( n_, a )

        ( n_, Pending defs next ) ->
            let
                _ =
                    Debug.log "(state, defs, resultn)"
                        ( n_
                        , defs
                        , results
                            |> List.head
                            |> Maybe.map Tuple.first
                        )
            in
            if attempts > 0 then
                eval (attempts - 1)
                    (List.drop 1 results)
                    (results
                        |> List.head
                        |> Maybe.withDefault ( "100", Encode.null )
                        |> next
                    )
                    n_

            else
                ( n_, Err (InternalError "timeout") )



-- Example


create a =
    define
        { function = a
        , args = Encode.null
        , expect = expectJson Decode.string
        }


error =
    define
        { function = "error"
        , args = Encode.null
        , expect = expectJson (Decode.fail "error")
        }


join3 : appendable -> appendable -> appendable -> appendable
join3 a b c =
    a ++ b ++ c


example : Task Error String
example =
    map3 join3
        (create "hello")
        (create "world")
        (create "!")
        |> andThenDo (create "foo")
        |> andThenDo
            (create "foo"
                |> andThenDo
                    (error
                        |> onError (\_ -> error)
                        |> onError (\_ -> error)
                        |> onError (\_ -> create "bar")
                    )
                |> andThenDo
                    (create "bar"
                        |> andThenDo (create "bar")
                        |> andThenDo (create "bar")
                        |> andThenDo
                            (create "bar"
                                |> andThenDo
                                    (map2 (++)
                                        (create "bar")
                                        (create "baz")
                                    )
                                |> andThenDo error
                                |> onError (\_ -> create "bar")
                            )
                        |> andThenDo (create "bar")
                    )
            )
        |> andThenDo (create "baz")


runExample : ( Id.Sequence, Result Error String )
runExample =
    eval
        20
        [ ( "0", fakeResponse "zero" )
        , ( "1", fakeResponse "one" )
        , ( "2", fakeResponse "two" )
        , ( "3", fakeResponse "three" )
        , ( "4", fakeResponse "four" )
        , ( "5", fakeResponse "five" )
        , ( "6", fakeResponse "six" )
        , ( "7", fakeResponse "seven" )
        , ( "8", fakeResponse "eight" )
        , ( "9", fakeResponse "nine" )
        , ( "10", fakeResponse "ten" )
        , ( "11", fakeResponse "eleven" )
        , ( "12", fakeResponse "twelve" )
        , ( "13", fakeResponse "thirteen" )
        , ( "14", fakeResponse "fourteen" )
        , ( "15", fakeResponse "fifteen" )
        , ( "16", fakeResponse "sixteen" )
        , ( "17", fakeResponse "seventeen" )
        , ( "18", fakeResponse "eighteen" )
        , ( "19", fakeResponse "nineteen" )
        , ( "20", fakeResponse "twenty" )
        ]
        example
        Id.init


fakeResponse : String -> Encode.Value
fakeResponse s =
    Encode.object
        [ ( "status", Encode.string "success" )
        , ( "value", Encode.string s )
        ]
