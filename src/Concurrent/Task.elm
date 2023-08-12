module Concurrent.Task exposing
    ( Task, Definition, define
    , Expect, expectJson, expectString, expectWhatever
    , succeed, fail, fromResult, andThen, andThenDo, return
    , sequence, batch
    , map, andMap, map2, map3, map4, map5
    , Error, mapError, onError, errorToString
    , attempt, pool, onProgress, Pool, AttemptId, RawResults
    )

{-| A near drop in replacement for `elm/core`'s `Task`


# Tasks

@docs Task, Definition, define


# Expectations

@docs Expect, expectJson, expectString, expectWhatever


# Chains

@docs succeed, fail, fromResult, andThen, andThenDo, return


# Bulk

@docs sequence, batch


# Maps

@docs map, andMap, map2, map3, map4, map5


# Errors

@docs Error, mapError, onError, errorToString


# Run a Task

@docs attempt, pool, onProgress, Pool, AttemptId, RawResults

-}

import Concurrent.Internal.Task as Internal
import Json.Decode exposing (Decoder)
import Json.Encode as Encode



-- Tasks


{-| A `Task` represents an asynchronous unit of work
-}
type alias Task x a =
    Internal.Task x a


{-| -}
type alias Definition a =
    { function : String
    , args : Encode.Value
    , expect : Expect a
    }


{-| -}
type alias Expect a =
    Internal.Expect a


{-| -}
type alias Error =
    Internal.Error


{-| -}
define : Definition a -> Task Error a
define =
    Internal.define



-- Expectations


{-| -}
expectJson : Decoder a -> Expect a
expectJson =
    Internal.expectJson


{-| -}
expectString : Expect String
expectString =
    Internal.expectString


{-| -}
expectWhatever : Expect ()
expectWhatever =
    Internal.expectWhatever



-- Chains


{-| -}
succeed : a -> Task x a
succeed =
    Internal.succeed


{-| -}
fail : x -> Task x a
fail =
    Internal.fail


{-| -}
fromResult : Result x a -> Task x a
fromResult =
    Internal.fromResult


{-| -}
andThen : (a -> Task x b) -> Task x a -> Task x b
andThen =
    Internal.andThen


{-| -}
andThenDo : Task x b -> Task x a -> Task x b
andThenDo =
    Internal.andThenDo


{-| -}
return : b -> Task x a -> Task x b
return =
    Internal.return



-- Bulk


{-| -}
sequence : List (Task x a) -> Task x (List a)
sequence =
    Internal.sequence


{-| -}
batch : List (Task x a) -> Task x (List a)
batch =
    Internal.batch



-- Maps


{-| -}
map : (a -> b) -> Task x a -> Task x b
map =
    Internal.map


{-| -}
andMap : Task x a -> Task x (a -> b) -> Task x b
andMap =
    Internal.andMap


{-| -}
map2 : (a -> b -> c) -> Task x a -> Task x b -> Task x c
map2 =
    Internal.map2


{-| -}
map3 :
    (a -> b -> c -> d)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
map3 =
    Internal.map3


{-| -}
map4 :
    (a -> b -> c -> d -> e)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
map4 =
    Internal.map4


{-| -}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
    -> Task x f
map5 =
    Internal.map5



-- Errors


{-| -}
mapError : (x -> y) -> Task x a -> Task y a
mapError =
    Internal.mapError


{-| -}
onError : (x -> Task y a) -> Task x a -> Task y a
onError =
    Internal.onError


{-| -}
errorToString : Error -> String
errorToString =
    Internal.errorToString



-- Run a Task


{-| -}
type alias AttemptId =
    String


{-| -}
type alias Pool x a =
    Internal.Pool x a


{-| -}
type alias RawResults =
    Internal.RawResults


{-| -}
attempt :
    { id : AttemptId
    , pool : Pool x a
    , send : Encode.Value -> Cmd msg
    , onComplete : AttemptId -> Result x a -> msg
    }
    -> Task x a
    -> ( Pool x a, Cmd msg )
attempt =
    Internal.attempt


{-| -}
pool : Pool x a
pool =
    Internal.pool


{-| -}
onProgress :
    { send : Encode.Value -> Cmd msg
    , receive : (RawResults -> msg) -> Sub msg
    , onComplete : AttemptId -> Result x a -> msg
    , onProgress : ( Pool x a, Cmd msg ) -> msg
    }
    -> Pool x a
    -> Sub msg
onProgress =
    Internal.onProgress
