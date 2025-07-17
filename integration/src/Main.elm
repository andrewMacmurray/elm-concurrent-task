port module Main exposing (main)

import Bytes
import Bytes.Decode
import Bytes.Encode
import ConcurrentTask as Task exposing (ConcurrentTask, UnexpectedError(..))
import ConcurrentTask.Http as Http
import ConcurrentTask.Process
import ConcurrentTask.Random
import ConcurrentTask.Time
import Dict
import Integration.Runner as Runner exposing (RunnerProgram)
import Integration.Spec as Spec exposing (Spec)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random
import Set
import Time



-- Integration Program


main : RunnerProgram
main =
    Runner.program
        { specs = specs
        , send = send
        , receive = receive
        , report = report
        }



-- All Specs


specs : List Spec
specs =
    [ largeBatchSpec
    , batchAndSequenceSpec
    , complexResponseSpec
    , missingFunctionSpec
    , httpJsonBodySpec
    , httpHeadersSpec
    , httpBytesSpec
    , httpMalformedSpec
    , httpStringSpec
    , httpTimeoutSpec
    , httpBadBodySpec
    , httpBadStatusSpec
    , httpBadUrlSpec
    , randomSpec
    , timeZoneSpec
    , timeHereSpec
    ]



-- Integration Specs


largeBatchSpec : Spec
largeBatchSpec =
    let
        batchSize : Int
        batchSize =
            100000
    in
    Spec.describe
        "large batches"
        "large batches should complete in reasonable time"
        (Spec.timeExecution
            (ConcurrentTask.Process.sleep 100
                |> List.repeat batchSize
                |> Task.batch
            )
        )
        (Spec.assertSuccess
            (\res ->
                Spec.assertAll
                    [ Spec.shouldHaveDurationLessThan 5000 res
                    , res.result |> Spec.shouldEqual (List.repeat batchSize ())
                    ]
            )
        )


batchAndSequenceSpec : Spec
batchAndSequenceSpec =
    Spec.describe
        "batch and sequence speed"
        "the batched branch should be faster than the sequential branch"
        (Task.map2 Tuple.pair
            (Spec.timeExecution
                (Task.batch
                    [ longRequest 100
                    , longRequest 100
                    , longRequest 100
                    , longRequest 100
                    ]
                )
            )
            (Spec.timeExecution
                (Task.sequence
                    [ longRequest 100
                    , longRequest 100
                    , longRequest 100
                    , longRequest 100
                    ]
                )
            )
        )
        (Spec.assertSuccess
            (\( batched, sequential ) ->
                Spec.assertAll
                    [ batched |> Spec.shouldBeFasterThan sequential
                    , batched.result |> Spec.shouldEqual sequential.result
                    ]
            )
        )


complexResponseSpec : Spec
complexResponseSpec =
    Spec.describe
        "complex responses"
        "task should decode and combine responses"
        (Task.map5 join5
            (longRequest 100)
            (longRequest 50
                |> Task.andThen
                    (\x ->
                        joinWith x
                            (Task.map2 join2
                                (longRequest 40)
                                (longRequest 60)
                            )
                    )
            )
            (longRequest 30
                |> Task.andThen
                    (\x ->
                        longRequest 10
                            |> joinWith x
                            |> Task.andThen
                                (\y ->
                                    [ longRequest 42
                                    , longRequest 20
                                    , longRequest 14
                                    , longRequest 86
                                    ]
                                        |> Task.batch
                                        |> Task.map (String.join ",")
                                        |> joinWith y
                                )
                    )
            )
            (longRequest 40)
            (longRequest 120)
        )
        (Spec.assertSuccess
            (Spec.shouldEqual
                (String.join ","
                    [ "done:100"
                    , "done:50"
                    , "done:40"
                    , "done:60"
                    , "done:30"
                    , "done:10"
                    , "done:42"
                    , "done:20"
                    , "done:14"
                    , "done:86"
                    , "done:40"
                    , "done:120"
                    ]
                )
            )
        )


missingFunctionSpec : Spec
missingFunctionSpec =
    Spec.describeUnexpected
        "missing functions"
        "task should abort immediately if a function is not registered"
        (Task.define
            { function = "fire_ze_missiles"
            , expect = Task.expectWhatever
            , errors = Task.expectThrows identity
            , args = Encode.null
            }
            |> Task.andThenDo (ConcurrentTask.Process.sleep 500)
            |> Task.return "Completed"
        )
        (Spec.shouldEqual
            (MissingFunction "fire_ze_missiles is not registered")
        )


httpJsonBodySpec : Spec
httpJsonBodySpec =
    let
        body : Encode.Value
        body =
            Encode.object
                [ ( "message1", Encode.string "hello" )
                , ( "message2", Encode.string "world" )
                ]

        response : Decoder String
        response =
            Decode.map2 join2
                (Decode.field "message1" Decode.string)
                (Decode.field "message2" Decode.string)
    in
    Spec.describe
        "http json body"
        "sends an http json body in a request"
        (Http.post
            { url = echoBody
            , headers = []
            , timeout = Nothing
            , expect = Http.expectJson response
            , body = Http.jsonBody body
            }
        )
        (Spec.assertSuccess
            (Spec.shouldEqual "hello,world")
        )


httpBytesSpec : Spec
httpBytesSpec =
    let
        body : Bytes.Encode.Encoder
        body =
            Bytes.Encode.sequence
                [ Bytes.Encode.unsignedInt32 Bytes.BE 41
                , Bytes.Encode.unsignedInt32 Bytes.BE 1
                ]

        response : Bytes.Decode.Decoder Int
        response =
            Bytes.Decode.map2 (+)
                (Bytes.Decode.unsignedInt32 Bytes.BE)
                (Bytes.Decode.unsignedInt32 Bytes.BE)
    in
    Spec.describe
        "http bytes"
        "sends http bytes body in a request and decodes them in response"
        (Http.post
            { url = echoBody
            , headers = []
            , timeout = Nothing
            , expect = Http.expectBytes response
            , body = Http.bytesBody "application/octet-stream" (Bytes.Encode.encode body)
            }
        )
        (Spec.assertSuccess
            (Spec.shouldEqual 42)
        )


httpMalformedSpec : Spec
httpMalformedSpec =
    Spec.describe
        "http malformed response"
        "should return a BadBody Error for non JSON responses when expecting JSON"
        (Http.get
            { url = malformedJson
            , headers = []
            , expect = Http.expectJson (Decode.field "invalid" Decode.string)
            , timeout = Nothing
            }
        )
        (Spec.assertError
            (badBodyShouldContainMessage "This is not valid JSON!")
        )


httpHeadersSpec : Spec
httpHeadersSpec =
    Spec.describe
        "http headers"
        "should send and receive http headers"
        (Http.post
            { url = echoBody
            , headers = [ Http.header "foo" "bar" ]
            , expect = Http.withMetadata always Http.expectWhatever
            , timeout = Nothing
            , body = Http.emptyBody
            }
        )
        (Spec.assertSuccess
            (\meta ->
                case Dict.get "foo" meta.headers of
                    Just "bar" ->
                        Spec.pass

                    Just x ->
                        Spec.failWith "Got a header but not the expected value" x

                    Nothing ->
                        Spec.failWith "Did not contain expected header" meta
            )
        )


httpStringSpec : Spec
httpStringSpec =
    Spec.describe
        "http string response"
        "should return a successful string response when expecting a String"
        (Http.get
            { url = malformedJson
            , headers = []
            , expect = Http.expectString
            , timeout = Nothing
            }
        )
        (Spec.assertSuccess (Spec.shouldEqual "{ 'invalid': 'json"))


httpTimeoutSpec : Spec
httpTimeoutSpec =
    Spec.describe
        "http timeout"
        "http requests should abort if request takes longer than given timeout"
        (Spec.timeExecution
            (Http.get
                { url = waitThenRespond 10000
                , headers = []
                , expect = Http.expectWhatever
                , timeout = Just 100
                }
            )
        )
        (Spec.assertError
            (\err ->
                Spec.assertAll
                    [ Spec.shouldEqual Http.Timeout err.result
                    , err |> Spec.shouldHaveDurationLessThan 3000 -- account for test flake
                    ]
            )
        )


httpBadBodySpec : Spec
httpBadBodySpec =
    Spec.describe
        "http bad body"
        "should surface a BadBody error if response doesn't match the decoder"
        (Http.get
            { url = waitThenRespond 0
            , headers = []
            , expect = Http.expectJson Decode.int
            , timeout = Nothing
            }
        )
        (Spec.assertError
            (badBodyShouldContainMessage "Expecting an INT")
        )


httpBadStatusSpec : Spec
httpBadStatusSpec =
    Spec.describe
        "http bad status"
        "should surface a BadStatus error if response is a non 200"
        (Http.get
            { url = httpError
            , headers = []
            , expect = Http.expectWhatever
            , timeout = Nothing
            }
        )
        (Spec.assertError (shouldHaveBadStatus 400))


httpBadUrlSpec : Spec
httpBadUrlSpec =
    Spec.describe "http bad url"
        "should surface a BadUrl error if url is invalid"
        (Http.get
            { url = "WAT WAT"
            , headers = []
            , expect = Http.expectWhatever
            , timeout = Nothing
            }
        )
        (Spec.assertError
            (Spec.shouldEqual (Http.BadUrl "WAT WAT"))
        )


randomSpec : Spec
randomSpec =
    Spec.describe
        "random generator"
        "produces random values"
        (ConcurrentTask.Random.generate
            (Random.list 50
                (Random.int 0 1000000)
            )
        )
        (Spec.assertSuccess
            (\numbers ->
                if allElementsUnique numbers then
                    Spec.pass

                else
                    Spec.failWith "Expected all numbers to be unique" numbers
            )
        )


allElementsUnique : List comparable -> Bool
allElementsUnique xs =
    List.length xs == Set.size (Set.fromList xs)


timeZoneSpec : Spec
timeZoneSpec =
    Spec.describe
        "Time.getZoneName"
        "smoke test for Time.getZoneName"
        ConcurrentTask.Time.getZoneName
        (Spec.assertSuccess
            (\zone ->
                case zone of
                    Time.Offset _ ->
                        Spec.failWith "Expected actual timezone but got an offset" zone

                    Time.Name _ ->
                        Spec.pass
            )
        )


timeHereSpec : Spec
timeHereSpec =
    Spec.describe
        "Time.here"
        "smoke test for Time.here"
        ConcurrentTask.Time.here
        (Spec.assertSuccess (\_ -> Spec.pass))



-- Expect Helpers


badBodyShouldContainMessage : String -> Http.Error -> Spec.Expect
badBodyShouldContainMessage message err =
    case err of
        Http.BadBody _ _ e ->
            if String.contains message (Decode.errorToString e) then
                Spec.pass

            else
                Spec.failWith "Got BadBody but with unexpected message" e

        _ ->
            Spec.failWith "Expected BadBody, got" err


shouldHaveBadStatus : Int -> Http.Error -> Spec.Expect
shouldHaveBadStatus code err =
    case err of
        Http.BadStatus meta _ ->
            if meta.statusCode == code then
                Spec.pass

            else
                Spec.failWith "Unexpected status code" meta

        _ ->
            Spec.failWith "Expected a BadStatus error" err



-- Helpers


joinWith : String -> ConcurrentTask x String -> ConcurrentTask x String
joinWith x task =
    Task.map (join2 x) task


join2 : String -> String -> String
join2 a b =
    String.join "," [ a, b ]


join5 : String -> String -> String -> String -> String -> String
join5 a b c d e =
    String.join "," [ a, b, c, d, e ]


longRequest : Int -> ConcurrentTask Http.Error String
longRequest ms =
    Http.get
        { url = waitThenRespond ms
        , headers = []
        , expect = Http.expectJson (Decode.field "message" Decode.string)
        , timeout = Nothing
        }


waitThenRespond : Int -> String
waitThenRespond ms =
    baseUrl ++ "/wait-then-respond/" ++ String.fromInt ms


echoBody : String
echoBody =
    baseUrl ++ "/echo"


malformedJson : String
malformedJson =
    baseUrl ++ "/malformed"


httpError : String
httpError =
    baseUrl ++ "/boom"


baseUrl : String
baseUrl =
    "http://localhost:4999"



-- Ports


port send : Decode.Value -> Cmd msg


port receive : (Decode.Value -> msg) -> Sub msg


port report : { message : String, passed : Bool } -> Cmd msg
