module Integration.Spec exposing
    ( Assertion
    , Expect
    , Spec(..)
    , assertAll
    , assertError
    , assertSuccess
    , describe
    , describeUnexpected
    , fail
    , failWith
    , pass
    , report
    , shouldBeFasterThan
    , shouldEqual
    , shouldHaveDurationLessThan
    )

import ConcurrentTask as Task exposing (ConcurrentTask, UnexpectedError)
import ConcurrentTask.Time
import Console


{-| Task Spec

A ConcurrentTask that contains assertions on either the Error or Success response

-}
type Spec
    = Spec (UnexpectedError -> Assertion) SpecTask


type alias SpecTask =
    ConcurrentTask Assertion Assertion


type Assertion
    = Assertion
        { name : String
        , description : String
        , expect : Expect
        }


type Expect
    = Pass
    | Fail String


pass : Expect
pass =
    Pass


fail : String -> Expect
fail =
    Fail


failWith : String -> a -> Expect
failWith message val =
    Fail (message ++ ": " ++ Debug.toString val)


assertSuccess : (a -> Expect) -> ConcurrentTask x a -> ConcurrentTask Expect Expect
assertSuccess f task =
    task
        |> Task.map f
        |> Task.mapError (failWith "The Task returned an error")


assertError : (x -> Expect) -> ConcurrentTask x a -> ConcurrentTask Expect Expect
assertError f task =
    task
        |> Task.mapError f
        |> Task.map (failWith "The Task was expected to fail but didn't, got")


assertAll : List Expect -> Expect
assertAll xs =
    if List.isEmpty xs then
        Fail "assertAll must have at least 1 assertion"

    else
        List.foldl
            (\val curr ->
                case curr of
                    Fail e ->
                        Fail e

                    Pass ->
                        val
            )
            Pass
            xs



-- Timed expectations


shouldHaveDurationLessThan : Int -> ConcurrentTask.Time.Duration a -> Expect
shouldHaveDurationLessThan ms a =
    if ConcurrentTask.Time.duration a < ms then
        Pass

    else
        Fail
            ("Duration was: "
                ++ String.fromInt (ConcurrentTask.Time.duration a)
                ++ ", Expected less than: "
                ++ String.fromInt ms
                ++ "ms"
            )


shouldBeFasterThan : ConcurrentTask.Time.Duration b -> ConcurrentTask.Time.Duration a -> Expect
shouldBeFasterThan b a =
    if ConcurrentTask.Time.duration a < ConcurrentTask.Time.duration b then
        Pass

    else
        Fail "Task was not faster than expected"


shouldEqual : a -> a -> Expect
shouldEqual a b =
    if a == b then
        Pass

    else
        Fail ("\n Expected: " ++ Debug.toString a ++ ",\n Got:      " ++ Debug.toString b)


describe :
    String
    -> String
    -> ConcurrentTask x a
    -> (ConcurrentTask x a -> ConcurrentTask Expect Expect)
    -> Spec
describe name description task assert =
    task
        |> assert
        |> Task.map (toAssertion name description)
        |> Task.mapError (toAssertion name description)
        |> Spec (failWith "An Unexpected Error was raised" >> toAssertion name description)


describeUnexpected : String -> String -> ConcurrentTask a b -> (UnexpectedError -> Expect) -> Spec
describeUnexpected name description task assertUnexpected =
    task
        |> Task.map (failWith "Task Succeeded but was expected to fail" >> toAssertion name description)
        |> Task.mapError (failWith "Task raised an Error" >> toAssertion name description)
        |> Spec (assertUnexpected >> toAssertion name description)


toAssertion : String -> String -> Expect -> Assertion
toAssertion name description expect =
    Assertion
        { name = name
        , description = description
        , expect = expect
        }


report : List Assertion -> { message : String, passed : Bool }
report r =
    { message = reportAssertions r
    , passed = allPassed r
    }


allPassed : List Assertion -> Bool
allPassed =
    List.all (\(Assertion x) -> x.expect == Pass)


reportAssertions : List Assertion -> String
reportAssertions assertions =
    "🧪 Test Results: \n" ++ String.join "\n" (List.map reportAssertion assertions)


reportAssertion : Assertion -> String
reportAssertion (Assertion a) =
    case a.expect of
        Pass ->
            Console.green ("PASS ✅ - " ++ a.name)

        Fail reason ->
            Console.red ("FAIL ❌ - " ++ a.name ++ " - " ++ reason)
