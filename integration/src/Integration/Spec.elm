module Integration.Spec exposing
    ( Assertion
    , Expect
    , Spec
    , assertAll
    , assertErrors
    , assertSuccess
    , describe
    , duration
    , fail
    , pass
    , report
    , shouldBeFasterThan
    , shouldEqual
    , shouldHaveDurationLessThan
    , timeExecution
    )

import ConcurrentTask as Task exposing (ConcurrentTask)
import ConcurrentTask.Time
import Console
import Time


{-| Task Spec

A ConcurrentTask that contains assertions on either the Error or Success response

-}
type alias Spec =
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


assertSuccess : (a -> Expect) -> ConcurrentTask x a -> ConcurrentTask Expect Expect
assertSuccess f task =
    task
        |> Task.map f
        |> Task.mapError (\e -> Fail ("The Task returned an error: " ++ Debug.toString e))


assertErrors : (x -> Expect) -> ConcurrentTask x a -> ConcurrentTask Expect Expect
assertErrors f task =
    task
        |> Task.mapError f
        |> Task.map (\a -> Fail ("The Task was expected to fail but didn't, got: " ++ Debug.toString a))


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



-- Time a Task


type alias Timed a =
    { start : Time.Posix
    , finish : Time.Posix
    , result : a
    }


timeExecution : ConcurrentTask x a -> ConcurrentTask x (Timed a)
timeExecution task =
    ConcurrentTask.Time.now
        |> Task.andThen
            (\start ->
                task
                    |> Task.andThen
                        (\res ->
                            ConcurrentTask.Time.now
                                |> Task.map
                                    (\finish ->
                                        { start = start
                                        , finish = finish
                                        , result = res
                                        }
                                    )
                        )
            )


shouldHaveDurationLessThan : Int -> Timed a -> Expect
shouldHaveDurationLessThan ms a =
    if duration a < ms then
        Pass

    else
        Fail
            ("Duration was: "
                ++ String.fromInt (duration a)
                ++ ", Expected less than: "
                ++ String.fromInt ms
                ++ "ms"
            )


shouldBeFasterThan : Timed b -> Timed a -> Expect
shouldBeFasterThan b a =
    if duration a < duration b then
        Pass

    else
        Fail "Task was not faster than expected"


shouldEqual : a -> a -> Expect
shouldEqual a b =
    if a == b then
        Pass

    else
        Fail ("Expected: " ++ Debug.toString b ++ ", Got: " ++ Debug.toString a)


duration : Timed a -> Int
duration timed =
    Time.posixToMillis timed.finish - Time.posixToMillis timed.start


describe :
    String
    -> String
    -> ConcurrentTask x a
    -> (ConcurrentTask x a -> ConcurrentTask Expect Expect)
    -> Spec
describe name description task assert =
    task
        |> assert
        |> Task.map
            (\expect ->
                Assertion
                    { name = name
                    , description = description
                    , expect = expect
                    }
            )
        |> Task.mapError
            (\expect ->
                Assertion
                    { name = name
                    , description = description
                    , expect = expect
                    }
            )


type alias Report =
    { assertions : List Assertion
    , errors : List Task.UnexpectedError
    }


report : Report -> { message : String, passed : Bool }
report r =
    { message = reportMessage r
    , passed = List.isEmpty r.errors && allPassed r.assertions
    }


reportMessage : Report -> String
reportMessage r =
    if List.isEmpty r.errors then
        reportAssertions r.assertions

    else
        reportAssertions r.assertions ++ "\n" ++ reportErrors r.errors


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


reportErrors : List Task.UnexpectedError -> String
reportErrors errs =
    if List.isEmpty errs then
        ""

    else
        Console.red "Unexpected Errors: "
            ++ "\n"
            ++ String.join "\n" (List.map Debug.toString errs)
