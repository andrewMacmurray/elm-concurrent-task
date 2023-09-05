module Integration.Spec exposing
    ( Assertion
    , Expect
    , Spec
    , assertAll
    , assertEquals
    , assertSuccess
    , duration
    , fail
    , pass
    , report
    , reportErrors
    , shouldBeFasterThan
    , taskSpec
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


shouldBeFasterThan : Timed b -> Timed a -> Expect
shouldBeFasterThan b a =
    if duration b < duration a then
        Pass

    else
        Fail "Task was not faster than expected"


assertEquals : a -> a -> Expect
assertEquals a b =
    if a == b then
        Pass

    else
        Fail ("Expected: " ++ Debug.toString b ++ ", Got: " ++ Debug.toString a)


duration : Timed a -> Int
duration timed =
    Time.posixToMillis timed.finish - Time.posixToMillis timed.start


taskSpec :
    String
    -> String
    -> ConcurrentTask x a
    -> (ConcurrentTask x a -> ConcurrentTask Expect Expect)
    -> Spec
taskSpec name description task assert =
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


report : List Assertion -> String
report assertions =
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
