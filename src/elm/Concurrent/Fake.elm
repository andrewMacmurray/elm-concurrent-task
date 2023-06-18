module Concurrent.Fake exposing (..)

-- State


type State a
    = State (Int -> ( Int, Task a ))


type Task a
    = Pending (State a)
    | Done a


map : (a -> b) -> State a -> State b
map f (State run) =
    State
        (\state1 ->
            let
                ( state2, task ) =
                    run state1
            in
            case task of
                Pending (State r) ->
                    mapHelp f r Pending state2

                Done a ->
                    ( state2, Done (f a) )
        )


type alias Unwrapped a =
    Int -> ( Int, Task a )


mapHelp : (a -> b) -> Unwrapped a -> (State b -> Task b) -> Unwrapped b
mapHelp f run build state =
    case run state of
        ( state_, Pending (State run_) ) ->
            mapHelp f run_ (\st -> build (wrap (Pending st))) state_

        ( state_, Done a ) ->
            ( state_, build (wrap (Done (f a))) )


wrap : Task a -> State a
wrap t =
    State (\s -> ( s, t ))


createTask : State String
createTask =
    State
        (\n ->
            ( n + 1
            , Pending (State (\n_ -> ( n_, Done "some value" )))
            )
        )


andThen : (a -> State b) -> State a -> State b
andThen f (State next) =
    State
        (\state1 ->
            let
                ( state2, task ) =
                    next state1

                (State next_) =
                    case task of
                        Pending s ->
                            State (\state_ -> ( state_, Pending (andThen f s) ))

                        Done a ->
                            f a
            in
            next_ state2
        )
