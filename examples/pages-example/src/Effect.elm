module Effect exposing (Effect(..), attemptTask, map)

import ConcurrentTask exposing (ConcurrentTask)


type Effect msg
    = None
      -- The task error and success types are both `msg` which allows `Main` to emit those messages when a task is done.
    | AttemptTask (ConcurrentTask msg msg)


map : (a -> b) -> Effect a -> Effect b
map f effect =
    case effect of
        None ->
            None

        AttemptTask task ->
            AttemptTask
                (task
                    |> ConcurrentTask.map f
                    |> ConcurrentTask.mapError f
                )


{-| This helper gives concurrent tasks an `elm/core` `Task.attempt` like api. e.g.:

    type Msg =
        GotPosts (Result Http.Error Posts)

    Effect.attemptTask GotPosts getPostsConcurrentTask

-}
attemptTask : (Result x a -> msg) -> ConcurrentTask x a -> Effect msg
attemptTask toMsg task =
    AttemptTask
        (task
            |> ConcurrentTask.toResult
            |> ConcurrentTask.map toMsg
        )
