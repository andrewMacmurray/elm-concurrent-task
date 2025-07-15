port module Spa.ConcurrentTask exposing (attempt, progress, subscriptions)

import ConcurrentTask exposing (ConcurrentTask)
import Effect
import Json.Decode as Decode


subscriptions :
    (( ConcurrentTask.Pool msg, Cmd msg ) -> msg)
    -> { model | tasks : ConcurrentTask.Pool msg }
    -> Sub msg
subscriptions onProgress { tasks } =
    ConcurrentTask.onProgress
        { onProgress = onProgress
        , send = send
        , receive = receive
        }
        tasks


progress :
    { model | tasks : pool }
    -> ( pool, Cmd msg )
    -> ( { model | tasks : pool }, Effect.Effect msg )
progress model ( tasks, cmd ) =
    ( { model | tasks = tasks }
    , Effect.sendCmd cmd
    )


attempt :
    { task : ConcurrentTask x a
    , pool : ConcurrentTask.Pool msg
    , onComplete : ConcurrentTask.Response x a -> msg
    }
    -> ( ConcurrentTask.Pool msg, Cmd msg )
attempt { task, pool, onComplete } =
    ConcurrentTask.attempt
        { send = send
        , pool = pool
        , onComplete = onComplete
        }
        task


port send : Decode.Value -> Cmd msg


port receive : (Decode.Value -> msg) -> Sub msg
