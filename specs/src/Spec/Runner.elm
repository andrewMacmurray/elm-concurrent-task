port module Spec.Runner exposing
    ( elmSpecOut
    , pick
    , program
    , skip
    )

import Spec exposing (Message, Spec)


port elmSpecOut : Message -> Cmd msg


port elmSpecIn : (Message -> msg) -> Sub msg


port elmSpecPick : () -> Cmd msg


config : Spec.Config msg
config =
    { send = elmSpecOut
    , listen = elmSpecIn
    }


pick : Spec.Scenario model msg -> Spec.Scenario model msg
pick =
    Spec.pick elmSpecPick


skip : Spec.Scenario model msg -> Spec.Scenario model msg
skip =
    Spec.skip


program : List (Spec model msg) -> Program Spec.Flags (Spec.Model model msg) (Spec.Msg msg)
program =
    Spec.program config
