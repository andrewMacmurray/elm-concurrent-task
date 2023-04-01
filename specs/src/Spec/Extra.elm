module Spec.Extra exposing (equals)

import Spec.Claim as Claim exposing (Claim)


equals : a -> Claim a
equals =
    Claim.isEqual Debug.toString
