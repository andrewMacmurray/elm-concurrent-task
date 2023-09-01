module ConcurrentTask.Time exposing (now, here, getZoneName)

{-| A drop in replacement for [elm/time's](https://package.elm-lang.org/packages/elm/time/latest/Time#now) `Time.now`

The JavaScript runner has this task builtin by default. If needed it can be overridden like so:

    Tasks.register({
      tasks: {},
      ports: app.ports,
      builtins: {
        timeNow: () => customTimeNow(),
      }
    });

@docs now, here, getZoneName

-}

import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as Decode
import Json.Encode as Encode
import Time


{-| Get the POSIX time at the moment when this task is run.
-}
now : ConcurrentTask x Time.Posix
now =
    ConcurrentTask.define
        { function = "builtin:timeNow"
        , expect = ConcurrentTask.expectJson (Decode.map Time.millisToPosix Decode.int)
        , errors = ConcurrentTask.catchAll (Time.millisToPosix 0)
        , args = Encode.null
        }


{-| Produce a `Zone` based on the current UTC offset.
-}
here : ConcurrentTask x Time.Zone
here =
    ConcurrentTask.define
        { function = "builtin:timeZoneOffset"
        , expect = ConcurrentTask.expectJson Decode.int
        , errors = ConcurrentTask.catchAll 0
        , args = Encode.null
        }
        |> ConcurrentTask.map (\offset -> Time.customZone offset [])


{-| Use `Intl.DateTimeFormat().resolvedOptions().timeZone` to try to get names like Europe/Moscow or America/Havana.
-}
getZoneName : ConcurrentTask x Time.ZoneName
getZoneName =
    ConcurrentTask.define
        { function = "builtin:timeZoneOffset"
        , expect = ConcurrentTask.expectJson decodeZoneName
        , errors = ConcurrentTask.catchAll (Time.Offset 0)
        , args = Encode.null
        }


decodeZoneName : Decode.Decoder Time.ZoneName
decodeZoneName =
    Decode.oneOf
        [ Decode.map Time.Offset Decode.int
        , Decode.map Time.Name Decode.string
        ]
