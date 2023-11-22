module ConcurrentTask.Time exposing (now, here, getZoneName)

{-| Drop in replacements for [elm/time's](https://package.elm-lang.org/packages/elm/time/latest/Time#now)'s `Task`s.

The JavaScript runner has these tasks builtin by default. If needed they can be overridden like so:

**NOTE:** The custom examples are the same as the built-in implementations.

    import * as ConcurrentTask from "@andrewmacmurray/elm-concurrent-task"

    ConcurrentTask.register({
      tasks: {},
      ports: app.ports,
      builtins: {
        timeNow: customTimeNow,
        timeZoneOffset: customTimeZoneOffset,
        timeZoneName: customTimeZoneName,
      }
    });

    function customTimeNow(): number {
      return Date.now()
    }

    function customTimeZoneOffset(): number {
      return -new Date().getTimezoneOffset();
    }

    function customTimeZoneName(): string | number {
      try {
        return Intl.DateTimeFormat().resolvedOptions().timeZone;
      } catch (e) {
        return new Date().getTimezoneOffset();
      }
    }

@docs now, here, getZoneName

-}

import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as Decode
import Json.Encode as Encode
import Time


{-| Get the POSIX time at the moment when this task is run.

A direct replacement for `elm/time`'s [`Time.now`](https://package.elm-lang.org/packages/elm/time/latest/Time#now).

-}
now : ConcurrentTask x Time.Posix
now =
    ConcurrentTask.define
        { function = "builtin:timeNow"
        , expect = ConcurrentTask.expectJson (Decode.map Time.millisToPosix Decode.int)
        , errors = ConcurrentTask.expectNoErrors
        , args = Encode.null
        }


{-| Produce a `Zone` based on the current UTC offset.

A direct replacement for `elm/time`'s [`Time.here`](https://package.elm-lang.org/packages/elm/time/latest/Time#here).

-}
here : ConcurrentTask x Time.Zone
here =
    ConcurrentTask.define
        { function = "builtin:timeZoneOffset"
        , expect = ConcurrentTask.expectJson Decode.int
        , errors = ConcurrentTask.expectNoErrors
        , args = Encode.null
        }
        |> ConcurrentTask.map (\offset -> Time.customZone offset [])


{-| Use `Intl.DateTimeFormat().resolvedOptions().timeZone` to try to get names like Europe/Moscow or America/Havana.

A direct replacement for `elm/time`'s [`Time.getZoneName`](https://package.elm-lang.org/packages/elm/time/latest/Time#getZoneName).

-}
getZoneName : ConcurrentTask x Time.ZoneName
getZoneName =
    ConcurrentTask.define
        { function = "builtin:timeZoneName"
        , expect = ConcurrentTask.expectJson decodeZoneName
        , errors = ConcurrentTask.expectNoErrors
        , args = Encode.null
        }


decodeZoneName : Decode.Decoder Time.ZoneName
decodeZoneName =
    Decode.oneOf
        [ Decode.map Time.Offset Decode.int
        , Decode.map Time.Name Decode.string
        ]
