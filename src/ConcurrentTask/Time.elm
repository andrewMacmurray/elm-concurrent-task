module ConcurrentTask.Time exposing
    ( now, here, getZoneName
    , Duration, withDuration, duration
    )

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


# Duration Helpers

Get the start and finish time of a task.

@docs Duration, withDuration, duration

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



-- Duration Helpers


{-| A result value of a task (success or error) with start and finish times.
-}
type alias Duration a =
    { start : Time.Posix
    , finish : Time.Posix
    , value : a
    }


{-| Add duration information onto a task success or failure.
-}
withDuration : ConcurrentTask x a -> ConcurrentTask (Duration x) (Duration a)
withDuration task =
    now
        |> ConcurrentTask.andThen
            (\start ->
                task
                    |> ConcurrentTask.onError
                        (\x ->
                            now
                                |> ConcurrentTask.andThen
                                    (\finish ->
                                        ConcurrentTask.fail
                                            { start = start
                                            , finish = finish
                                            , value = x
                                            }
                                    )
                        )
                    |> ConcurrentTask.andThen
                        (\a ->
                            now
                                |> ConcurrentTask.map
                                    (\finish ->
                                        { start = start
                                        , finish = finish
                                        , value = a
                                        }
                                    )
                        )
            )


{-| Get the duration in millis of a task result (`finish` - `start`)
-}
duration : Duration a -> Int
duration d =
    Time.posixToMillis d.finish - Time.posixToMillis d.start
