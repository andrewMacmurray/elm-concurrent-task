# Elm Concurrent Task

## What?

- An alternative `Task` api - run a tree of tasks concurrently.
- A hack free implementation of `Task Ports` - call JavaScript functions as tasks.
- Run anywhere - works in the Browser or NodeJS.

This package is heavily inspired by [elm-pages](https://github.com/dillonkearns/elm-pages)' `BackendTask` and is intended to be a standalone implementation that can be dropped into any Elm app - big kudos to [Dillon](https://github.com/dillonkearns) for the idea.

See the [examples](https://github.com/andrewMacmurray/elm-concurrent-task/tree/main/examples) for more things you can do!

Preview [the docs here](https://elm-doc-preview.netlify.app/?repo=andrewMacmurray/elm-concurrent-task&version=main).

I would love your help beta testing this library! If you're interested please checkout the [Beta Testing Section](#beta-testing).

## Why?

### Structured Concurrency

`Task.map2`, `Task.map3`+ In `elm/core` run each subtask in sequence.

Whilst it's possible to run these subtasks concurrently as separate `Cmd`s, it can be a lot of wiring and boilerplate, including:

- Batching task commands together.
- Handling each task's success case.
- Handling each task's error case.
- Checking if all other tasks are completed every time an individual task finishes.

[Elm Task Parallel](https://github.com/0ui/elm-task-parallel) handles this nicely but only at the top level (sub tasks cannot be parallelised).

And what if you want to speed up a complex nested task like:

```elm
Task.map2 combine
    (task1
        |> Task.andThen task2
        |> Task.andThen
            (\res ->
                Task.map2 combine
                    (task3 res)
                    task4
            )
    )
    (Task.map2 combine
        task5
        task6
    )
```

This is the elm equivalent of "callback hell".

This library helps you do this with a lot less boilerplate.

### A Sequenceable JavaScript FFI

Sometimes you want to call JavaScript from elm in order. For example sequencing updates to localstorage:

**NOTE**: See a [full working localstorage example here](https://github.com/andrewMacmurray/elm-concurrent-task/tree/main/examples/localstorage-fruit-trees).

```elm
import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Preferences


type alias Preferences =
    { contrast : Int
    , brightness : Int
    }


setContrast : Int -> ConcurrentTask Error ()
setContrast contrast =
    getItem "preferences" decodePreferences
        |> ConcurrentTask.map (\preferences -> { preferences | contrast = contrast })
        |> ConcurrentTask.andThen (encodePreferences >> setItem "preferences")


encodePreferences : Preferences -> Encode.Value
encodePreferences p =
    Encode.object
        [ ( "contrast", Encode.int p.contrast )
        , ( "brightness", Encode.int p.brightness )
        ]


decodePreferences : Decoder Preferences
decodePreferences =
    Decode.map2 Preferences
        (Decode.field "contrast" Decode.int)
        (Decode.field "brightness" Decode.int)



-- Localstorage


type Error
    = NoValue
    | ReadBlocked
    | DecodeError Decode.Error
    | WriteError String


getItem : String -> Decoder a -> ConcurrentTask Error a
getItem key decoder =
    ConcurrentTask.define
        { function = "localstorage:getItem"
        , expect = ConcurrentTask.expectString
        , errors = ConcurrentTask.expectErrors decodeReadErrors
        , args = Encode.object [ ( "key", Encode.string key ) ]
        }
        |> ConcurrentTask.map (Decode.decodeString decoder >> Result.mapError DecodeError)
        |> ConcurrentTask.andThen ConcurrentTask.fromResult


setItem : String -> Encode.Value -> ConcurrentTask Error ()
setItem key value =
    ConcurrentTask.define
        { function = "localstorage:setItem"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectThrows WriteError
        , args =
            Encode.object
                [ ( "key", Encode.string key )
                , ( "value", Encode.string (Encode.encode 0 value) )
                ]
        }


decodeReadErrors : Decoder Error
decodeReadErrors =
    Decode.string
        |> Decode.andThen
            (\reason ->
                case reason of
                    "NO_VALUE" ->
                        Decode.succeed NoValue

                    "READ_BLOCKED" ->
                        Decode.succeed ReadBlocked

                    _ ->
                        Decode.fail ("Unrecognized Read Error: " ++ reason)
            )
```

## Hack Free you say?

Popular implementations of `Task Ports` rely on either:

- `ServiceWorkers` - intercept certain http requests and call custom JavaScript from the service worker.
- Monkeypatching `XMLHttpRequest` - Modify methods on the global `XMLHttpRequest` to intercept http requests and call custom JavaScript.

Both methods are not ideal (modifying global methods is pretty dodgy), and neither are portable to other environments like node (`ServiceWorker` and `XMLHttpRequest` are only native in the browser and require pollyfills).

### It's just ports!

`elm-concurrent-task` uses plain ports and a bit of wiring to create a nice Task api.

This makes it dependency free - so more portable (🤓) and less likely to break (😄).

## Caveats

Because `elm-concurrent-task` uses a different type to `elm/core` `Task` it's unfortunately not compatible with `elm/core` `Task`s.

However, there are a number of tasks built into the JavaScript runner and supporting modules that should cover a large amount of the existing functionality of `elm/core` `Task`s.

Check out the built-ins for more details:

- [`Browser.Dom`](https://package.elm-lang.org/packages/andrewMacmurray/elm-concurrent-task/1.0.0/ConcurrentTask-Browser-Dom/)
- [`Http`](https://package.elm-lang.org/packages/andrewMacmurray/elm-concurrent-task/1.0.0/ConcurrentTask-Http/)
- [`Process`](https://package.elm-lang.org/packages/andrewMacmurray/elm-concurrent-task/1.0.0/ConcurrentTask-Process/)
- [`Random`](https://package.elm-lang.org/packages/andrewMacmurray/elm-concurrent-task/1.0.0/ConcurrentTask-Random/)
- [`Time`](https://package.elm-lang.org/packages/andrewMacmurray/elm-concurrent-task/1.0.0/ConcurrentTask-Time/)

## How?

## Beta Testing

I would love your help beta testing this lib before publishing it!

If you want to try it out, here are a few steps:

1. Clone this repo into your elm project:

```
git clone git@github.com:andrewMacmurray/elm-concurrent-task.git
```

2. Add `./elm-concurrent-task/src` to your `elm.json` `source-directories`.

```json
"source-directories": [
    "./src",
    "./elm-concurrent-task/src"
],
```

3. Follow steps 2 & 3 in the [Getting Started](#getting-started) section below to try it out in your project.

   You can preview [the docs here](https://elm-doc-preview.netlify.app/?repo=andrewMacmurray/elm-concurrent-task&version=main).

4. Leave an [issue](https://github.com/andrewMacmurray/elm-concurrent-task/issues) or chat to me on the [`#Elm Slack`](https://elmlang.slack.com/team/U4Y56SNH3) - I'd love to hear from you with any feedback 😄.

### Getting Started

### 1. Install Elm and JavaScript/TypeScript packages

Install the elm package with

```
elm install andrewMacmurray/elm-concurrent-task
```

Install the JavaScript/TypeScript runner with

```
npm install @andrewMacmurray/elm-concurrent-task
```

### 2. Add to your Elm app

Your Elm program needs

- A `ConcurrentTask.Pool` in your `Model` to keep track of each task attempt:

  ```elm
  type alias Model =
      { tasks : ConcurrentTask.Pool Msg Error Success
      }
  ```

- 2 `Msg`s to handle task updates:

  ```elm
  type Msg
      = OnProgress ( ConcurrentTask.Pool Msg Error Success, Cmd Msg ) -- updates task progress
      | OnComplete (ConcurrentTask.Response Error Success) -- called when a task completes
  ```

- 2 ports with the following signatures:

  ```elm
  port send : Decode.Value -> Cmd msg
  port receive : (Decode.Value -> msg) -> Sub msg
  ```

Here's a simple complete program that fetches 3 resources concurrently:

```elm
port module Example exposing (main)

import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Http as Http
import Json.Decode as Decode


type alias Model =
    { tasks : ConcurrentTask.Pool Msg Error Titles
    }


type Msg
    = OnProgress ( ConcurrentTask.Pool Msg Error Titles, Cmd Msg )
    | OnComplete (ConcurrentTask.Response Error Titles)


type alias Error =
    Http.Error



-- Get All Titles Task


type alias Titles =
    { todo : String
    , post : String
    , album : String
    }


getAllTitles : ConcurrentTask Error Titles
getAllTitles =
    ConcurrentTask.succeed Titles
        |> ConcurrentTask.andMap (getTitle "/todos/1")
        |> ConcurrentTask.andMap (getTitle "/posts/1")
        |> ConcurrentTask.andMap (getTitle "/albums/1")


getTitle : String -> ConcurrentTask Error String
getTitle path =
    Http.get
        { url = "https://jsonplaceholder.typicode.com" ++ path
        , headers = []
        , expect = Http.expectJson (Decode.field "title" Decode.string)
        , timeout = Nothing
        }



-- Program


init : ( Model, Cmd Msg )
init =
    let
        ( tasks, cmd ) =
            ConcurrentTask.attempt
                { send = send
                , pool = ConcurrentTask.pool
                , onComplete = OnComplete
                }
                getAllTitles
    in
    ( { tasks = tasks }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnComplete response ->
            let
                _ =
                    Debug.log "response" response
            in
            ( model, Cmd.none )

        OnProgress ( tasks, cmd ) ->
            ( { model | tasks = tasks }, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    ConcurrentTask.onProgress
        { send = send
        , receive = receive
        , onProgress = OnProgress
        }
        model.tasks


port send : Decode.Value -> Cmd msg


port receive : (Decode.Value -> msg) -> Sub msg


main : Program {} Model Msg
main =
    Platform.worker
        { init = always init
        , update = update
        , subscriptions = subscriptions
        }
```

### 3. Register the runner in your JavaScript/TypeScript app

Connect the runner to your Elm app:

```ts
import * as ConcurrentTask from "@andrewMacmurray/elm-concurrent-task";
// if you're beta testing this lib: import * as ConcurrentTask from "./elm-concurrent-task/runner"

const app = Elm.Main.init({});

ConcurrentTask.register({
  tasks: {},
  ports: {
    send: app.ports.send,
    receive: app.ports.receive,
  },
});
```

The value passed to `tasks` is an object of task names to functions (the functions can return plain synchronous values or promises)

e.g. tasks for reading and writing to localStorage:

```js
const tasks = {
  "storage:get": (args) => localStorage.getItem(args.key),
  "storage:set": (args) => localStorage.setItem(args.key, args.item),
};
```

## Develop Locally

Install Dependencies:

```
npm install
```

Run the tests with:

```
npm test
```

To preview any changes, try some of the examples in the [examples folder](https://github.com/andrewMacmurray/elm-concurrent-task/tree/main/examples).

View the docs locally with:

```
npm run docs
```
