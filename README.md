# Elm Concurrent Task

## What?

- An alternative `Task` api - run a tree of tasks concurrently.
- A hack free implementation of `Task Ports` - call JavaScript functions as tasks.
- Run anywhere - works in the Browser or NodeJS.

This package is heavily inspired by [elm-pages](https://github.com/dillonkearns/elm-pages)' `BackendTask` and is intended to be a standalone implementation that can be dropped into any Elm app - big kudos to [Dillon](https://github.com/dillonkearns) for the idea.

See the [examples](https://github.com/andrewMacmurray/elm-concurrent-task/tree/main/examples/src) for more things you can do!

## Why?

### Structured Concurrency

`Task.map2`, `Task.map3`+ In `elm/core` run each subtask in sequence.

Whilst it's possible to run these subtasks concurrently as separate `Cmd`s, it can be a lot of wiring and boilerplate, including:

- Batching task commands together
- Handling each task's success case
- Handling each task's error case
- Checking if all other tasks are completed every time an individual task finishes

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

This is the elm equivalent of "callback hell"

This library helps you do this with a lot less boilerplate.

### A Sequenceable JavaScript FFI

Sometimes you want to call JavaScript from elm in order.

For example sequencing updates to localstorage:

```elm
import Concurrent.Task as Task exposing (Error, Task)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


updateTheme : Theme -> Task Error ()
updateTheme theme =
    getItem "preferences" decodePreferences
        |> Task.map (setTheme theme)
        |> Task.andThen
            (\preferences ->
                preferences
                    |> encodePreferences
                    |> setItem "preferences"
            )


setItem : String -> Encode.Value -> Task Error ()
setItem key item =
    Task.define
        { function = "storage:setItem"
        , expect = Task.expectWhatever
        , args =
            Encode.object
                [ ( "key", Encode.string key )
                , ( "item", item )
                ]
        }


getItem : String -> Decoder a -> Task Error a
getItem key decoder =
    Task.define
        { function = "storage:getItem"
        , expect = Task.expectJson decoder
        , args = Encode.object [ ( "key", Encode.string key ) ]
        }
```

## Hack Free you say?

Popular implementations of `Task Ports` rely on either:

- `ServiceWorkers` - intercept certain http requests and call custom JavaScript from the service worker.
- Monkeypatching `XMLHttpRequest` - Modify methods on the global `XMLHttpRequest` to intercept http requests and call custom JavaScript.

Both methods are not ideal (modifying global methods is pretty dodgy), and neither are portable to other environments like node (`ServiceWorker` and `XMLHttpRequest` are only native in the browser and require pollyfills).

### It's just ports!

`elm-concurrent-task` uses plain ports and a bit of wiring to create a nice Task api.

This makes it dependency free - so more portable (🤓) and less likely to break (😄).

## How?

### Getting Started

### 1. Install Elm and JavaScript/TypeScript packages

Install the elm package with

```
elm install andrewMacMurray/elm-concurrent-task
```

Install the JavaScript/TypeScript runner with

```
npm install @andrewMacmurray/elm-concurrent-task
```

### 2. Add to your Elm app

Your Elm program needs

- A `Task.Pool` in your `Model` to keep track of each task attempt:

  ```elm
  type alias Model =
      { tasks : Task.Pool Error Success
      }
  ```

- 2 `Msg`s to handle task updates:

  ```elm
  type Msg
      = OnProgress ( Task.Pool Error Success, Cmd Msg ) -- updates task progress
      | OnComplete Task.AttemptId (Result Error Success) -- called when a task completes
  ```

- 2 ports with the following signatures:

  ```elm
  port send : Decode.Value -> Cmd msg
  port receive : (Decode.Value -> msg) -> Sub msg
  ```

Here's a simple complete program that fetches 3 resources concurrently:

```elm
port module Example exposing (main)

import Concurrent.Task as Task exposing (Task)
import Concurrent.Task.Http as Http
import Json.Decode as Decode


type alias Model =
    { tasks : Task.Pool Error Titles
    }


type Msg
    = OnProgress ( Task.Pool Error Titles, Cmd Msg )
    | OnComplete Task.AttemptId (Result Error Titles)


type alias Error =
    Http.Error



-- Get All Titles Task


type alias Titles =
    { todo : String
    , post : String
    , album : String
    }


getAllTitles : Task Error Titles
getAllTitles =
    Task.succeed Titles
        |> Task.andMap (getTitle "/todos/1")
        |> Task.andMap (getTitle "/posts/1")
        |> Task.andMap (getTitle "/albums/1")


getTitle : String -> Task Error String
getTitle path =
    Http.request
        { url = "https://jsonplaceholder.typicode.com" ++ path
        , method = "GET"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.field "title" Decode.string)
        }



-- Program


init : ( Model, Cmd Msg )
init =
    let
        ( tasks, cmd ) =
            Task.attempt
                { id = "attempt-id"
                , pool = Task.pool
                , onComplete = OnComplete
                , send = send
                }
                getAllTitles
    in
    ( { tasks = tasks }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnComplete attempt result ->
            let
                _ =
                    Debug.log attempt result
            in
            ( model
            , Cmd.none
            )

        OnProgress ( tasks, cmd ) ->
            ( { model | tasks = tasks }, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Task.onProgress
        { send = send
        , receive = receive
        , onComplete = OnComplete
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
import * as Tasks from "@andrewMacmurray/elm-concurrent-task";

const app = Elm.Main.init({});

Tasks.register({
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

TODO: Cleanup local setup with examples
