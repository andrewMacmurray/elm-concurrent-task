import * as ConcurrentTask from "../../../runner";

// This is called BEFORE your Elm app starts up
//
// The value returned here will be passed as flags
// into your `Shared.init` function.
export const flags = ({ env }) => {};

// This is called AFTER your Elm app starts up
//
// Here you can work with `app.ports` to send messages
// to your Elm application, or subscribe to incoming
// messages from Elm
export const onReady = ({ app, env }) => {
  ConcurrentTask.register({
    tasks: {},
    ports: {
      send: app.ports.send,
      receive: app.ports.receive,
    },
  });
};
