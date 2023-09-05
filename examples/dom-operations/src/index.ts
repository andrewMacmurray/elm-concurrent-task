import { Elm } from "./Main.elm";
import * as ConcurrentTask from "../../../src-ts";

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: null,
});

ConcurrentTask.register({
  tasks: {},
  ports: {
    send: app.ports.send,
    receive: app.ports.receive,
  },
});
