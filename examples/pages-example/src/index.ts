import "../../util/style.css"

import { Elm } from "./Main.elm";
import * as ConcurrentTask from "../../../runner";

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: {},
});

ConcurrentTask.register({
  tasks: {},
  ports: {
    send: app.ports.send,
    receive: app.ports.receive,
  },
});
