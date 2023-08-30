import { Elm } from "./Requests.elm";
import * as Tasks from "../../src/runner";
import * as Logger from "./Common/logger";

// App

const { ports } = Elm.Requests.init({ flags: null });

Tasks.register({
  tasks: {},
  ports: {
    send: ports.send,
    receive: ports.receive,
  },
  debug: { taskStart: true },
});

ports.printResult.subscribe((res) => {
  Logger.log({ level: "INFO", message: res });
});
