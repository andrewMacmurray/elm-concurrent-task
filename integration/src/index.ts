import { Elm } from "./Integration/Runner.elm";
import * as ConcurrentTask from "../../src-ts";

const { ports } = Elm.Integration.Runner.init({ flags: null });

ConcurrentTask.register({
  tasks: {},
  ports: {
    send: ports.send,
    receive: ports.receive,
  },
  debug: { taskStart: true },
});

ports.report.subscribe((res: { assertions: string; errors: string }) => {
  console.log(res.assertions);
  if (res.errors) {
    console.log(res.errors);
  }
});
