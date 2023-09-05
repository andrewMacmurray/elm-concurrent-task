import { Elm } from "./Integration/Runner.elm";
import * as ConcurrentTask from "../../src-ts";

const { ports } = Elm.Integration.Runner.init({ flags: null });

ConcurrentTask.register({
  tasks: {},
  ports: {
    send: ports.send,
    receive: ports.receive,
  },
});

ports.report.subscribe((res: { message: string; passed: boolean }) => {
  console.log(res.message);
  if (res.passed) {
    process.exit(0);
  } else {
    process.exit(1);
  }
});
