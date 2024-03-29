import { Elm } from "./Main.elm";
import * as ConcurrentTask from "../../runner";

const { ports } = Elm.Main.init({ flags: null });

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
