import { Elm } from "./Main.elm";
import * as Tasks from "../../../src-ts";

const { ports } = Elm.Main.init({ flags: null });

Tasks.register({
  tasks: {},
  ports: {
    send: ports.send,
    receive: ports.receive,
  },
  debug: { taskStart: true },
});

ports.printResult.subscribe((res: string) => {
  console.log(res);
});
