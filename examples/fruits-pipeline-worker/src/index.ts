import { Elm } from "./Main.elm";
import * as Tasks from "../../../src/runner";
import * as S3 from "./Aws/s3";
import * as SQS from "./Aws/sqs";
import * as SNS from "./Aws/sns";
import * as Env from "./Common/env";
import * as Uuid from "./Common/uuid";
import * as Logger from "./Common/logger";

// App

const { ports } = Elm.Main.init({ flags: null });

Tasks.register({
  tasks: {
    ...S3.tasks(),
    ...SQS.tasks(),
    ...SNS.tasks(),
    ...Env.tasks(),
    ...Uuid.tasks(),
    ...Logger.tasks(),
  },
  ports: {
    send: ports.send,
    receive: ports.receive,
  },
  debug: { taskStart: false },
});

ports.printError.subscribe((msg) => {
  Logger.log({ level: "ERROR", message: msg });
});
