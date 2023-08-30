import { Elm } from "./Worker.elm";
import crypto from "node:crypto";
import * as Tasks from "../../src/runner";
import readline from "node:readline/promises";
import * as S3 from "./Aws/s3";
import * as SQS from "./Aws/sqs";
import * as SNS from "./Aws/sns";
import * as Env from "./Common/env";
import * as Uuid from "./Common/uuid";
import * as Logger from "./Common/logger";

// Tasks

const CustomTasks = {
  slowInt: (i) => waitRandom().then(() => i),
  "console:time": (label) => console.time(label),
  "console:timeEnd": (label) => console.timeEnd(label),
};

function waitRandom() {
  return wait(crypto.randomInt(0, 500));
}

function wait(ms) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
}

// App

const { ports } = Elm.Worker.init({ flags: null });

Tasks.register({
  tasks: {
    ...S3.tasks(),
    ...SQS.tasks(),
    ...SNS.tasks(),
    ...Env.tasks(),
    ...Uuid.tasks(),
    ...Logger.tasks(),
    ...CustomTasks,
  },
  ports: {
    send: ports.send,
    receive: ports.receive,
  },
  debug: { taskStart: false },
});

// const rl = readline.createInterface({
//   input: process.stdin,
//   output: process.stdout,
// });

// ask();
// fireMany();

// ports.sendResult.subscribe((result) => {
//   console.log(result);
//   ask();
// });

function fireMany() {
  for (let i = 0; i < 10; i++) {
    waitRandom().then(() => {
      ports.fireMany.send(i);
    });
  }
}

function ask() {
  rl.question("enter an attempt id: ").then((id) => {
    ports.manualEnter.send(id);
  });
}
