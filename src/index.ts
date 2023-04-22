import { Elm } from "./elm/Main.elm";
import crypto from "node:crypto";
import * as TaskRunner from "./tasks";
import { http } from "./tasks/http/axios";
import readline from "node:readline/promises";

// Task

const Tasks = {
  slowInt: (i) => waitRandom().then(() => i),
  getEnv: (x) => process.env[x],
};

function waitRandom() {
  const randomN = crypto.randomInt(0, 500);
  return new Promise((resolve) => {
    setTimeout(resolve, randomN);
  });
}

// App

const app = Elm.Main.init({ flags: null });

TaskRunner.register({
  tasks: Tasks,
  ports: app.ports,
  builtins: { http },
});

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

loopAsk();

function fireMany() {
  for (let i = 0; i < 10; i++) {
    waitRandom().then(() => {
      app.ports.fireMany.send(i);
    });
  }
}

function loopAsk() {
  rl.question("enter an attempt id: ")
    .then((id) => {
      app.ports.manualEnter.send(id);
    })
    .then(() => loopAsk());
}
