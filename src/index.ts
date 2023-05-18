import { Elm } from "./elm/Main.elm";
import crypto from "node:crypto";
import * as TaskRunner from "./tasks";
import { http } from "./tasks/http/axios";
import readline from "node:readline/promises";

// Task

const Tasks = {
  slowInt: (i) => waitRandom().then(() => i),
  a: (x) => {
    return x;
  },
  b: (x) => {
    return x;
  },
  c: (x) => {
    return x;
  },
  d: (x) => {
    return x;
  },
  e: (x) => {
    return x;
  },
  f: (x) => {
    return x;
  },
  getEnv: (x) => process.env[x],
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

ask();

app.ports.sendResult.subscribe((result) => {
  console.log(result);
  ask();
});

function fireMany() {
  for (let i = 0; i < 10; i++) {
    waitRandom().then(() => {
      app.ports.fireMany.send(i);
    });
  }
}

function ask() {
  rl.question("enter an attempt id: ").then((id) => {
    app.ports.manualEnter.send(id);
  });
}
