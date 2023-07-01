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
  return wait(crypto.randomInt(0, 500));
}

function wait(ms) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
}

// App

const { ports } = Elm.Main.init({ flags: null });

TaskRunner.register({
  tasks: Tasks,
  ports: { send: ports.send, receive: ports.receive },
  builtins: { http },
});

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

ask();
// fireMany();

ports.sendResult.subscribe((result) => {
  console.log(result);
  ask();
});

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
