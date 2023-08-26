import { Elm } from "./Main.elm";
import crypto from "node:crypto";
import * as TaskRunner from "../../src/runner";
import readline from "node:readline/promises";

// Task

const Tasks = {
  slowInt: (i) => waitRandom().then(() => i),
  "env:load": () => process.env,
  consoleTime: (label) => console.time(label),
  consoleTimeEnd: (label) => console.timeEnd(label),
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
  ports: {
    send: ports.send,
    receive: ports.receive,
  },
  debug: { taskStart: true },
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
