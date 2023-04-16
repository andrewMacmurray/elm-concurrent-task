import axios from "axios";
import { Elm } from "./elm/Main.elm";
import crypto from "node:crypto";
import * as TaskRunner from "./task-runner";

// Task

const Tasks = {
  slowInt: (i) => waitRandom().then(() => i),
  getEnv: (x) => process.env[x],
};

function waitRandom() {
  const randomN = crypto.randomInt(0, 500);
  console.log(`Waiting for ${randomN}`);
  return new Promise((res) => {
    setTimeout(res, randomN);
  });
}

// App

const app = Elm.Main.init({ flags: null });

TaskRunner.register({
  tasks: Tasks,
  ports: app.ports,
});
