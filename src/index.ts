import axios from "axios";
import { Elm } from "./elm/Main.elm";
import crypto from "node:crypto";
import * as TaskRunner from "./task-runner";

// Task

const Tasks = {
  slowInt: (i) => waitRandom().then(() => i),
  timeNow: () => Date.now(),
  randomSeed: () => crypto.randomInt(0, 1000000000),
  getEnv: (x) => process.env[x],
  httpRequest: (r) => {
    return axios
      .request({
        method: r.method,
        url: r.url,
        headers: Object.fromEntries(r.headers.map((h) => [h.name, h.value])),
        data: r.body,
      })
      .then((response) => ({
        data: response.data,
        status: response.status,
        statusText: response.statusText,
      }))
      .catch((err) => ({
        errorCode: err.code,
        data: err.response?.data,
        status: err.response?.status,
        statusText: err.response?.statusText,
      }));
  },
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
