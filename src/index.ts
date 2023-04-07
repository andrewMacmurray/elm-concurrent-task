import axios from "axios";
import { Elm } from "./elm/Main.elm";
import crypto from "node:crypto";

// Ffi

const Ffi = {
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
      .then((response) => response.data);
  },
};

function waitRandom() {
  const randomN = crypto.randomInt(1000, 2000);
  console.log(`Waiting for ${randomN}`);
  return new Promise((res) => {
    setTimeout(res, randomN);
  });
}

// App

const app = Elm.Main.init({ flags: null });

app.ports.send.subscribe(async (defs) => {
  for (let i = 0; i < defs.length; i++) {
    const def = defs[i];
    if (!Ffi[def.function]) {
      return app.ports.receive.send({
        status: "error",
        error: {
          reason: "missing_function",
          message: `${def.function} is not registered`,
        },
      });
    }
  }

  Promise.all(
    defs.map(async (def) => {
      try {
        const result = await Ffi[def.function](def.args);
        return [def.id, result];
      } catch (e) {
        throw new Error(`${def.function} threw an execption: ${e.message}`);
      }
    })
  )
    .then((res) => {
      const results = Object.fromEntries(res);
      console.log(`results: ${JSON.stringify(results, null, 2)}`);
      app.ports.receive.send({ status: "success", results: results });
    })
    .catch((e) => {
      app.ports.receive.send({
        status: "error",
        error: {
          reason: "js_exception",
          message: e.message,
        },
      });
    });
});
