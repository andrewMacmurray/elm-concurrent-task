import { Elm } from "./elm/Main.elm";
import crypto from "node:crypto";

// Ffi

const Ffi = {
  slowInt: (i) => waitRandom().then(() => i),
};

function waitRandom() {
  const randomN = crypto.randomInt(1000, 2000);
  console.log(`Waiting for ${randomN}`);
  return new Promise((res) => {
    setTimeout(res, randomN);
  });
}

const app = Elm.Main.init({ flags: null });

app.ports.send.subscribe(async (defs) => {
  Promise.all(
    defs.map(async (def) => {
      return [def.id, await Ffi[def.function](def.args)];
    })
  ).then((res) => {
    console.log(res);
    const results = Object.fromEntries(res);
    console.log(`results: ${JSON.stringify(results, null, 2)}`);
    app.ports.receive.send(results);
  });
});
