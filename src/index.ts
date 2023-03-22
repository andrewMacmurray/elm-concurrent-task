import { Elm } from "./elm/Main.elm";
import xhr from "xhr2";
import * as TaskPort from "elm-taskport/js/taskport.js";
import crypto from "node:crypto";

global.XMLHttpRequest = xhr;
global.ProgressEvent = xhr.ProgressEvent;

const Ffi = {
  slowInt: () => waitRandom().then(() => crypto.randomInt(10, 50)),
};

function waitRandom() {
  const randomN = crypto.randomInt(1000, 2000);
  console.log(`Waiting for ${randomN}`);
  return new Promise((res) => {
    setTimeout(res, randomN);
  });
}

TaskPort.install({ logErrors: false }, undefined);
TaskPort.register("fanout", async (definitions) => {
  return Promise.all(
    definitions.map(async (d) => [
      d.hash,
      await Ffi[d.definition.function](d.definition.args),
    ])
  ).then((res) => {
    const final = Object.fromEntries(res);
    console.log(final);
    return final;
  });
});

const app = Elm.Main.init({ flags: null });

// run each definition in JS
