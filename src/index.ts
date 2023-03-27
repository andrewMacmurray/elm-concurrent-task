import { Elm } from "./elm/Main.elm";
import xhr from "xhr2";
import * as TaskPort from "elm-taskport/js/taskport.js";
import crypto from "node:crypto";

global.XMLHttpRequest = xhr;
global.ProgressEvent = xhr.ProgressEvent;

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

TaskPort.install({ logErrors: false }, undefined);
TaskPort.register("fanout", (definitions) => {
  return Promise.all(
    definitions.map((d) => Ffi[d.definition.function](d.definition.args))
  ).then((res) => {
    console.log(`${res}`);
    return res;
  });
});

const app = Elm.Main.init({ flags: null });
