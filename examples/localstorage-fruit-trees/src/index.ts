import "../../util/style.css"

import { Elm } from "./Main.elm";
import * as ConcurrentTask from "../../../runner";

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: {
    basket: loadBasket(),
    trees: loadTrees(),
  },
});

ConcurrentTask.register({
  tasks: {
    "localstorage:getItem": getItem,
    "localstorage:setItem": setItem,
  },
  ports: {
    send: app.ports.send,
    receive: app.ports.receive,
  },
});

type ReadError = "NO_VALUE" | "READ_BLOCKED";

function getItem(options: { key: string }): string | { error: ReadError } {
  try {
    const item = localStorage.getItem(options.key);
    if (item === null) {
      return { error: "NO_VALUE" };
    } else {
      return item;
    }
  } catch (e) {
    return { error: "READ_BLOCKED" };
  }
}

type WriteError = "QUOTA_EXCEEDED" | "WRITE_BLOCKED";

function setItem(options: {
  key: string;
  value: string;
}): void | { error: WriteError } {
  try {
    localStorage.setItem(options.key, options.value);
  } catch (e) {
    if (e.name === "QuotaExceededError") {
      return { error: "QUOTA_EXCEEDED" };
    } else {
      return { error: "WRITE_BLOCKED" };
    }
  }
}

// Load Initial Fruits

interface Fruits {
  apples: number;
  oranges: number;
  peaches: number;
  pears: number;
}

function loadTrees(): Fruits {
  return {
    apples: loadInt("tree:apples"),
    oranges: loadInt("tree:oranges"),
    peaches: loadInt("tree:peaches"),
    pears: loadInt("tree:pears"),
  };
}

function loadBasket(): Fruits {
  return {
    apples: loadInt("basket:apples"),
    oranges: loadInt("basket:oranges"),
    peaches: loadInt("basket:peaches"),
    pears: loadInt("basket:pears"),
  };
}

function loadInt(key): number {
  const val = localStorage.getItem(key);
  try {
    if (val) {
      return parseInt(val);
    }
    return 0;
  } catch (e) {
    return 0;
  }
}
