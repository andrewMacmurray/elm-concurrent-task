import axios from "axios";

doBatch();
// doSequence();

function doBatch() {
  Promise.all(Array.from({ length: 800 }).map((_, i) => doRequest(i)))
    .then((xs) => xs.join(","))
    .then(console.log);
}

function doSequence() {
  Array.from({ length: 800 })
    .reduce(
      (acc, _, i) =>
        acc.then((res) => doRequest(i).then((res2) => `${res},${res2}`)),
      Promise.resolve("start")
    )
    .then(console.log);
}

function doRequest(i) {
  console.log(`STARTING - ${i}`);
  return axios
    .get("http://localhost:4000/wait-then-respond/0")
    .then((x) => x.data.message);
}
