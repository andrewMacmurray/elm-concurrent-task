import axios from "axios";

doBatch();
// doSequence();

function doBatch() {
  console.time("batch");
  Promise.all(Array.from({ length: 1000 }).map((_, i) => doRequest(i)))
    .then((xs) => xs.join(","))
    .then(console.log)
    .then(() => console.timeEnd("batch"));
}

function doSequence() {
  console.time("sequence");
  Array.from({ length: 10000 })
    .reduce(
      (acc, _, i) =>
        acc.then((res) => doRequest(i).then((res2) => `${res},${res2}`)),
      Promise.resolve("start")
    )
    .then(console.log)
    .then(() => console.timeEnd("sequence"));
}

function doRequest(i) {
  console.log(`STARTING - ${i}`);
  return axios
    .get("http://localhost:4000/wait-then-respond/0")
    .then((x) => x.data.message);
}
