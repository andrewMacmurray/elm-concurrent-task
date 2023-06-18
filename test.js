import axios from "axios";

main();

function main() {
  Promise.all(
    Array.from({ length: 2000 }).map(() =>
      axios
        .get("http://localhost:4000/wait-then-respond/0")
        .then((x) => x.data.message)
    )
  )
    .then((xs) => xs.join(","))
    .then(console.log);
}
