import express from "express";
import morgan from "morgan";
import fs from "node:fs/promises";

const app = express();
const PORT = 4000;

app.use(morgan("tiny"));

app.get("/wait-then-respond/:time", (req, res) => {
  setTimeout(() => {
    res.send({ message: `done:${req.params.time}` });
  }, parseInt(req.params.time));
});

app.get("/big-file", (req, res) => {
  fs.readFile("./500kb.csv").then((file) => res.send(file));
});

app.get("/boom", (req, res) => {
  res.status(400);
  res.send({ message: "error" });
});

app.get("/malformed", (req, res) => {
  res.send("{ 'invalid': 'json");
});

app.listen(PORT, () => {
  console.log(`Listening on port ${PORT}`);
});
