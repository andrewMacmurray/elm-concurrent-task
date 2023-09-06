import express from "express";
import morgan from "morgan";

const app = express();
const PORT = process.env.PORT || 4000;

app.use(morgan("tiny"));
app.use(express.json());

app.get("/wait-then-respond/:time", (req, res) => {
  setTimeout(() => {
    res.send({ message: `done:${req.params.time}` });
  }, parseInt(req.params.time));
});

app.get("/boom", (req, res) => {
  res.status(400);
  res.send({ message: "error" });
});

app.get("/flaky", (req, res) => {
  if (Math.random() > 0.7) {
    res.send({ message: "ok" });
  } else {
    res.status(400);
    res.send({ message: "error" });
  }
});

app.listen(PORT, () => {
  console.log(`Listening on port ${PORT}`);
});
