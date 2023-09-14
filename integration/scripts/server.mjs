import express from "express";
import morgan from "morgan";

const app = express();
const PORT = process.env.PORT || 4999;

app.use(morgan("tiny"));
app.use(express.json());

app.get("/wait-then-respond/:time", (req, res) => {
  setTimeout(() => {
    res.send({ message: `done:${req.params.time}` });
  }, parseInt(req.params.time));
});

app.post("/echo", (req, res) => {
  res.send(req.body);
});

app.get("/boom", (_, res) => {
  res.status(400);
  res.send({ message: "error" });
});

app.get("/malformed", (_, res) => {
  res.send("{ 'invalid': 'json");
});

app.listen(PORT, () => {
  console.log(`Listening on port ${PORT}`);
});
