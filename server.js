import express from "express";

const app = express();
const PORT = 4000;

app.get("/wait-then-respond/:time", (req, res) => {
  setTimeout(() => {
    res.send({ message: "done" });
  }, parseInt(req.params.time));
});

app.get("/boom", (req, res) => {
  res.status(400);
  res.send({ message: "error" });
});

app.listen(PORT, () => {
  console.log(`Listening on port ${PORT}`);
});
