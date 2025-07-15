import { sqs, queueUrl } from "./sqs.mts";
import body from "./message-body.json" with { type: "json" };

sqs
  .sendMessage({
    QueueUrl: queueUrl,
    MessageBody: JSON.stringify(body),
  })
  .then(() => console.log("Message sent"));
