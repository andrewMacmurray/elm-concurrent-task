import { sqs, queueUrl } from "./sqs.mts";

sqs
  .receiveMessage({
    QueueUrl: queueUrl,
    AttributeNames: ["All"],
    MessageAttributeNames: ["All"],
  })
  .then((msg) => {
    console.log("Received message:")
    console.dir(msg, { depth: null });
  });
