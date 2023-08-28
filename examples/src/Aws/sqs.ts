import { SQS } from "@aws-sdk/client-sqs";

const sqs = new SQS({
  region: "eu-west-1",
  endpoint: "http://localhost:4566",
  credentials: {
    accessKeyId: "ANY",
    secretAccessKey: "ANY_SECRET",
  },
});

export function tasks() {
  return {
    "sqs:sendMessage": (args) => sendMessage(args),
    "sqs:deleteMessage": (args) => deleteMessage(args),
    "sqs:receiveMessage": (args) => receiveMessage(args),
  };
}

function receiveMessage(args) {
  return sqs
    .receiveMessage({
      QueueUrl: queueUrl(args.queueName),
      AttributeNames: ["All"],
      VisibilityTimeout: args.visibilityTimeout,
      MaxNumberOfMessages: args.maxMessages,
      WaitTimeSeconds: args.waitTimeSeconds,
    })
    .then((res) => res.Messages || []);
}

function sendMessage(args) {
  return sqs.sendMessage({
    QueueUrl: queueUrl(args.queueName),
    MessageBody: args.message,
  });
}

function deleteMessage(args) {
  return sqs.deleteMessage({
    QueueUrl: queueUrl(args.queueName),
    ReceiptHandle: args.receiptHandle,
  });
}

function queueUrl(name) {
  return `http://localhost:4566/000000000000/${name}`;
}
