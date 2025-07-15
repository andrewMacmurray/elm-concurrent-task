import { SQS } from "@aws-sdk/client-sqs";

const endpoint = "http://localhost:4566";

export const queueUrl = `${endpoint}/000000000000/tasks-in`;

export const sqs = new SQS({
  region: "eu-west-1",
  endpoint: endpoint,
  credentials: {
    accessKeyId: "ANY",
    secretAccessKey: "ANY_SECRET",
  },
});
