import { SNS } from "@aws-sdk/client-sns";

const sns = new SNS({
  region: "eu-west-1",
  endpoint: "http://localhost:4566",
  credentials: {
    accessKeyId: "ANY",
    secretAccessKey: "ANY_SECRET",
  },
});

export function tasks() {
  return {
    "sns:publish": (args) => publish(args),
  };
}

function publish(args) {
  return sns.publish({
    TopicArn: `arn:aws:sns:eu-west-1:000000000000:${args.topicName}`,
    Message: args.message,
  });
}
