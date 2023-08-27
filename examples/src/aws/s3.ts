import { S3 } from "@aws-sdk/client-s3";

const s3 = new S3({
  region: "eu-west-1",
  endpoint: "http://localhost:9000",
  credentials: {
    accessKeyId: "ANY",
    secretAccessKey: "ANY_SECRET",
  },
  forcePathStyle: true,
});

export function tasks() {
  return {
    "s3:getObject": (args) => getObject(args),
    "s3:putObject": (args) => putObject(args),
  };
}

function getObject(args) {
  return s3
    .getObject({ Bucket: args.bucket, Key: args.key })
    .then((res) => res.Body?.transformToString());
}

function putObject(args) {
  return s3.putObject({
    Bucket: args.bucket,
    Key: args.key,
    Body: args.contents,
  });
}
