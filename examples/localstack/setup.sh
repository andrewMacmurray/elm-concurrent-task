#!/bin/sh

set -e

LOCALSTACK_HOME="/etc/localstack/init/ready.d"

# Input
awslocal sqs create-queue --queue-name tasks-in-dl
awslocal sqs create-queue \
    --queue-name tasks-in \
    --attributes 'RedrivePolicy="{
        \"deadLetterTargetArn\":\"arn:aws:sqs:eu-west-1:000000000000:tasks-in-dl\",
        \"maxReceiveCount\":\"5\"
    }"'

# Output
awslocal sns create-topic --name tasks-out
awslocal sqs create-queue --queue-name tasks-out
awslocal sns subscribe --topic-arn arn:aws:sns:eu-west-1:000000000000:tasks-out \
    --protocol sqs \
    --attributes RawMessageDelivery=true \
    --notification-endpoint arn:aws:sqs:eu-west-1:000000000000:tasks-out

# S3
awslocal s3 mb s3://in-bucket
awslocal s3 mb s3://out-bucket

awslocal s3 cp --recursive "${LOCALSTACK_HOME}/s3/in-bucket/" s3://in-bucket