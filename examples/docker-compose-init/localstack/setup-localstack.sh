#!/bin/sh

set -e

LOCALSTACK_ENDPOINT=${LOCALSTACK_ENDPOINT:-http://${HOSTNAME}:4566}

# Input
aws --endpoint-url "${LOCALSTACK_ENDPOINT}" sqs create-queue --queue-name tasks-in

# Output
aws --endpoint-url "${LOCALSTACK_ENDPOINT}" sns create-topic --name tasks-out
aws --endpoint-url "${LOCALSTACK_ENDPOINT}" sqs create-queue --queue-name tasks-out
aws --endpoint-url "${LOCALSTACK_ENDPOINT}" sns subscribe --topic-arn arn:aws:sns:eu-west-1:000000000000:tasks-out \
    --protocol sqs \
    --attributes RawMessageDelivery=true \
    --notification-endpoint arn:aws:sqs:eu-west-1:123456789012:tasks-out
