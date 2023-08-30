#! /bin/bash

set -e

aws sqs send-message \
  --region="eu-west-1" \
  --endpoint-url=http://localhost:4566 \
  --queue-url http://localhost:4566/queue/eu-west-1/000000000000/tasks-in \
  --message-body "$(cat ./scripts/message-body.json)"