## Fruits Pipeline Worker

To run the pipeline worker example:

Add the env vars:

```
export IN_QUEUE=tasks-in
export OUT_BUCKET=out-bucket
export OUT_TOPIC=tasks-out
export AWS_ACCESS_KEY_ID=ANY
export AWS_SECRET_ACCESS_KEY=ANY_SECRET
```

1. Install dependencies:

```
npm install
```

2. Start local infrastructure (localstack):

```
npm run infra
```

3. Start the worker process:

```
npm start
```

4. Send a message to SQS for the worker to pick up:

```
npm run send-message
```

You should see the worker picking up the message and harvesting the fruits! 🍓
