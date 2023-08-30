## Fruits Pipeline Worker

To run the pipeline worker example:

1. Start local infrastructure (localstack):

```
npm run infra
```

2. Start the worker process:

```
npm start
```

3. Send a message to SQS for the worker to pick up:

```
npm run send-message
```

You should see the worker picking up the message and harvesting the fruits! 🍓
