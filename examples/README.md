# Examples

TODO: Add some detailed docs

## Many Requests

To run teh requests example:

1. Start the local server

```
npm run server
```

2. Run the requests example:

```
npm run examples:requests
```

## Pipeline Worker

To run the pipeline worker example:

1. Start local infrastructure (localstack):

```
npm run infra
```

2. Start the worker process:

```
npm run examples:worker
```

3. Send a message to SQS for the worker to pick up:

```
npm run send-message
```

You should see the worker picking up the message and harvesting the fruits! 🍓
