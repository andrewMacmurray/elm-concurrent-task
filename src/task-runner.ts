import axios, { AxiosError } from "axios";

interface ElmPorts {
  send: {
    subscribe: (callback: (defs: TaskDefinition[]) => Promise<void>) => void;
  };
  receive: { send: (result: Result[]) => void };
}

interface TaskDefinition {
  id: string;
  function: string;
  args: any;
}

interface Result {
  id: string;
  result: Success | Error;
}

interface Success {
  status: "success";
  value: any;
}

interface Error {
  status: "error";
  error: {
    reason: string;
    message: string;
  };
}

// Built In Tasks

const BuiltInTasks = {
  "builtin:timeNow": () => Date.now(),
  "builtin:randomSeed": () => Math.round(Math.random() * 1000000000000),
  "builtin:httpRequest": (request) => doAxiosRequest(request),
};

// Http Task

interface HttpRequest {
  url: string;
  method: string;
  headers: { name: string; value: string }[];
  body: any;
}

type HttpResponse = HttpResponseSuccess | HttpResponseError;

interface HttpResponseSuccess {
  body: any;
  status: number;
  statusText: string;
}

type HttpError = "BAD_URL" | "NETWORK_ERROR" | "TIMEOUT" | "UNKNOWN";

interface HttpResponseError {
  error: HttpError;
  body: any;
  status: number;
  statusText: string;
}

function doAxiosRequest(request: HttpRequest): Promise<HttpResponse> {
  return axios
    .request({
      method: request.method,
      url: request.url,
      data: request.body,
      headers: Object.fromEntries(
        request.headers.map((header) => [header.name, header.value])
      ),
    })
    .then((response) => ({
      body: response.data,
      status: response.status,
      statusText: response.statusText,
    }))
    .catch((err) => ({
      error: toHttpError(err),
      body: err.response?.data,
      status: err.response?.status,
      statusText: err.response?.statusText,
    }));
}

function toHttpError(err: AxiosError): HttpError {
  switch (err.code) {
    case "ECONNABORTED":
      return "TIMEOUT";
    case "ERR_NETWORK":
      return "NETWORK_ERROR";
    case "ERR_INVALID_URL":
      return "BAD_URL";
    default:
      return "UNKNOWN";
  }
}

// Register Runner

interface Options {
  tasks: any;
  ports: ElmPorts;
}

export function register(options: Options): void {
  const tasks = { ...BuiltInTasks, ...options.tasks };
  const subscribe = options.ports.send.subscribe;
  const send = options.ports.receive.send;

  subscribe(async (defs) => {
    for (let i = 0; i < defs.length; i++) {
      const def = defs[i];
      if (!tasks[def.function]) {
        return send([
          {
            id: def.id,
            result: {
              status: "error",
              error: {
                reason: "missing_function",
                message: `${def.function} is not registered`,
              },
            },
          },
        ]);
      }
    }

    defs.map(async (def) => {
      try {
        console.log("--STARTING--", def.function, def.id);
        const result = await tasks[def.function](def.args);
        send([
          {
            id: def.id,
            result: { status: "success", value: result },
          },
        ]);
      } catch (e) {
        send([
          {
            id: def.id,
            result: {
              status: "error",
              error: {
                reason: "js_exception",
                message: `${def.function} threw an execption: ${e.message}`,
              },
            },
          },
        ]);
      }
    });
  });
}
