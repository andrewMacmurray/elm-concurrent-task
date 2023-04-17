import * as http from "./http/task";

export interface ElmPorts {
  send: {
    subscribe: (callback: (defs: TaskDefinition[]) => Promise<void>) => void;
  };
  receive: { send: (result: Result[]) => void };
}

export interface Builtins {
  http?: (request: http.Request) => Promise<http.Response>;
  timeNow?: () => number;
}

export type Tasks = { [fn: string]: (any) => any };

export interface TaskDefinition {
  id: string;
  function: string;
  args: any;
}

export interface Result {
  id: string;
  result: Success | Error;
}

export interface Success {
  status: "success";
  value: any;
}

export interface Error {
  status: "error";
  error: {
    reason: string;
    message: string;
  };
}

// Built In Tasks

const BuiltInTasks = {
  "builtin:timeNow": () => Date.now(),
};

// Register Runner

export interface Options {
  tasks: Tasks;
  ports: ElmPorts;
  builtins?: Builtins;
}

export function register(options: Options): void {
  const tasks = createTasks(options);
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

function createTasks(options: Options): Tasks {
  const tasks = { ...BuiltInTasks, ...options.tasks };
  if (options.builtins?.http) {
    tasks["builtin:httpRequest"] = options.builtins.http;
  }
  if (options.builtins?.timeNow) {
    tasks["builtin:timeNow"] = options.builtins.timeNow;
  }
  return tasks;
}
