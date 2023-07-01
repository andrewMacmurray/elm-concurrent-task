import * as http from "./http";

export interface ElmPorts {
  send: {
    subscribe: (callback: (defs: TaskDefinition[]) => Promise<void>) => void;
  };
  receive: { send: (result: TaskResult[]) => void };
}

export interface Builtins {
  http?: (request: http.Request) => Promise<http.Response>;
  timeNow?: () => number;
  randomSeed?: () => number;
  sleep?: (ms: number) => Promise<void>;
}

export type Tasks = { [fn: string]: (any) => any };

export interface TaskDefinition {
  attemptId: string;
  taskId: string;
  function: string;
  args: any;
}

export interface TaskResult {
  attemptId: string;
  taskId: string;
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
  "builtin:sleep": (ms: number) => sleep(ms),
  "builtin:randomSeed": () => Date.now(),
};

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

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
            attemptId: def.attemptId,
            taskId: def.taskId,
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

    const debouncedSend = debounce(send, debounceInterval(defs));

    defs.map(async (def) => {
      try {
        console.log(
          "--STARTING--",
          def.function,
          `${def.attemptId} - ${def.taskId}`
        );
        const result = await tasks[def.function](def.args);
        debouncedSend({
          attemptId: def.attemptId,
          taskId: def.taskId,
          result: { status: "success", value: result },
        });
      } catch (e) {
        debouncedSend({
          attemptId: def.attemptId,
          taskId: def.taskId,
          result: {
            status: "error",
            error: {
              reason: "js_exception",
              message: `${def.function} threw an execption: ${e.message}`,
            },
          },
        });
      }
    });
  });
}

function debounceInterval(defs: TaskDefinition[]): number {
  return defs.length > 10 ? 20 : 0;
}

function debounce(send: (res: TaskResult[]) => void, wait: number) {
  let timeout;
  let results: TaskResult[] = [];

  return function enqueueResult(taskResult: TaskResult) {
    // queue up each task result
    results.push(taskResult);

    const later = () => {
      // clean up timeout after debounce has ended
      clearTimeout(timeout);

      // send batch results to elm
      send(results);
      // clear the queue after results sent
      results = [];
    };

    // Reset the waiting every function execution.
    // Prevents send being called if another task result arrives quickly
    clearTimeout(timeout);

    // Restart the waiting period.
    // setTimeout returns a truthy value
    timeout = setTimeout(later, wait);
  };
}

function createTasks(options: Options): Tasks {
  const tasks = { ...BuiltInTasks, ...options.tasks };
  if (options.builtins?.http) {
    tasks["builtin:httpRequest"] = options.builtins.http;
  }
  if (options.builtins?.timeNow) {
    tasks["builtin:timeNow"] = options.builtins.timeNow;
  }
  if (options.builtins?.randomSeed) {
    tasks["builtin:randomSeed"] = options.builtins.randomSeed;
  }
  return tasks;
}
