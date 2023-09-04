import * as http from "./http";
import * as dom from "./browser/dom";
import * as fetchAdapter from "./http/fetch";

export interface ElmPorts {
  send: {
    subscribe: (callback: (defs: TaskDefinition[]) => Promise<void>) => void;
  };
  receive: { send: (result: TaskResult[]) => void };
}

export interface Builtins {
  http?: (request: http.Request) => Promise<http.Response>;
  timeNow?: () => number;
  timeZoneOffset?: () => number;
  timeZoneName?: () => string | number;
  randomSeed?: () => number;
  sleep?: (ms: number) => Promise<void>;
  domFocus?: (id: string) => void | dom.Error;
  domBlur?: (id: string) => void | dom.Error;
  domGetViewport?: () => dom.Viewport;
  domGetViewportOf?: (id: string) => dom.Viewport | dom.Error;
  domSetViewport?: (args: dom.SetViewport) => void;
  domSetViewportOf?: (args: dom.SetViewportOf) => void | dom.Error;
  domGetElement?: (id: string) => dom.DomElement | dom.Error;
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
  value: any;
}

export interface Error {
  error: {
    reason: string;
    message: string;
  };
}

// Built In Tasks

const BuiltInTasks = {
  "builtin:timeNow": () => Date.now(),
  "builtin:timeZoneOffset": () => getTimezoneOffset(),
  "builtin:timeZoneName": () => getTimeZoneName(),
  "builtin:randomSeed": () => Date.now(),
  "builtin:sleep": sleep,
  "builtin:http": fetchAdapter.http,
  "builtin:domFocus": dom.focus,
  "builtin:domBlur": dom.blur,
  "builtin:domGetViewport": dom.getViewport,
  "builtin:domGetViewportOf": dom.getViewportOf,
  "builtin:domSetViewport": dom.setViewport,
  "builtin:domSetViewportOf": dom.setViewportOf,
  "builtin:domGetElement": dom.getElement,
};

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function getTimezoneOffset() {
  return -new Date().getTimezoneOffset();
}

function getTimeZoneName(): string | number {
  try {
    return Intl.DateTimeFormat().resolvedOptions().timeZone;
  } catch (e) {
    return new Date().getTimezoneOffset();
  }
}

// Debug Options

interface DebugOptions {
  taskStart?: boolean;
  taskFinish?: boolean;
}

// Register Runner

export interface Options {
  tasks: Tasks;
  ports: ElmPorts;
  builtins?: Builtins;
  debug?: boolean | DebugOptions;
}

export function register(options: Options): void {
  const tasks = createTasks(options);
  const subscribe = options.ports.send.subscribe;
  const send = options.ports.receive.send;

  subscribe(async (defs) => {
    const debouncedSend = debounce(send, debounceThreshold(defs));

    for (let i = 0; i < defs.length; i++) {
      const def = defs[i];
      if (!tasks[def.function]) {
        return debouncedSend({
          attemptId: def.attemptId,
          taskId: def.taskId,
          result: {
            error: {
              reason: "missing_function",
              message: `${def.function} is not registered`,
            },
          },
        });
      }
    }

    defs.map(async (def) => {
      try {
        logTaskStart(def, options);
        const result = await tasks[def.function](def.args);
        logTaskFinish(def, options);
        debouncedSend({
          attemptId: def.attemptId,
          taskId: def.taskId,
          result: { value: result },
        });
      } catch (e) {
        debouncedSend({
          attemptId: def.attemptId,
          taskId: def.taskId,
          result: {
            error: {
              reason: "js_exception",
              message: `${e.name}: ${e.message}`,
            },
          },
        });
      }
    });
  });
}

function logTaskStart(def: TaskDefinition, options: Options): void {
  const logStart =
    options.debug &&
    typeof options.debug !== "boolean" &&
    options.debug.taskStart;

  if (logStart || options.debug === true) {
    console.info(
      `--starting-- ${def.function} attempt-${def.attemptId} id-${def.taskId}`
    );
  }
}

function logTaskFinish(def: TaskDefinition, options: Options): void {
  const logStart =
    options.debug &&
    typeof options.debug !== "boolean" &&
    options.debug.taskFinish;

  if (logStart || options.debug === true) {
    console.info(
      `--complete-- ${def.function} attempt - ${def.attemptId} id - ${def.taskId}`
    );
  }
}

function debounceThreshold(defs: TaskDefinition[]): number {
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

  Object.entries(options.builtins || {}).forEach(([name, override]) => {
    tasks[`builtin:${name}`] = override;
  });

  return tasks;
}
