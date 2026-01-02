import { HttpRequest, HttpResponse } from "./http/index.js";
import * as fetchAdapter from "./http/fetch.js";
import {
  DomError,
  DomElement,
  Viewport,
  SetViewportOptions,
  SetViewportOfOptions,
} from "./browser/index.js";
import * as dom from "./browser/dom.js";

export * from "./http/index.js";
export * from "./browser/index.js";

export interface ElmPorts {
  send: { subscribe: (callback: (defs: TaskDefinition[]) => Promise<void>) => void; };
  receive: { send: (result: TaskResult[]) => void };
}

export type Tasks = { [fn: string]: (arg: any) => any };

export interface TaskDefinition {
  function: string;
  attemptId: string;
  taskId: string;
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
    raw?: any;
  };
}

// Built In Tasks

export interface Builtins {
  debugLog?: (message: string) => void;
  http?: (request: HttpRequest) => Promise<HttpResponse>;
  timeNow?: () => number;
  timeZoneOffset?: () => number;
  timeZoneName?: () => string | number;
  randomSeed?: () => number;
  sleep?: (ms: number) => Promise<void>;
  domFocus?: (id: string) => void | DomError;
  domBlur?: (id: string) => void | DomError;
  domGetViewport?: () => Viewport;
  domGetViewportOf?: (id: string) => Viewport | DomError;
  domSetViewport?: (args: SetViewportOptions) => void;
  domSetViewportOf?: (args: SetViewportOfOptions) => void | DomError;
  domGetElement?: (id: string) => DomElement | DomError;
}

const BuiltInTasks: Builtins = {
  debugLog: console.log,
  http: fetchAdapter.http,
  timeNow: () => Date.now(),
  timeZoneOffset: () => getTimezoneOffset(),
  timeZoneName: () => getTimeZoneName(),
  randomSeed: () => Date.now(),
  sleep: sleep,
  domFocus: dom.focus,
  domBlur: dom.blur,
  domGetViewport: dom.getViewport,
  domGetViewportOf: dom.getViewportOf,
  domSetViewport: dom.setViewport,
  domSetViewportOf: dom.setViewportOf,
  domGetElement: dom.getElement,
};

// Equivalent Elm Kernel code: https://github.com/elm/core/blob/master/src/Elm/Kernel/Process.js#L9-L18
// Note: this implementation uses a promise rather than the scheduler callback
function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

// Equivalent Elm Kernel code: https://github.com/elm/time/blob/1.0.0/src/Elm/Kernel/Time.js#L27-L35
function getTimezoneOffset(): number {
  return -new Date().getTimezoneOffset();
}

// Equivalent Elm Kernel code: https://github.com/elm/time/blob/1.0.0/src/Elm/Kernel/Time.js#L38-L52
function getTimeZoneName(): string | number {
  try {
    return Intl.DateTimeFormat().resolvedOptions().timeZone;
  } catch (e) {
    return new Date().getTimezoneOffset();
  }
}

// Debug Options

export interface DebugOptions {
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

  subscribe(async (payload) => {
    const debouncedSend = debounce(send, debounceThreshold(payload));

    for (const def of payload) {
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

    payload.map(async (def) => {
      try {
        logTaskStart(def, options);
        const result = await tasks[def.function]?.(def.args);
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
              raw: e,
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

// If lots of responses come back close together, it's more efficient to send them all at once to Elm (less task pool bookeeping required).
// The debounce colects results that arrive close together and then calls send after the wait threshold elapses.
function debounce(send: (res: TaskResult[]) => void, wait: number) {
  let timeout: ReturnType<typeof setTimeout>;
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
  const builtins = {
    ...BuiltInTasks,
    ...(options.builtins || {}),
  };

  return {
    ...prefixWith("builtin:", builtins),
    ...options.tasks,
  };
}

function prefixWith(prefix: string, tasks: Tasks): Tasks {
  return Object.fromEntries(
    Object.entries(tasks).map(([key, fn]) => [`${prefix}${key}`, fn])
  );
}
