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

// Register Runner

interface Options {
  tasks: any;
  ports: ElmPorts;
}

export function register(options: Options): void {
  const tasks = options.tasks;
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
