import pino from "pino";
import pretty from "pino-pretty";

const stream = pretty({ colorize: true });
const logger = pino({}, stream);

interface Options {
  level: Level;
  message: string;
}

type Level = "DEBUG" | "INFO" | "WARN" | "ERROR";

export function tasks() {
  return {
    "console:log": (args) => {
      return log(args);
    },
  };
}

export function log(options: Options): void {
  switch (options.level) {
    case "DEBUG":
      return logger.debug(options.message);
    case "INFO":
      return logger.info(options.message);
    case "ERROR":
      return logger.error(options.message);
    case "WARN":
      return logger.warn(options.message);
    default:
      throw new Error(`Unrecognized Log Level: ${options}`);
  }
}
