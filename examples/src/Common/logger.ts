import chalk from "chalk";

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
      return debug(options.message);
    case "INFO":
      return info(options.message);
    case "ERROR":
      return error(options.message);
    case "WARN":
      return warn(options.message);
    default:
      throw new Error(`Unrecognized Log Level: ${options}`);
  }
}

export function debug(message: string): void {
  console.debug(formatMessage(chalk.magenta, "DEBUG", message));
}

export function info(message: string): void {
  console.info(formatMessage(chalk.cyan, "INFO", message));
}

export function warn(message: string): void {
  console.warn(formatMessage(chalk.yellow, "WARN", message));
}

export function error(message: string): void {
  console.error(formatMessage(chalk.red, "ERROR", message));
}

function formatMessage(
  toColor: (string) => string,
  level: Level,
  message: string
): string {
  const logPrefix = `[${level}]`.padEnd(7, " ");
  return `${toColor(logPrefix)} ${toColor(":")} ${message}`;
}
