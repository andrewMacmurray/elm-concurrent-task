import { createLogger } from "vite";

// Custom logger for vite that bypasses the vite-plugin-elm warning
export const customLogger = createLogger();
const loggerWarn = customLogger.warn;

customLogger.warn = (msg, options) => {
  if (msg.includes("vite-plugin-elm") && msg.includes("Did you pass a URL?")) return;
  return loggerWarn(msg, options);
};
