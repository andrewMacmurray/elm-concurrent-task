import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import path from "path";
import { customLogger } from "../util/vite-logger";

export default defineConfig({
  root: "./src",
  plugins: [elmPlugin()],
  customLogger,
});
