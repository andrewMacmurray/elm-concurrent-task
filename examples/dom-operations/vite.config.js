import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  root: "./src",
  plugins: [elmPlugin()],
});
