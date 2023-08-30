import * as esbuild from "esbuild";
import ElmPlugin from "esbuild-plugin-elm";
import start from "@es-exec/esbuild-plugin-start";

esbuild
  .context({
    entryPoints: ["src/index.ts"],
    bundle: true,
    outfile: "dist/bundle.js",
    platform: "node",
    plugins: [ElmPlugin({}), start({ script: "node ./dist/bundle.js" })],
  })
  .then((ctx) => ctx.watch());
