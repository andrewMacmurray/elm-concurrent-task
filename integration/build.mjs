import * as esbuild from "esbuild";
import ElmPlugin from "esbuild-plugin-elm";
import start from "@es-exec/esbuild-plugin-start";

function main() {
  if (process.argv.includes("--ci")) {
    return ci();
  } else {
    return watch();
  }
}

const options = {
  ENTRTY: "src/index.ts",
  OUTFILE: "dist/index.js",
  START: "node ./dist/index.js",
};

function ci() {
  esbuild.build({
    entryPoints: [options.ENTRTY],
    bundle: true,
    outfile: options.OUTFILE,
    platform: "node",
    plugins: [ElmPlugin({}), start({ script: options.START })],
  });
}

function watch() {
  esbuild
    .context({
      entryPoints: [options.ENTRTY],
      bundle: true,
      outfile: options.OUTFILE,
      platform: "node",
      plugins: [ElmPlugin({}), start({ script: options.START })],
    })
    .then((ctx) => ctx.watch());
}

main();
