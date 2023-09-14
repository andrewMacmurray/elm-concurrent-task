import * as esbuild from "esbuild";
import ElmPlugin from "esbuild-plugin-elm";
import start from "@es-exec/esbuild-plugin-start";

function main() {
  if (process.argv.includes("--ci")) {
    return build();
  } else {
    return watch();
  }
}

const options = {
  ENTRTY: "src/index.ts",
  OUTFILE: "dist/index.js",
};

function build() {
  esbuild.build({
    entryPoints: [options.ENTRTY],
    bundle: true,
    outfile: options.OUTFILE,
    platform: "node",
    plugins: [ElmPlugin({})],
  });
}

function watch() {
  esbuild
    .context({
      entryPoints: [options.ENTRTY],
      bundle: true,
      outfile: options.OUTFILE,
      platform: "node",
      plugins: [ElmPlugin({}), start({ script: "node ./dist/index.js" })],
    })
    .then((ctx) => ctx.watch());
}

main();
