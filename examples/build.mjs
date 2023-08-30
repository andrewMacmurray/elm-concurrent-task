import * as esbuild from "esbuild";
import ElmPlugin from "esbuild-plugin-elm";
import start from "@es-exec/esbuild-plugin-start";

const target = process.argv[2];

esbuild
  .context({
    entryPoints: [`src/${target}.ts`],
    bundle: true,
    outfile: `dist/${target}.js`,
    platform: "node",
    plugins: [ElmPlugin({}), start({ script: `node ./dist/${target}.js` })],
  })
  .then((ctx) => ctx.watch());
