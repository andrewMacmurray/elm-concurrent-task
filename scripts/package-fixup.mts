import fs from "node:fs/promises";

// Adds package.json files to cjs/mjs subtrees

const writePackageJson = (path: string, contents: Record<string, string>) => {
  return fs.writeFile(`${path}/package.json`, JSON.stringify(contents, null, 2));
};

Promise.all([
  writePackageJson("lib/cjs", { type: "commonjs" }),
  writePackageJson("lib/mjs", { type: "module" }),
]).then(() => {
  console.log("Wrote sub package JSONs for compiled runners ✅");
});
