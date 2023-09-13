const packageJson = require("../package.json");
const elmJson = require("../elm.json");

check();

function check() {
  if (packageJson.version === elmJson.version) {
    process.exit(0);
  } else {
    console.error("ERROR: elm and npm package versions do not match.");
    process.exit(1);
  }
}
