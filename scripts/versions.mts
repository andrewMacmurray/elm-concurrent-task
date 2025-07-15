import packageJson from "../package.json" with { type: "json" }
import elmJson from "../elm.json" with { type: "json" }

check();

function check() {
  if (packageJson.version === elmJson.version) {
    console.log("Versions match ✅");
    process.exit(0);
  } else {
    console.error("ERROR: elm and npm package versions do not match.");
    process.exit(1);
  }
}
