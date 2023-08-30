import crypto from "node:crypto";

export function tasks() {
  return {
    "uuid:generate": () => crypto.randomUUID(),
  };
}
