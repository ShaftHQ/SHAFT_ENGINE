#!/usr/bin/env node
"use strict";

const fs = require("fs");
const path = require("path");
const { spawnSync } = require("child_process");

function guardPath() {
  let root = process.cwd();
  while (true) {
    for (const relative of [
      ".chaos-engine/hooks/guard.py",
      "plugins/chaos-engine/hooks/guard.py",
    ]) {
      const candidate = path.join(root, relative);
      if (fs.existsSync(candidate)) return candidate;
    }
    const parent = path.dirname(root);
    if (parent === root) return null;
    root = parent;
  }
}

const guard = guardPath();
if (!guard) {
  process.stdout.write("{}\n");
  process.exit(0);
}

const input = fs.readFileSync(0);
const candidates = process.platform === "win32"
  ? [["py", ["-3"]], ["python3", []], ["python", []]]
  : [["python3", []], ["python", []]];
for (const [command, prefix] of candidates) {
  const result = spawnSync(command, [...prefix, guard], {
    input,
    env: { ...process.env, CHAOS_ENGINE_HOST: process.argv[2] || "unknown" },
    encoding: "buffer",
  });
  if (result.error && result.error.code === "ENOENT") continue;
  if (result.stdout) process.stdout.write(result.stdout);
  if (result.stderr) process.stderr.write(result.stderr);
  process.exit(result.status === null ? 1 : result.status);
}
process.stdout.write("{}\n");
