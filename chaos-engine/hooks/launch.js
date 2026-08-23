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
      "chaos-engine/hooks/guard.py",
    ]) {
      const candidate = path.join(root, relative);
      if (fs.existsSync(candidate)) return candidate;
    }
    const parent = path.dirname(root);
    if (parent === root) return null;
    root = parent;
  }
}

const input = fs.readFileSync(0);
function matchesHook() {
  try {
    const event = JSON.parse(input.toString("utf8"));
    const policy = JSON.parse(fs.readFileSync(path.join(__dirname, "matchers.json"), "utf8"));
    const preventive = policy.preventive.join("|");
    const observational = policy.observational.join("|");
    const eventName = event.hook_event_name || event.hookEventName || "";
    const toolName = event.tool_name || event.toolName || "";
    const matcher = ["PreToolUse", "preToolUse", "BeforeTool"].includes(eventName)
      ? preventive
      : ["PostToolUse", "postToolUse", "PostToolUseFailure", "postToolUseFailure", "AfterTool"].includes(eventName)
        ? observational
        : null;
    return matcher === null || new RegExp(`^(?:${matcher})$`).test(toolName);
  } catch (_) {
    return true;
  }
}

if (!matchesHook()) {
  process.stdout.write("{}\n");
  process.exit(0);
}

const guard = guardPath();
if (!guard) {
  process.stdout.write("{}\n");
  process.exit(0);
}

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
