#!/usr/bin/env node
"use strict";

const fs = require("fs");
const path = require("path");
const { spawnSync } = require("child_process");

const CWD_UNAVAILABLE =
  "repository working directory unavailable; restore the original mount or checkout, then retry";
const GUARD_UNAVAILABLE =
  "ChaosEngine guard unavailable; repair the original installation, then retry";
const CWD_UNAVAILABLE_CODES = new Set(["ENOENT", "ESTALE", "ENOTCONN"]);

function deny(message, code = 2) {
  process.stdout.write(JSON.stringify({ decision: "block", reason: message }) + "\n");
  process.exit(code);
}

function unavailableError(error) {
  return Boolean(error && CWD_UNAVAILABLE_CODES.has(error.code));
}

function currentRoot() {
  try {
    return process.cwd();
  } catch (error) {
    if (unavailableError(error)) deny(CWD_UNAVAILABLE);
    throw error;
  }
}

function guardPath() {
  let root = currentRoot();
  while (true) {
    for (const relative of [
      ".chaos-engine/hooks/guard.py",
      "plugins/chaos-engine/hooks/guard.py",
      "chaos-engine/hooks/guard.py",
    ]) {
      const candidate = path.join(root, relative);
      try {
        if (fs.existsSync(candidate)) return candidate;
      } catch (error) {
        if (unavailableError(error)) deny(CWD_UNAVAILABLE);
        throw error;
      }
    }
    const parent = path.dirname(root);
    if (parent === root) return null;
    root = parent;
  }
}

let input;
try {
  input = fs.readFileSync(0);
} catch (error) {
  if (unavailableError(error)) deny(CWD_UNAVAILABLE);
  throw error;
}

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
  } catch (error) {
    if (unavailableError(error)) deny(CWD_UNAVAILABLE);
    return true;
  }
}

if (!matchesHook()) {
  process.stdout.write("{}\n");
  process.exit(0);
}

const guard = guardPath();
if (!guard) {
  deny(GUARD_UNAVAILABLE);
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
  if (result.error && unavailableError(result.error)) deny(CWD_UNAVAILABLE);
  if (result.stdout) process.stdout.write(result.stdout);
  if (result.stderr) process.stderr.write(result.stderr);
  process.exit(result.status === null ? 1 : result.status);
}
deny(GUARD_UNAVAILABLE);
