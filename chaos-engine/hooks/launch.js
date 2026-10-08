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

function guardFrom(start) {
  let root = start;
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

// #6632: a host may run the hook outside the repository (Copilot runs repo
// hooks from `/`); fall back to the project directory and the payload `cwd`.
function guardPath() {
  const found = guardFrom(currentRoot());
  if (found) return found;
  let payloadCwd = "";
  try {
    const event = JSON.parse(input.toString("utf8") || "{}");
    payloadCwd = typeof event.cwd === "string" ? event.cwd : "";
  } catch (error) {
    payloadCwd = "";
  }
  for (const hint of [process.env.GEMINI_PROJECT_DIR, process.env.CLAUDE_PROJECT_DIR, payloadCwd]) {
    if (!hint) continue;
    try {
      const candidate = guardFrom(path.resolve(hint));
      if (candidate) return candidate;
    } catch (error) {
      if (!unavailableError(error)) throw error;
    }
  }
  return null;
}

let input;
try {
  input = fs.readFileSync(0);
} catch (error) {
  if (unavailableError(error)) deny(CWD_UNAVAILABLE);
  throw error;
}

// Copilot registers one command per event and omits the event name from the
// payload. argv[3] is that event (preToolUse, sessionStart, ...).
function applyEventHint(raw) {
  const hinted = process.argv[3] || "";
  if (!hinted) return raw;
  let event = {};
  const text = raw.toString("utf8").trim();
  if (text) {
    try {
      event = JSON.parse(text);
    } catch (error) {
      if (unavailableError(error)) deny(CWD_UNAVAILABLE);
      return raw;
    }
  }
  if (!event || typeof event !== "object" || Array.isArray(event)) return raw;
  if (!event.hook_event_name && !event.hookEventName) {
    event.hook_event_name = hinted;
    return Buffer.from(JSON.stringify(event));
  }
  return raw;
}

input = applyEventHint(input);

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
    return matcher === null || new RegExp(`^(?:${matcher})$`, "i").test(toolName);
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

// #6199: the managed interpreter lives in the untracked hook-python pointer,
// never in tracked host files.
function pointerPython(guardFile) {
  const base = path.resolve(path.dirname(guardFile), "..", "..");
  try {
    const recorded = fs.readFileSync(path.join(base, ".chaos-engine-state/hook-python"), "utf8").trim();
    return recorded && fs.existsSync(recorded) ? [[recorded, []]] : [];
  } catch (error) {
    return [];
  }
}

const candidates = [
  ...pointerPython(guard),
  ...(process.platform === "win32"
    ? [["py", ["-3"]], ["python3", []], ["python", []]]
    : [["python3", []], ["python", []]]),
];
// #6632: skip interpreters that are missing, older than 3.11, or the Windows
// Store `python3`/`python` alias stub (it exits 9009 instead of running).
function usable(command, prefix) {
  const probe = spawnSync(command, [...prefix, "-c", "import sys;sys.exit(sys.version_info<(3,11))"], {
    stdio: "ignore",
    timeout: 10000,
  });
  return !probe.error && probe.status === 0;
}

for (const [command, prefix] of candidates) {
  if (!usable(command, prefix)) continue;
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
