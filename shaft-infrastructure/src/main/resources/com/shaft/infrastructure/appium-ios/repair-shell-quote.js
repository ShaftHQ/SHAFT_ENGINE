#!/usr/bin/env node
/**
 * Replace every nested shell-quote copy under node_modules with the hoisted
 * shell-quote@>=1.11.0 install. npm overrides cannot rewrite packages that
 * arrive via bundleDependencies (appium-windows-driver bundles teen_process,
 * which still vendors shell-quote@1.10.0). See #7451 / GHSA-pqg4-j6r4-53mv.
 */
"use strict";

const fs = require("fs");
const path = require("path");

const src = path.join("node_modules", "shell-quote");
if (!fs.existsSync(src)) {
  process.exit(0);
}

const srcPkg = JSON.parse(fs.readFileSync(path.join(src, "package.json"), "utf8"));
const parts = String(srcPkg.version || "").split(".").map(Number);
if (parts.length < 3 || parts[0] < 1 || (parts[0] === 1 && parts[1] < 11)) {
  console.error(`hoisted shell-quote is ${srcPkg.version}; need >=1.11.0`);
  process.exit(1);
}

function walk(dir) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (!entry.isDirectory()) {
      continue;
    }
    if (entry.name === "shell-quote" && path.resolve(full) !== path.resolve(src)) {
      fs.rmSync(full, { recursive: true, force: true });
      fs.cpSync(src, full, { recursive: true });
      console.log(`repaired ${full} -> ${srcPkg.version}`);
      continue;
    }
    if (entry.name === ".bin") {
      continue;
    }
    walk(full);
  }
}

walk("node_modules");
