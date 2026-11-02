#!/usr/bin/env node
/**
 * Replace every nested shell-quote copy under node_modules with the hoisted
 * shell-quote@>=1.11.0 install. npm overrides cannot rewrite packages that
 * arrive via bundleDependencies (appium-windows-driver bundles teen_process,
 * which still vendors shell-quote@1.10.0). See #7451 / GHSA-pqg4-j6r4-53mv.
 *
 * Fails (exit 1) when any shell-quote tree remains below 1.11.0, including
 * inBundle copies. A missing hoisted install is only OK when no nested copy
 * below 1.11.0 exists.
 */
"use strict";

const fs = require("fs");
const path = require("path");

const MIN = [1, 11, 0];
const HOISTED = path.join("node_modules", "shell-quote");

function readVersion(pkgDir) {
  const pkgPath = path.join(pkgDir, "package.json");
  if (!fs.existsSync(pkgPath)) {
    return null;
  }
  const pkg = JSON.parse(fs.readFileSync(pkgPath, "utf8"));
  return String(pkg.version || "");
}

function parseSemver(version) {
  const parts = String(version || "").split(".").map((p) => Number(p));
  if (parts.length < 3 || parts.some((n) => Number.isNaN(n))) {
    return null;
  }
  return parts.slice(0, 3);
}

function belowMin(version) {
  const parts = parseSemver(version);
  if (parts === null) {
    return true;
  }
  for (let i = 0; i < 3; i++) {
    if (parts[i] !== MIN[i]) {
      return parts[i] < MIN[i];
    }
  }
  return false;
}

function collectShellQuoteDirs(dir, out) {
  if (!fs.existsSync(dir)) {
    return;
  }
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (!entry.isDirectory()) {
      continue;
    }
    if (entry.name === ".bin") {
      continue;
    }
    if (entry.name === "shell-quote") {
      out.push(full);
      continue;
    }
    collectShellQuoteDirs(full, out);
  }
}

const copies = [];
collectShellQuoteDirs("node_modules", copies);
const vulnerable = copies.filter((p) => belowMin(readVersion(p) || ""));

if (vulnerable.length === 0) {
  process.exit(0);
}

if (!fs.existsSync(HOISTED)) {
  console.error(
    "repair-shell-quote: node_modules/shell-quote is missing but nested copies below 1.11.0 remain:"
  );
  for (const p of vulnerable) {
    console.error(`  ${p}@${readVersion(p)}`);
  }
  process.exit(1);
}

const srcVersion = readVersion(HOISTED);
if (belowMin(srcVersion || "")) {
  console.error(`hoisted shell-quote is ${srcVersion}; need >=1.11.0`);
  process.exit(1);
}

for (const full of copies) {
  if (path.resolve(full) === path.resolve(HOISTED)) {
    continue;
  }
  fs.rmSync(full, { recursive: true, force: true });
  fs.cpSync(HOISTED, full, { recursive: true });
  console.log(`repaired ${full} -> ${srcVersion}`);
}

const remaining = [];
collectShellQuoteDirs("node_modules", remaining);
const stillBad = remaining.filter((p) => belowMin(readVersion(p) || ""));
if (stillBad.length > 0) {
  console.error("repair-shell-quote: shell-quote below 1.11.0 remains after repair:");
  for (const p of stillBad) {
    console.error(`  ${p}@${readVersion(p)}`);
  }
  process.exit(1);
}
