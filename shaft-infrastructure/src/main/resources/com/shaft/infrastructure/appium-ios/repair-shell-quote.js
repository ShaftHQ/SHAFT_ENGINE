#!/usr/bin/env node
/**
 * Replace nested bundled copies that npm overrides cannot rewrite.
 *
 * bundleDependencies (appium drivers) ignore overrides. shell-quote below
 * 1.11.0, glob below 14.0.2, minimatch below 11.0.1, and follow-redirects
 * below 1.16.2 are copied from the hoisted patched install at postinstall.
 * Glob is repaired before minimatch so replacing a nested glob tree cannot
 * leave a vulnerable minimatch behind inside it. See #7451 and the appium
 * Dependabot alerts for the nested copies.
 *
 * Fails (exit 1) when a vulnerable nested copy remains and the hoisted
 * install is missing or itself below the floor. No node_modules is success.
 */
"use strict";

const fs = require("fs");
const path = require("path");

const TARGETS = [
  { name: "glob", min: [14, 0, 2], label: ">=14.0.2" },
  { name: "minimatch", min: [11, 0, 1], label: ">=11.0.1" },
  { name: "follow-redirects", min: [1, 16, 2], label: ">=1.16.2" },
  { name: "shell-quote", min: [1, 11, 0], label: ">=1.11.0" },
];

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

function belowMin(version, min) {
  const parts = parseSemver(version);
  if (parts === null) {
    return true;
  }
  for (let i = 0; i < 3; i++) {
    if (parts[i] !== min[i]) {
      return parts[i] < min[i];
    }
  }
  return false;
}

function collectDirs(packageName, dir, out) {
  if (!fs.existsSync(dir)) {
    return;
  }
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (!entry.isDirectory() || entry.name === ".bin") {
      continue;
    }
    if (entry.name === packageName) {
      out.push(full);
      continue;
    }
    collectDirs(packageName, full, out);
  }
}

function repairPackage(spec) {
  const copies = [];
  collectDirs(spec.name, "node_modules", copies);
  const vulnerable = copies.filter((dir) => belowMin(readVersion(dir) || "", spec.min));
  if (vulnerable.length === 0) {
    return;
  }
  const hoisted = path.join("node_modules", spec.name);
  const prefix = spec.name === "shell-quote" ? "repair-shell-quote" : "repair-bundled";
  if (!fs.existsSync(hoisted)) {
    console.error(
      `${prefix}: node_modules/${spec.name} is missing but nested copies below ${spec.label} remain:`
    );
    for (const dir of vulnerable) {
      console.error(`  ${dir}@${readVersion(dir)}`);
    }
    process.exit(1);
  }
  const srcVersion = readVersion(hoisted);
  if (belowMin(srcVersion || "", spec.min)) {
    console.error(`hoisted ${spec.name} is ${srcVersion}; need ${spec.label}`);
    process.exit(1);
  }
  for (const full of vulnerable) {
    if (path.resolve(full) === path.resolve(hoisted)) {
      continue;
    }
    fs.rmSync(full, { recursive: true, force: true });
    fs.cpSync(hoisted, full, { recursive: true });
    console.log(`repaired ${full} -> ${srcVersion}`);
  }
  const remaining = [];
  collectDirs(spec.name, "node_modules", remaining);
  const stillBad = remaining.filter((dir) => belowMin(readVersion(dir) || "", spec.min));
  if (stillBad.length > 0) {
    console.error(`${prefix}: ${spec.name} below ${spec.label} remains after repair:`);
    for (const dir of stillBad) {
      console.error(`  ${dir}@${readVersion(dir)}`);
    }
    process.exit(1);
  }
}

for (const spec of TARGETS) {
  repairPackage(spec);
}
