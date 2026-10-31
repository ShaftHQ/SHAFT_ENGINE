---
name: appium-driver-npm-recover
description: Use when Appium driver install hits npm's null-package error; uninstall then reinstall from npm at the pinned version.
---

# Appium driver npm null-package recover

## Rule

When `appium driver install` fails with npm's null-package / missing tarball
error: `appium driver uninstall <pkg>` then
`appium driver install --source=npm <pkg>@<ver>` at the pinned version. Do not
loop on the broken cache entry alone.

## Sources

Field retro 2026-10-31.
