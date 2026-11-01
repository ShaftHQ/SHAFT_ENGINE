---
name: appium-driver-npm-recover
description: Use when Appium npm null-package; reinstall from catalog.
---

# Appium driver npm null-package recover

## Rule

When `appium driver install` fails with npm's null-package / missing tarball
error: `appium driver uninstall <pkg>` then
`appium driver install --source=npm <pkg>@<ver>` at the pinned version. Do not
loop on the broken cache entry alone.

## Sources

Field retro 2026-10-31.
