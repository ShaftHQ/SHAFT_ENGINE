---
name: shaft-mobile-actions
description: Use when implementing SHAFT native or mobile-web sessions, context switches, taps, gestures, rotation, keyboard, or app lifecycle behavior.
---

# SHAFT Mobile Actions

Start with [SHAFT Developer](../shaft-developer/SKILL.md), then read and follow
[the mobile-actions playbook](references/playbook.md).

Tap with `driver.touch().tap(locator)`. Coordinate taps are forbidden. File silent no-ops upstream.
On iOS XCUITest, `hittable=false` means the tap did not land.
