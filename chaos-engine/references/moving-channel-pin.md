# Moving-channel version pin policy

## Rule

1. **Immutable artifacts** (git commit sha, tarball digest, release asset):
   pin **exact**.
2. **Moving vendor channels** (SDK emulator packages, browser channels, npm
   dist-tags that float): pin as a **minimum within the same major**
   (`installed >= pinned` and `major(installed) == major(pinned)`), **or**
   resolve the current channel version at plan time and record it in the
   receipt. Do not require exact equality against a channel that keeps
   publishing patches.
3. Add a nightly / scheduled **drift check** that reports when the channel
   tip moved past the pinned minimum so the pin can be bumped deliberately.

Helper: `chaos-engine/moving_channel_pin.channel_pin_allows(installed, pinned, mode="min"|"exact")`.

## Why

Exact pin `emulator@37.2.12` broke CI when the stable channel served
`37.2.13` (managed-android pin drift).

## Check

- `mode=min` allows newer patch/minor within major; rejects other majors.
- `mode=exact` requires equality.

## Follow-up

[ci-status-economy](ci-status-economy.md), infrastructure pin tests.
