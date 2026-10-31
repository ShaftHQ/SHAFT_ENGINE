# CE 2026.10.31.7 — WoW + memory dump + L66–L77

## Scope

- `chaos-engine/` (gates, references, lessons, VERSION)
- `scripts/ci/` (`file_claim_lock.py`, overlay_pre_push wiring)
- `tests/scripts/` (contracts + proven-red)

## Cards

| Id | Card | Module |
| --- | --- | --- |
| WoW | [ways-of-working](../references/details/ce-rules-2026-10-31-7/ways-of-working.md) | `scripts/ci/file_claim_lock.py` |
| Memory | [memory-full-dump](../references/details/ce-rules-2026-10-31-7/memory-full-dump.md) | [memory_full_dump.py](../rules/memory_full_dump.py) |
| L65-fu+L73 | design lesson `motion-graphics-gate` | [motion_graphics_gate.py](../addons/design-skills/scripts/motion_graphics_gate.py) |
| L66 | [web-ui-gate](../references/details/ce-rules-2026-10-31-7/web-ui-gate.md) | [web_ui_gate.py](../rules/web_ui_gate.py) |
| L67 | [l67-reject-to-rule](../references/details/ce-rules-2026-10-31-7/l67-reject-to-rule.md) | (process) |
| L70 | [coach-pre-publish](../references/details/ce-rules-2026-10-31-7/coach-pre-publish.md) | [coach_pre_publish.py](../rules/coach_pre_publish.py) |
| L71 | [avatar-likeness-gate](../references/details/ce-rules-2026-10-31-7/avatar-likeness-gate.md) | [avatar_likeness_gate.py](../rules/avatar_likeness_gate.py) |
| L72 | [l72-design-fix-loop](../references/details/ce-rules-2026-10-31-7/l72-design-fix-loop.md) | [design_fix_loop.py](../rules/design_fix_loop.py) |
| L74 | [gate-proven-red](../references/details/ce-rules-2026-10-31-7/gate-proven-red.md) | [gate_proven_red.py](../rules/gate_proven_red.py) |
| L75 | [no-probe-live](../references/details/ce-rules-2026-10-31-7/no-probe-live.md) | [no_probe_live.py](../rules/no_probe_live.py) |
| L76 | [preview-at-real-width](../references/details/ce-rules-2026-10-31-7/preview-at-real-width.md) | [preview_at_real_width.py](../rules/preview_at_real_width.py) |
| L77 | [no-token-in-remote](../references/details/ce-rules-2026-10-31-7/no-token-in-remote.md) | [remote_credential_guard.py](../rules/remote_credential_guard.py) |

Coach abstract (24 new) + Carl generic folded into WoW / L70 / L71 / L75; skip dup of video sign-off already in PR 7638.

## Remotes (identity)

ChaosEngine identity only for ShaftHQ/SHAFT_ENGINE + ShaftHQ/shafthq.github.io. Portable cards stay free of personal email.
