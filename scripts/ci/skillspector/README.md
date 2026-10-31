# SkillSpector gate

NVIDIA [SkillSpector](https://github.com/NVIDIA/SkillSpector) static scans
(`--no-llm`) of first-party ChaosEngine skills.

| File | Role |
|------|------|
| `PIN.json` | Pinned SkillSpector commit + gate thresholds |
| `.skillspector-baseline.yaml` | Accepted findings with audit reasons |

## Gate policy

- Fail when **risk score > 50**, or any **unsuppressed HIGH/CRITICAL** finding.
- CI: `.github/workflows/skillspector.yml` (repo root)
- Local: `scripts/ci/overlay_pre_push.py` calls `scripts/ci/skillspector_gate.py` when skill paths change (warns if scanner missing; set `CHAOS_SKILLSPECTOR_REQUIRED=1` to fail).

## Refreshing the baseline

After an intentional skill change that alters accepted fingerprints:

1. Install the pinned scanner (`PIN.json`).
2. Re-scan the skill and triage every finding (FP / accepted agency / real fix).
3. Regenerate fingerprints with `skillspector baseline <skill-dir> --no-llm` and merge reasons into `.skillspector-baseline.yaml`.
4. Never add a broad `AST4` glob that would hide new malicious scripts.
