---
name: self-improve
description: >
  ChaosEngine standalone self-improvement skill. Use at Learning Session /
  post-delivery, or when the user asks to observe harness friction or product
  enhancements. Captures privacy-safe lessons into learning.py (harness +
  product dual track). Also trigger on "self-improve", "task observer", or
  "one skill to rule them all" phrasing. Keep SessionStart cost to the locator
  line only — load references on demand.
---

# self-improve — ChaosEngine learning & adapting

Lean CE-native skill informed by Task Observer methodology
(Eoghan Henn / rebelytics, **CC BY 4.0** — see [LICENSE](LICENSE) and
[UPSTREAM.md](UPSTREAM.md)). Not a blind clone.

## When

- **Primary:** root-owned Learning Session after confirmed delivery.
- **Secondary:** explicit operator request mid-session.
- **Not:** every casual turn — keep always-on cost low.

## Dual track

1. **Harness** — skills, hooks, MemPalace, Graphify, Headroom, installer/doctor.
2. **Product** — enhancements for the product under development (queued issues).

Details: [references/observation-taxonomy.md](references/observation-taxonomy.md).
Activation: [references/activation.md](references/activation.md).
Adopt/reject research: [references/research-adopt-reject.md](references/research-adopt-reject.md).

## How (wraps learning.py)

1. Classify each finding (harness vs product; category allow-list).
2. Write minimal fields only: `category`, `title`, `lesson`, `proposedChange`,
   `benefit`, `estimatedTokens`.
3. Queue through `learning.py` so privacy gates + GitHub filing invariants hold.
4. Never auto-install skill patches; stage proposals for human/CI review.
5. "Nothing durable" is a valid outcome.

Example:

```bash
cat > /tmp/learning-candidate.json <<'EOF'
{
  "category": "tooling",
  "title": "Doctor headroom fix-next missing",
  "lesson": "Operators lacked a single install command after pin landed",
  "proposedChange": "Surface uv tool install pin in doctor fix-next",
  "benefit": "Faster Headroom provisioning on adopter hosts",
  "estimatedTokens": 120
}
EOF
python3 .chaos-engine/learning.py queue \
  --state .chaos-engine-state/learning \
  --upstream Owner/ExampleRepo \
  --candidate /tmp/learning-candidate.json
```

## Local smoke

```bash
# From a temp project with ChaosEngine installed, or the source tree:
python3 -c "from pathlib import Path; assert Path('chaos-engine/skills/self-improve/SKILL.md').is_file()"
# Queue one harness + one product candidate (privacy-safe fixtures) via learning.py
# then confirm queue.json grew by two items without secrets/paths.
```
