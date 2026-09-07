# Headroom vendor pin

Owned inventory for the managed `headroom-ai` companion. Upstream package bytes
are **not** copied into this tree (too large); CE pins provenance + ships a
CE-authored skill and policy module.

- Pin: [PIN.json](PIN.json)
- License: [LICENSE](LICENSE) (Apache-2.0)
- Skill: [skills/headroom/SKILL.md](skills/headroom/SKILL.md)
- Policy: [`../../headroom_policy.py`](../../headroom_policy.py)
- Docs: [`../../references/headroom.md`](../../references/headroom.md)

Refresh by bumping `version` / digests / commit in PIN.json together with
THIRD_PARTY_NOTICES and installer/doctor references.
