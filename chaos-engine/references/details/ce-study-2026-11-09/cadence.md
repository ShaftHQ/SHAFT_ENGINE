# Monthly local trending pass

Continuous improvement stays on this cadence. It is not a new GitHub Actions schedule and not a cloud job.

- When: once a month, on this machine, started by a person.
- Where: this checkout. Local tools only (`gh` and the files in this repository).
- No cloud agent. No extra machine. No new GitHub schedule, workflow, or hosted cron.
- What: one note per distinct trending repository under `chaos-engine/references/details/`. Record reusable patterns, what to adopt, what to skip, and a license check from `gh repo view` or the repository `LICENSE` file, including when no license file exists.
- A missing or incompatible license blocks any skill edit and blocks vendoring.
- Adopt ideas. Never copy third-party code. Do not vendor a harness.
- Record the license check before any small skill edit. File a follow-up issue when the change would be a new product.
- The 2026-11-09 pass is the first record. The next pass is the following month on this machine.
- A deep pass uses [learn-from-external-harnesses](../../../skills/learn-from-external-harnesses/SKILL.md). A one-note skim is not that pass.

The identity principle that points here is in [identity.md](../../../identity.md).
