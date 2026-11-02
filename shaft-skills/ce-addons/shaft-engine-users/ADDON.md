# SHAFT engine users add-on

Optional ChaosEngine add-on for people who use SHAFT in their own test
projects. It is never installed by default.

- Install: `--with-shaft-engine-users` (PowerShell `-WithShaftEngineUsers`,
  or `CHAOS_ENGINE_ADDONS=shaft-engine-users`).
- Remove: `--without-shaft-engine-users`.
- Installed at `.chaos-engine/addons/shaft-engine-users/`.
- Entry point: `shaft-developer/SKILL.md` (repository: `shaft-skills/shaft-developer/`;
  installed: `.chaos-engine/addons/shaft-engine-users/shaft-developer/SKILL.md`).
  It routes to exactly one SHAFT specialist per task.

The skills are the same files the SHAFT agent plugin publishes, included in
place from `shaft-skills/`. Nothing is copied, so the two never drift. SHAFT
contributors also want `shaft-core-developers`, which requires this add-on.
