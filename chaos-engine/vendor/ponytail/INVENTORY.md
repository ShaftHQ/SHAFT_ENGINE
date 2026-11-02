# Ponytail vendor pin

Owned inventory for the pinned DietrichGebert/ponytail companion. The skill
body stays verbatim upstream. ChaosEngine applies a project-mode overlay on
`hooks/ponytail-config.js`: when a ChaosEngine router exists in the walk from
the process cwd, mode resolution never falls through to user-level ponytail
config. That overlay returns `ultra`, the highest runtime level. `review`
stays session-only. `DEFAULT_MODE` stays `full` outside a ChaosEngine project.
Re-apply that overlay after any upstream pin refresh.

- Pin: [PIN.json](PIN.json)
- License: [LICENSE](LICENSE)
- Skill: [skills/ponytail/SKILL.md](skills/ponytail/SKILL.md)
- [hooks/ponytail-activate.js](hooks/ponytail-activate.js)
- [hooks/ponytail-config.js](hooks/ponytail-config.js)
- [hooks/ponytail-instructions.js](hooks/ponytail-instructions.js)
- [hooks/ponytail-mode-tracker.js](hooks/ponytail-mode-tracker.js)
- [hooks/ponytail-runtime.js](hooks/ponytail-runtime.js)
- [hooks/ponytail-subagent.js](hooks/ponytail-subagent.js)
- [hooks/claude-codex-hooks.json](hooks/claude-codex-hooks.json)

Refresh by replacing the pin and the listed blobs together, then re-apply the
ChaosEngine project-mode overlay and update the digest in `PIN.json`.
