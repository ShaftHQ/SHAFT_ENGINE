# L94 INSTALLER-SELF-PROVISION

Installer auto-provisions missing JDK (managed Temurin, user-level, no admin),
build tool (`mvnw` or managed Apache distribution), `JAVA_HOME`, and `PATH` for
the current session and the user environment on Windows/macOS/Linux.

Fail only when auto-provision is truly unfixable (download/verify impossible).

## Failure UX (exactly three user-facing items)

1. `Installation failed`
2. Prefill GitHub new-issue URL (diagnostics redacted, length-capped; details
   also in `.chaos-engine-state/heal-handoff.md`)
3. `Launch your agent with this prompt:` copy-paste block that tells an agent
   to fix the install and open an upstream issue/PR in the ChaosEngine
   upstream repository (`chaos-engine/` tree)

Diagnostics (cause, fix-next, filesystem, status/doctor) stay in the handoff
and issue form — never dumped as extra stderr noise to the user.

## L95 residuals

POSIX user env persists via ChaosEngine user-data `env.sh` only (no shell-profile
rewrite). See [installer-posix-env](../ce-rules-2026-10-31-12/installer-posix-env.md).
