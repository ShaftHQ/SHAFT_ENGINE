# L95 CE-INSTALLER-SELF-PROVISION residuals

Follow-ups from adversarial review of the self-provision installer PR (PASS, non-blocking):

1. POSIX user JDK persist writes ChaosEngine user-data `env.sh` only — never
   rewrites `~/.profile` / `bashrc` / `zshrc` (parity with
   `publish_user_tool_paths`). Doctor/status may print a `source <env.sh>` hint.
2. JDK auto-provision path has a proven-red + success-path mock for
   `ensure_managed_temurin_jdk` (same strength as the managed build-tool test).
3. Dead `wrapper` soft-pass removed from `preflight_maven_tools_jdk`; CLI
   resolution stays in `resolve_maven_tools_build_cli`.
