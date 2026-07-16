#!/usr/bin/env python3
"""Deterministic PreToolUse / SessionStart guardrail hook for SHAFT_ENGINE."""
# Note on docstring style: Codacy runs both D212 and D213, so multi-line
# docstrings always flag one of them; keep docstrings single-line and put
# detail in comments like this block.
#
# Enforces (via code, not prose) the four hard safety rules from AGENTS.md /
# .memory gotchas that previously only existed as instructions an agent could
# forget or misread:
#
#   R1 Maven test scoping + headless execution
#      (mirrors .memory/memory/gotchas/
#       mvn-test-must-force-headlessexecution-true-and-never-invoke-allure-serve-report-open.md
#       and .memory/memory/gotchas/
#       unscoped-am-mvn-test-can-crash-the-jvm-across-the-whole-reactor.md --
#       both repo-tracked so they travel with every clone/worktree)
#   R2 Never auto-open/serve Allure reports
#   R3 Never run GUI-opening commands on Windows (AGENTS.md Windows/Codex Safety)
#   R4 (documentation only, not mechanically enforceable here) one
#      branch/worktree/PR per session -- reported by --session-start as a
#      reminder since a PreToolUse hook has no cross-tool-call session state.
#   R5 graphify pre-check nudge -- non-blocking. After several Read/Grep
#      calls in a session with no sign the graphify repository-map cache
#      (tools/repository-map/resolve_graph_out.py) was consulted, emit ONE
#      PreToolUse "allow" + additionalContext reminder. Never denies --
#      getting the "broad exploration" heuristic wrong should be a mild
#      nudge, not friction on legitimate targeted lookups. See #3611.
#
# Stdlib only. Must run under both `py -3` (Windows launcher, this repo's
# documented convention) and `python3` (Linux/CI/macOS), so avoid anything
# Windows-launcher-specific or POSIX-only.
#
# --------------------------------------------------------------------------
# Hook protocol used (verified against the current Claude Code hooks docs at
# https://docs.anthropic.com/en/docs/claude-code/hooks via the claude-code-guide
# agent on 2026-07-07 -- NOT assumed from training-data memory):
#
# PreToolUse:
#   * Input: one JSON object on stdin. Relevant fields: "tool_name" (e.g.
#     "Bash") and "tool_input" (an object; for the Bash tool the shell command
#     string is at tool_input["command"]).
#   * ALLOW: exit 0 with no stdout output (or stdout that isn't the deny JSON
#     below) -- Claude Code falls through to its normal permission flow.
#   * DENY (the mechanism this script uses): exit 0 and print a single JSON
#     object to stdout of the form
#       {"hookSpecificOutput": {"hookEventName": "PreToolUse",
#                                "permissionDecision": "deny",
#                                "permissionDecisionReason": "<message>"}}
#     This is the CURRENT documented mechanism (permissionDecision one of
#     "allow" | "deny" | "ask" | "defer"). The legacy "exit code 2 + stderr"
#     mechanism still works per the docs but is not what this script uses,
#     since the JSON form is the documented current/recommended approach and
#     lets us attach a precise, structured reason string.
#
# SessionStart:
#   * Input: JSON on stdin with "hook_event_name": "SessionStart" (plus
#     session_id/cwd/source, unused here).
#   * Output: this repo's ask (task spec) wants a fast, dependency-free,
#     human-readable 4-line reminder. The documented mechanism for feeding
#     context back to Claude is
#       {"hookSpecificOutput": {"hookEventName": "SessionStart",
#                                "additionalContext": "<plain string>"}}
#     on stdout with exit 0. additionalContext is a plain string (not nested
#     JSON) that Claude Code inserts directly into the model's context. This
#     script emits that JSON envelope so the reminder actually reaches Claude
#     context (plain stdout text with no JSON envelope is not documented to be
#     read by Claude Code for SessionStart), while keeping the human-readable
#     4-line reminder as the literal value of additionalContext.
#
# settings.json wiring (as actually configured, not assumed):
#   * PreToolUse matcher "Bash|PowerShell" -- this harness exposes Bash and
#     PowerShell as distinct tool names, so both are matched explicitly and
#     `run_pretooluse` below checks `tool_name in ("Bash", "PowerShell")`.
#   * ${CLAUDE_PROJECT_DIR} is expanded by Claude Code itself before the
#     process is spawned (confirmed via the claude-code-guide agent against
#     the current hooks docs), and is documented as reliable on Windows. To
#     avoid any shell-specific quoting differences (Windows hook commands may
#     be spawned via Git Bash or PowerShell depending on environment), this
#     repo's settings.json wires the hook using the exec-style
#     "command" + "args" array form (command: "python3", args:
#     ["${CLAUDE_PROJECT_DIR}/.claude/hooks/guard.py"]) rather than a single
#     shell-parsed command string, so there is no shell in the loop to
#     reinterpret quoting/backslashes on either platform. `python3` resolves
#     on this machine (WindowsApps shim); `py -3` remains this repo's
#     documented convention for commands run directly by an agent
#     (AGENTS.md Windows/Codex Safety: "Run via py -3, node, ...").
# --------------------------------------------------------------------------

from __future__ import annotations

import json
import os
import re
import sys
import tempfile

# ---------------------------------------------------------------------------
# R1: Maven test scoping + headless execution
# ---------------------------------------------------------------------------

_MVN_TEST_GOALS = (
    "test",
    "verify",
    "install",
    "deploy",
    "package",
    "surefire:test",
    "failsafe:integration-test",
)

_SKIP_TESTS_RE = re.compile(r"-DskipTests\b|-Dmaven\.test\.skip=true\b", re.IGNORECASE)
_DTEST_RE = re.compile(r"-Dtest=", re.IGNORECASE)
_AM_RE = re.compile(r"(?<![\w-])(?:-am|--also-make)(?![\w-])", re.IGNORECASE)
_PL_RE = re.compile(r"(?<![\w-])(?:-pl|--projects)(?![\w-])", re.IGNORECASE)
_HEADLESS_TRUE_RE = re.compile(r"-DheadlessExecution=true\b", re.IGNORECASE)

# R1/R2 must match the actual command head, not command-looking text quoted as
# DATA (a `gh pr create --body` describing a Maven run, a commit message, a
# heredoc). Multi-line string bodies are stripped before segmentation, and the
# mvn/allure token must then be the executable at the start of its command
# segment (allowing env-var assignments and common runner/wrapper prefixes).

# Bash/POSIX heredoc bodies: <<EOF ... EOF (optionally quoted/indented tag).
_BASH_HEREDOC_RE = re.compile(
    r"<<-?\s*([\"']?)(\w+)\1.*?(?:\r?\n)\s*\2(?=\s|$)", re.DOTALL
)
# PowerShell here-strings: @' ... '@ and @" ... "@ (bodies are data).
_PS_HERE_STRING_RE = re.compile(r"@(['\"])\r?\n.*?\r?\n\1@", re.DOTALL)
# Quoted strings that span multiple lines are data (PR/commit bodies); quoted
# single-line tokens like '-Dtest=Foo' are real arguments and must survive.
_MULTILINE_DQUOTE_RE = re.compile(r'"(?:[^"\\]|\\.)*?\n(?:[^"\\]|\\.)*?"', re.DOTALL)
_MULTILINE_SQUOTE_RE = re.compile(r"'[^']*?\n[^']*?'", re.DOTALL)
# Line continuations keep one logical command in one segment.
_LINE_CONTINUATION_RE = re.compile(r"(?:\\|`)\r?\n")

# Tokens that may legitimately precede the real executable in a segment.
_RUNNER_PREFIX_TOKENS = frozenset(
    {"time", "nohup", "nice", "xvfb-run", "npx", "pnpm", "yarn", "dlx", "exec"}
)
_ENV_ASSIGNMENT_RE = re.compile(r"^[A-Za-z_]\w*=\S*$")


def _sanitize_for_command_head(command: str) -> str:
    """Strip data-only string bodies and join continuation lines."""
    sanitized = _LINE_CONTINUATION_RE.sub(" ", command)
    sanitized = _BASH_HEREDOC_RE.sub(" ", sanitized)
    sanitized = _PS_HERE_STRING_RE.sub(" ", sanitized)
    sanitized = _MULTILINE_DQUOTE_RE.sub(" ", sanitized)
    sanitized = _MULTILINE_SQUOTE_RE.sub(" ", sanitized)
    return sanitized


def _command_segments(command: str) -> list[str]:
    """Split into command segments (separators plus real newlines)."""
    return re.split(r"(?:;|&&|\|\||\||&|\r?\n)", command)


def _segment_tokens(segment: str) -> list[str]:
    stripped = segment.strip()
    stripped = re.sub(r"^&\s*", "", stripped)  # PowerShell call operator
    return stripped.split()


def _head_executable_matches(segment: str, names: frozenset[str]) -> bool:
    """True when the segment's executable token (basename) is one of `names`."""
    tokens = _segment_tokens(segment)
    index = 0
    while index < len(tokens):
        token = tokens[index]
        if _ENV_ASSIGNMENT_RE.match(token):
            index += 1
            continue
        basename = re.split(r"[/\\]", token.strip("\"'"))[-1].lower()
        if basename in names:
            return True
        if basename in _RUNNER_PREFIX_TOKENS:
            index += 1
            # `timeout 60 mvn ...`-style: skip a numeric argument after a runner
            if index < len(tokens) and re.match(r"^\d+[smhd]?$", tokens[index]):
                index += 1
            continue
        if basename == "timeout":
            index += 1
            if index < len(tokens) and re.match(r"^\d+[smhd]?$", tokens[index]):
                index += 1
            continue
        return False
    return False


_MVN_NAMES = frozenset({"mvn", "mvn.cmd", "mvn.bat"})
_ALLURE_NAMES = frozenset({"allure", "allure.cmd", "allure.bat"})


def _mvn_segments(command: str) -> list[str]:
    """Segments whose executable is mvn (command position, not quoted prose)."""
    return [
        segment
        for segment in _command_segments(_sanitize_for_command_head(command))
        if _head_executable_matches(segment, _MVN_NAMES)
    ]


def _segment_has_test_goal(segment: str) -> bool:
    for goal in _MVN_TEST_GOALS:
        # word-boundary aware match for the goal token (handles "surefire:test" too)
        pattern = re.compile(r"(?<![\w:.\-])" + re.escape(goal) + r"(?![\w:.\-])", re.IGNORECASE)
        if pattern.search(segment):
            return True
    return False


def check_r1_maven(command: str) -> str | None:
    """Return a block reason string, or None if R1 does not apply / is satisfied."""
    for segment in _mvn_segments(command):
        if not _segment_has_test_goal(segment):
            continue
        if _SKIP_TESTS_RE.search(segment):
            continue  # tests are skipped entirely -- rule does not apply

        has_dtest = bool(_DTEST_RE.search(segment))
        has_am = bool(_AM_RE.search(segment))
        has_pl = bool(_PL_RE.search(segment))

        if not has_dtest and (has_am or not has_pl):
            return (
                "R1 (Maven test scoping): this command runs a Maven test-executing "
                "goal/phase without -Dtest= scoping, and either uses -am/--also-make "
                "or has no -pl/--projects scoping at all. Running an unscoped/-am "
                "test phase across the whole reactor has previously crashed the JVM "
                "(EXCEPTION_ACCESS_VIOLATION) by pulling in every upstream module's "
                "test suite (see .memory scoped-test-execution-policy). Scope the "
                "run with -Dtest=<SpecificClass>, or use -pl <module> WITHOUT -am "
                "(compile/install the dependency once separately if needed)."
            )

        if not _HEADLESS_TRUE_RE.search(segment):
            return (
                "R1 (headless execution): this command runs Maven tests that can "
                "reach SHAFT-driver-based browser tests but does not pass "
                "-DheadlessExecution=true. Browser-capable test runs must force "
                "headless execution to avoid launching a real, unprompted browser "
                "window on the user's own machine (see .memory gotcha "
                "mvn-test-must-force-headlessexecution-true...). Add "
                "-DheadlessExecution=true."
            )

    return None


# ---------------------------------------------------------------------------
# R2: Allure must never be auto-served/opened
# ---------------------------------------------------------------------------

_ALLURE_SERVE_RE = re.compile(r"^\s*(serve|open)(?![\w-])", re.IGNORECASE)
_ALLURE_MVN_SERVE_RE = re.compile(r"(?<![\w:.\-])allure:serve(?![\w:.\-])", re.IGNORECASE)


def _segment_runs_allure_serve(segment: str) -> bool:
    tokens = _segment_tokens(segment)
    for index, token in enumerate(tokens):
        basename = re.split(r"[/\\]", token.strip("\"'"))[-1].lower()
        if basename in _ALLURE_NAMES:
            prefixes = tokens[:index]
            prefix_ok = all(
                _ENV_ASSIGNMENT_RE.match(prefix)
                or re.split(r"[/\\]", prefix.strip("\"'"))[-1].lower() in _RUNNER_PREFIX_TOKENS
                for prefix in prefixes
            )
            rest = " ".join(tokens[index + 1:])
            return prefix_ok and bool(_ALLURE_SERVE_RE.match(rest))
    return False


def check_r2_allure(command: str) -> str | None:
    sanitized = _sanitize_for_command_head(command)
    serve_in_command_position = any(
        _segment_runs_allure_serve(segment) for segment in _command_segments(sanitized)
    )
    mvn_allure_serve = any(
        _ALLURE_MVN_SERVE_RE.search(segment) for segment in _mvn_segments(command)
    )
    if serve_in_command_position or mvn_allure_serve:
        return (
            "R2 (Allure auto-open): this command runs 'allure serve', "
            "'allure open', or the Maven 'allure:serve' goal. Never auto-open "
            "or serve Allure reports -- generate reports (e.g. `allure "
            "generate`) and leave them under target/allure-results / "
            "allure-report for the user to open explicitly."
        )
    return None


# ---------------------------------------------------------------------------
# R3: GUI-opening verbs (Windows safety, AGENTS.md)
# ---------------------------------------------------------------------------

# Verbs that are unsafe as substrings-of-a-larger-identifier but safe as a
# clearly delimited word/command token. Word-boundary-based, case-insensitive.
_GUI_WORD_VERBS = (
    r"Start-Process",
    r"Invoke-Item",
    r"rundll32",
    r"os\.startfile",
    r"explorer",
)
_GUI_WORD_RE = re.compile(
    r"(?<![\w.\-])(?:" + "|".join(_GUI_WORD_VERBS) + r")(?![\w.\-])", re.IGNORECASE
)

# `ii` only counts as the GUI-open PowerShell alias when it stands alone as a
# command word (not inside another identifier like "ascii" or "radii").
_II_COMMAND_RE = re.compile(r"(?<![\w.\-])ii(?![\w.\-])", re.IGNORECASE)

# `cmd /c start ...`
_CMD_C_START_RE = re.compile(r"(?<![\w.\-])cmd(?:\.exe)?\s+/c\s+start(?![\w.\-])", re.IGNORECASE)

# Command-segment separators used to find "start of a command position".
_SEGMENT_SPLIT_RE = re.compile(r"(?:;|&&|\|\||\||&)")


def _segments(command: str) -> list[str]:
    return _SEGMENT_SPLIT_RE.split(command)


def _segment_starts_with_start(segment: str) -> bool:
    """True if `start` is the first word of this command segment."""
    # Deliberately excludes lookalikes: "--start-maximized", "restart",
    # "capture_start" are NOT matches because they either are not the first
    # token or `start` is not a standalone word there.
    stripped = segment.strip()
    # Only strip a leading PowerShell call operator "&" followed by whitespace;
    # do not attempt to skip past other prefixes -- we want `start` to be the
    # literal first word of the segment.
    candidate = stripped
    call_op_match = re.match(r"^&\s*", stripped)
    if call_op_match:
        candidate = stripped[call_op_match.end():]
    first_word_match = re.match(r"^([A-Za-z_][\w.\-]*)", candidate)
    if not first_word_match:
        return False
    first_word = first_word_match.group(1)
    return first_word.lower() == "start"


def check_r3_gui_open(command: str) -> str | None:
    if _GUI_WORD_RE.search(command):
        return (
            "R3 (GUI-open verb): this command invokes a GUI-opening verb "
            "(Start-Process / Invoke-Item / rundll32 / os.startfile / "
            "explorer). Per AGENTS.md Windows/Codex Safety, do not run "
            "commands that open GUI applications, file explorers, or "
            "dialogs -- use py -3 / node / mvn / git / non-interactive CLI "
            "invocations instead."
        )
    if _II_COMMAND_RE.search(command):
        return (
            "R3 (GUI-open verb): this command invokes `ii`, the PowerShell "
            "alias for Invoke-Item, as a standalone command word. Per "
            "AGENTS.md Windows/Codex Safety, do not open items via GUI "
            "handlers."
        )
    if _CMD_C_START_RE.search(command):
        return (
            "R3 (GUI-open verb): this command runs `cmd /c start ...`, which "
            "opens a GUI/file-association handler. Per AGENTS.md Windows/"
            "Codex Safety, do not use `start` to launch GUI content."
        )
    for segment in _segments(command):
        if _segment_starts_with_start(segment):
            return (
                "R3 (GUI-open verb): `start` appears as the first word of a "
                "command segment, which on Windows launches a new "
                "GUI/console window or opens a file via its default handler. "
                "Per AGENTS.md Windows/Codex Safety, avoid `start` as a "
                "command verb (this is not triggered by substrings like "
                "--start-maximized, restart, or capture_start)."
            )
    return None


# ---------------------------------------------------------------------------
# R5: graphify pre-check nudge (non-blocking; PreToolUse only)
# ---------------------------------------------------------------------------
#
# graphify (.claude/skills/graphify/SKILL.md) asks agents to resolve the
# shared repository-map cache (`python3 tools/repository-map/
# resolve_graph_out.py --check`) before broad Grep/Read sweeps, but until now
# that was guidance only -- nothing nudged an agent that skipped straight to
# Grep/Read. This adds a ONE-TIME, NON-BLOCKING reminder (PreToolUse "allow"
# + additionalContext, never "deny") once a session has made several
# Read/Grep calls with no evidence graphify was consulted. State is a small
# best-effort per-session file in the OS temp dir (or $SHAFT_GUARD_STATE_DIR
# when set, used by the self-test below); any failure to read/write it fails
# open (no nudge, no crash, no block).

_BROAD_SEARCH_TOOLS = frozenset({"Read", "Grep"})
_GRAPHIFY_NUDGE_THRESHOLD = 6  # broad Read/Grep calls before the one-time nudge
_RESOLVE_GRAPH_OUT_RE = re.compile(r"resolve_graph_out\.py", re.IGNORECASE)
_GRAPHIFY_OUT_PATH_RE = re.compile(r"graphify-out", re.IGNORECASE)
_SESSION_ID_SAFE_RE = re.compile(r"[^A-Za-z0-9_.-]")

_GRAPHIFY_NUDGE_MESSAGE = (
    "Reminder (graphify): this session has made several Read/Grep calls "
    "with no sign the shared repository-map cache was consulted. Before "
    "further broad exploration, try `python3 tools/repository-map/"
    "resolve_graph_out.py --check` -- if it prints a path, query that graph "
    "first (see .claude/skills/graphify/SKILL.md). One-time, non-blocking "
    "reminder for this session."
)


def _graphify_state_dir() -> str:
    override = os.environ.get("SHAFT_GUARD_STATE_DIR")
    return override if override else os.path.join(tempfile.gettempdir(), "shaft_guard_graphify_state")


def _graphify_state_path(session_id: str) -> str:
    safe_id = _SESSION_ID_SAFE_RE.sub("_", session_id) or "unknown"
    return os.path.join(_graphify_state_dir(), f"{safe_id}.json")


def _load_graphify_state(session_id: str) -> dict:
    try:
        with open(_graphify_state_path(session_id), "r", encoding="utf-8") as handle:
            data = json.load(handle)
            if isinstance(data, dict):
                return data
    except (OSError, ValueError):
        pass
    return {"broad_search_count": 0, "graphify_seen": False, "nudged": False}


def _save_graphify_state(session_id: str, state: dict) -> None:
    try:
        os.makedirs(_graphify_state_dir(), exist_ok=True)
        with open(_graphify_state_path(session_id), "w", encoding="utf-8") as handle:
            json.dump(state, handle)
    except OSError:
        pass  # best-effort only -- state loss just means a missed/repeated nudge


def note_graphify_reference_from_command(command: str, session_id: str) -> None:
    """Mark graphify as consulted when a Bash/PowerShell command resolves the cache."""
    if not session_id or not _RESOLVE_GRAPH_OUT_RE.search(command):
        return
    state = _load_graphify_state(session_id)
    if not state.get("graphify_seen"):
        state["graphify_seen"] = True
        _save_graphify_state(session_id, state)


def graphify_nudge_for_read_or_grep(hook_input: dict, session_id: str) -> str | None:
    """Return an additionalContext nudge string, or None. Never blocks."""
    if not session_id:
        return None
    tool_input = hook_input.get("tool_input") or {}
    target = str(tool_input.get("file_path") or tool_input.get("path") or "")

    state = _load_graphify_state(session_id)

    if _GRAPHIFY_OUT_PATH_RE.search(target):
        if not state.get("graphify_seen"):
            state["graphify_seen"] = True
            _save_graphify_state(session_id, state)
        return None

    if state.get("graphify_seen") or state.get("nudged"):
        return None

    state["broad_search_count"] = state.get("broad_search_count", 0) + 1
    if state["broad_search_count"] < _GRAPHIFY_NUDGE_THRESHOLD:
        _save_graphify_state(session_id, state)
        return None

    state["nudged"] = True
    _save_graphify_state(session_id, state)
    return _GRAPHIFY_NUDGE_MESSAGE


# ---------------------------------------------------------------------------
# Dispatcher
# ---------------------------------------------------------------------------

_CHECKS = (check_r1_maven, check_r2_allure, check_r3_gui_open)


def evaluate_command(command: str) -> str | None:
    """Return the first blocking reason found, or None if the command is allowed."""
    for check in _CHECKS:
        reason = check(command)
        if reason is not None:
            return reason
    return None


def _extract_command(hook_input: dict) -> str:
    tool_input = hook_input.get("tool_input") or {}
    command = tool_input.get("command")
    if isinstance(command, str):
        return command
    return ""


def _print_deny(reason: str) -> None:
    output = {
        "hookSpecificOutput": {
            "hookEventName": "PreToolUse",
            "permissionDecision": "deny",
            "permissionDecisionReason": reason,
        }
    }
    print(json.dumps(output))


def _print_allow_with_context(additional_context: str) -> None:
    output = {
        "hookSpecificOutput": {
            "hookEventName": "PreToolUse",
            "permissionDecision": "allow",
            "additionalContext": additional_context,
        }
    }
    print(json.dumps(output))


def run_pretooluse(hook_input: dict) -> int:
    tool_name = hook_input.get("tool_name", "")
    session_id = hook_input.get("session_id", "")

    if tool_name in ("Bash", "PowerShell"):
        command = _extract_command(hook_input)
        if not command:
            return 0
        note_graphify_reference_from_command(command, session_id)
        reason = evaluate_command(command)
        if reason is not None:
            _print_deny(reason)  # per protocol: deny is signaled via JSON on stdout, exit 0
        return 0

    if tool_name in _BROAD_SEARCH_TOOLS:
        nudge = graphify_nudge_for_read_or_grep(hook_input, session_id)
        if nudge is not None:
            _print_allow_with_context(nudge)
        return 0

    return 0  # not a tool this hook checks


# ---------------------------------------------------------------------------
# SessionStart mode
# ---------------------------------------------------------------------------

_SESSION_START_REMINDER = (
    "Hard safety rules for this repo (deterministically enforced by "
    ".claude/hooks/guard.py PreToolUse where possible):\n"
    "1. Scoped Maven tests only: use -Dtest=<Class>, or -pl <module> WITHOUT -am.\n"
    "2. Always pass -DheadlessExecution=true for any Maven test run that can reach browser tests.\n"
    "3. Never run `allure serve`, `allure open`, or `allure:serve` -- generate reports, never auto-open them.\n"
    "4. One branch/worktree/PR per session; no GUI-open commands (Start-Process, Invoke-Item, rundll32, os.startfile, explorer, `start`, `cmd /c start`)."
)


def run_session_start() -> int:
    output = {
        "hookSpecificOutput": {
            "hookEventName": "SessionStart",
            "additionalContext": _SESSION_START_REMINDER,
        }
    }
    print(json.dumps(output))
    return 0


# ---------------------------------------------------------------------------
# Self-test
# ---------------------------------------------------------------------------

# Each row: (description, command string, expect_block: bool)
_SELF_TEST_CASES: list[tuple[str, str, bool]] = [
    # --- MUST-PASS (allow) examples from the task spec ---
    ("scoped -pl + -Dtest + headless", "mvn -pl shaft-mcp test '-Dtest=Foo' '-DheadlessExecution=true'", False),
    ("skip tests with -am", "mvn -pl shaft-capture -am -DskipTests -Dgpg.skip=true verify", False),
    ("test-compile only, not a test-executing goal", "mvn -pl shaft-engine test-compile", False),
    ("plain git status", "git status", False),
    ("py -3 validator script", "py -3 scripts/ci/validate_agent_setup.py", False),
    ("allure generate via npx", "npx allure generate", False),

    # --- MUST-BLOCK examples from the task spec ---
    ("mvn -am test unscoped", "mvn -pl shaft-engine -am test", True),
    ("bare mvn test", "mvn test", True),
    ("mvn -pl test missing headless", "mvn -pl shaft-mcp test -Dtest=Foo", True),
    ("allure serve", "allure serve target/allure-results", True),
    ("mvn allure:serve", "mvn allure:serve", True),
    ("Start-Process", "Start-Process notepad", True),
    ("cmd /c start", "cmd /c start report.html", True),
    ("start as first word", "start chrome", True),

    # --- Additional edge cases ---
    ("maven.test.skip=true satisfies skip", "mvn install -Dmaven.test.skip=true", False),
    ("-pl without -am, with -Dtest, no headless still blocks on headless", "mvn -pl shaft-mcp -Dtest=Foo test", True),
    ("-pl without -am and without -Dtest but has headless (still needs scoping via -Dtest OR pl-without-am -> allowed since pl present without am)",
     "mvn -pl shaft-mcp test -DheadlessExecution=true", False),
    ("--projects long form without --also-make, no -Dtest, headless present", "mvn --projects shaft-mcp test -DheadlessExecution=true", False),
    ("--also-make long form blocks even with -pl", "mvn --projects shaft-mcp --also-make test -DheadlessExecution=true", True),
    ("surefire:test goal triggers rule (no -pl, no -Dtest)", "mvn surefire:test", True),
    ("failsafe:integration-test scoped with -pl (no -am) and headless is allowed", "mvn -pl shaft-engine failsafe:integration-test -DheadlessExecution=true", False),
    ("package goal scoped with -pl (no -am) and headless is allowed", "mvn -pl shaft-engine package -DheadlessExecution=true", False),
    ("package goal unscoped (no -pl/-Dtest) blocks on scoping", "mvn package -DheadlessExecution=true", True),
    ("verify with -am and -Dtest is fine (has -Dtest)", "mvn -pl shaft-engine -am verify -Dtest=FooTest -DheadlessExecution=true", False),
    ("mvnw wrapper is not matched (not literal 'mvn' token)", "./mvnw test", False),
    ("mvn.cmd variant matched", "mvn.cmd test", True),
    ("allure:report without serve is allowed", "mvn allure:report", False),
    ("allure serve case-insensitive", "ALLURE SERVE target/allure-results", True),
    ("Invoke-Item blocked", "Invoke-Item .\\report.html", True),
    ("rundll32 blocked", "rundll32 shell32.dll,OpenAs_RunDLL report.html", True),
    ("os.startfile blocked", "python3 -c \"import os; os.startfile('report.html')\"", True),
    ("explorer word blocked", "explorer report.html", True),
    ("standalone ii blocked", "ii .\\report.html", True),
    ("ii inside word not blocked", "radii.txt", False),
    ("--start-maximized not blocked", "chromedriver --start-maximized", False),
    ("restart not blocked", "sudo systemctl restart nginx", False),
    ("capture_start not blocked", "mcp__shaft-mcp__capture_start", False),
    ("start after semicolon blocked", "git status; start chrome", True),
    ("start after && blocked", "git status && start chrome", True),
    ("start after pipe blocked", "echo hi | start", True),
    ("start after & blocked", "git status & start chrome", True),
    ("start mid-word in later segment not blocked", "git status && echo restart-service", False),
    ("empty command allowed", "", False),

    # --- Command-head matching: quoted/heredoc PROSE about Maven must not block (issue #3422 item 14) ---
    ("gh pr create body mentioning mvn test is prose, not a command",
     'gh pr create --title "Fix" --body "Verified with:\nmvn -pl shaft-mcp test\nAll green."', False),
    ("git commit -m quoting mvn test is prose",
     'git commit -m "mvn test now passes"', False),
    ("bash heredoc PR body mentioning mvn test is prose",
     "gh pr create --body-file - <<'EOF'\nRan mvn test without flags.\nEOF", False),
    ("powershell here-string body mentioning mvn test is prose",
     "gh pr create --body @'\nmvn test\n'@", False),
    ("echo quoting allure serve is prose", 'echo "allure serve target"', False),
    ("mvn test after && is still a real command", "git status && mvn test", True),
    ("mvn test on its own line is still a real command", "git status\nmvn test", True),
    ("env-var prefixed mvn test is still a real command", "FOO=1 mvn test", True),
    ("timeout-wrapped mvn test is still a real command", "timeout 60 mvn test", True),
    ("npx allure serve is still a real command", "npx allure serve target/allure-results", True),
    ("multi-line mvn continuation keeps its scoping flags",
     "mvn -pl shaft-mcp test \\\n  -Dtest=Foo \\\n  -DheadlessExecution=true", False),
]


def run_self_test() -> int:
    failures: list[str] = []
    for description, command, expect_block in _SELF_TEST_CASES:
        reason = evaluate_command(command)
        blocked = reason is not None
        ok = blocked == expect_block
        status = "PASS" if ok else "FAIL"
        detail = f"  reason={reason!r}" if blocked else ""
        print(f"[{status}] {description}: {command!r} (expected_block={expect_block}, got_block={blocked}){detail}")
        if not ok:
            failures.append(description)

    total = len(_SELF_TEST_CASES)
    passed = total - len(failures)
    print(f"\nSelf-test summary: {passed}/{total} passed, {len(failures)} failed.")
    if failures:
        print("Failed cases: " + ", ".join(failures))
        return 1
    return 0


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def run_graphify_self_test() -> int:
    """Exercises the R5 nudge across a simulated sequence of PreToolUse calls."""
    import contextlib
    import io
    import shutil

    failures: list[str] = []

    def call(tool_name: str, tool_input: dict, session_id: str) -> str:
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            run_pretooluse({"tool_name": tool_name, "tool_input": tool_input, "session_id": session_id})
        return buf.getvalue()

    def check(description: str, condition: bool) -> None:
        status = "PASS" if condition else "FAIL"
        print(f"[{status}] {description}")
        if not condition:
            failures.append(description)

    tmp_dir = tempfile.mkdtemp(prefix="shaft_guard_graphify_test_")
    old_override = os.environ.get("SHAFT_GUARD_STATE_DIR")
    os.environ["SHAFT_GUARD_STATE_DIR"] = tmp_dir
    try:
        for i in range(_GRAPHIFY_NUDGE_THRESHOLD - 1):
            out = call("Grep", {"pattern": "foo", "path": "src"}, "sess-a")
            check(f"call {i + 1}/{_GRAPHIFY_NUDGE_THRESHOLD - 1} below threshold stays silent", out == "")

        out = call("Read", {"file_path": "src/Foo.java"}, "sess-a")
        parsed = json.loads(out) if out.strip() else None
        check(
            "threshold call emits a non-blocking allow+additionalContext nudge",
            parsed is not None
            and parsed["hookSpecificOutput"]["permissionDecision"] == "allow"
            and "additionalContext" in parsed["hookSpecificOutput"],
        )

        out = call("Grep", {"pattern": "bar", "path": "src"}, "sess-a")
        check("nudge does not repeat within the same session", out == "")

        for _ in range(3):
            call("Grep", {"pattern": "foo", "path": "src"}, "sess-b")
        call(
            "Bash",
            {"command": "python3 tools/repository-map/resolve_graph_out.py --check"},
            "sess-b",
        )
        out = ""
        for _ in range(_GRAPHIFY_NUDGE_THRESHOLD):
            out = call("Grep", {"pattern": "foo", "path": "src"}, "sess-b")
        check("resolve_graph_out.py bash call suppresses the nudge", out == "")

        for _ in range(2):
            call("Grep", {"pattern": "foo", "path": "src"}, "sess-c")
        call("Read", {"file_path": "graphify-out/manifest.json"}, "sess-c")
        out = ""
        for _ in range(_GRAPHIFY_NUDGE_THRESHOLD):
            out = call("Grep", {"pattern": "foo", "path": "src"}, "sess-c")
        check("reading a graphify-out/ path suppresses the nudge", out == "")

        out = call("Grep", {"pattern": "foo", "path": "src"}, "")
        check("missing session_id fails open (no nudge, no crash)", out == "")
    finally:
        if old_override is None:
            os.environ.pop("SHAFT_GUARD_STATE_DIR", None)
        else:
            os.environ["SHAFT_GUARD_STATE_DIR"] = old_override
        shutil.rmtree(tmp_dir, ignore_errors=True)

    total_checks = len(failures)
    print(f"\nGraphify nudge self-test summary: {total_checks} failed.")
    if failures:
        print("Failed cases: " + ", ".join(failures))
        return 1
    return 0


def main(argv: list[str]) -> int:
    if "--self-test" in argv:
        command_result = run_self_test()
        graphify_result = run_graphify_self_test()
        return command_result or graphify_result

    if "--session-start" in argv:
        return run_session_start()

    raw = sys.stdin.read()
    if not raw.strip():
        return 0
    try:
        hook_input = json.loads(raw)
    except json.JSONDecodeError:
        return 0  # malformed input: fail open (allow), nothing to safely block on

    hook_event = hook_input.get("hook_event_name", "")
    if hook_event == "SessionStart":
        return run_session_start()

    return run_pretooluse(hook_input)


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
