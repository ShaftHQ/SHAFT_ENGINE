#!/usr/bin/env python3
"""Portable SHAFT lifecycle and PreToolUse guard for Claude, Codex, and Grok."""
# Stdlib only. Input is normalized before evaluation; policy below is shared.
# Rules:
#
#   R1 Maven test scoping + headless execution
#      (mirrors .memory/memory/gotchas/
#       mvn-test-must-force-headlessexecution-true-and-never-invoke-allure-serve-report-open.md
#       and .memory/memory/gotchas/
#       unscoped-am-mvn-test-can-crash-the-jvm-across-the-whole-reactor.md --
#       both repo-tracked so they travel with every clone/worktree)
#      CI exception: e2eTests.yml's JUnit E2E reactor job deliberately builds
#      dependencies with -am. It remains bounded by -pl shaft-engine, its
#      -Dtest=Junit*,MoonTests default, and -DheadlessExecution=true, unlike
#      the local test commands this guard protects.
#   R2 Never auto-open/serve Allure reports
#   R3 Never run GUI-opening commands on Windows (AGENTS.md Windows/Codex Safety)
#   R8 Deny mutating `git stash` subcommands (pop/drop/apply/clear/push, and
#      bare `git stash`) in Bash/PowerShell commands. The stash list lives in
#      the shared .git dir, common to the main checkout and every
#      `git worktree add`-created worktree; an empty `stash push` followed by
#      `stash pop` can pop and DROP an unrelated entry from another
#      session/worktree (issue #4130). Read-only `git stash list` / `git
#      stash show` stay allowed.
#   R9 `git worktree add` guardrails (issue #4126, #4496): (B1) deny when the
#      Bash tool's worktree path argument contains backslashes -- Git Bash/MSYS
#      consumes each backslash as an escape and the path silently collapses
#      into one garbage segment at the repo root, exit 0. PowerShell is
#      unaffected, so this check is Bash-only. (B2) deny any
#      `git worktree add` missing `-c core.longpaths=true`, which otherwise
#      aborts with `Filename too long` checking out existing over-long
#      .memory/** paths. (B3) deny `-b`/`-B` with a branch name that does not
#      start with `ChaosEngine/` -- the entrypoint's Task isolation section
#      requires it, and tools that key off the prefix (PR watchers, worktree
#      cleanup) go blind to a non-conforming branch. `--detach` and checking
#      out an existing branch (no `-b`/`-B`) create no new branch, so neither
#      is checked. All three fail open on anything not confidently parsed.
#   R10 Deny `git add`/`git stage`/`git commit` when a changed file is almost
#      entirely NUL bytes (issue #4437). After an unclean shutdown the
#      filesystem can record an allocation without ever flushing the data
#      blocks, leaving files of a plausible size filled with zeros -- 652 of
#      653 files in one worktree, presented by `git status` as ordinary ` M`
#      entries. Unlike R1-R9 this rule reads repository state, so it needs a
#      working directory and is dispatched alongside R9 rather than from the
#      pure `evaluate_command` path. Every uncertain step fails open.
#      Deliberate limits: it targets WHOLLY zeroed files (>= 95% NUL across
#      head, middle, and tail), not partial corruption of a large binary; it
#      cannot resolve a user's `git` aliases, which are invisible in the
#      command string; and it does not descend into submodules, whose gitlink
#      is all `git diff` reports.
#
# Claude-compatible and Codex use snake_case input and hookSpecificOutput.
# Grok may supply camelCase fields and uses top-level deny/reason. The
# normalizer and host-specific emitter keep rule evaluation identical.

from __future__ import annotations

import hashlib
import json
import os
import re
import subprocess  # nosec B404 - R10 runs one fixed, read-only git query.
import sys
import tempfile
from typing import NamedTuple

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
    tokens: list[str] = []
    token: list[str] = []
    quote: str | None = None
    index = 0
    while index < len(stripped):
        character = stripped[index]
        following = stripped[index + 1] if index + 1 < len(stripped) else ""
        if quote is not None:
            if character == quote:
                if quote == "'" and following == "'":
                    token.append("'")  # PowerShell's apostrophe escape.
                    index += 2
                    continue
                quote = None
            elif quote == '"' and character == "`" and following:
                token.append(following)  # PowerShell escapes the next character.
                index += 2
                continue
            elif quote == '"' and character == "\\" and following == '"':
                token.append('"')
                index += 2
                continue
            else:
                token.append(character)
            index += 1
            continue
        if character.isspace():
            if token:
                tokens.append("".join(token))
                token = []
        elif character in ("'", '"'):
            quote = character
        elif character == "\\" and following in ("'", '"', " "):
            token.append(following)
            index += 1
        else:
            token.append(character)
        index += 1
    if token:
        tokens.append("".join(token))
    return tokens


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

# The multi-word/dotted verbs above collide with a different shape than the
# short aliases below: not a quoted regex character class, but ordinary PROSE
# -- a commit message, PR/issue body, or code comment merely discussing one of
# these five patterns (reported live, twice, this session: `git commit -m` and
# `gh issue create --body`; see issue #4147). Restricting to "first word of a
# command segment" (the short-alias fix) does not apply here without losing
# real detection: `os.startfile` is never a command's first token -- it is
# always embedded in an interpreter's script argument (`python3 -c "..."`) --
# and a real invocation can also be nested inside a QUOTED script argument to
# another interpreter, e.g. `powershell -Command "Start-Process notepad"` run
# via the Bash tool. So instead of restricting position, reuse the existing
# multi-line-quote/heredoc sanitizer (`_sanitize_for_command_head`, already
# used ahead of the Maven/Allure command-head checks) and additionally blank
# single-line quoted strings that are pure data -- UNLESS the quote
# immediately follows a "treat this quoted text as code to execute" flag
# (-c / -Command / /c / --command), which keeps every real nested-interpreter
# invocation scanned. This is deliberately scoped to the interpreters this
# harness actually exposes (PowerShell/cmd/python/py via the Bash and
# PowerShell tools); an obfuscated `-EncodedCommand` (base64) is undetectable
# either way, before or after this fix.
_EXEC_FLAG_QUOTE_RE = re.compile(
    r"(?:--?[Cc]ommand|-[Cc]|/[Cc])\s+(\"(?:[^\"\\]|\\.)*\"|'(?:[^'\\]|\\.)*')"
    r"|(\"(?:[^\"\\]|\\.)*\"|'(?:[^'\\]|\\.)*')"
)


def _blank_prose_quotes(command: str) -> str:
    """Blank single-line quoted strings that are data, keeping nested-code quotes intact."""

    def repl(match: re.Match[str]) -> str:
        if match.group(1) is not None:
            return match.group(0)  # protected: flag + quoted code, scan it as-is
        return " " * len(match.group(0))  # prose quote: blank so its words can't match

    return _EXEC_FLAG_QUOTE_RE.sub(repl, command)


def _sanitize_for_gui_word_check(command: str) -> str:
    """Strip data-only quoted/heredoc prose before the R3 multi-word-verb search."""
    return _blank_prose_quotes(_sanitize_for_command_head(command))


def _unquote_exec_flag_payload(command: str) -> str:
    """Drop the quote characters around an exec-flag-quoted payload, blank prose.

    `_blank_prose_quotes` keeps an exec-flag-quoted payload (after -c /
    -Command / /c / --command) intact, quotes included -- fine for
    `_GUI_WORD_RE`, a plain substring search unaffected by surrounding quote
    characters. But `_CMD_C_START_RE` requires the verb to follow `/c` with
    only WHITESPACE in between, so a quote character right after `/c`
    defeats it even though the quote changes nothing about what actually
    runs (issue #4152: `cmd /c "start report.html"` is `cmd /c start
    report.html` from the shell's point of view). Replacing the quote
    characters with spaces -- while still blanking pure-prose quotes, as
    `_blank_prose_quotes` does -- lets the structural whitespace-only check
    see through the quoting without weakening the prose exemption.
    """

    def repl(match: re.Match[str]) -> str:
        quoted = match.group(1)
        if quoted is not None:
            prefix = match.group(0)[: -len(quoted)]
            return prefix + " " + quoted[1:-1] + " "
        return " " * len(match.group(0))  # prose quote: blank so its words can't match

    return _EXEC_FLAG_QUOTE_RE.sub(repl, command)


def _sanitize_for_cmd_c_check(command: str) -> str:
    """Strip data-only quoted/heredoc prose and unquote exec-flag payloads."""
    return _unquote_exec_flag_payload(_sanitize_for_command_head(command))


# `ii` and `start` are only the GUI-open PowerShell alias / verb when they
# stand alone as the FIRST WORD of a command segment (real command position).
# A plain word-boundary regex over the whole raw command string (the earlier
# approach for `ii`) false-positives on any quoted interpreter argument that
# merely contains the two letters as a delimited "word" -- e.g. a Python
# regex character class `r'[Ii]mplement'` passed to `py -3 -c "..."` is not a
# PowerShell command at all, yet `(?<![\w.\-])ii(?![\w.\-])` still matched it
# (reported live; see PR description). Restricting to "first word of the
# segment" (like `start` already did) fixes this without losing any real
# detection: a bare `ii <path>` -- or `ii` after `;`/`&&`/`|`/`&` -- is still
# the first word of its own segment and stays blocked.

# `cmd /c start ...`
_CMD_C_START_RE = re.compile(r"(?<![\w.\-])cmd(?:\.exe)?\s+/c\s+start(?![\w.\-])", re.IGNORECASE)

# Command-segment separators used to find "start of a command position".
_SEGMENT_SPLIT_RE = re.compile(r"(?:;|&&|\|\||\||&)")


def _segments(command: str) -> list[str]:
    return _SEGMENT_SPLIT_RE.split(command)


def _segment_starts_with_word(segment: str, word: str) -> bool:
    """True if `word` is the first word of this command segment (command position).

    Deliberately excludes lookalikes: a short alias/verb inside a larger
    identifier ("--start-maximized", "restart", "capture_start", "radii"),
    embedded in a quoted interpreter argument (a regex character class like
    `[Ii]mplement` passed to `py -3 -c "..."`, or ordinary prose like
    "start something" inside a quoted string), or appearing later in the
    same segment are NOT matches -- in every one of those cases the alias is
    not the token actually being executed as a command.
    """
    stripped = segment.strip()
    # Only strip a leading PowerShell call operator "&" followed by whitespace;
    # do not attempt to skip past other prefixes -- we want `word` to be the
    # literal first word of the segment.
    candidate = stripped
    call_op_match = re.match(r"^&\s*", stripped)
    if call_op_match:
        candidate = stripped[call_op_match.end():]
    first_word_match = re.match(r"^([A-Za-z_][\w.\-]*)", candidate)
    if not first_word_match:
        return False
    return first_word_match.group(1).lower() == word.lower()


def check_r3_gui_open(command: str) -> str | None:
    if _GUI_WORD_RE.search(_sanitize_for_gui_word_check(command)):
        return (
            "R3 (GUI-open verb): this command invokes a GUI-opening verb "
            "(Start-Process / Invoke-Item / rundll32 / os.startfile / "
            "explorer). Per AGENTS.md Windows/Codex Safety, do not run "
            "commands that open GUI applications, file explorers, or "
            "dialogs -- use py -3 / node / mvn / git / non-interactive CLI "
            "invocations instead."
        )
    if _CMD_C_START_RE.search(_sanitize_for_cmd_c_check(command)):
        return (
            "R3 (GUI-open verb): this command runs `cmd /c start ...`, which "
            "opens a GUI/file-association handler. Per AGENTS.md Windows/"
            "Codex Safety, do not use `start` to launch GUI content."
        )
    for segment in _segments(command):
        if _segment_starts_with_word(segment, "ii"):
            return (
                "R3 (GUI-open verb): this command invokes `ii`, the "
                "PowerShell alias for Invoke-Item, as the first word of a "
                "command segment (real command position). Per AGENTS.md "
                "Windows/Codex Safety, do not open items via GUI handlers."
            )
        if _segment_starts_with_word(segment, "start"):
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
# R8: deny mutating `git stash` subcommands (shared across worktrees)
# ---------------------------------------------------------------------------

_GIT_NAMES = frozenset({"git", "git.exe"})


def _git_segments(command: str) -> list[str]:
    """Segments whose executable is git (command position, not quoted prose)."""
    return [
        segment
        for segment in _command_segments(_sanitize_for_command_head(command))
        if _head_executable_matches(segment, _GIT_NAMES)
    ]


def _tokens_after_head(segment: str, names: frozenset[str]) -> list[str] | None:
    """Return the tokens following the segment's matching head executable, or None."""
    tokens = _segment_tokens(segment)
    index = 0
    while index < len(tokens):
        token = tokens[index]
        if _ENV_ASSIGNMENT_RE.match(token):
            index += 1
            continue
        basename = re.split(r"[/\\]", token.strip("\"'"))[-1].lower()
        if basename in names:
            return tokens[index + 1:]
        if basename in _RUNNER_PREFIX_TOKENS or basename == "timeout":
            index += 1
            if index < len(tokens) and re.match(r"^\d+[smhd]?$", tokens[index]):
                index += 1
            continue
        return None
    return None


_GIT_STASH_MUTATING_SUBCOMMANDS = frozenset({"pop", "drop", "apply", "clear", "push"})
_GIT_STASH_READONLY_SUBCOMMANDS = frozenset({"list", "show"})
_GIT_GLOBAL_OPTS_WITH_ARG = frozenset({"-c", "-C"})


def _stash_subcommand(rest: list[str]) -> tuple[bool, str | None]:
    """Return (is_stash_command, subcommand_or_None) for tokens following the git executable."""
    index = 0
    while index < len(rest):
        token = rest[index]
        if token in _GIT_GLOBAL_OPTS_WITH_ARG:
            index += 2
            continue
        if token.startswith("-"):
            index += 1
            continue
        break
    if index >= len(rest) or rest[index].lower() != "stash":
        return False, None
    index += 1
    while index < len(rest):
        token = rest[index]
        if token.startswith("-"):
            index += 1
            continue
        return True, token.lower()
    return True, None


def check_r8_git_stash(command: str) -> str | None:
    """Return a block reason for a mutating `git stash` subcommand, or None."""
    for segment in _git_segments(command):
        rest = _tokens_after_head(segment, _GIT_NAMES)
        if rest is None:
            continue
        is_stash, sub = _stash_subcommand(rest)
        if not is_stash or sub in _GIT_STASH_READONLY_SUBCOMMANDS:
            continue
        if sub is None or sub in _GIT_STASH_MUTATING_SUBCOMMANDS:
            return (
                "R8 (git stash shared across worktrees): the stash list lives in "
                "the shared .git directory, common to the main checkout and every "
                "`git worktree add`-created worktree. A `git stash` that finds "
                "nothing to save still lets a later `git stash pop` pop and DROP "
                "an unrelated entry from a different session/worktree -- this has "
                "already destroyed a months-old stash and silently reverted a "
                "tracked AGENTS.md in this repo (issue #4130). Do not run mutating "
                f"`git stash{(' ' + sub) if sub else ''}` in a shared-.git "
                "worktree -- commit your work to your own branch instead "
                "(`git add -A && git commit`), or use read-only `git stash list` "
                "/ `git stash show` / `git diff` to inspect state without "
                "mutating the shared stash."
            )
    return None


# ---------------------------------------------------------------------------
# R9: `git worktree add` guardrails (backslash-via-Bash, missing longpaths)
# ---------------------------------------------------------------------------

_GIT_WORKTREE_ADD_FLAGS_WITH_ARG = frozenset({"-b", "-B", "--reason"})
_LONGPATHS_RE = re.compile(
    r"(?<![\w.-])-c\s+[\"']?core\.longpaths=true[\"']?(?![\w.-])", re.IGNORECASE
)


def _worktree_add_rest(rest: list[str]) -> list[str] | None:
    """Return tokens after 'worktree add' in `rest`, or None if not confidently a match."""
    index = 0
    seen_worktree = False
    while index < len(rest):
        token = rest[index]
        if not seen_worktree:
            if token.lower() == "worktree":
                seen_worktree = True
                index += 1
                continue
            if token in _GIT_GLOBAL_OPTS_WITH_ARG:
                index += 2
                continue
            if token.startswith("-"):
                index += 1
                continue
            return None  # unexpected token before 'worktree': not confidently parseable
        if token.lower() == "add":
            return rest[index + 1:]
        return None  # 'worktree' subcommand other than 'add'
    return None


def _worktree_add_path(rest_after_add: list[str]) -> str | None:
    """Return the worktree path token, or None if it cannot be confidently identified."""
    index = 0
    while index < len(rest_after_add):
        token = rest_after_add[index]
        if token in _GIT_WORKTREE_ADD_FLAGS_WITH_ARG:
            index += 2
            continue
        if token.startswith("-"):
            index += 1
            continue
        return token
    return None


_CHAOS_ENGINE_BRANCH_PREFIX = "ChaosEngine/"


def _worktree_add_branch(rest_after_add: list[str]) -> str | None:
    """Return the `-b`/`-B` branch name in `rest_after_add`, or None if absent.

    None also covers `--detach` and checking out an existing branch (no
    `-b`/`-B` at all) -- neither creates a new branch, so neither is subject
    to the ChaosEngine/* naming requirement.
    """
    index = 0
    while index < len(rest_after_add):
        token = rest_after_add[index]
        if token in ("-b", "-B"):
            return rest_after_add[index + 1] if index + 1 < len(rest_after_add) else None
        if token in _GIT_WORKTREE_ADD_FLAGS_WITH_ARG:
            index += 2
            continue
        index += 1
    return None


def check_r9_worktree_add(command: str, tool_name: str) -> str | None:
    """Return a block reason for an unsafe `git worktree add` invocation, or None."""
    for segment in _git_segments(command):
        rest = _tokens_after_head(segment, _GIT_NAMES)
        if rest is None:
            continue
        rest_after_add = _worktree_add_rest(rest)
        if rest_after_add is None:
            continue

        if tool_name == "Bash":
            path = _worktree_add_path(rest_after_add)
            if path is not None and "\\" in path:
                return (
                    "R9 (git worktree add backslash path via Bash): the Bash "
                    "tool runs Git Bash/MSYS, which consumes each backslash in "
                    f"'{path}' as an escape -- the path silently collapses into "
                    "one garbage segment at the repo root (exit code 0, no "
                    "error). Confirmed twice this session, including "
                    "`worktrees\\w4067` collapsing into one path segment (issue "
                    "#4126). Use forward slashes instead, e.g. "
                    f"'{path.replace(chr(92), '/')}'."
                )

        branch = _worktree_add_branch(rest_after_add)
        if branch is not None and not branch.startswith(_CHAOS_ENGINE_BRANCH_PREFIX):
            return (
                "R9 (git worktree add non-conforming branch prefix): the "
                "entrypoint's Task isolation section requires every session "
                f"branch to be `{_CHAOS_ENGINE_BRANCH_PREFIX}*` off a fresh "
                f"`origin/main` -- '{branch}' does not start with that prefix. "
                "Tools that key off the prefix (PR watchers, worktree "
                "cleanup) go blind to work on a non-conforming branch (issue "
                f"#4496). Use `{_CHAOS_ENGINE_BRANCH_PREFIX}<slug>` instead."
            )

        if not _LONGPATHS_RE.search(segment):
            match = re.search(r"\bworktree\b", segment, re.IGNORECASE)
            if match:
                corrected = (
                    segment[: match.start()] + "-c core.longpaths=true " + segment[match.start():]
                )
            else:
                corrected = segment.strip() + " (add -c core.longpaths=true before 'worktree')"
            return (
                "R9 (git worktree add missing longpaths): plain `git worktree "
                "add` aborts with `Filename too long` checking out existing "
                "over-long .memory/** paths, stopping an agent before it starts "
                "any work (issue #4126). Add -c core.longpaths=true: "
                f"`{corrected.strip()}`"
            )
    return None


# ---------------------------------------------------------------------------
# R10: refuse to stage or commit NUL-corrupted files
# ---------------------------------------------------------------------------

_STAGING_SUBCOMMANDS = frozenset({"add", "stage", "commit"})

# A file whose sampled bytes are almost entirely NUL is not content anyone
# authored. 0.95 (rather than 1.0) keeps a partially-flushed tail in range
# while staying far above any real format: an archive, image, or class file
# is never near-uniformly zero across head, middle, and tail.
_NUL_RATIO_THRESHOLD = 0.95
# Sample three windows instead of a prefix: a legitimately zero-padded file
# can open with a long run of NUL and still carry real content later. 8 KiB
# per window matches git's own binary sniff (the first 8000 bytes) and keeps
# the worst case -- _NUL_MAX_SCANNED_PATHS files -- to tens of megabytes read.
_NUL_SAMPLE_WINDOW_BYTES = 8 * 1024
_NUL_MAX_SCANNED_PATHS = 2000
_NUL_MAX_REPORTED_PATHS = 5
# Must leave room, inside the PreToolUse hook timeout the host configs declare
# (10s in .claude/settings.json and .codex/hooks.json), for two git queries
# plus the sampling loop -- measured at 0.84s for 700 zeroed files. Exceeding
# the hook budget would have the host kill the guard mid-query instead of
# letting this rule fail open.
_GIT_QUERY_TIMEOUT_SECONDS = 4


def nul_byte_ratio(path) -> float | None:
    """Fraction of NUL bytes in `path`, or None when empty/unreadable."""
    # Reads at most three windows (head, middle, tail) so a large file costs a
    # bounded number of reads and a zero-padded head cannot masquerade as
    # whole-file corruption.
    try:
        size = os.path.getsize(path)
        if size <= 0:
            return None
        with open(path, "rb") as handle:
            if size <= 3 * _NUL_SAMPLE_WINDOW_BYTES:
                sample = handle.read()
            else:
                chunks = []
                for offset in (
                    0,
                    (size // 2) - (_NUL_SAMPLE_WINDOW_BYTES // 2),
                    size - _NUL_SAMPLE_WINDOW_BYTES,
                ):
                    handle.seek(offset)
                    chunks.append(handle.read(_NUL_SAMPLE_WINDOW_BYTES))
                sample = b"".join(chunks)
    except OSError:
        return None
    if not sample:
        return None
    return sample.count(0) / len(sample)


class _StagingInvocation(NamedTuple):
    """What a staging command stages, and where."""

    subcommand: str
    directories: list[str]  # -C / --work-tree values, applied in order
    pathspecs: list[str]  # empty means "everything"
    include_worktree: bool = False


def _normalize_pathspec(value: str) -> str | None:
    """Reduce a pathspec to a repository-relative posix prefix, or None."""
    cleaned = value.replace("\\", "/").strip()
    while cleaned.startswith("./"):
        cleaned = cleaned[2:]
    cleaned = cleaned.rstrip("/")
    if not cleaned or cleaned in (".", "*") or cleaned.startswith(":"):
        return None  # "everything", or magic pathspec syntax: do not narrow
    if "*" in cleaned or "?" in cleaned or "[" in cleaned:
        return None  # a glob: matching it here would risk a false negative
    return cleaned


_NESTED_COMMAND_RE = re.compile(
    r"(?:--?[Cc]ommand|-[Cc]|/[Cc])\s+(\"(?:[^\"\\]|\\.)*\"|'(?:[^'\\]|\\.)*')"
)
_COMMIT_OPTIONS_WITH_ARGUMENT = frozenset(
    {
        "-C",
        "-F",
        "-c",
        "-m",
        "-t",
        "--author",
        "--cleanup",
        "--date",
        "--file",
        "--fixup",
        "--message",
        "--reedit-message",
        "--reuse-message",
        "--squash",
        "--template",
        "--trailer",
    }
)


def _staging_invocation(command: str) -> _StagingInvocation | None:
    """Describe the first command that really stages content, or None."""
    # A nested interpreter payload (`bash -c "git add -A"`) is retried as a
    # command of its own only when the outer command stages nothing, so the
    # common case keeps its quoted arguments intact -- unwrapping every quoted
    # string up front would blank a legitimate `--work-tree="<path>"` value.
    direct = _staging_invocation_in(command)
    if direct is not None:
        return direct
    for payload in _NESTED_COMMAND_RE.findall(command):
        nested = _staging_invocation_in(payload[1:-1])
        if nested is not None:
            return nested
    return None


# Options that redirect git at a different tree than the calling directory.
# Named `argument` rather than `token` throughout: Bandit's B105 reads a
# comparison between a variable called `token` and a string literal as a
# hardcoded credential.
_DIRECTORY_OPTIONS = frozenset({"-C", "--work-tree"})
_PATHSPEC_SEPARATOR = "--"


def _split_global_options(rest: list[str]) -> tuple[str | None, list[str], int]:
    """Return (subcommand, -C/--work-tree values, index after the subcommand)."""
    directories: list[str] = []
    index = 0
    while index < len(rest):
        argument = rest[index]
        if argument in _DIRECTORY_OPTIONS and index + 1 < len(rest):
            directories.append(rest[index + 1].strip("\"'"))
            index += 2
            continue
        if argument.startswith("--work-tree="):
            directories.append(argument.split("=", 1)[1].strip("\"'"))
            index += 1
            continue
        if argument in _GIT_GLOBAL_OPTS_WITH_ARG:
            index += 2
            continue
        if argument.startswith("-"):
            index += 1
            continue
        return argument.lower(), directories, index + 1
    return None, directories, index


def _commit_option_takes_argument(argument: str) -> bool:
    """True when this `git commit` option consumes the argument after it."""
    if argument in _COMMIT_OPTIONS_WITH_ARGUMENT:
        return True
    return bool(re.match(r"^-[A-Za-z]*[mFCct]$", argument))


def _commit_option_carries_its_value(argument: str) -> bool:
    """True for a `--option=value` form, which consumes nothing after it."""
    lowered = argument.lower()
    return any(
        lowered.startswith(option.lower() + "=")
        for option in _COMMIT_OPTIONS_WITH_ARGUMENT
        if option.startswith("--")
    )


def _stages_the_whole_worktree(argument: str) -> bool:
    """True for `git commit --all` / `-a`, which stages tracked changes itself."""
    lowered = argument.lower()
    return lowered == "--all" or bool(re.match(r"^-[^-]*a", lowered))


def _staging_pathspecs(rest: list[str], subcommand: str) -> tuple[list[str], bool]:
    """Return (paths this command names, whether it stages the whole worktree)."""
    # `git add <path>` names what it stages, and honouring that lets an agent
    # rescue healthy files while one corrupt file sits in the tree. For `git
    # commit`, known option values are skipped first so a message is never read
    # as a path; the remaining positional arguments are commit pathspecs.
    # An empty pathspec list means "everything".
    pathspecs: list[str] = []
    narrowable = subcommand in ("add", "stage")
    include_worktree = False
    after_separator = False
    index = 0
    while index < len(rest):
        argument = rest[index]
        index += 1
        if argument == _PATHSPEC_SEPARATOR:
            after_separator = True
            narrowable = True
            continue
        if argument.startswith("--pathspec-from-file"):
            return [], subcommand == "commit"  # the list lives in a file
        if subcommand == "commit" and not after_separator:
            if _stages_the_whole_worktree(argument):
                include_worktree = True
            if _commit_option_takes_argument(argument):
                index += 1
                continue
            if _commit_option_carries_its_value(argument):
                continue
        if argument.startswith("-") and not after_separator:
            continue
        if subcommand == "commit":
            include_worktree = True
            narrowable = True
        if not (narrowable or after_separator):
            continue
        normalized = _normalize_pathspec(argument)
        if normalized is None:
            return [], include_worktree
        pathspecs.append(normalized)
    return pathspecs, include_worktree


def _staging_invocation_in(command: str) -> _StagingInvocation | None:
    """Describe the first git segment of `command` that stages content."""
    # Every git segment is examined, not just the first: `git status && git add
    # -A` is an ordinary agent shape, and stopping at the leading read-only
    # segment would wave the commit through.
    for segment in _git_segments(command):
        rest = _tokens_after_head(segment, _GIT_NAMES)
        if rest is None:
            continue
        subcommand, directories, index = _split_global_options(rest)
        if subcommand not in _STAGING_SUBCOMMANDS:
            continue
        pathspecs, include_worktree = _staging_pathspecs(rest[index:], subcommand)
        return _StagingInvocation(subcommand, directories, pathspecs, include_worktree)
    return None


def _git_paths(cwd: str, *arguments: str) -> list[str] | None:
    """Run a NUL-delimited, read-only git path query, or None when untrusted."""
    # Output is decoded with `os.fsdecode` rather than the process locale:
    # `text=True` on a non-UTF-8 host mangles a non-ASCII filename into one
    # that no longer resolves on disk, which silently exempts it from
    # scanning. `core.quotePath=false` stops git octal-escaping the same names.
    try:
        completed = subprocess.run(  # nosec B603 B607 - fixed read-only git query.
            ["git", "-c", "core.quotePath=false", *arguments],
            cwd=cwd,
            capture_output=True,
            timeout=_GIT_QUERY_TIMEOUT_SECONDS,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if completed.returncode != 0:
        return None  # no HEAD, not a repository, or git refused: allow
    return [os.fsdecode(entry) for entry in completed.stdout.split(b"\0") if entry]


def _candidate_paths(
    cwd: str, *, staged_only: bool = False, include_untracked: bool = True
) -> list[str] | None:
    """Every path a stage/commit could capture: changed, staged, or untracked."""
    # `git diff HEAD` spans the index and the working tree in one call,
    # covering `git add`, `git commit`, and `git commit -a`. It cannot see an
    # untracked file, so `git ls-files --others` supplies those -- without them
    # a single `git add -A && git commit` would carry a newly zeroed file
    # straight through. Ignored paths are excluded, so build output is never
    # scanned.
    diff_arguments = ["diff"]
    if staged_only:
        diff_arguments.append("--cached")
    diff_arguments.extend(("--name-only", "-z", "HEAD"))
    changed = _git_paths(cwd, *diff_arguments)
    if changed is None:
        return None
    untracked = []
    if include_untracked:
        untracked = _git_paths(cwd, "ls-files", "--others", "--exclude-standard", "-z") or []
    seen: dict[str, None] = {}
    for path in (*changed, *untracked):
        seen.setdefault(path, None)
    return list(seen)


def scan_for_nul_corruption(
    cwd: str,
    pathspecs: list[str] | None = None,
    *,
    staged_only: bool = False,
    include_untracked: bool = True,
) -> tuple[list[str], int, bool]:
    """Return (corrupted paths, candidate count, scan truncated) for a directory."""
    # Shared with the repository's worktree hygiene report so one definition of
    # "this file is zeroed" serves both the deny guard and the reporting path.
    # Fails open as an empty result whenever git cannot be trusted.
    #
    # Every candidate is sampled directly rather than pre-filtered on the
    # diff's line counts: a `diff` attribute in .gitattributes can give a
    # NUL-filled file non-zero insertion/deletion counts, and a pre-filter
    # keyed on those counts would exempt exactly the file it needs to catch.
    candidates = _candidate_paths(
        cwd, staged_only=staged_only, include_untracked=include_untracked
    )
    if not candidates:
        return [], 0, False
    if pathspecs:
        candidates = [
            path
            for path in candidates
            if any(path == spec or path.startswith(spec + "/") for spec in pathspecs)
        ]
    candidate_count = len(candidates)
    examined = candidates[:_NUL_MAX_SCANNED_PATHS]
    corrupt = [
        path
        for path in examined
        # An unreadable file yields None and is treated as healthy: this rule
        # denies a tool call, so it must never block on what it cannot read.
        if (nul_byte_ratio(os.path.join(cwd, path)) or 0.0) >= _NUL_RATIO_THRESHOLD
    ]
    return corrupt, candidate_count, candidate_count > len(examined)


def check_r10_nul_corruption(command: str, cwd: str | None = None) -> str | None:
    """Return a block reason when staging/committing NUL-corrupted files."""
    if not cwd:
        return None
    invocation = _staging_invocation(command)
    if invocation is None:
        return None

    directory = cwd
    for part in invocation.directories:
        directory = os.path.join(directory, part)

    plain_commit = (
        invocation.subcommand == "commit"
        and not invocation.include_worktree
        and not invocation.pathspecs
    )
    corrupt, candidate_count, truncated = scan_for_nul_corruption(
        directory,
        invocation.pathspecs,
        staged_only=plain_commit,
        include_untracked=invocation.subcommand != "commit",
    )
    if truncated and not corrupt:
        return (
            f"R10 (NUL-byte corruption scan limit): {candidate_count} candidate "
            f"files exceed the safe {_NUL_MAX_SCANNED_PATHS}-file scan. The "
            "guard will not allow unchecked content through; name smaller path "
            "sets in separate `git add <paths>` commands, then commit matching "
            "sets with `git commit -- <paths>`."
        )
    if not corrupt:
        return None

    shown = ", ".join(corrupt[:_NUL_MAX_REPORTED_PATHS])
    if len(corrupt) > _NUL_MAX_REPORTED_PATHS:
        shown += f", ... (+{len(corrupt) - _NUL_MAX_REPORTED_PATHS} more)"
    restore_target = (
        corrupt[0] if len(corrupt) == 1 else "<each corrupt path listed above>"
    )
    return (
        f"R10 (NUL-byte corruption): {len(corrupt)} of "
        f"{min(candidate_count, _NUL_MAX_SCANNED_PATHS)} examined candidate "
        "file(s) are almost entirely NUL bytes and would be committed as "
        f"zeroed content: {shown}. Files of a plausible size filled with NUL "
        "are the signature of an unclean shutdown -- the filesystem recorded "
        "the allocation but never flushed the data blocks. This reads as "
        "ordinary ' M' entries in `git status`, and `git diff --shortstat` "
        "reports the changed files with 0 insertions(+) and 0 deletions(-), so "
        "committing it looks like a large but unremarkable diff (issue #4437: "
        "652 of 653 files in one worktree were zeroed this way). Do not stage "
        "or commit the zeroed content. Confirm with `git diff --stat HEAD -- "
        "<path>`, then restore only the corrupt paths: `git restore "
        f"--source=HEAD --staged --worktree -- {restore_target}`. Do not "
        "restore the whole worktree -- that would discard the healthy "
        "uncommitted work alongside it. Healthy files can still be committed "
        "by naming them, e.g. `git add <healthy path>`. Re-create any work "
        "that existed only in the corrupt files."
    )


# ---------------------------------------------------------------------------
# Dispatcher
# ---------------------------------------------------------------------------

_CHECKS = (check_r1_maven, check_r2_allure, check_r3_gui_open, check_r8_git_stash)


# ---------------------------------------------------------------------------
# R11: refuse an MCP memory write issued from a linked worktree
# ---------------------------------------------------------------------------

# Only the write path. Reads are the session-entry point AGENTS.md mandates,
# and a read cannot strand work in the wrong tree, so blocking them would make
# worktree isolation cost an agent its memory for no safety gain.
_MEMORY_WRITE_TOOLS = frozenset(
    {
        "mcp__shaft-memory__remember_memory",
        "mcp__shaft-memory__save_memory_patch",
    }
)


def worktree_root(cwd: str | None) -> str | None:
    """Return the checkout root containing `cwd`, or None when there is none."""
    if not cwd:
        return None
    try:
        current = os.path.abspath(cwd)
    except (OSError, ValueError):
        return None
    while True:
        if os.path.exists(os.path.join(current, ".git")):
            return current
        parent = os.path.dirname(current)
        if parent == current:
            return None
        current = parent


def is_linked_worktree(cwd: str | None) -> bool:
    """True when `cwd` sits inside a linked worktree rather than the primary checkout.

    A `.git` *file* is not the tell on its own, which an earlier revision of
    this rule assumed: `git init --separate-git-dir` gives an ordinary primary
    checkout the same shape, and so does a submodule. Nor is the pointer's
    *path* the tell, which the revision after that assumed -- a separate-git-dir
    checkout whose admin directory happens to sit under any folder named
    `worktrees` matched it too. Two path heuristics, two misclassifications.

    The tell is structural instead: git writes `gitdir` and `commondir` files
    INSIDE a linked worktree's admin directory, and nowhere else. Their presence
    is what a linked worktree is, rather than something its path looks like.

    Read off the filesystem rather than by shelling out, because this runs
    inside a PreToolUse hook whose budget the host caps at 10 seconds and R10
    already spends two git queries of that.

    Fails open -- outside a repository there is no linked worktree to be in, and
    a guard that denies where it cannot tell would block memory writes in every
    checkout it does not understand.
    """
    root = worktree_root(cwd)
    if root is None:
        return False
    marker = os.path.join(root, ".git")
    if not os.path.isfile(marker):
        return False
    try:
        # utf-8-sig, because `str.strip()` does not strip a U+FEFF BOM and the
        # `gitdir:` match would then miss. Git never writes one; a Windows
        # editor that rewrote the file might.
        with open(marker, encoding="utf-8-sig", errors="replace") as handle:
            pointer = handle.read(4096).strip()
    except OSError:
        return False
    if not pointer.lower().startswith("gitdir:"):
        return False
    admin = pointer.split(":", 1)[1].strip()
    if not os.path.isabs(admin):
        # `git worktree add --relative-paths` writes a pointer relative to the
        # checkout holding the `.git` file.
        admin = os.path.join(root, admin)
    return os.path.isfile(os.path.join(admin, "gitdir")) and os.path.isfile(
        os.path.join(admin, "commondir")
    )


def _targets_this_worktree(tool_input: dict | None, root: str) -> bool:
    """True when `project_root` provably selects `root` for this one tool call."""
    # Two conditions, and neither is redundant.
    #
    # ABSOLUTE, because the server resolves `resolve(server_cwd, project_root ??
    # ".")` (server.js:11538) and the server's cwd is not something this hook
    # can observe -- so a relative value, including the default ".", lands
    # somewhere this rule cannot compute. The server's own argument description
    # recommends absolute paths for exactly this reason.
    #
    # And the target's WORKTREE ROOT rather than the target itself, because the
    # server does not use the path it is given: `resolveProjectPaths` runs it
    # through `findGitRoot` (`git rev-parse --show-toplevel`) and adopts that
    # (server.js:1226-1240). Demanding exact equality was stricter than the
    # server's own semantics and refused a write that would have landed
    # correctly -- an agent one directory deep, say `<worktree>/shaft-engine`,
    # passing its own cwd.
    if not isinstance(tool_input, dict):
        return False
    target = tool_input.get("project_root")
    if not isinstance(target, str) or not target.strip() or not os.path.isabs(target):
        return False
    try:
        resolved = worktree_root(os.path.realpath(target))
        if resolved is None:
            return False
        return os.path.normcase(resolved) == os.path.normcase(
            os.path.realpath(root)
        )
    except (OSError, ValueError):
        return False


def check_r11_memory_write_worktree(
    tool_name: str, cwd: str | None, tool_input: dict | None = None
) -> str | None:
    """Return a block reason for an untargeted MCP memory write from a linked worktree."""
    if tool_name not in _MEMORY_WRITE_TOOLS:
        return None
    # `is_linked_worktree` already returns False whenever `worktree_root` is
    # None, so a `root is None` disjunct here could never be the reason and was
    # dead code.
    root = worktree_root(cwd)
    if not is_linked_worktree(cwd):
        return None
    if _targets_this_worktree(tool_input, root):
        return None
    return (
        "R11 (untargeted MCP memory write from a linked worktree): one "
        "shaft-memory server serves the whole session and `.mcp.json` roots it "
        "with `\"cwd\": \".\"`, so an untargeted write resolves against the "
        "server's launch directory rather than this worktree. When those "
        "differ the object lands in another session's tree -- uncommitted, on "
        "a branch that is not yours. Observed twice in one session by two "
        "different agents (issue #4505), and `memory check` passes either way, "
        "because the object is valid; it is only in the wrong tree, so nothing "
        "in your own verification can detect it. Name the destination instead "
        f"of inheriting it: pass an absolute `project_root` of '{root}', which "
        "the server resolves per call, or run `memory remember --stdin` from "
        "this worktree, where the CLI resolves its own root with `git "
        "rev-parse --show-toplevel`."
    )


def evaluate_command(command: str) -> str | None:
    """Return the first blocking reason found, or None if the command is allowed."""
    for check in _CHECKS:
        reason = check(command)
        if reason is not None:
            return reason
    return None


_FIELD_ALIASES = {
    "hookEventName": "hook_event_name",
    "toolName": "tool_name",
    "toolInput": "tool_input",
    "sessionId": "session_id",
    "agentType": "agent_type",
}
_TOOL_ALIASES = {
    "bash": "Bash",
    "powershell": "PowerShell",
    "shell_command": "PowerShell",
    "shellcommand": "PowerShell",
    "read": "Read",
    "grep": "Grep",
    "edit": "Edit",
    "write": "Write",
    "skill": "Skill",
    "agent": "Agent",
    "applypatch": "apply_patch",
    "apply_patch": "apply_patch",
}


def normalize_hook_input(raw: dict) -> dict:
    """Normalize supported host field and tool aliases into one rule input."""
    normalized = dict(raw)
    for source, target in _FIELD_ALIASES.items():
        if target not in normalized and source in raw:
            normalized[target] = raw[source]
    if not normalized.get("hook_event_name") and os.environ.get("GROK_HOOK_EVENT"):
        normalized["hook_event_name"] = os.environ["GROK_HOOK_EVENT"]

    tool_input = normalized.get("tool_input")
    if isinstance(tool_input, dict):
        tool_input = dict(tool_input)
        if "file_path" not in tool_input and "filePath" in tool_input:
            tool_input["file_path"] = tool_input["filePath"]
        normalized["tool_input"] = tool_input

    raw_tool_name = str(normalized.get("tool_name") or "")
    key = re.sub(r"[^a-z_]", "", raw_tool_name.lower())
    normalized["tool_name"] = _TOOL_ALIASES.get(key, raw_tool_name)
    return normalized


def hook_host(raw: dict) -> str:
    """Return output protocol host without affecting shared rule evaluation."""
    configured = os.environ.get("SHAFT_GUARD_HOST", "").strip().lower()
    if configured in {"claude", "codex", "grok"}:
        return configured
    if os.environ.get("GROK_HOOK_EVENT") or any(key in raw for key in _FIELD_ALIASES):
        return "grok"
    return "portable"


def _extract_command(hook_input: dict) -> str:
    tool_input = hook_input.get("tool_input") or {}
    command = tool_input.get("command")
    if isinstance(command, str):
        return command
    return ""


def _hook_working_directory(hook_input: dict) -> str | None:
    """Directory the guarded command will run in, or None when unknown."""
    # Hosts that report the session directory win over the hook process's own
    # directory, which is not guaranteed to be the checkout the command
    # targets.
    supplied = hook_input.get("cwd")
    if isinstance(supplied, str) and supplied.strip():
        return supplied
    try:
        return os.getcwd()
    except OSError:
        return None


def _print_deny(reason: str, host: str) -> None:
    if host == "grok":
        output = {"decision": "deny", "reason": reason}
    else:
        output = {
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": "deny",
                "permissionDecisionReason": reason,
            }
        }
    print(json.dumps(output))


# R12: iron law 3 -- no production code before an observed failing test.
# Production means compiled source under any module's src/main/. Guidance,
# configuration, tests, scripts and docs are excluded because the entrypoint
# excludes them itself: they "may skip test-first; validate their structure or
# affected flow instead".
_PRODUCTION_PATH = re.compile(r"(?:^|/)src/main/", re.IGNORECASE)
_TEST_RUNNER = frozenset({"py", "python", "python3", "pytest", "mvn", "mvnw"})
# Token equality rather than a regex: the file already tokenises segments for
# R1 and R2, and a substring match would read "latest" or "protest" as a test
# run. `-Dtest=` is a prefix rather than a token, so it is checked separately.
_TEST_TOKENS = frozenset({"unittest", "pytest", "surefire", "test", "verify"})
_WRITE_TOOLS = frozenset({"Write", "Edit", "NotebookEdit"})
# Every helper query shares one budget, well inside the 10s PreToolUse
# timeout in .claude/settings.json and .codex/hooks.json. It was 8s, which
# left no margin: one slow `git` or `gh` on a contended machine and the hook
# is killed mid-decision. That matters more since the matcher widened to
# Write|Edit, because this now runs on every edit rather than every command.
SUBPROCESS_TIMEOUT = 4
# Blank line between collected Stop reasons. A named constant rather than an
# inline escape, because an inline one has to survive every future edit to
# this file to keep run_stop parseable, and it did not survive the first.
STOP_REASON_SEPARATOR = "\n\n"


def looks_like_a_test_run(command: str) -> bool:
    """True when this command is plausibly running tests.

    Deliberately generous about what counts. A false positive costs one
    unearned production write; a false negative blocks honest work, and the
    gate that blocks honest work is the gate that gets deleted. The command
    head must still be a real runner in command position, reusing the same
    segmentation R1 and R2 rely on, so prose quoting `mvn test` in a commit
    message does not satisfy the law.
    """
    if not command:
        return False
    for segment in _command_segments(command):
        if not _head_executable_matches(segment, _TEST_RUNNER):
            continue
        tokens = _segment_tokens(segment)
        if _TEST_TOKENS.intersection(tokens) or any(
            token.startswith("-Dtest=") for token in tokens
        ):
            return True
    return False


def check_r12_test_before_production(hook_input: dict, tool_name: str) -> str | None:
    """Block a production-source write when no test run was observed this session.

    Iron law 3 with a mechanism. The law predates every check in this file and
    has never had one: `test_agent_router_contract.py` pins that the sentence
    exists, which cannot observe whether production code was written first.

    Scope is narrow on purpose. Only compiled source under a module's
    `src/main/` counts, because the entrypoint exempts the rest itself --
    documentation, guidance, configuration and generated code "may skip
    test-first". Writing the failing test is never blocked, since blocking the
    RED step would make the law unsatisfiable: the only way to observe a
    failing test is to write it.

    Session-scoped rather than per-edit. One observed test run unlocks
    production writes for the session, which enforces "a test ran before you
    wrote production code" without blocking the second edit of a two-line fix.
    A coarse rule everybody keeps is worth more than a precise one nobody does.
    """
    if tool_name not in _WRITE_TOOLS:
        return None
    tool_input = hook_input.get("tool_input")
    if not isinstance(tool_input, dict):
        return None
    path = tool_input.get("file_path") or tool_input.get("path") or ""
    if not isinstance(path, str) or not path:
        return None
    normalized = path.replace("\\", "/")
    if not _PRODUCTION_PATH.search(normalized) or "/src/test/" in normalized:
        return None
    if not _ledger_path(hook_input):
        return None  # no session, no record, no basis to block
    if "test-run" in ledger_events(hook_input):
        return None
    return (
        "R12 blocked: iron law 3 -- no production code before an observed "
        f"failing test. {path} is production source under src/main/, and no "
        "test run has been observed in this session. Write the focused test "
        "first and run it (for example `py -3 -m unittest tests.scripts.test_x` "
        "or `mvn -Dtest=YourTest test`); an expected assertion failure is RED "
        "and unblocks this write, while a setup, syntax or environment error "
        "is not. Tests, guidance, configuration and docs are never blocked by "
        "this rule."
    )


def _unpushed_commit_count(branch: str) -> int | None:
    """Commits on `branch` that exist on no remote, or None if unanswerable.

    None and 0 stay distinct, which #4542 is the record of paying for: "git
    would not answer" and "nothing would be lost" are opposite facts about
    whether to stop a deletion.
    """
    if not branch:
        return None
    try:
        completed = subprocess.run(  # nosec B603 B607 - fixed read-only git query.
            ["git", "rev-list", "--count", branch, "--not", "--remotes"],
            capture_output=True,
            text=True,
            timeout=SUBPROCESS_TIMEOUT,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if completed.returncode != 0:
        return None
    try:
        return int((completed.stdout or "").strip())
    except ValueError:
        return None


def _uncommitted_file_count(cwd: object) -> int | None:
    """Changed files in the working tree, or None if git will not answer."""
    if not cwd:
        return None
    try:
        completed = subprocess.run(  # nosec B603 B607 - fixed read-only git query.
            ["git", "-c", "core.longpaths=true", "status", "--porcelain"],
            cwd=str(cwd),
            capture_output=True,
            text=True,
            timeout=SUBPROCESS_TIMEOUT,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if completed.returncode != 0:
        return None
    return len([line for line in (completed.stdout or "").splitlines() if line.strip()])


def check_r13_push_before_delete(command: str, tool_name: str) -> str | None:
    """Refuse a force-delete of a branch whose commits exist nowhere else.

    The entrypoint's cleanup order exists because it is not interchangeable:
    push anything a remote has never seen first, then delete. Reversed, the
    only copy of that work is gone.

    Only `-D` is guarded. `git branch -d` already refuses an unmerged branch,
    so git enforces the safe form itself, and restating it here would add
    noise without safety -- the surest way to get a guard removed.
    """
    if tool_name not in ("Bash", "PowerShell") or not command:
        return None
    for segment in _git_segments(command):
        rest = _tokens_after_head(segment, frozenset({"git"}))
        if not rest or rest[0] != "branch" or "-D" not in rest[1:]:
            continue
        for name in [token for token in rest[1:] if not token.startswith("-")]:
            unpushed = _unpushed_commit_count(name)
            if unpushed is None or unpushed <= 0:
                continue
            return (
                f"R13 blocked: {name} carries {unpushed} commit(s) that exist on no "
                "remote, and `git branch -D` would destroy the only copy. The "
                "entrypoint's cleanup order is push first, delete second, and it is "
                f"not interchangeable. Run `git push -u origin {name}` (or confirm the "
                "work is genuinely disposable) before deleting. `git branch -d` is "
                "unaffected: git already refuses an unmerged branch itself."
            )
    return None


def check_r14_hard_reset(command: str, tool_name: str, cwd: object) -> str | None:
    """Refuse `git reset --hard` while the working tree carries uncommitted work.

    Written after it happened. Setting up a probe branch for R13, this file's
    author ran `git reset --hard HEAD~1` with R13's implementation and tests
    uncommitted; both were destroyed instantly, and nothing here caught it.
    R8 guarded `git stash`, R9 `git worktree add`, R13 `git branch -D` -- and
    the most destructive command of the four was unguarded.

    `--hard` alone triggers this. A soft or mixed reset leaves the working
    tree alone, and `--hard` on a clean tree destroys nothing, so neither is
    this rule's business.
    """
    if tool_name not in ("Bash", "PowerShell") or not command:
        return None
    for segment in _git_segments(command):
        rest = _tokens_after_head(segment, frozenset({"git"}))
        if not rest or rest[0] != "reset" or "--hard" not in rest[1:]:
            continue
        changed = _uncommitted_file_count(cwd)
        if changed is None or changed <= 0:
            continue
        return (
            f"R14 blocked: the working tree has {changed} uncommitted file(s) and "
            "`git reset --hard` discards them with no reflog entry and no recovery. "
            "Commit them (`git add -A && git commit`), or stash deliberately, or "
            "reset without `--hard` if you only meant to move the branch pointer. "
            "This rule exists because an agent building the neighbouring guard lost "
            "a finished, tested change to exactly this command."
        )
    return None


def _independent_review_count(target: str | None) -> int | None:
    """Reviews by someone other than the author, or None if unanswerable.

    None and 0 stay distinct: "gh could not answer" and "nobody has reviewed
    this" are opposite facts about whether to stop an irreversible step.
    """
    arguments = ["gh", "pr", "view"]
    if target:
        arguments.append(target)
    arguments += ["--json", "reviews,author"]
    try:
        completed = subprocess.run(  # nosec B603 B607 - fixed read-only gh query.
            arguments, capture_output=True, text=True, timeout=SUBPROCESS_TIMEOUT, check=False
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if completed.returncode != 0:
        return None
    try:
        payload = json.loads(completed.stdout or "{}")
    except ValueError:
        return None
    if not isinstance(payload, dict):
        return None
    author = (payload.get("author") or {}).get("login")
    reviews = payload.get("reviews")
    if not isinstance(reviews, list):
        return None
    return len(
        [
            review
            for review in reviews
            if isinstance(review, dict)
            and (review.get("author") or {}).get("login")
            and (review.get("author") or {}).get("login") != author
        ]
    )


def check_r15_review_before_arming(command: str, tool_name: str) -> str | None:
    """Refuse arming auto-merge before an independent review exists.

    Iron law 6 requires an independent adversarial review before the next step
    starts, and the Ownership section requires arming only once that gate
    passes. Handing a diff to auto-merge is the one irreversible step in the
    whole workflow -- after it, the next green run merges without asking --
    and it rested entirely on remembering.

    A review by the pull request's own author does not count. The point is an
    independent reader, and self-review is precisely the shape
    `constraint.always-address-pr-review-comments-not-just-ci-checks-and-merge-conflicts`
    was written against.
    """
    if tool_name not in ("Bash", "PowerShell") or not command:
        return None
    for segment in _command_segments(command):
        rest = _tokens_after_head(segment, frozenset({"gh"}))
        if not rest or rest[:2] != ["pr", "merge"]:
            continue
        arguments = rest[2:]
        positional = [token for token in arguments if not token.startswith("-")]
        target = positional[0] if positional else None
        reviews = _independent_review_count(target)
        if reviews is None or reviews > 0:
            continue
        label = f"#{target}" if target else "this pull request"
        return (
            f"R15 blocked: {label} has no review from anyone other than its author, "
            "and iron law 6 requires an independent adversarial review before the "
            "next step starts. Arming auto-merge is the irreversible one: after it, "
            "the next green run merges without asking. Get a separate instance to "
            "review the actual diff -- and address bot annotations as well as human "
            "comments -- then arm. If a review exists and this still fires, `gh` "
            "could not read it; that case is allowed through."
        )
    return None


DEFAULT_BRANCHES = frozenset({"main", "master"})


def check_r19_fresh_base(hook_input: dict, tool_name: str) -> str | None:
    """Refuse a write while HEAD is the default branch.

    Task isolation requires a fresh `ChaosEngine/*` branch cut from fetched
    `origin/main` before task-specific edits. Only part of that is
    mechanisable, and this is deliberately only that part.

    Editing on `main` is unambiguous and always wrong: the work has no branch
    of its own, cannot become a pull request without a later rescue, and
    collides with anything else sharing the checkout.

    Whether an existing `ChaosEngine/*` branch is fresh *enough* is judgement
    -- the entrypoint explicitly permits reusing one for dependent work in the
    same task -- so a hook that guessed would block legitimate continuation
    every time it was right about nothing. A gate that fires on correct work
    is the gate that gets deleted, so this one does not guess.

    The remedy it names carries uncommitted changes with it and touches
    nothing, so it can never trap an agent that already has work in flight.
    """
    if tool_name not in _WRITE_TOOLS:
        return None
    branch = _current_branch(hook_input.get("cwd"))
    if not branch or branch not in DEFAULT_BRANCHES:
        return None
    return (
        f"R19 blocked: HEAD is {branch}, and task work never lands on the default "
        "branch. Cut the session's branch first -- `git fetch --prune origin && git "
        "checkout -b ChaosEngine/<task> origin/main` -- which carries any uncommitted "
        "changes across and touches nothing. Reusing an existing ChaosEngine/* branch "
        "for dependent work in the same task is fine and is not blocked."
    )


def run_pretooluse(hook_input: dict, host: str = "portable") -> int:
    tool_name = hook_input.get("tool_name", "")

    reason = check_r11_memory_write_worktree(
        tool_name,
        _hook_working_directory(hook_input),
        hook_input.get("tool_input"),
    )
    if reason is not None:
        _print_deny(reason, host)
        return 0

    reason = check_r19_fresh_base(hook_input, tool_name)
    if reason is not None:
        _print_deny(reason, host)
        return 0

    reason = check_r12_test_before_production(hook_input, tool_name)

    # R16 reads these. Recorded here because a later hook invocation is a
    # fresh process: if the event is not written down as it happens, the
    # Stop hook has no way to know it ever did.
    if tool_name.startswith("mcp__shaft-memory__"):
        ledger_record(hook_input, "memory-write")
    if reason is not None:
        _print_deny(reason, host)
        return 0

    if tool_name in ("Bash", "PowerShell"):
        command = _extract_command(hook_input)
        if not command:
            return 0
        reason = evaluate_command(command)
        if reason is None:
            reason = check_r9_worktree_add(command, tool_name)
        if reason is None:
            reason = check_r10_nul_corruption(command, _hook_working_directory(hook_input))
        if reason is None:
            reason = check_r13_push_before_delete(command, tool_name)
        if reason is None:
            reason = check_r15_review_before_arming(command, tool_name)
        if reason is None:
            reason = check_r14_hard_reset(
                command, tool_name, _hook_working_directory(hook_input)
            )
        if reason is not None:
            _print_deny(reason, host)
            return 0
        # Observed, not judged. R12 reads this ledger; recording here is the
        # only place a test run is visible, since a later hook invocation is a
        # fresh process with no memory of it.
        if looks_like_a_test_run(command):
            ledger_record(hook_input, "test-run")
        if any(
            _tokens_after_head(segment, frozenset({"git"}))[:1] == ["commit"]
            for segment in _git_segments(command)
            if _tokens_after_head(segment, frozenset({"git"}))
        ):
            ledger_record(hook_input, "commit")
        return 0

    return 0  # not a tool this hook checks


def _harness_root() -> str:
    return os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def _worktree_report(cwd: str | None) -> dict | None:
    """Run the existing read-only hygiene reporter, without importing it circularly."""
    helper = os.path.join(_harness_root(), "scripts", "ci", "worktree_hygiene.py")
    if not cwd or not os.path.isfile(helper):
        return None
    try:
        completed = subprocess.run(  # nosec B603 - fixed repository helper.
            [sys.executable, helper, "--root", cwd, "--format", "json"],
            cwd=_harness_root(),
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
        if completed.returncode != 0:
            return None
        result = json.loads(completed.stdout)
        return result if isinstance(result, dict) else None
    except (OSError, subprocess.SubprocessError, json.JSONDecodeError):
        return None


def _sync_advisory() -> str | None:
    """Return a user-harness drift advisory; never mutate deployed state."""
    helper = os.path.join(_harness_root(), "scripts", "agents", "sync_user_harness.py")
    if not os.path.isfile(helper):
        return None
    try:
        completed = subprocess.run(  # nosec B603 - fixed repository helper.
            [sys.executable, helper],
            cwd=_harness_root(),
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return "User harness sync check was unavailable; verify it before completion."
    if completed.returncode == 0:
        return None
    return (
        "User harness drift detected. Review the tracked harness, then run "
        "`py -3 scripts/agents/sync_user_harness.py --apply` and re-check it."
    )


def _ledger_path(hook_input: dict) -> str | None:
    """Return this session's ledger file, or None when one cannot be sited.

    Keyed by session, never by repository. Concurrent agents each own a
    worktree and run their own hooks, so a repository-keyed ledger would let
    one delegate's test run unlock a production write for a different one --
    and a gate somebody else can satisfy is not a gate.

    Sited outside the repository on purpose: this is runtime state, and
    `AGENTS.md` forbids generated files in git. `session_id` is already
    normalised across Claude, Codex and Grok by `_FIELD_ALIASES`, so this
    needs no per-host branch.
    """
    session = hook_input.get("session_id")
    if not isinstance(session, str) or not session.strip():
        return None
    # Hashed rather than interpolated: a session id is host-supplied and would
    # otherwise reach the filesystem as a path component.
    key = hashlib.sha256(session.strip().encode("utf-8")).hexdigest()[:32]
    directory = os.path.join(tempfile.gettempdir(), "shaft-agent-ledger")
    try:
        os.makedirs(directory, exist_ok=True)
    except OSError:
        return None
    return os.path.join(directory, f"{key}.json")


def ledger_record(hook_input: dict, event: str) -> bool:
    """Append one observed event to this session's ledger. True when recorded.

    Read-modify-write rather than an append-only text file, because the
    reader has to tolerate a partial line from a concurrent hook. A lost
    event costs one re-observation; a corrupt read that raises would cost the
    session.
    """
    path = _ledger_path(hook_input)
    if not path or not isinstance(event, str) or not event:
        return False
    events = ledger_events(hook_input)
    events.append(event)
    try:
        with open(path, "w", encoding="utf-8") as handle:
            json.dump(events, handle)
    except (OSError, ValueError):
        return False
    return True


def ledger_events(hook_input: dict) -> list[str]:
    """Return the events observed so far in this session, oldest first.

    Every failure reads as "nothing observed yet". That is the safe direction
    for a *record* of what happened; the gates built on it decide separately
    whether an empty record should block, and each of those fails open.
    """
    path = _ledger_path(hook_input)
    if not path or not os.path.isfile(path):
        return []
    try:
        with open(path, encoding="utf-8") as handle:
            events = json.load(handle)
    except (OSError, ValueError, UnicodeDecodeError):
        return []
    if not isinstance(events, list):
        return []
    return [item for item in events if isinstance(item, str)]


def _standing_constraints(working_directory: object) -> str | None:
    """Return the titles of every stored constraint, as one injectable block.

    The harness's retrieval duty lives in `routing.md`, which is loaded only
    when the entrypoint routes a deliverable -- so the rule that says "query
    the stores before broad discovery" sits behind a load it is meant to
    precede. Reminding harder is the mitigation the literature measures and
    finds insufficient, so this does not remind: it carries the constraints in
    before the first tool call, which costs the agent no adherence and cannot
    decay with context length.

    Titles only. Twelve objects are ~950 bytes of title against tens of
    kilobytes of body, and a title is enough to know a constraint exists and
    go read it -- which is all an always-injected index has to achieve.
    Bodies stay behind `memory inspect`, where they cost nothing until wanted.

    Fails open in every direction. A repository with no store, an unreadable
    object, or malformed JSON yields no block rather than a broken session:
    this runs before every task on every host, so its worst failure mode must
    be silence, never a session that cannot start.
    """
    if not working_directory:
        return None
    # `os.path` throughout, because this module imports no `pathlib` and one
    # convenience import is not worth a second path idiom in a hook that has
    # to stay portable and start fast.
    directory = os.path.join(str(working_directory), ".memory", "memory", "constraints")
    if not os.path.isdir(directory):
        return None
    titles: list[str] = []
    for name in sorted(os.listdir(directory)):
        if not name.endswith(".json"):
            continue
        try:
            with open(os.path.join(directory, name), encoding="utf-8") as handle:
                title = json.load(handle).get("title")
        except (OSError, ValueError, AttributeError):
            continue
        if isinstance(title, str) and title.strip():
            titles.append(title.strip())
    if not titles:
        return None
    listed = "\n".join(f"- {title}" for title in titles)
    return (
        f"Standing constraints already stored ({len(titles)}), so they need no "
        "recall:\n"
        f"{listed}\n"
        "Read one with `memory inspect <id>`. For anything task-specific run "
        '`memory load "<task>"`, and consult MemPalace for history spanning '
        "sessions and Graphify for blast radius, before broad manual discovery."
    )


def run_session_start(hook_input: dict) -> int:
    """Inject the mandatory entrypoint plus read-only hygiene and sync findings."""
    context = [
        "Harness preflight: load and follow "
        "`.agents/skills/act-as-mohab/SKILL.md` before task work."
    ]
    constraints = _standing_constraints(_hook_working_directory(hook_input))
    if constraints:
        context.append(constraints)
    report = _worktree_report(_hook_working_directory(hook_input))
    if report is None:
        context.append("Worktree hygiene could not be verified; inspect it before cleanup.")
    else:
        context.extend(str(item) for item in report.get("advisories", []))
    sync = _sync_advisory()
    if sync:
        context.append(sync)
    print(
        json.dumps(
            {
                "hookSpecificOutput": {
                    "hookEventName": "SessionStart",
                    "additionalContext": "\n".join(context),
                }
            }
        )
    )
    return 0


def _open_pull_request_count(branch: str | None) -> int | None:
    """Open pull requests for `branch`, or None when the question cannot be answered.

    None and 0 are different facts and must stay different: "the lookup did
    not run" versus "it ran and found none". Collapsing them is #4542, where
    the Stop hook read an unperformed lookup as an absent pull request and
    blocked a delivered branch on every turn.

    One bounded call for one branch, not a survey. `worktree_hygiene.py` owns
    the same query behind `--check-pull-requests`, but the Stop hook runs it
    across every worktree under a 10-second budget, and this needs to answer
    for the current branch only.
    """
    if not branch:
        return None
    try:
        completed = subprocess.run(  # nosec B603 B607 - fixed read-only gh query.
            [
                "gh",
                "pr",
                "list",
                "--head",
                branch,
                "--state",
                "open",
                "--json",
                "number",
            ],
            capture_output=True,
            text=True,
            timeout=SUBPROCESS_TIMEOUT,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if completed.returncode != 0:
        return None
    try:
        listed = json.loads(completed.stdout or "[]")
    except ValueError:
        return None
    return len(listed) if isinstance(listed, list) else None


def check_r16_learning_loop(hook_input: dict) -> str | None:
    """Interrupt once when a session changed things and routed no learning.

    The entrypoint requires the learned-lessons workflow before reporting
    done, and it had no mechanism. This session is the evidence: an iteration
    reported done having skipped the mandatory retrieval entirely, and the
    owner caught it rather than any check.

    A reminder that blocks once, not a hard gate, and the distinction is
    load-bearing. "Nothing durable surfaced" is a legitimate outcome the
    entrypoint explicitly endorses, so a rule that could not be satisfied by
    saying so would manufacture memory objects rather than learnings. And a
    delegate in a linked worktree cannot write memory at all -- R11 refuses it
    by design -- so a hard gate would strand every worktree agent
    permanently. `run_stop` returns 0 when `stop_hook_active` is set, so the
    second attempt always proceeds.
    """
    events = ledger_events(hook_input)
    if "commit" not in events:
        return None  # a read-only session owes no learning
    if "memory-write" in events:
        return None
    return (
        "Learning loop: this session committed work and routed no learning. Before "
        "reporting done, run the routing table once -- a fact that cost you time to "
        "native Memory with its evidence, a decision someone would re-litigate as a "
        "decision, a cross-entity relation to MemPalace, a structural change flagged "
        "for Graphify, a procedure that misled you fixed in the guidance file that "
        "should have carried it, and adjacent work you skipped searched for and then "
        "filed. Nothing durable is a valid result -- say so and end the turn; this "
        "interrupts once and will not ask again."
    )


def _unarmed_reviewed_pull_request(cwd: object) -> str | None:
    """PR number when one is reviewed and still unarmed, else None.

    Returns None for every uncertainty and for every earlier pipeline stage:
    no pull request, no independent review yet, already armed, or `gh` unable
    to answer. Only the one actionable state produces a number.
    """
    arguments = [
        "gh",
        "pr",
        "view",
        "--json",
        "number,autoMergeRequest,reviews,author",
    ]
    try:
        completed = subprocess.run(  # nosec B603 B607 - fixed read-only gh query.
            arguments,
            cwd=str(cwd) if cwd else None,
            capture_output=True,
            text=True,
            timeout=SUBPROCESS_TIMEOUT,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if completed.returncode != 0:
        return None
    try:
        payload = json.loads(completed.stdout or "{}")
    except ValueError:
        return None
    if not isinstance(payload, dict) or payload.get("autoMergeRequest"):
        return None
    author = (payload.get("author") or {}).get("login")
    reviews = payload.get("reviews")
    if not isinstance(reviews, list):
        return None
    independent = [
        review
        for review in reviews
        if isinstance(review, dict)
        and (review.get("author") or {}).get("login")
        and (review.get("author") or {}).get("login") != author
    ]
    if not independent:
        return None  # R15 would refuse arming; demanding it here would deadlock
    number = payload.get("number")
    return str(number) if number else None


def check_r17_unarmed_pull_request(hook_input: dict) -> str | None:
    """Report a reviewed pull request that nobody armed.

    Opening a pull request does not end the duty -- the entrypoint says arm
    auto-merge once the review gate passes, then watch until the remote
    confirms merged. A reviewed pull request left unarmed is the precise
    silence that rule exists to prevent: nothing is waiting, and nothing will
    merge.

    Fires only once a review exists, and that condition is load-bearing.
    Blocking on any unarmed pull request would deadlock against R15, which
    refuses `gh pr merge --auto` without an independent review: Stop would
    demand arming while R15 refused it, leaving no legal state and making the
    deletion of one guard the cheapest exit, which iron law 4 forbids.
    """
    number = _unarmed_reviewed_pull_request(hook_input.get("cwd"))
    if not number:
        return None
    return (
        f"Pull request #{number} has an independent review and auto-merge is not "
        "armed. Opening a pull request does not end the duty: arm it now with "
        f"`gh pr merge {number} --auto --squash`, then watch with `py -3 "
        "scripts/ci/watch_pr_checks.py` until the remote confirms merged. Red and "
        "conflicting are yours to fix; stale emits no event, so ask for it. This "
        "interrupts once."
    )


def _current_branch(cwd: object) -> str | None:
    """Current branch name, or None when detached or unanswerable."""
    try:
        completed = subprocess.run(  # nosec B603 B607 - fixed read-only git query.
            ["git", "rev-parse", "--abbrev-ref", "HEAD"],
            cwd=str(cwd) if cwd else None,
            capture_output=True,
            text=True,
            timeout=SUBPROCESS_TIMEOUT,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if completed.returncode != 0:
        return None
    name = (completed.stdout or "").strip()
    return name if name and name != "HEAD" else None


def check_r18_unpushed_work(hook_input: dict) -> str | None:
    """Report commits that exist on no remote at the end of a turn.

    #4538 and #4530, reduced to the half a mechanism can hold. The owner
    standard was set after a delegate ran 25 minutes with nothing pushed:
    everything it had completed existed in one worktree on one machine. A
    branch is recoverable only by whoever can see that machine; a pushed
    branch is recoverable by anyone.

    The five-minute interval is the practice, and no hook can observe it
    without a wall clock the agent does not share. Unpushed-at-turn-end is the
    failure the interval exists to prevent, and it is observable exactly when
    it matters -- at the moment the session might end for good.

    Fires only when a branch exists: a detached HEAD has nothing to push, and
    treating inapplicable as unpushed would demand an impossible remedy, the
    collapse #4542 was filed for. Aligned with R13 rather than opposed to it --
    both are satisfied by the same `git push`.
    """
    branch = _current_branch(hook_input.get("cwd"))
    if not branch:
        return None
    unpushed = _unpushed_commit_count(branch)
    if unpushed is None or unpushed <= 0:
        return None
    return (
        f"{branch} carries {unpushed} commit(s) that exist on no remote. Work only "
        "this machine can see is lost if this session ends here, and a pushed branch "
        f"is recoverable by anyone. Run `git push -u origin {branch}` before ending "
        "the turn. This interrupts once."
    )


def run_stop(hook_input: dict) -> int:
    """Continue incomplete repository work once, without creating a Stop loop."""
    if hook_input.get("stop_hook_active") is True:
        return 0

    # Collected, never short-circuited. Returning on the first reason meant
    # exactly one Stop rule could ever fire per session: `stop_hook_active`
    # makes the second attempt return 0 immediately, so whichever rule was
    # listed first starved every rule below it. Each Stop rule added made
    # that worse. It is also better for the reader -- an agent ending its
    # turn learns everything it owes at once, instead of discovering the
    # next duty only after satisfying the previous one.
    reasons = [
        item
        for item in (
            check_r16_learning_loop(hook_input),
            check_r17_unarmed_pull_request(hook_input),
            check_r18_unpushed_work(hook_input),
        )
        if item is not None
    ]
    report = _worktree_report(_hook_working_directory(hook_input))
    if report is None:
        reason = "Completion hygiene could not be verified; inspect the current worktree."
    else:
        current = next(
            (item for item in report.get("worktrees", []) if item.get("is_current")),
            None,
        )
        state = current.get("state") if current else None
        # #4542: `pending` is decided by commit count alone, so a branch that
        # is clean, pushed and covered by an open pull request blocked here on
        # every turn -- the work was delivered and the hook never asked. Ask
        # now, for this branch only, and fail open: a machine with no `gh`,
        # no credentials or no network must not be stranded, which the
        # requirement of any agent on any machine makes non-negotiable.
        # Confirmed zero still blocks, because commits nobody else can see are
        # exactly what this check is for.
        # Only ask where the question applies. A pending worktree on no branch
        # -- a detached HEAD holding unique commits -- cannot be covered by a
        # pull request at all, so it keeps blocking: that work has no delivery
        # vehicle, which is the case this check most needs to catch. Treating
        # "inapplicable" as "unknown" would fail it open, the same collapse
        # #4542 is about, one level up.
        if state == "pending":
            branch = current.get("branch") if current else None
            if branch:
                covered = _open_pull_request_count(branch)
                if covered is None or covered > 0:
                    # Delivered. Not a `return`: reasons collected above this
                    # point would be discarded, which is the starvation this
                    # refactor exists to remove. An unmapped state simply
                    # contributes nothing.
                    state = "delivered"
        completion_route = (
            "Re-read the act-as-mohab Completion section and apply its routed, "
            "authorization-aware preservation, validation, delivery, and cleanup steps."
        )
        # `state_reasons`, not `reasons`: the collected list above owns that
        # name, and shadowing it here made every worktree state raise instead
        # of report.
        state_reasons = {
            "corrupt": (
                "Current worktree contains NUL-corrupt files. Preserve healthy work and "
                "restore only confirmed corrupt paths before continuing."
            ),
            "uncommitted": (
                "Current worktree has uncommitted work. " + completion_route
            ),
            "unknown": (
                "Current worktree state is unknown. Inspect it and preserve any useful "
                "work before cleanup."
            ),
            "pending": (
                "Current branch still carries pending work. " + completion_route
            ),
            "abandoned": (
                "Current work appears abandoned. " + completion_route
            ),
        }
        reason = (
            state_reasons.get(state)
            if state is not None
            else "The current worktree could not be identified; inspect it before stopping."
        )
    if reason is not None:
        reasons.append(reason)
    if not reasons:
        return 0
    print(json.dumps({"decision": "block", "reason": STOP_REASON_SEPARATOR.join(reasons)}))
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
    ("ii after && in command position still blocked", "git status && ii report.html", True),
    ("ii inside quoted regex char class in a py -c argument is not blocked (issue: R3 false positive)",
     "py -3 -c \"import re; p = re.compile(r'[Ii]mplement')\"", False),
    ("start inside quoted text in a py -c argument is not blocked (sibling check, same root cause)",
     "py -3 -c \"s = 'start something'\"", False),
    ("--start-maximized not blocked", "chromedriver --start-maximized", False),
    ("restart not blocked", "sudo systemctl restart nginx", False),
    ("capture_start not blocked", "mcp__shaft-mcp__capture_start", False),
    ("start after semicolon blocked", "git status; start chrome", True),
    ("start after && blocked", "git status && start chrome", True),
    ("start after pipe blocked", "echo hi | start", True),
    ("start after & blocked", "git status & start chrome", True),
    ("start mid-word in later segment not blocked", "git status && echo restart-service", False),
    ("empty command allowed", "", False),

    # --- R3 GUI-word verbs (Start-Process/Invoke-Item/rundll32/os.startfile/
    # explorer): realistic REAL invocation shapes must stay blocked, including
    # ones where the verb is not the literal first token (issue #4147) ---
    ("Start-Process after && separator", "git status && Start-Process notepad", True),
    ("Invoke-Item after ; separator", "git status; Invoke-Item report.html", True),
    ("rundll32 after & separator", "git status & rundll32 shell32.dll,OpenAs_RunDLL report.html", True),
    ("Start-Process after | separator", "echo hi | Start-Process notepad", True),
    ("Start-Process after PowerShell call operator", "& Start-Process notepad", True),
    ("Start-Process on right side of an assignment", "$result = Start-Process notepad -PassThru", True),
    ("rundll32 with a bash env-assignment prefix", "FOO=1 rundll32 shell32.dll,OpenAs_RunDLL report.html", True),
    ("Start-Process nested inside powershell -Command \"...\"",
     'powershell -Command "Start-Process notepad"', True),
    ("Invoke-Item nested inside powershell -c \"...\" (short flag)",
     'powershell -c "Invoke-Item report.html"', True),
    ("rundll32 nested inside cmd /c \"...\"",
     'cmd /c "rundll32 shell32.dll,OpenAs_RunDLL report.html"', True),
    ("os.startfile nested inside py -3 -c \"...\" (this repo's documented convention)",
     "py -3 -c \"import os; os.startfile('report.html')\"", True),

    # --- R3 `cmd /c start`: a quote between /c and the verb must not defeat
    # the structural check (issue #4152, false negative -- the dangerous
    # direction). ---
    ("cmd /c \"start ...\" (quoted) must stay blocked -- issue #4152",
     'cmd /c "start report.html"', True),
    ("cmd.exe /c \"start ...\" (quoted, .exe form) must stay blocked -- issue #4152",
     'cmd.exe /c "start https://example.com"', True),
    ("git commit -m prose mentioning the verb stays allowed alongside the #4152 fix",
     'git commit -m "we never start a browser"', False),

    # --- R3 GUI-word verbs: quoted PROSE merely discussing a denylisted verb
    # must NOT block -- reproduced live this session via `git commit -m` and
    # `gh issue create --body` (issue #4147) ---
    ("git commit -m mentioning 'explorer' in prose is not a real command",
     'git commit -m "explorer word appears in this commit message only"', False),
    ("gh issue create --body mentioning 'rundll32' in prose is not a real command",
     'gh issue create --body "this body discusses rundll32 in prose only"', False),
    ("gh pr create --title mentioning 'Start-Process' in prose is not a real command",
     'gh pr create --title "Fix Start-Process false positive"', False),
    ("bash heredoc PR body mentioning 'explorer' is prose, not a command",
     "gh pr create --body-file - <<'EOF'\nThis body discusses explorer in prose.\nEOF", False),
    ("multi-line quoted body mentioning 'Invoke-Item' is prose, not a command",
     'gh pr create --title "Fix" --body "Notes:\nInvoke-Item was mentioned here.\nAll good."', False),
    ("inline quoted tag mentioning 'test-explorer' is prose, not a command (mempalace precedent)",
     'echo "tag: test-explorer"', False),

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

    # --- R8: git stash mutating subcommands blocked, read-only allowed (issue #4130) ---
    ("bare git stash blocked", "git stash", True),
    ("git stash push blocked", "git stash push", True),
    ("git stash pop blocked", "git stash pop", True),
    ("git stash drop blocked", "git stash drop", True),
    ("git stash apply blocked", "git stash apply", True),
    ("git stash clear blocked", "git stash clear", True),
    ("git stash list allowed (read-only)", "git stash list", False),
    ("git stash show allowed (read-only)", "git stash show stash@{0}", False),
    ("git stash pop with stash ref still blocked", "git stash pop stash@{1}", True),
    ("non-stash git command allowed", "git status", False),
    ("git stash mentioned in commit message prose is not a real command",
     'git commit -m "ran git stash pop earlier"', False),

    # --- R10: staging/committing is never blocked by command SHAPE alone
    # (issue #4437). R10 reads repository state and is dispatched with a
    # working directory, exactly like R9's tool-name-dependent checks. These
    # rows fail the moment someone moves R10 into `_CHECKS`, which would make
    # every `git add`/`git commit` deniable from the command string with no
    # repository to justify it. The corruption behaviour itself is proven
    # against a real fixture in run_r10_nul_corruption_self_test. ---
    ("git add -A allowed without repository context", "git add -A", False),
    ("git add . allowed without repository context", "git add .", False),
    ("git commit allowed without repository context", 'git commit -m "message"', False),
    ("git commit -am allowed without repository context", 'git commit -am "message"', False),
    ("git stage allowed without repository context", "git stage src/Example.java", False),
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

def run_r9_worktree_self_test() -> int:
    """Exercises R9: git worktree add backslash-via-Bash + missing-longpaths guardrails."""
    failures: list[str] = []

    def check(description: str, condition: bool) -> None:
        status = "PASS" if condition else "FAIL"
        print(f"[{status}] {description}")
        if not condition:
            failures.append(description)

    # --- B1: backslash worktree path via Bash is denied ---
    reason = check_r9_worktree_add(
        r"git -c core.longpaths=true worktree add worktrees\w4067 -b ChaosEngine/w4067 origin/main",
        "Bash",
    )
    check("Bash backslash worktree path (with longpaths) is denied", reason is not None)
    check("Bash backslash denial names forward slashes", reason is not None and "forward slash" in reason)

    # --- B1 does not fire for PowerShell (backslashes are normal there) ---
    reason = check_r9_worktree_add(
        r"git -c core.longpaths=true worktree add worktrees\w4067 -b ChaosEngine/w4067 origin/main",
        "PowerShell",
    )
    check("PowerShell backslash worktree path is allowed (B1 is Bash-only)", reason is None)

    # --- B2: missing -c core.longpaths=true is denied (both tools), with a corrected command ---
    reason = check_r9_worktree_add(
        "git worktree add worktrees/w4067 -b ChaosEngine/w4067 origin/main",
        "Bash",
    )
    check("Bash worktree add missing longpaths is denied", reason is not None)
    check("longpaths denial includes the corrected command", reason is not None and "core.longpaths=true" in reason)

    reason = check_r9_worktree_add(
        "git worktree add worktrees/w4067 -b ChaosEngine/w4067 origin/main",
        "PowerShell",
    )
    check("PowerShell worktree add missing longpaths is denied too (B2 applies to both tools)", reason is not None)

    # --- Fully correct invocation (forward slashes + longpaths) is allowed ---
    reason = check_r9_worktree_add(
        "git -c core.longpaths=true worktree add worktrees/w4067 "
        "-b ChaosEngine/w4067 origin/main",
        "Bash",
    )
    check("Correct invocation (forward slashes + longpaths) is allowed", reason is None)

    # --- Not a worktree-add command at all: fails open ---
    reason = check_r9_worktree_add(r"git worktree list", "Bash")
    check("git worktree list (no add) is allowed", reason is None)

    reason = check_r9_worktree_add(r"git status", "Bash")
    check("unrelated git command is allowed", reason is None)

    # --- Prose mention in a quoted string is not a real command ---
    reason = check_r9_worktree_add(
        'git commit -m "ran git worktree add worktrees\\w4067 earlier"', "Bash"
    )
    check("git worktree add mentioned in commit message prose is not a real command", reason is None)

    # --- B3: non-ChaosEngine/* branch via -b is denied (issue #4496) ---
    reason = check_r9_worktree_add(
        "git -c core.longpaths=true worktree add worktrees/x "
        "-b not-chaos-engine/whatever origin/main",
        "Bash",
    )
    check("non-ChaosEngine/* branch via -b is denied", reason is not None)
    check(
        "branch-prefix denial names ChaosEngine/",
        reason is not None and "ChaosEngine/" in reason,
    )

    # --- B3 also applies to -B (force-create/reset) and to PowerShell ---
    reason = check_r9_worktree_add(
        "git -c core.longpaths=true worktree add worktrees/x "
        "-B not-chaos-engine/whatever origin/main",
        "PowerShell",
    )
    check("non-ChaosEngine/* branch via -B is denied on PowerShell too", reason is not None)

    # --- --detach (no branch created at all) stays allowed ---
    reason = check_r9_worktree_add(
        "git -c core.longpaths=true worktree add worktrees/probe --detach origin/main",
        "Bash",
    )
    check("git worktree add --detach (no branch created) stays allowed", reason is None)

    # --- Checking out an existing branch (no -b/-B) stays allowed ---
    reason = check_r9_worktree_add(
        "git -c core.longpaths=true worktree add worktrees/x not-chaos-engine/existing",
        "Bash",
    )
    check(
        "worktree add checking out an existing branch (no -b/-B) stays allowed",
        reason is None,
    )

    # --- A non-conforming branch mentioned in commit-message prose is not a real command ---
    reason = check_r9_worktree_add(
        'git commit -m "use git worktree add x -b not-chaos-engine/y next time"',
        "Bash",
    )
    check(
        "git worktree add -b mentioned in commit message prose is not a real command",
        reason is None,
    )

    total_checks = len(failures)
    print(f"\nR9 worktree-add self-test summary: {total_checks} failed.")
    if failures:
        print("Failed cases: " + ", ".join(failures))
        return 1
    return 0


def run_r10_nul_corruption_self_test() -> int:
    """Exercise R10 against a real NUL-corrupted file in a throwaway repository."""
    # The 2026-08-04 corruption was invisible to inspection -- `git status`
    # showed ordinary ' M' entries -- so this builds the failure on disk and
    # runs the real git rather than asserting against a hand-written diff.
    #
    # Deliberately overlaps tests/scripts/test_guard_nul_corruption.py: the
    # guard ships to hosts that have neither this repository's test runner nor
    # its test tree, and `--self-test` is the only way to check it there.
    import shutil
    import tempfile

    failures: list[str] = []

    def check(description: str, condition: bool) -> None:
        status = "PASS" if condition else "FAIL"
        print(f"[{status}] {description}")
        if not condition:
            failures.append(description)

    if shutil.which("git") is None:
        print("[SKIP] R10 self-test: git is unavailable on PATH")
        return 0

    with tempfile.TemporaryDirectory() as directory:
        def git(*arguments: str) -> int:
            return subprocess.run(  # nosec B603 B607 - fixed git commands on a temp fixture.
                ["git", *arguments],
                cwd=directory,
                capture_output=True,
                text=True,
                check=False,
            ).returncode

        git("init", "-q", "-b", "main", ".")
        git("config", "user.email", "harness@example.invalid")
        git("config", "user.name", "Harness")
        source = os.path.join(directory, "Example.java")
        with open(source, "wb") as handle:
            handle.write(b"class Example {\n}\n")
        healthy = os.path.join(directory, "notes.md")
        with open(healthy, "wb") as handle:
            handle.write(b"# Notes\n")
        git("add", "-A")
        git("commit", "-qm", "initial")

        reason = check_r10_nul_corruption("git add -A", directory)
        check("clean worktree is allowed", reason is None)

        with open(healthy, "wb") as handle:
            handle.write(b"# Notes\n\nAn ordinary edit.\n")
        reason = check_r10_nul_corruption("git add -A", directory)
        check("ordinary text edit is allowed", reason is None)

        # The incident shape: a plausible size, every byte zero.
        with open(source, "wb") as handle:
            handle.write(b"\x00" * 68)

        reason = check_r10_nul_corruption("git add -A", directory)
        check("NUL-filled tracked file blocks `git add`", reason is not None)
        check(
            "denial names the corrupt path",
            reason is not None and "Example.java" in reason,
        )
        check(
            "denial names the restore command",
            reason is not None and "git restore" in reason,
        )

        reason = check_r10_nul_corruption("git status && git add -A", directory)
        check("staging chained after a read-only git command is inspected", reason is not None)

        reason = check_r10_nul_corruption("git add notes.md", directory)
        check("an explicit healthy pathspec is allowed", reason is None)

        reason = check_r10_nul_corruption("git add Example.java", directory)
        check("an explicit corrupt pathspec is denied", reason is not None)

        with tempfile.TemporaryDirectory() as elsewhere:
            reason = check_r10_nul_corruption(f'git -C "{directory}" add -A', elsewhere)
            check("`git -C <repo>` inspects the repository it names", reason is not None)

        git("add", "-A")
        reason = check_r10_nul_corruption('git commit -m "wip"', directory)
        check("NUL-filled staged file blocks `git commit`", reason is not None)

        untracked = os.path.join(directory, "fresh.md")
        with open(untracked, "wb") as handle:
            handle.write(b"\x00" * 300)
        reason = check_r10_nul_corruption('git add -A && git commit -m "wip"', directory)
        check("an untracked NUL file is caught before `add && commit`", reason is not None)
        os.remove(untracked)

        reason = check_r10_nul_corruption("git status", directory)
        check("read-only `git status` is not inspected", reason is None)

        reason = check_r10_nul_corruption("git push origin main", directory)
        check("`git push` is not inspected", reason is None)

        reason = check_r10_nul_corruption(
            'gh pr create --body "run git add -A first"', directory
        )
        check("prose mentioning `git add` is not a real command", reason is None)

        reason = check_r10_nul_corruption("git add -A", None)
        check("missing working directory fails open", reason is None)

    with tempfile.TemporaryDirectory() as outside:
        reason = check_r10_nul_corruption("git add -A", outside)
        check("directory outside any repository fails open", reason is None)

    print(f"\nR10 NUL-corruption self-test summary: {len(failures)} failed.")
    if failures:
        print("Failed cases: " + ", ".join(failures))
        return 1
    return 0


def run_r11_memory_write_self_test() -> int:
    """Exercise R11 against a real linked worktree in a throwaway repository."""
    # R9 and R10 each ship one of these and `--self-test` runs all of them, so
    # a rule without one is invisible to the number agents quote as evidence:
    # `--self-test 93/93` could not move if R11 broke entirely.
    #
    # Imported locally for the same reason R10's suite does: the guard ships to
    # hosts that have neither this repository's test runner nor its test tree,
    # and `--self-test` is the only way to check it there.
    import tempfile

    failures: list[str] = []

    def check(description: str, condition: bool) -> None:
        status = "PASS" if condition else "FAIL"
        print(f"[{status}] {description}")
        if not condition:
            failures.append(description)

    def git(cwd: str, *arguments: str) -> None:
        subprocess.run(  # nosec B603 B607 - fixed git commands on a temp fixture.
            ["git", "-c", "core.longpaths=true", *arguments],
            cwd=cwd,
            capture_output=True,
            text=True,
            check=False,
        )

    with tempfile.TemporaryDirectory() as container:
        primary = os.path.join(container, "primary")
        linked = os.path.join(container, "linked")
        os.makedirs(primary)
        git(primary, "init", "-q", "-b", "main", ".")
        git(primary, "config", "user.email", "harness@example.invalid")
        git(primary, "config", "user.name", "Harness")
        with open(os.path.join(primary, "notes.md"), "w", encoding="utf-8") as handle:
            handle.write("committed\n")
        git(primary, "add", "notes.md")
        git(primary, "commit", "-qm", "initial")
        git(primary, "worktree", "add", linked, "-b", "ChaosEngine/selftest")

        check("fixture linked worktree is detected", is_linked_worktree(linked))
        check("fixture primary checkout is not linked", not is_linked_worktree(primary))

        reason = check_r11_memory_write_worktree(
            "mcp__shaft-memory__remember_memory", linked, None
        )
        check("untargeted memory write from a linked worktree is denied", reason is not None)
        check(
            "denial names project_root as the remedy",
            reason is not None and "project_root" in reason,
        )
        check(
            "denial names the CLI as the other remedy",
            reason is not None and "memory remember" in reason,
        )
        check(
            "denial does not claim where the write would land",
            reason is not None and "would land in the PRIMARY" not in reason,
        )

        reason = check_r11_memory_write_worktree(
            "mcp__shaft-memory__remember_memory", linked, {"project_root": linked}
        )
        check("write targeted at this worktree is allowed", reason is None)

        reason = check_r11_memory_write_worktree(
            "mcp__shaft-memory__remember_memory", linked, {"project_root": primary}
        )
        check("write targeted at another tree is denied", reason is not None)

        reason = check_r11_memory_write_worktree(
            "mcp__shaft-memory__remember_memory", linked, {"project_root": "."}
        )
        check("relative project_root is denied", reason is not None)

        reason = check_r11_memory_write_worktree(
            "mcp__shaft-memory__remember_memory", primary, None
        )
        check("memory write from the primary checkout is allowed", reason is None)

        reason = check_r11_memory_write_worktree(
            "mcp__shaft-memory__load_memory", linked, None
        )
        check("memory read from a linked worktree is allowed", reason is None)

        reason = check_r11_memory_write_worktree("Bash", linked, None)
        check("an unrelated tool is not touched by R11", reason is None)

        reason = check_r11_memory_write_worktree(
            "mcp__shaft-memory__remember_memory", container, None
        )
        check("directory outside any repository fails open", reason is None)

    print(f"\nR11 memory-write self-test summary: {len(failures)} failed.")
    if failures:
        print("Failed cases: " + ", ".join(failures))
        return 1
    return 0


def main(argv: list[str]) -> int:
    if "--self-test" in argv:
        command_result = run_self_test()
        r9_result = run_r9_worktree_self_test()
        r10_result = run_r10_nul_corruption_self_test()
        r11_result = run_r11_memory_write_self_test()
        return command_result or r9_result or r10_result or r11_result

    raw = sys.stdin.read()
    if not raw.strip():
        return 0
    try:
        raw_hook_input = json.loads(raw)
    except json.JSONDecodeError:
        return 0  # malformed input: fail open (allow), nothing to safely block on

    if not isinstance(raw_hook_input, dict):
        return 0
    hook_input = normalize_hook_input(raw_hook_input)
    event = hook_input.get("hook_event_name") or "PreToolUse"
    if event == "SessionStart":
        return run_session_start(hook_input)
    if event == "Stop":
        return run_stop(hook_input)
    if event == "PreToolUse":
        return run_pretooluse(hook_input, hook_host(raw_hook_input))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
