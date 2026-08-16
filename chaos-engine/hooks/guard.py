#!/usr/bin/env python3
"""Activate ChaosEngine and reject catastrophic shell scope."""

from __future__ import annotations

import json
import hashlib
import posixpath
import re
import shlex
import sys
from pathlib import Path

try:
    import reflection
except ImportError:  # Repository source layout; installed hooks keep it beside guard.py.
    repository_root = Path(__file__).resolve().parents[2]
    sys.path.insert(0, str(repository_root))
    from scripts.agents import reflection


ACTIVATION = "Follow .chaos-engine/skills/chaos-engine/SKILL.md before continuing."
ROOT_DRIVE = re.compile(r"(?i)(?:^|\s)[a-z]:\\(?:\s|$)")
ENV_ASSIGNMENT = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*=")
SHELLS = {"bash", "sh", "zsh"}
DOWNLOADERS = {"curl", "fetch", "wget"}
TERMINAL_LABELS = (
    "elapsed estimate",
    "main time consumer",
    "repeated failures or corrections",
    "changed assumption or approach",
    "successful proof",
    "remaining risk or follow-up",
    "learning loop disposition",
)


def outcome_target(command: str, tool_name: str, explicit: object = None) -> str:
    if isinstance(explicit, str) and explicit.strip():
        normalized = re.sub(r"\s+", " ", explicit.strip().casefold())
        return "logical-" + hashlib.sha256(normalized.encode("utf-8")).hexdigest()[:20]
    if not command:
        return tool_name or "unknown"
    normalized = re.sub(r"\s+", " ", command.strip().casefold())
    return "command-" + hashlib.sha256(normalized.encode("utf-8")).hexdigest()[:20]


def test_command(command: str) -> bool:
    lowered = command.casefold()
    return any(
        marker in lowered
        for marker in (" -m unittest", "pytest", "mvn test", "mvn verify", "gradle test", "npm test")
    )


def mutation_command(command: str) -> bool:
    lowered = command.casefold()
    return bool(
        re.search(r"\b(?:set-content|add-content|remove-item|new-item|out-file|touch|rm|mv|cp)\b", lowered)
        or re.search(r"\bgit\s+(?:add|commit|push|merge|rebase|reset|checkout|switch|clean)\b", lowered)
    )


def shell_tokens(command: str) -> list[str]:
    try:
        lexer = shlex.shlex(command, posix=True, punctuation_chars=";&|")
        lexer.whitespace_split = True
        lexer.commenters = ""
        return list(lexer)
    except ValueError:
        return []


def tracker_command(command: str) -> bool:
    parsed = shell_tokens(command)
    if not parsed or any(item in {";", "&&", "||", "|", "&"} for item in parsed):
        return False
    head, arguments = command_head(parsed)
    return head == "gh" and arguments[:2] in (["issue", "comment"], ["issue", "edit"])


def delivery_command(command: str) -> bool:
    parsed = shell_tokens(command)
    if not parsed or any(item in {";", "&&", "||", "|", "&"} for item in parsed):
        return False
    head, arguments = command_head(parsed)
    if head == "git" and arguments[:1] == ["push"]:
        return True
    if head != "gh" or len(arguments) < 2:
        return False
    return arguments[0] in {"pr", "issue"} and arguments[1] in {
        "create", "edit", "merge", "comment", "review", "ready", "close", "reopen"
    }


def reflection_recovery(command: str) -> str | None:
    arguments = shell_tokens(command)
    if not arguments or any(item in {";", "&&", "||", "|", "&"} for item in arguments):
        return None
    head, remaining = command_head(arguments)
    if head not in {"py", "python", "python3"}:
        return None
    script_index = 0
    while script_index < len(remaining) and remaining[script_index] in {"-3", "-u", "-B"}:
        script_index += 1
    if script_index + 1 >= len(remaining):
        return None
    supplied = Path(remaining[script_index])
    if not supplied.is_absolute():
        supplied = Path.cwd() / supplied
    try:
        if supplied.resolve() != Path(__file__).resolve().with_name("reflection.py"):
            return None
    except OSError:
        return None
    operation = remaining[script_index + 1]
    return operation if operation in {"receipt", "trigger", "non-attempt"} and "--session-id" in remaining else None


def tokens(command: str) -> list[str]:
    try:
        return shlex.split(command, posix=True)
    except ValueError:
        return []


def command_head(arguments: list[str]) -> tuple[str, list[str]]:
    index = 0
    while index < len(arguments):
        item = arguments[index]
        if ENV_ASSIGNMENT.match(item):
            index += 1
            continue
        head = re.split(r"[/\\]", item.strip("\"'"))[-1].casefold()
        if head in {"command", "env", "sudo", "timeout"}:
            index += 1
            while index < len(arguments) and (
                arguments[index].startswith("-") or ENV_ASSIGNMENT.match(arguments[index])
            ):
                index += 1
            if head == "timeout" and index < len(arguments) and re.fullmatch(
                r"\d+[smhd]?", arguments[index]
            ):
                index += 1
            continue
        return head, arguments[index + 1 :]
    return "", []


def catastrophic_target(target: str) -> bool:
    normalized = posixpath.normpath(re.sub(r"/+", "/", target.replace("\\", "/")))
    if normalized in {"/", "/*", "~", "$home", "${home}"}:
        return True
    return bool(re.fullmatch(r"/(?:bin|boot|dev|etc|lib|sbin|usr|var)(?:/\*)?", normalized))


def broad_rm(command: str) -> bool:
    head, arguments = command_head(tokens(command))
    if head != "rm":
        return False
    recursive = False
    targets: list[str] = []
    for argument in arguments:
        lowered = argument.casefold()
        if lowered == "--recursive":
            recursive = True
        elif lowered.startswith("-") and not lowered.startswith("--"):
            recursive = recursive or "r" in lowered
        else:
            targets.append(lowered)
    return recursive and any(catastrophic_target(target) for target in targets)


def catastrophic_posix(command: str) -> bool:
    if any(
        command_head(tokens(stage))[0] in DOWNLOADERS
        and any(command_head(tokens(later))[0] in SHELLS for later in pipeline[index + 1 :])
        for pipeline in (re.split(r"(?<!\|)\|(?!\|)", statement) for statement in re.split(r"&&|\|\||;|\r?\n", command))
        for index, stage in enumerate(pipeline)
    ):
        return True
    for segment in re.split(r"&&|\|\||;|\r?\n", command):
        head, arguments = command_head(tokens(segment))
        if head == "rm" and broad_rm(segment):
            return True
        if head == "find" and arguments and catastrophic_target(arguments[0]) and (
            "-delete" in arguments or "-exec" in arguments
        ):
            return True
        if head == "dd" and any(re.fullmatch(r"of=/dev/(?:disk|hd|mmcblk|nvme|sd|vd|xvd).+", item) for item in arguments):
            return True
        if re.fullmatch(r"mkfs(?:\.[a-z0-9]+)?", head) and any(item.startswith("/dev/") for item in arguments):
            return True
        if head == "chmod":
            values = [item for item in arguments if not item.startswith("-")]
            if len(values) >= 2 and re.fullmatch(r"[0-7]*777[0-7]*", values[0]) and catastrophic_target(values[1]):
                return True
    return bool(re.search(r": ?\(\)\s*\{", command))


def wrapped_exec_commands(source: str) -> tuple[str, ...]:
    commands: list[str] = []
    for match in re.finditer(
        r'''\btools\.exec_command\s*\(\s*\{.*?\b(?:cmd|command)\s*:\s*(?P<literal>"(?:\\.|[^"\\])*")''',
        source,
        re.DOTALL,
    ):
        try:
            command = json.loads(match.group("literal"))
        except (json.JSONDecodeError, ValueError):
            continue
        if isinstance(command, str) and command:
            commands.append(command)
    return tuple(commands)


def wrapped_exec_call_count(source: str) -> int:
    return len(re.findall(r"\btools\.exec_command\s*\(", source))


def main() -> int:
    try:
        event = json.load(sys.stdin)
    except (json.JSONDecodeError, OSError):
        event = {}
    tool_input = event.get("tool_input", {}) if isinstance(event, dict) else {}
    tool_name = str(event.get("tool_name", "")) if isinstance(event, dict) else ""
    if isinstance(tool_input, dict):
        command = str(tool_input.get("command") or tool_input.get("cmd") or "")
        commands = (command,) if command else ()
    elif tool_name == "functions.exec" and isinstance(tool_input, str):
        commands = wrapped_exec_commands(tool_input)
    else:
        commands = ()
    event_name = str(event.get("hook_event_name", "")) if isinstance(event, dict) else ""
    session_id = str(event.get("session_id") or event.get("sessionId") or "")
    if event_name == "SessionStart":
        token = reflection.record_session_start(session_id)
    else:
        token = None
    result = event.get("tool_response", event.get("tool_result")) if isinstance(event, dict) else None
    result_failed = event_name == "PostToolUseFailure" or bool(
        isinstance(result, dict)
        and (
            result.get("isError") is True
            or result.get("interrupted") is True
            or str(result.get("status", "")).casefold() in {"error", "failed", "failure"}
            or result.get("exit_code", result.get("exitCode", 0)) not in {0, None}
        )
    )
    if event_name in {"PostToolUse", "PostToolUseFailure"} and result_failed:
        target = outcome_target(
            commands[0] if commands else "",
            tool_name,
            event.get("target") or event.get("job") or event.get("test"),
        )
        reflection.record_failure(
            session_id,
            phase="tool-outcome",
            target=target,
            failure_class="interrupted" if event.get("is_interrupt") else "tool-failure",
            platform=event.get("platform") or sys.platform,
            observation_id=event.get("tool_use_id") or event.get("toolUseId"),
        )
        checkpoint = reflection.pending_checkpoint(session_id)
        if checkpoint:
            print(json.dumps({"additionalContext": f"Reflection required ({checkpoint['depth']}). Pause mutation and unchanged retries; append a validated receipt before resuming."}))
            return 0
    checkpoint = reflection.pending_checkpoint(session_id)
    receipt_command = any(reflection_recovery(candidate) for candidate in commands)
    active_targets = {
        item.get("target")
        for item in reflection.active_entries(session_id)
        if item.get("kind") == "task-failure"
    }
    unchanged_test = any(
        test_command(candidate)
        and outcome_target(
            candidate, tool_name, event.get("target") or event.get("job") or event.get("test")
        ) in active_targets
        for candidate in commands
    )
    mutation = tool_name in {"Write", "Edit", "apply_patch"} or any(
        mutation_command(candidate) and not tracker_command(candidate) for candidate in commands
    )
    if event_name == "PreToolUse" and checkpoint and not receipt_command and (mutation or unchanged_test):
        print(json.dumps({"decision": "block", "reason": f"Reflection required ({checkpoint['depth']}); mutation and unchanged retries remain blocked until a validated receipt is appended."}))
        return 2
    uninspectable = (
        tool_name == "functions.exec"
        and isinstance(tool_input, str)
        and wrapped_exec_call_count(tool_input) != len(commands)
    )
    destructive = uninspectable or any(
        catastrophic_posix(candidate)
        or "git reset --hard" in candidate.casefold()
        or (
            "remove-item" in candidate.casefold()
            and "-recurse" in candidate.casefold()
            and (
                ROOT_DRIVE.search(candidate) is not None
                or "$home" in candidate.casefold()
                or "~" in candidate
            )
        )
        for candidate in commands
    )
    if destructive:
        print(
            json.dumps(
                {
                    "decision": "block",
                    "reason": "ChaosEngine rejected destructive broad scope.",
                }
            )
        )
        return 2
    if event_name == "PostToolUse" and not receipt_command and (
        mutation or any(delivery_command(candidate) for candidate in commands)
    ):
        reflection.record_activity(session_id, "mutation-or-delivery")
    if event_name in {"Stop", "SubagentStop"}:
        elapsed = reflection.session_elapsed_seconds(session_id)
        if elapsed is not None and elapsed > 3600 and not reflection.has_valid_terminal_receipt(session_id):
            print(json.dumps({"decision": "block", "reason": "Terminal reflection required before this session can stop."}))
            return 2
        if elapsed is not None and elapsed > 3600:
            message = str(
                event.get("last_assistant_message")
                or event.get("lastAssistantMessage")
                or ""
            ).casefold()
            missing = [label for label in TERMINAL_LABELS if label not in message]
            if missing:
                print(json.dumps({"decision": "block", "reason": "Terminal reflection summary is missing: " + ", ".join(missing) + "."}))
                return 2
    if event_name in {"Stop", "SubagentStop"} and not bool(event.get("stop_hook_active")):
        print(
            json.dumps(
                {
                    "decision": "block",
                    "reason": "Complete verification, independent review, delivery status, and the learning loop before stopping.",
                }
            )
        )
        return 2
    context = f"ChaosEngine: {ACTIVATION}"
    if token:
        context += f" Reflection session token (never track it): {token}"
    print(json.dumps({"additionalContext": context}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
