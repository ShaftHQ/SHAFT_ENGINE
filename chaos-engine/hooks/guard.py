#!/usr/bin/env python3
"""Activate ChaosEngine and reject catastrophic shell scope."""

from __future__ import annotations

import json
import hashlib
import importlib.util
import posixpath
import re
import shlex
import sys
from pathlib import Path

def _load_sibling(module_name: str):
    path = Path(__file__).resolve().with_name(f"{module_name}.py")
    if not path.is_file():
        return None
    specification = importlib.util.spec_from_file_location(f"chaos_engine_{module_name}", path)
    if specification is None or specification.loader is None:
        raise RuntimeError(f"ChaosEngine {module_name} module is unavailable")
    module = importlib.util.module_from_spec(specification)
    previous = sys.modules.get(specification.name)
    sys.modules[specification.name] = module
    try:
        specification.loader.exec_module(module)
    except (Exception, KeyboardInterrupt, SystemExit):
        if previous is None:
            sys.modules.pop(specification.name, None)
        else:
            sys.modules[specification.name] = previous
        raise
    return module


_lifecycle = _load_sibling("lifecycle")
if _lifecycle is None:
    raise RuntimeError("ChaosEngine lifecycle core is unavailable")
_kernel = _load_sibling("kernel")
if _kernel is None:
    raise RuntimeError("ChaosEngine policy kernel is unavailable")
reflection = _load_sibling("reflection")
if reflection is None:  # Repository adapter fallback for a source-only layout.
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
    "learning session disposition",
)


def learning_session_reason(session_id: str, event: dict) -> str | None:
    if bool(event.get("stop_hook_active") or event.get("stopHookActive")):
        return None
    recorded = reflection.entries(session_id)
    activities = {
        item.get("activity")
        for item in recorded
        if item.get("kind") == "task-activity"
    }
    if "delivery-complete" not in activities:
        return None
    if "learning-session-complete" in activities:
        return None
    if any(
        item.get("kind") == "reflection-receipt"
        and item.get("durableDisposition") in reflection.DISPOSITIONS
        for item in recorded
    ):
        return None
    return (
        "Learning Session: delivery is complete. Run exactly one terminal Learning "
        "Session immediately before the final report."
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


def terminal_delivery_command(command: str) -> bool:
    """True only for canonical final delivery-status verification."""
    return "delivery-status" in shell_tokens(command)


def learning_session_finalize_command(command: str) -> bool:
    """True only for one direct terminal Learning Session finalizer."""
    parsed = shell_tokens(command)
    if not parsed or any(item in {";", "&&", "||", "|", "&"} for item in parsed):
        return False
    head, arguments = command_head(parsed)
    if head not in {"py", "python", "python3"}:
        return False
    while arguments and arguments[0] in {"-3", "-u", "-B"}:
        arguments = arguments[1:]
    if len(arguments) < 4:
        return False
    script = arguments[0].replace("\\", "/").casefold()
    return bool(
        script.endswith("scripts/agents/learning_session.py")
        and arguments[1] == "finalize"
        and "--session-id" in arguments[2:]
    )


def read_only_diagnostic_command(command: str) -> bool:
    parsed = shell_tokens(command)
    if not parsed or any(item in {";", "&&", "||", "|", "&"} for item in parsed):
        return False
    head, arguments = command_head(parsed)
    if head in {"rg", "grep", "get-content"}:
        return True
    if head != "git" or not arguments:
        return False
    if arguments[0] in {"status", "diff", "show", "log", "rev-parse"}:
        return True
    return arguments == ["branch", "--show-current"]


def checkpoint_reason(checkpoint: dict) -> str:
    fingerprints = ",".join(checkpoint["failureFingerprints"])
    return (
        f"Reflection required ({checkpoint['depth']}). Sanitized fingerprints: "
        f"{fingerprints}. Pause mutation and unchanged retries; append a validated "
        "receipt before resuming."
    )


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


def functions_exec_source(tool_input: object) -> str:
    if isinstance(tool_input, str):
        return tool_input
    if isinstance(tool_input, dict):
        for key in ("input", "source", "code"):
            source = tool_input.get(key)
            if isinstance(source, str):
                return source
    return ""


def functions_exec_direct_command(tool_input: object) -> str:
    if isinstance(tool_input, dict):
        command = tool_input.get("cmd") or tool_input.get("command")
        if isinstance(command, str):
            return command
    return ""


def _event_commands(tool_name: str, tool_input: object) -> tuple[str, ...]:
    functions_source = functions_exec_source(tool_input)
    functions_direct = functions_exec_direct_command(tool_input)
    if tool_name == "functions.exec" and functions_direct:
        return (functions_direct,)
    if tool_name == "functions.exec" and functions_source:
        return wrapped_exec_commands(functions_source)
    if not isinstance(tool_input, dict):
        return ()
    command = str(tool_input.get("command") or tool_input.get("cmd") or "")
    return (command,) if command else ()


def _tool_result_failed(event_name: str, result: object) -> bool:
    if event_name == "PostToolUseFailure":
        return True
    return bool(
        isinstance(result, dict)
        and (
            result.get("isError") is True
            or result.get("interrupted") is True
            or str(result.get("status", "")).casefold() in {"error", "failed", "failure"}
            or result.get("exit_code", result.get("exitCode", 0)) not in {0, None}
        )
    )


def _command_is_destructive(command: str) -> bool:
    folded = command.casefold()
    broad_remove = (
        "remove-item" in folded
        and "-recurse" in folded
        and (ROOT_DRIVE.search(command) is not None or "$home" in folded or "~" in command)
    )
    return catastrophic_posix(command) or "git reset --hard" in folded or broad_remove


def _stop_block_reason(event: dict, session_id: str) -> str:
    if event.get("hook_event_name") == "SubagentStop" or bool(
        event.get("stop_hook_active") or event.get("stopHookActive")
    ):
        return ""
    elapsed = reflection.session_elapsed_seconds(session_id)
    if elapsed is not None and elapsed > 3600 and not reflection.has_valid_terminal_receipt(session_id):
        return "Terminal reflection required before this session can stop."
    if elapsed is not None and elapsed > 3600:
        message = str(event.get("last_assistant_message") or event.get("lastAssistantMessage") or "").casefold()
        missing = [label for label in TERMINAL_LABELS if label not in message]
        if missing:
            return "Terminal reflection summary is missing: " + ", ".join(missing) + "."
    loop_reason = learning_session_reason(session_id, event)
    if loop_reason:
        return loop_reason
    return ""


def _record_failed_result(
    event: dict, event_name: str, commands: tuple[str, ...], tool_name: str, session_id: str
) -> bool:
    result = event.get("tool_response", event.get("tool_result"))
    if event_name not in {"PostToolUse", "PostToolUseFailure"} or not _tool_result_failed(event_name, result):
        return False
    target = outcome_target(
        commands[0] if commands else "",
        tool_name,
        event.get("target") or event.get("job") or event.get("test"),
    )
    read_only = tool_name in {"Read", "Grep", "Glob", "WebSearch", "WebFetch", "Skill"} or bool(
        commands and all(read_only_diagnostic_command(command) for command in commands)
    )
    reflection.record_failure(
        session_id,
        phase="tool-outcome",
        target=target,
        failure_class="interrupted" if event.get("is_interrupt") else "tool-failure",
        platform=event.get("platform") or sys.platform,
        attempted=not read_only,
        observation_id=event.get("tool_use_id") or event.get("toolUseId"),
    )
    checkpoint = reflection.pending_checkpoint(session_id)
    if checkpoint:
        print(json.dumps({"additionalContext": checkpoint_reason(checkpoint)}))
    return bool(checkpoint)


def _unchanged_test_requested(event: dict, commands: tuple[str, ...], tool_name: str, session_id: str) -> bool:
    active_targets = {
        item.get("target")
        for item in reflection.active_entries(session_id)
        if item.get("kind") == "task-failure"
    }
    target = event.get("target") or event.get("job") or event.get("test")
    return any(
        test_command(candidate) and outcome_target(candidate, tool_name, target) in active_targets
        for candidate in commands
    )


def _uninspectable_functions_call(
    tool_name: str, tool_input: object, functions_source: str,
    functions_direct: str, commands: tuple[str, ...],
) -> bool:
    return bool(
        tool_name == "functions.exec"
        and not functions_direct
        and (
            not isinstance(tool_input, (str, dict))
            or (isinstance(tool_input, dict) and not functions_source)
            or wrapped_exec_call_count(functions_source) != len(commands)
        )
    )


def _command_guard_state(
    event: dict, event_name: str, commands: tuple[str, ...], tool_name: str, tool_input: object,
    functions_source: str, functions_direct: str, session_id: str,
) -> tuple[bool, bool, str]:
    receipt_command = any(reflection_recovery(candidate) for candidate in commands)
    mutation = (
        tool_name in {"Write", "Edit", "apply_patch"}
        or bool(re.search(r"\btools\.(?:apply_patch|store)\s*\(", functions_source))
        or any(
        mutation_command(candidate) and not tracker_command(candidate) for candidate in commands
        )
    )
    checkpoint = reflection.pending_checkpoint(session_id)
    unchanged_test = _unchanged_test_requested(event, commands, tool_name, session_id)
    if event_name == "PreToolUse" and checkpoint and not receipt_command and (mutation or unchanged_test):
        return receipt_command, mutation, checkpoint_reason(checkpoint)
    uninspectable = _uninspectable_functions_call(
        tool_name, tool_input, functions_source, functions_direct, commands
    )
    if uninspectable or any(_command_is_destructive(candidate) for candidate in commands):
        return receipt_command, mutation, "ChaosEngine rejected destructive broad scope."
    return receipt_command, mutation, ""


def _event_context(event_name: str, token: object) -> str:
    if event_name == "SessionStart":
        return _lifecycle.session_start_context(token, ACTIVATION)
    context = f"ChaosEngine: {ACTIVATION}"
    if token:
        context += f" Reflection session token (never track it): {token}"
    return context


def _run_event(event: dict, _host: str) -> int:
    tool_input = event.get("tool_input", {}) if isinstance(event, dict) else {}
    tool_name = str(event.get("tool_name", "")) if isinstance(event, dict) else ""
    functions_source = functions_exec_source(tool_input)
    functions_direct = functions_exec_direct_command(tool_input)
    commands = _event_commands(tool_name, tool_input)
    event_name = (
        str(event.get("hook_event_name") or event.get("hookEventName") or "")
        if isinstance(event, dict)
        else ""
    )
    root_session_id = str(event.get("session_id") or event.get("sessionId") or "")
    session_id = reflection.scope_session_id(
        root_session_id, event.get("agent_id") or event.get("agentId")
    )
    kernel_event = dict(event)
    kernel_event["session_id"] = session_id
    kernel_event["agent_id"] = ""
    normalized_kernel_event = _kernel.normalize_event(kernel_event, _host)
    if event_name in {"PostToolUse", "PostToolUseFailure"} and not normalized_kernel_event.target_phase:
        kernel_report = _kernel.evaluate(normalized_kernel_event)
    else:
        kernel_journal = _kernel.EffectJournal(
            reflection.ledger_path(session_id).with_suffix(".kernel-v3.jsonl")
        )
        kernel_report = _kernel.evaluate_session(normalized_kernel_event, kernel_journal)
    if kernel_report.decision == "deny":
        print(json.dumps({"decision": "block", "reason": kernel_report.reason}))
        return 2
    if event_name == "SessionStart":
        token = reflection.record_session_start(session_id)
    else:
        token = None
    if _record_failed_result(event, event_name, commands, tool_name, session_id):
        return 0
    receipt_command, mutation, guard_reason = _command_guard_state(
        event, event_name, commands, tool_name, tool_input, functions_source, functions_direct, session_id
    )
    if guard_reason:
        print(json.dumps({"decision": "block", "reason": guard_reason}))
        return 2
    if event_name == "PostToolUse" and not receipt_command:
        if any(learning_session_finalize_command(candidate) for candidate in commands):
            reflection.record_activity(session_id, "learning-session-complete")
        elif any(terminal_delivery_command(candidate) for candidate in commands):
            reflection.record_activity(session_id, "delivery-complete")
        elif mutation or any(delivery_command(candidate) for candidate in commands):
            reflection.record_activity(session_id, "mutation")
    if event_name in {"Stop", "SubagentStop"}:
        stop_reason = _stop_block_reason(event, session_id)
        if stop_reason:
            print(json.dumps({"decision": "block", "reason": stop_reason}))
            return 2
    print(json.dumps({"additionalContext": _event_context(event_name, token)}))
    return 0


def main() -> int:
    callbacks = {event: _run_event for event in _lifecycle.LIFECYCLE_EVENTS}
    return _lifecycle.run_hook_protocol(
        sys.stdin.read(),
        callbacks,
        normalize=_kernel.normalize_hook_input,
        host_for_input=_kernel.detect_host,
        adapt_output=_kernel.adapt_hook_output,
    )


if __name__ == "__main__":
    raise SystemExit(main())
