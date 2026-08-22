#!/usr/bin/env python3
"""Provider-neutral ChaosEngine lifecycle policy kernel."""

from __future__ import annotations

import contextlib
import hashlib
import json
import os
import re
import threading
import time
from dataclasses import dataclass, field
from pathlib import Path
from types import MappingProxyType
from typing import Callable, Mapping


SCHEMA_VERSION = 1
STATE_SCHEMA_VERSION = 2
CANONICAL_IDENTITY = "chaos-engine"
LEGACY_IDENTITIES = ("act-as-mohab",)
TERMINAL_PHASES = frozenset({"Complete", "Blocked"})
LIFECYCLE_TRANSITIONS: Mapping[str, tuple[str, ...]] = {
    "ReadOnly": ("Complete", "Planned", "Blocked"),
    "Planned": ("Approved", "Blocked"),
    "Approved": ("Isolated", "Blocked"),
    "Isolated": ("Red", "Blocked"),
    "Red": ("Green", "Blocked"),
    "Green": ("PullRequest", "Blocked"),
    "PullRequest": ("Reviewed", "Blocked"),
    "Reviewed": ("Authorized", "Blocked"),
    "Authorized": ("Merged", "Blocked"),
    "Merged": ("Learned", "Blocked"),
    "Learned": ("Complete", "Blocked"),
    "Complete": (),
    "Blocked": (),
}

CLAUDE_EVENTS = (
    "SessionStart",
    "UserPromptSubmit",
    "PreToolUse",
    "PostToolUse",
    "PostToolUseFailure",
    "Stop",
    "SubagentStop",
    "PreCompact",
    "SessionEnd",
)
CODEX_EVENTS = CLAUDE_EVENTS[:7]
GROK_EVENTS = CODEX_EVENTS
GEMINI_EVENTS = (
    "SessionStart",
    "UserPromptSubmit",
    "PreToolUse",
    "PostToolUse",
    "Stop",
    "PreCompact",
    "SessionEnd",
)
COPILOT_EVENTS = (
    "SessionStart",
    "UserPromptSubmit",
    "PreToolUse",
    "PostToolUse",
    "Stop",
    "SessionEnd",
)


@dataclass(frozen=True)
class HostCapability:
    instruction_paths: tuple[str, ...]
    event_aliases: Mapping[str, str]
    supported_events: tuple[str, ...]
    strict_json_stdout: bool = True
    live_gate: bool = True


def _aliases(supported: tuple[str, ...], **values: str) -> Mapping[str, str]:
    base = {event: event for event in supported}
    base.update(values)
    return MappingProxyType(base)


HOST_CAPABILITIES: Mapping[str, HostCapability] = {
    "codex": HostCapability(
        ("AGENTS.md", ".agents/skills/chaos-engine/SKILL.md"),
        _aliases(CODEX_EVENTS, preToolUse="PreToolUse", postToolUse="PostToolUse", agentStop="Stop"),
        CODEX_EVENTS,
    ),
    "claude": HostCapability(
        ("CLAUDE.md", ".claude/skills/chaos-engine/SKILL.md"),
        _aliases(CLAUDE_EVENTS),
        CLAUDE_EVENTS,
    ),
    "gemini": HostCapability(
        ("GEMINI.md", ".gemini/skills/chaos-engine/SKILL.md"),
        _aliases(
            GEMINI_EVENTS,
            BeforeTool="PreToolUse",
            AfterTool="PostToolUse",
            BeforeAgent="UserPromptSubmit",
            AfterAgent="Stop",
            PreCompress="PreCompact",
        ),
        GEMINI_EVENTS,
    ),
    "grok": HostCapability(
        ("AGENTS.md", ".grok/plugins/chaos-engine"),
        _aliases(GROK_EVENTS, preToolUse="PreToolUse", postToolUse="PostToolUse", agentStop="Stop"),
        GROK_EVENTS,
    ),
    "copilot": HostCapability(
        ("AGENTS.md", ".github/copilot-instructions.md", ".github/skills/chaos-engine/SKILL.md"),
        _aliases(
            COPILOT_EVENTS,
            sessionStart="SessionStart",
            userPromptSubmitted="UserPromptSubmit",
            preToolUse="PreToolUse",
            postToolUse="PostToolUse",
            agentStop="Stop",
            sessionEnd="SessionEnd",
        ),
        COPILOT_EVENTS,
    ),
}

FIELD_ALIASES = {
    "hookEventName": "hook_event_name",
    "toolName": "tool_name",
    "toolInput": "tool_input",
    "toolArgs": "tool_input",
    "sessionId": "session_id",
    "agentId": "agent_id",
    "agentType": "agent_type",
    "toolResponse": "tool_response",
    "toolResult": "tool_result",
    "lastAssistantMessage": "last_assistant_message",
    "stopHookActive": "stop_hook_active",
    "toolUseId": "tool_use_id",
    "targetPhase": "target_phase",
}
TOOL_ALIASES = {
    "bash": "PowerShell",
    "powershell": "PowerShell",
    "shellcommand": "PowerShell",
    "shell_command": "PowerShell",
    "execcommand": "PowerShell",
    "exec_command": "PowerShell",
    "runshellcommand": "PowerShell",
    "run_shell_command": "PowerShell",
    "read": "Read",
    "grep": "Grep",
    "edit": "Edit",
    "write": "Write",
    "applypatch": "apply_patch",
    "apply_patch": "apply_patch",
}

_DIRECT_MUTATION_TOOLS = frozenset(
    {
        "applypatch",
        "copy",
        "create",
        "delete",
        "edit",
        "mkdir",
        "move",
        "notebookedit",
        "touch",
        "write",
    }
)
_READ_ONLY_SHELL_COMMAND = re.compile(
    r"(?:git\s+(?:status|diff|log|show|rev-parse|ls-files|ls-tree|cat-file)\b[^;&|>`\r\n]*|"
    r"gh\s+(?:(?:issue|pr)\s+(?:view|list|status|checks|diff)|status)\b[^;&|>`\r\n]*|"
    r"(?:get-content|test-path|resolve-path|pwd)\b[^;&|>`\r\n]*)",
    re.IGNORECASE,
)


@dataclass(frozen=True)
class HookEvent:
    name: str
    host: str
    session_id: str
    agent_id: str
    tool_name: str
    tool_input: Mapping[str, object]
    stop_hook_active: bool
    stateful_mutation: bool
    raw_fields: tuple[str, ...]
    phase: str = "ReadOnly"
    target_phase: str = ""


def detect_host(raw: Mapping[str, object] | None = None) -> str:
    configured = os.environ.get("SHAFT_GUARD_HOST", "").strip().lower()
    if configured in HOST_CAPABILITIES:
        return configured
    if os.environ.get("GROK_HOOK_EVENT"):
        return "grok"
    supplied = raw or {}
    declared = str(supplied.get("host") or supplied.get("provider") or "").strip().lower()
    if declared in HOST_CAPABILITIES:
        return declared
    if str(supplied.get("hook_event_name", "")).startswith(("Before", "After")):
        return "gemini"
    if any(key in supplied for key in ("toolArgs", "stopReason", "sessionId")):
        return "copilot"
    if os.environ.get("CLAUDE_CODE_ENTRYPOINT") or os.environ.get("CLAUDE_PLUGIN_ROOT"):
        return "claude"
    if os.environ.get("CODEX_HOME"):
        return "codex"
    return "unknown"


def _event_name(value: object, host: str) -> str:
    rendered = str(value or "")
    capability = HOST_CAPABILITIES.get(host)
    if capability is not None:
        return capability.event_aliases.get(rendered, rendered)
    return rendered


def _tool_name(value: object) -> str:
    rendered = str(value or "")
    key = re.sub(r"[^a-z_]", "", rendered.casefold())
    return TOOL_ALIASES.get(key, rendered)


def _is_mutation(tool_name: str, tool_input: Mapping[str, object]) -> bool:
    compact_name = re.sub(r"[^a-z]", "", tool_name.casefold())
    if compact_name in _DIRECT_MUTATION_TOOLS:
        return True
    command = str(tool_input.get("command") or tool_input.get("cmd") or "").strip()
    if not command:
        return False
    # Shell syntax is open-ended. Only a narrow, anchored read-only grammar is
    # safe to exempt; unknown commands fail closed as possible mutations.
    return _READ_ONLY_SHELL_COMMAND.fullmatch(command) is None


def normalize_event(raw: Mapping[str, object], host: str | None = None) -> HookEvent:
    selected = host or detect_host(raw)
    normalized = dict(raw)
    for source, target in FIELD_ALIASES.items():
        if target not in normalized and source in normalized:
            normalized[target] = normalized[source]
    if not normalized.get("hook_event_name") and os.environ.get("GROK_HOOK_EVENT"):
        normalized["hook_event_name"] = os.environ["GROK_HOOK_EVENT"]
    name = _event_name(normalized.get("hook_event_name"), selected)
    tool_input = normalized.get("tool_input")
    if isinstance(tool_input, str):
        try:
            tool_input = json.loads(tool_input)
        except (json.JSONDecodeError, ValueError):
            tool_input = {}
    if not isinstance(tool_input, Mapping):
        tool_input = {}
    tool_input = dict(tool_input)
    if "file_path" not in tool_input and "filePath" in tool_input:
        tool_input["file_path"] = tool_input["filePath"]
    tool_name = _tool_name(normalized.get("tool_name"))
    return HookEvent(
        name=name,
        host=selected,
        session_id=str(normalized.get("session_id") or "").strip(),
        agent_id=str(normalized.get("agent_id") or "").strip(),
        tool_name=tool_name,
        tool_input=tool_input,
        stop_hook_active=bool(normalized.get("stop_hook_active")),
        stateful_mutation=_is_mutation(tool_name, tool_input),
        raw_fields=tuple(sorted(str(key) for key in raw)),
        phase=str(normalized.get("phase") or "ReadOnly"),
        target_phase=str(normalized.get("target_phase") or ""),
    )


def normalize_hook_input(raw: Mapping[str, object]) -> dict[str, object]:
    """Compatibility projection for existing source callbacks."""
    event = normalize_event(raw)
    normalized = dict(raw)
    normalized.update(
        {
            "hook_event_name": event.name,
            "session_id": event.session_id,
            "agent_id": event.agent_id,
            "tool_name": event.tool_name,
            "tool_input": dict(event.tool_input),
            "stop_hook_active": event.stop_hook_active,
        }
    )
    return normalized


@dataclass
class HarnessSnapshot:
    providers: Mapping[str, Callable[[], object]] = field(default_factory=dict)
    _cache: dict[str, object] = field(default_factory=dict, init=False, repr=False)
    _lock: threading.RLock = field(default_factory=threading.RLock, init=False, repr=False)
    _resolving: set[str] = field(default_factory=set, init=False, repr=False)

    def __post_init__(self) -> None:
        self.providers = MappingProxyType(dict(self.providers))

    def fact(self, name: str) -> object:
        with self._lock:
            if name in self._cache:
                return self._cache[name]
            if name in self._resolving:
                return "unknown"
            provider = self.providers.get(name)
            self._resolving.add(name)
            try:
                value = "unknown" if provider is None else provider()
            except Exception:
                value = "unknown"
            finally:
                self._resolving.remove(name)
            self._cache[name] = value
            return value

    @property
    def used_facts(self) -> tuple[str, ...]:
        with self._lock:
            return tuple(sorted(self._cache))


@dataclass(frozen=True)
class Rule:
    code: str
    event: str
    priority: int
    decision: str
    remedy: str | None
    terminal: bool
    predicate: Callable[[HookEvent, HarnessSnapshot], bool] = field(compare=False, repr=False)
    remedy_code: str | None = None


@dataclass(frozen=True)
class Effect:
    session_id: str
    event: str
    tool_call_id: str
    rule: str
    effect: str
    payload: Mapping[str, object] = field(default_factory=dict)

    def __post_init__(self) -> None:
        if not self.session_id.strip():
            raise ValueError("effect requires a non-empty session identity")

    @property
    def key(self) -> str:
        rendered = "\0".join(
            (self.session_id, self.event, self.tool_call_id, self.rule, self.effect)
        )
        return hashlib.sha256(rendered.encode("utf-8")).hexdigest()

    def to_record(self) -> dict[str, object]:
        return {
            "schemaVersion": STATE_SCHEMA_VERSION,
            "identity": CANONICAL_IDENTITY,
            "idempotencyKey": self.key,
            "sessionId": self.session_id,
            "event": self.event,
            "toolCallId": self.tool_call_id,
            "rule": self.rule,
            "effect": self.effect,
            "payloadDigest": hashlib.sha256(
                json.dumps(self.payload, sort_keys=True, default=str).encode("utf-8")
            ).hexdigest(),
        }


@dataclass(frozen=True)
class EvaluationReport:
    host: str
    event: str
    phase: str
    decision: str
    diagnostic_code: str
    reason: str
    remedy: str | None
    terminal_reason: str | None
    facts_used: tuple[str, ...] = ()
    effects: tuple[Effect, ...] = ()

    def to_dict(self) -> dict[str, object]:
        return {
            "schemaVersion": SCHEMA_VERSION,
            "identity": CANONICAL_IDENTITY,
            "host": self.host,
            "event": self.event,
            "phase": self.phase,
            "decision": self.decision,
            "diagnosticCode": self.diagnostic_code,
            "reason": self.reason,
            "remedy": self.remedy,
            "factsUsed": list(self.facts_used),
            "effects": [effect.to_record() for effect in self.effects],
            "terminalReason": self.terminal_reason,
        }


RULES = (
    Rule(
        code="CE_SESSION_REQUIRED",
        event="PreToolUse",
        priority=100,
        decision="deny",
        remedy="Retry from a host event carrying a unique session identity.",
        terminal=False,
        predicate=lambda event, _snapshot: not event.session_id and event.stateful_mutation,
        remedy_code="retry_with_session",
    ),
    Rule(
        code="CE_SESSION_MISSING_STOP",
        event="Stop",
        priority=100,
        decision="allow",
        remedy="Configure the host adapter to provide session identity for stateful enforcement.",
        terminal=True,
        predicate=lambda event, _snapshot: not event.session_id,
    ),
)


def validate_rules(rules: tuple[Rule, ...]) -> list[str]:
    errors: list[str] = []
    codes: set[str] = set()
    satisfying_remedies = {"retry_with_session"}
    for index, rule in enumerate(rules):
        if rule.code in codes:
            errors.append(f"duplicate rule code: {rule.code}")
        codes.add(rule.code)
        for prior in rules[:index]:
            events_overlap = rule.event == prior.event or "*" in {rule.event, prior.event}
            if (
                events_overlap
                and rule.priority == prior.priority
                and rule.decision != prior.decision
            ):
                scope = "wildcard overlap" if "*" in {rule.event, prior.event} else rule.event
                errors.append(
                    f"conflicting decisions at {scope} priority {rule.priority}"
                )
        if rule.decision == "deny" and not rule.terminal:
            if not rule.remedy:
                errors.append(f"nonterminal denial lacks remedy: {rule.code}")
            if rule.remedy_code not in satisfying_remedies:
                errors.append(f"nonterminal denial lacks satisfiable remedy: {rule.code}")
    return errors


def validate_lifecycle(
    transitions: Mapping[str, tuple[str, ...]] = LIFECYCLE_TRANSITIONS,
) -> list[str]:
    errors: list[str] = []
    phases = set(transitions)
    for phase, targets in transitions.items():
        missing = set(targets) - phases
        if missing:
            errors.append(f"{phase} targets unknown phases: {sorted(missing)}")
        if phase in TERMINAL_PHASES and targets:
            errors.append(f"terminal phase has outgoing transitions: {phase}")
        frontier = [phase]
        visited: set[str] = set()
        while frontier:
            current = frontier.pop()
            if current in visited:
                continue
            visited.add(current)
            frontier.extend(transitions.get(current, ()))
        if phase not in TERMINAL_PHASES and not visited.intersection(TERMINAL_PHASES):
            errors.append(f"phase has no terminal path: {phase}")
    colors: dict[str, int] = {}

    def visits_cycle(phase: str) -> bool:
        colors[phase] = 1
        for target in transitions.get(phase, ()):
            if target in TERMINAL_PHASES:
                continue
            if colors.get(target) == 1 or (colors.get(target, 0) == 0 and visits_cycle(target)):
                return True
        colors[phase] = 2
        return False

    for phase in phases - TERMINAL_PHASES:
        if colors.get(phase, 0) == 0 and visits_cycle(phase):
            errors.append("lifecycle contains a nonterminal cycle")
            break
    return errors


def evaluate(
    event: HookEvent, snapshot: HarnessSnapshot | None = None,
    rules: tuple[Rule, ...] = RULES,
) -> EvaluationReport:
    observed = snapshot or HarnessSnapshot()
    current_phase = event.phase
    if current_phase not in LIFECYCLE_TRANSITIONS:
        return EvaluationReport(
            host=event.host,
            event=event.name,
            phase="ReadOnly",
            decision="deny",
            diagnostic_code="CE_UNKNOWN_PHASE",
            reason=f"Unknown lifecycle phase: {current_phase}",
            remedy="Retry with a phase declared by the lifecycle registry.",
            terminal_reason=None,
        )
    applicable = sorted(
        (rule for rule in rules if rule.event in {event.name, "*"}),
        key=lambda rule: (-rule.priority, rule.code),
    )
    for rule in applicable:
        if rule.predicate(event, observed):
            reason = {
                "CE_SESSION_REQUIRED": "Stateful lifecycle enforcement requires a unique session identity.",
                "CE_SESSION_MISSING_STOP": "Stop allowed without state lookup because host omitted session identity.",
            }.get(rule.code, rule.code)
            next_phase = (
                "Blocked"
                if rule.terminal and rule.decision == "deny"
                else "Complete"
                if rule.terminal and rule.decision == "allow"
                else current_phase
            )
            if (
                next_phase != current_phase
                and next_phase not in LIFECYCLE_TRANSITIONS[current_phase]
            ):
                return EvaluationReport(
                    host=event.host,
                    event=event.name,
                    phase=current_phase,
                    decision="deny",
                    diagnostic_code="CE_INVALID_TRANSITION",
                    reason=(
                        f"Lifecycle transition {current_phase} to {next_phase} is not declared."
                    ),
                    remedy="Request one declared transition from the current phase.",
                    terminal_reason=None,
                    facts_used=observed.used_facts,
                )
            return EvaluationReport(
                host=event.host,
                event=event.name,
                phase=next_phase,
                decision=rule.decision,
                diagnostic_code=rule.code,
                reason=reason,
                remedy=rule.remedy,
                terminal_reason=(
                    "blocked"
                    if rule.terminal and rule.decision == "deny"
                    else "complete"
                    if rule.terminal and rule.decision == "allow"
                    else None
                ),
                facts_used=observed.used_facts,
            )
    next_phase = event.target_phase or current_phase
    if next_phase != current_phase and next_phase not in LIFECYCLE_TRANSITIONS[current_phase]:
        return EvaluationReport(
            host=event.host,
            event=event.name,
            phase=current_phase,
            decision="deny",
            diagnostic_code="CE_INVALID_TRANSITION",
            reason=f"Lifecycle transition {current_phase} to {next_phase} is not declared.",
            remedy="Request one declared transition from the current phase.",
            terminal_reason=None,
            facts_used=observed.used_facts,
        )
    return EvaluationReport(
        host=event.host,
        event=event.name,
        phase=next_phase,
        decision="allow",
        diagnostic_code="CE_OK",
        reason="No lifecycle rule blocked this event.",
        remedy=None,
        terminal_reason=(
            "complete" if next_phase == "Complete" else "blocked" if next_phase == "Blocked" else None
        ),
        facts_used=observed.used_facts,
    )


class JournalCorruptionError(ValueError):
    pass


class EffectJournal:
    def __init__(self, path: Path, lock_timeout: float = 5.0):
        self.path = path
        self.lock_path = path.with_suffix(path.suffix + ".lock")
        self.lock_timeout = lock_timeout

    @staticmethod
    def _try_lock(handle) -> bool:
        try:
            if os.name == "nt":
                import msvcrt

                msvcrt.locking(handle.fileno(), msvcrt.LK_NBLCK, 1)
            else:
                import fcntl

                fcntl.flock(handle.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
            return True
        except (BlockingIOError, OSError):
            return False

    @staticmethod
    def _unlock(handle) -> None:
        handle.seek(0)
        if os.name == "nt":
            import msvcrt

            msvcrt.locking(handle.fileno(), msvcrt.LK_UNLCK, 1)
        else:
            import fcntl

            fcntl.flock(handle.fileno(), fcntl.LOCK_UN)

    @contextlib.contextmanager
    def _lock(self):
        self.lock_path.parent.mkdir(parents=True, exist_ok=True)
        with self.lock_path.open("a+b") as handle:
            handle.seek(0, os.SEEK_END)
            if handle.tell() == 0:
                handle.write(b"0")
                handle.flush()
            handle.seek(0)
            deadline = time.monotonic() + self.lock_timeout
            while not self._try_lock(handle):
                if time.monotonic() >= deadline:
                    raise TimeoutError(f"effect journal lock timed out after {self.lock_timeout}s")
                time.sleep(min(0.01, max(0.0, deadline - time.monotonic())))
            try:
                yield
            finally:
                self._unlock(handle)

    def _records_unlocked(self, session_id: str) -> list[dict[str, object]]:
        if not self.path.is_file():
            return []
        records: list[dict[str, object]] = []
        for line_number, line in enumerate(
            self.path.read_text(encoding="utf-8").splitlines(), start=1
        ):
            try:
                item = json.loads(line)
            except (json.JSONDecodeError, ValueError) as error:
                raise JournalCorruptionError(
                    f"invalid effect journal JSON at line {line_number}"
                ) from error
            if not isinstance(item, dict) or type(item.get("schemaVersion")) is not int:
                raise JournalCorruptionError(
                    f"invalid effect journal record at line {line_number}"
                )
            if item["schemaVersion"] not in {1, 2}:
                raise JournalCorruptionError(
                    f"invalid effect journal record at line {line_number}"
                )
            if item["schemaVersion"] == 2:
                required_strings = (
                    "identity",
                    "idempotencyKey",
                    "sessionId",
                    "event",
                    "toolCallId",
                    "rule",
                    "effect",
                    "payloadDigest",
                )
                if any(type(item.get(field)) is not str for field in required_strings):
                    raise JournalCorruptionError(
                        f"invalid effect journal record at line {line_number}"
                    )
                expected_key = hashlib.sha256(
                    "\0".join(
                        str(item[field])
                        for field in ("sessionId", "event", "toolCallId", "rule", "effect")
                    ).encode("utf-8")
                ).hexdigest()
                digest = str(item["payloadDigest"])
                if (
                    item["identity"] != CANONICAL_IDENTITY
                    or not str(item["sessionId"]).strip()
                    or item["idempotencyKey"] != expected_key
                    or re.fullmatch(r"[0-9a-f]{64}", digest) is None
                ):
                    raise JournalCorruptionError(
                        f"invalid effect journal record at line {line_number}"
                    )
            if item.get("sessionId") == session_id:
                records.append(item)
        return records

    def records(self, session_id: str) -> list[dict[str, object]]:
        if not session_id.strip():
            raise ValueError("journal read requires a non-empty session identity")
        with self._lock():
            return self._records_unlocked(session_id)

    def append(self, effect: Effect) -> bool:
        with self._lock():
            if any(
                item.get("idempotencyKey") == effect.key
                for item in self._records_unlocked(effect.session_id)
            ):
                return False
            self.path.parent.mkdir(parents=True, exist_ok=True)
            with self.path.open("a", encoding="utf-8", newline="\n") as handle:
                handle.write(json.dumps(effect.to_record(), sort_keys=True, separators=(",", ":")))
                handle.write("\n")
                handle.flush()
                os.fsync(handle.fileno())
            return True
