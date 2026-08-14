#!/usr/bin/env python3
"""Activate ChaosEngine and reject catastrophic shell scope."""

from __future__ import annotations

import json
import posixpath
import re
import shlex
import sys


ACTIVATION = "Follow .chaos-engine/skills/chaos-engine/SKILL.md before continuing."
ROOT_DRIVE = re.compile(r"(?i)(?:^|\s)[a-z]:\\(?:\s|$)")
ENV_ASSIGNMENT = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*=")
SHELLS = {"bash", "sh", "zsh"}
DOWNLOADERS = {"curl", "fetch", "wget"}


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


def main() -> int:
    try:
        event = json.load(sys.stdin)
    except (json.JSONDecodeError, OSError):
        event = {}
    tool_input = event.get("tool_input", {}) if isinstance(event, dict) else {}
    command = str(tool_input.get("command", "")) if isinstance(tool_input, dict) else ""
    event_name = str(event.get("hook_event_name", "")) if isinstance(event, dict) else ""
    lowered = command.casefold()
    destructive = (
        catastrophic_posix(command)
        or "git reset --hard" in lowered
        or (
            "remove-item" in lowered
            and "-recurse" in lowered
            and (ROOT_DRIVE.search(command) is not None or "$home" in lowered or "~" in command)
        )
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
    print(json.dumps({"additionalContext": f"ChaosEngine: {ACTIVATION}"}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
