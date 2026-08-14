#!/usr/bin/env python3
"""Activate ChaosEngine and reject catastrophic shell scope."""

from __future__ import annotations

import json
import re
import shlex
import sys


ACTIVATION = "Follow .chaos-engine/skills/chaos-engine/SKILL.md before continuing."
ROOT_DRIVE = re.compile(r"(?i)(?:^|\s)[a-z]:\\(?:\s|$)")


def broad_rm(command: str) -> bool:
    try:
        arguments = shlex.split(command, posix=True)
    except ValueError:
        return False
    if not arguments or arguments[0].casefold() != "rm":
        return False
    recursive = False
    force = False
    targets: list[str] = []
    for argument in arguments[1:]:
        lowered = argument.casefold()
        if lowered == "--recursive":
            recursive = True
        elif lowered == "--force":
            force = True
        elif lowered.startswith("-") and not lowered.startswith("--"):
            recursive = recursive or "r" in lowered
            force = force or "f" in lowered
        else:
            targets.append(lowered)
    return recursive and force and any(target in {"/", "~", "$home"} for target in targets)


def main() -> int:
    try:
        event = json.load(sys.stdin)
    except (json.JSONDecodeError, OSError):
        event = {}
    tool_input = event.get("tool_input", {}) if isinstance(event, dict) else {}
    command = str(tool_input.get("command", "")) if isinstance(tool_input, dict) else ""
    lowered = command.casefold()
    destructive = (
        broad_rm(command)
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
    print(json.dumps({"additionalContext": ACTIVATION}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
