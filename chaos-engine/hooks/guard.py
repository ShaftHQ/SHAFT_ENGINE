#!/usr/bin/env python3
"""Activate ChaosEngine and reject catastrophic shell scope."""

from __future__ import annotations

import json
import hashlib
import os
import posixpath
import re
import shlex
import shutil
import subprocess  # nosec B404 - fixed Git/gh argv only.
import sys
from pathlib import Path

try:
    import reflection
except ImportError:  # Repository source layout; installed hooks keep it beside guard.py.
    source_root = Path(__file__).resolve().parents[2]
    sys.path.insert(0, str(source_root))
    from scripts.agents import reflection


ACTIVATION = "Follow .chaos-engine/skills/chaos-engine/SKILL.md before continuing."
ROOT_DRIVE = re.compile(r"(?i)(?:^|\s)[a-z]:\\(?:\s|$)")
ENV_ASSIGNMENT = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*=")
SHELLS = {"bash", "sh", "zsh"}
DOWNLOADERS = {"curl", "fetch", "wget"}
TERMINAL_LABELS = (
    "elapsed estimate",
    "main time consumer",
    "main token consumer",
    "repeated failures or corrections",
    "changed assumption or approach",
    "successful proof",
    "remaining risk or follow-up",
    "learning loop disposition",
    "next-session optimization",
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
    for parsed in command_stages(command):
        head, arguments = command_head(parsed)
        git_arguments = list(arguments)
        while head == "git" and git_arguments and git_arguments[0].startswith("-"):
            option = git_arguments.pop(0).casefold()
            if option in {"-c", "--git-dir", "--work-tree"} and git_arguments:
                git_arguments.pop(0)
        if head in {"set-content", "add-content", "clear-content", "remove-content", "remove-item", "new-item", "out-file", "copy-item", "rename-item", "move-item", "touch", "rm", "mv", "cp"}:
            return True
        if any(redirect_operator(token) for token in parsed):
            return True
        if head == "git" and git_arguments and git_arguments[0].casefold() in {"add", "commit", "push", "merge", "rebase", "reset", "restore", "checkout", "switch", "clean", "rm", "mv", "tag", "branch", "cherry-pick"}:
            return True
        if head == "gh" and arguments[:2] in (["pr", "create"], ["pr", "edit"], ["pr", "merge"], ["pr", "close"], ["issue", "create"], ["issue", "edit"], ["issue", "close"]):
            return True
    return False


def shell_tokens(command: str) -> list[str]:
    try:
        lexer = shlex.shlex(command.replace("\r\n", "\n").replace("\n", " ; "), posix=True, punctuation_chars=";&|>")
        lexer.whitespace_split = True
        lexer.commenters = ""
        lexer.escape = ""
        return list(lexer)
    except ValueError:
        return []


def command_stages(command: str) -> list[list[str]]:
    stages: list[list[str]] = [[]]
    for token in shell_tokens(command):
        if token in {";", "&&", "||", "|", "&"}:
            if stages[-1]:
                stages.append([])
        else:
            stages[-1].append(token)
    return [stage for stage in stages if stage]


def redirect_operator(token: str) -> bool:
    redirect = chr(62)
    return bool(token) and not token.strip(redirect)


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
        if head.endswith(".exe"):
            head = head[:-4]
        if head in {"command", "env", "sudo", "timeout"}:
            index += 1
            while index < len(arguments) and (arguments[index].startswith("-") or ENV_ASSIGNMENT.match(arguments[index])):
                option = arguments[index].casefold()
                index += 1
                if option in {"-u", "--user", "-g", "--group", "-s", "--signal", "-k", "--kill-after"} and index < len(arguments):
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


def wrapped_exec_calls(source: str) -> tuple[tuple[str, str | None], ...] | None:
    calls: list[tuple[str, str | None]] = []
    matches = tuple(re.finditer(
        r'''\btools\.exec_command\s*\(\s*\{(?P<body>(?:"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'|[^{}])*)\}\s*\)''',
        source, re.DOTALL,
    ))
    if len(matches) != wrapped_exec_call_count(source):
        return None
    for match in matches:
        body = match.group("body")
        structural = re.sub(r'''"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*' ''', lambda value: value.group(0)[0] + value.group(0)[-1], body, flags=re.VERBOSE)
        if "..." in structural or "[" in structural or "]" in structural:
            return None
        if len(re.findall(r"\b(?:cmd|command)\s*:", structural)) != 1 or len(re.findall(r"\bworkdir\s*:", structural)) > 1:
            return None
        command_match = re.search(r'''\b(?:cmd|command)\s*:\s*(?P<literal>"(?:\\.|[^"\\])*")''', body)
        if command_match is None:
            return None
        try:
            command = json.loads(command_match.group("literal"))
        except (json.JSONDecodeError, ValueError):
            return None
        workdir_match = re.search(r'''\bworkdir\s*:\s*(?P<literal>"(?:\\.|[^"\\])*")''', body)
        if "workdir" in structural and workdir_match is None:
            return None
        workdir = json.loads(workdir_match.group("literal")) if workdir_match else None
        if not isinstance(command, str) or not command or (workdir is not None and (not isinstance(workdir, str) or not workdir.strip())):
            return None
        calls.append((command, workdir))
    return tuple(calls)


def wrapped_exec_commands(source: str) -> tuple[str, ...]:
    calls = wrapped_exec_calls(source)
    return tuple(command for command, _workdir in calls) if calls is not None else ()


def wrapped_exec_call_count(source: str) -> int:
    return len(re.findall(r"\btools\.exec_command\s*\(", js_structure(source)))


def js_structure(source: str) -> str:
    return re.sub(
        r'''"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'|`(?:\\.|[^`\\])*`''',
        lambda value: value.group(0)[0] + value.group(0)[-1],
        source,
        flags=re.DOTALL,
    )


def wrapped_patch_call_count(source: str) -> int:
    return len(re.findall(r"\btools\.apply_patch\s*\(", js_structure(source)))


def wrapped_patch_targets(source: str) -> tuple[str, ...] | None:
    count = wrapped_patch_call_count(source)
    matches = tuple(re.finditer(r'''\btools\.apply_patch\s*\(\s*(?P<literal>"(?:\\.|[^"\\])*")\s*\)''', source, re.DOTALL))
    if len(matches) != count:
        return None
    targets = []
    for match in matches:
        try:
            patch_text = json.loads(match.group("literal"))
        except (json.JSONDecodeError, ValueError):
            return None
        targets.extend(item.group(1).strip() for item in re.finditer(r"(?m)^\*\*\* (?:Add|Update|Delete) File:\s*(.+?)\s*$", patch_text))
    return tuple(targets)


def git_output(cwd: object, *arguments: str) -> str | None:
    executable = shutil.which("git")
    if not executable:
        return None
    try:
        result = subprocess.run([executable, *arguments], cwd=str(cwd or "."), capture_output=True, text=True, timeout=0.35, check=False)  # nosec B603
    except (OSError, subprocess.TimeoutExpired):
        return None
    return result.stdout.strip() if result.returncode == 0 else None


def current_branch(cwd: object) -> str | None:
    return git_output(cwd, "branch", "--show-current")


def repository_root(cwd: object) -> str | None:
    root = git_output(cwd, "rev-parse", "--show-toplevel")
    return os.path.realpath(root) if root else None


def on_default_branch(cwd: object) -> bool:
    branch = current_branch(cwd)
    remote_head = git_output(cwd, "symbolic-ref", "--short", "refs/remotes/origin/HEAD")
    default = remote_head.split("/", 1)[1] if remote_head and "/" in remote_head else None
    return bool(
        branch
        and (
            branch == default
            or (default is None and not branch.startswith("ChaosEngine/"))
        )
    )


def command_targets(command: str) -> tuple[str, ...]:
    targets: list[str] = []
    for parsed in command_stages(command):
        head, arguments = command_head(parsed)
        targets.extend(
            parsed[index + 1]
            for index, token in enumerate(parsed[:-1])
            if redirect_operator(token)
        )
        if head in {"set-content", "add-content", "clear-content", "remove-content", "remove-item", "new-item", "out-file", "touch", "rm"}:
            if head == "touch" and any(token.casefold() in {"-d", "--date", "-r", "--reference", "-t"} or token.casefold().startswith(("--date=", "--reference=")) for token in arguments):
                targets.append("$UNINSPECTABLE")
                continue
            path = next((arguments[index + 1] for index, token in enumerate(arguments[:-1]) if token.casefold() in {"-path", "-literalpath", "-filepath"}), None)
            values = [token for token in arguments if not token.startswith("-")]
            if head in {"rm", "touch"} and path is None:
                targets.extend(values)
            elif path or values:
                targets.append(path or values[0])
        elif head in {"copy-item", "rename-item", "move-item", "cp", "mv"}:
            destination = next((arguments[index + 1] for index, token in enumerate(arguments[:-1]) if token.casefold() in {"-t", "--target-directory", "-destination"}), None)
            destination = destination or next((token.split("=", 1)[1] for token in arguments if token.casefold().startswith("--target-directory=")), None)
            values = [token for token in arguments if not token.startswith("-")]
            if destination or len(values) >= 2:
                targets.append(destination or values[-1])
        elif head == "git":
            for index, token in enumerate(arguments[:-1]):
                if token.casefold() in {"-c", "--git-dir", "--work-tree"}:
                    targets.append(arguments[index + 1])
            for token in arguments:
                lowered = token.casefold()
                if lowered.startswith(("--git-dir=", "--work-tree=")):
                    targets.append(token.split("=", 1)[1])
                elif lowered.startswith("-c") and len(token) > 2:
                    targets.append(token[2:])
    return tuple(targets)


def target_on_default_branch(target: str, cwd: object) -> bool:
    if not target or re.search(r"[$%*?{}]", target):
        return True
    resolved = os.path.realpath(os.path.join(str(cwd or "."), target))
    probe = resolved if os.path.isdir(resolved) else os.path.dirname(resolved)
    if os.path.basename(probe).casefold() == ".git":
        probe = os.path.dirname(probe)
    while probe and not os.path.exists(probe):
        parent = os.path.dirname(probe)
        if parent == probe:
            break
        probe = parent
    root = repository_root(probe)
    return bool(root and on_default_branch(root))


def target_checkout_root(target: str, cwd: object) -> str | None:
    if not target or re.search(r"[$%*?{}]", target):
        return None
    resolved = os.path.realpath(os.path.join(str(cwd or "."), target))
    probe = resolved if os.path.isdir(resolved) else os.path.dirname(resolved)
    while probe and not os.path.exists(probe):
        parent = os.path.dirname(probe)
        if parent == probe:
            break
        probe = parent
    return repository_root(probe)


def effective_cwd(outer_cwd: object, workdir: object) -> object:
    if not isinstance(workdir, str) or not workdir.strip():
        return outer_cwd
    if os.path.isabs(workdir):
        return workdir
    return os.path.realpath(os.path.join(str(outer_cwd or "."), workdir))


def github_repository(remote: object) -> str | None:
    if not isinstance(remote, str):
        return None
    match = re.fullmatch(
        r"(?:https://github\.com/|git@github\.com:)([^/\s:]+/[^/\s]+?)(?:\.git)?",
        remote.strip(),
        re.IGNORECASE,
    )
    return match.group(1) if match else None


def parsed_options(
    options: list[str], *, flags: frozenset[str], values: frozenset[str]
) -> tuple[frozenset[str], dict[str, str]] | None:
    parsed_flags: set[str] = set()
    parsed_values: dict[str, str] = {}
    index = 0
    while index < len(options):
        token = options[index]
        name, separator, inline_value = token.partition("=")
        name = name.casefold()
        if name in flags and not separator and name not in parsed_flags:
            parsed_flags.add(name)
            index += 1
            continue
        if name not in values or name in parsed_values:
            return None
        if separator:
            value = inline_value
        else:
            index += 1
            if index >= len(options):
                return None
            value = options[index]
        if not value or value.startswith("-"):
            return None
        parsed_values[name] = value
        index += 1
    return frozenset(parsed_flags), parsed_values


def recovery_git_push(arguments: list[str], branch: str) -> bool:
    allowed_options = {"-u", "--set-upstream", "--porcelain", "--no-verify"}
    if any(token.startswith("-") and token.casefold() not in allowed_options for token in arguments):
        return False
    positional = [token for token in arguments if not token.startswith("-")]
    allowed_refspecs = {"HEAD", branch, f"HEAD:{branch}", f"HEAD:refs/heads/{branch}"}
    return len(positional) >= 2 and positional[0] == "origin" and all(
        refspec in allowed_refspecs for refspec in positional[1:]
    )


def recovery_pr_edit(options: list[str], repository: str | None) -> bool:
    parsed = parsed_options(
        options, flags=frozenset(), values=frozenset({"--body", "--body-file", "--repo"})
    )
    if parsed is None:
        return False
    _flags, values = parsed
    bodies = [name for name in ("--body", "--body-file") if name in values]
    return len(bodies) == 1 and ("--repo" not in values or values["--repo"] == repository)


def recovery_pr_create(
    options: list[str], cwd: object, branch: str, repository: str | None
) -> bool:
    parsed = parsed_options(
        options,
        flags=frozenset({"--draft"}),
        values=frozenset({"--base", "--head", "--body", "--body-file", "--repo"}),
    )
    if parsed is None:
        return False
    flags, values = parsed
    if values.get("--repo", repository) != repository:
        return False
    if len([name for name in ("--body", "--body-file") if name in values]) != 1:
        return False
    gh = shutil.which("gh")
    if not gh:
        return False
    try:
        result = subprocess.run([gh, "repo", "view", "--json", "defaultBranchRef"], cwd=str(cwd), capture_output=True, text=True, timeout=3, check=False)  # nosec B603
        default = (json.loads(result.stdout).get("defaultBranchRef") or {}).get("name") if result.returncode == 0 else None
    except (OSError, subprocess.TimeoutExpired, json.JSONDecodeError):
        return False
    return "--draft" in flags and values.get("--base") == default and values.get("--head") == branch


def initial_draft_recovery(
    command: str, cwd: object, branch: str, repository: str | None = None
) -> bool:
    parsed = shell_tokens(command)
    if not parsed or any(token in {";", "&&", "||", "|", "&"} for token in parsed):
        return False
    if any(ENV_ASSIGNMENT.match(token) for token in parsed):
        return False
    if any(os.environ.get(name) for name in ("GH_REPO", "GH_HOST", "GIT_DIR", "GIT_WORK_TREE")):
        return False
    head, arguments = command_head(parsed)
    if head == "git" and arguments[:1] == ["push"]:
        return recovery_git_push(arguments[1:], branch)
    if head == "git" and arguments[:1] == ["commit"]:
        return "--allow-empty" in arguments and "--amend" not in arguments and git_output(cwd, "status", "--porcelain", "--untracked-files=all") == ""
    if head == "gh" and arguments[:2] == ["pr", "edit"]:
        return recovery_pr_edit(arguments[2:], repository)
    if head == "gh" and arguments[:2] == ["pr", "create"]:
        return recovery_pr_create(arguments[2:], cwd, branch, repository)
    return False


def initial_plan_complete(body: object) -> bool:
    if not isinstance(body, str):
        return False
    visible = re.sub(r"<!--[\s\S]*?-->", "", body)
    visible = re.sub(r"(?ms)^```.*?^```[ \t]*$", "", visible)
    for section in ("Plan", "Scope", "Proof"):
        match = re.search(rf"(?ims)^##[ \t]+{section}[ \t]*\r?\n(.*?)(?=^##[ \t]+|\Z)", visible)
        if match is None or len(match.group(1).strip()) < 20:
            return False
    return True


def checkout_mismatch(cwd: object, targets: list[str] | tuple[str, ...]) -> bool:
    cwd_root = repository_root(cwd)
    roots = {root for target in targets if (root := target_checkout_root(target, cwd))}
    return bool(cwd_root and any(os.path.normcase(root) != os.path.normcase(cwd_root) for root in roots))


def command_contexts(outer_cwd: object, effective) -> tuple[str | None, list[object]]:
    cwds: list[object] = []
    for command, workdir in effective:
        if not mutation_command(command):
            continue
        cwd = effective_cwd(outer_cwd, workdir)
        cwds.append(cwd)
        if on_default_branch(cwd):
            return "R19 blocked: task work never lands on the default branch.", []
        targets = command_targets(command)
        if checkout_mismatch(cwd, targets):
            return "R19 blocked: the mutation target resolves inside a different checkout.", []
        if any(target_on_default_branch(target, cwd) for target in targets):
            return "R19 blocked: the mutation target resolves inside a default-branch checkout.", []
    return None, cwds


def direct_file_context(outer_cwd: object, tool_input: object) -> str | None:
    details = tool_input if isinstance(tool_input, dict) else {}
    targets: list[str] = []
    path = details.get("file_path") or details.get("path") or details.get("notebook_path")
    if path:
        targets.append(str(path))
    patch_text = str(details.get("patch") or details.get("input") or "")
    targets.extend(re.findall(r"(?m)^\*\*\* (?:Add|Update|Delete) File:\s*(.+?)\s*$", patch_text))
    if checkout_mismatch(outer_cwd, targets):
        return "R19 blocked: a file tool targets a different checkout."
    if on_default_branch(outer_cwd) or any(target_on_default_branch(target, outer_cwd) for target in targets):
        return "R19 blocked: task work never lands on the default branch."
    return None


def mutation_contexts(
    event: dict, tool_name: str, tool_input: object
) -> tuple[str | None, list[object], tuple[tuple[str, str | None], ...]]:
    outer_cwd = event.get("cwd") or "."
    calls = wrapped_exec_calls(tool_input) if tool_name == "functions.exec" and isinstance(tool_input, str) else None
    if tool_name == "functions.exec" and calls is None:
        return "R19 blocked: wrapped mutation command or workdir is ambiguous.", [], ()
    if tool_name == "functions.exec" and isinstance(tool_input, str) and wrapped_patch_call_count(tool_input):
        patch_targets = wrapped_patch_targets(tool_input)
        if patch_targets is None:
            return "R19 blocked: pass one JSON string literal directly to wrapped apply_patch.", [], ()
        if checkout_mismatch(outer_cwd, patch_targets):
            return "R19 blocked: a wrapped patch targets a different checkout.", [], ()
        if any(target_on_default_branch(target, outer_cwd) for target in patch_targets):
            return "R19 blocked: a wrapped patch targets a default-branch checkout.", [], ()
    direct_workdir = tool_input.get("workdir") if isinstance(tool_input, dict) else None
    effective = calls or tuple((command, direct_workdir) for command in ((str(tool_input.get("command") or tool_input.get("cmd") or ""),) if isinstance(tool_input, dict) else ()))
    reason, mutation_cwds = command_contexts(outer_cwd, effective)
    if reason:
        return reason, [], ()
    if tool_name in {"Write", "Edit", "NotebookEdit", "apply_patch"}:
        mutation_cwds.append(outer_cwd)
        if reason := direct_file_context(outer_cwd, tool_input):
            return reason, [], ()
    if tool_name == "functions.exec" and isinstance(tool_input, str) and wrapped_patch_call_count(tool_input):
        mutation_cwds.append(outer_cwd)
    return None, mutation_cwds, effective


def task_location(cwds: list[object], outer_cwd: object) -> tuple[str | None, object, str | None]:
    task_locations = [
        (repository_root(cwd) or os.path.realpath(str(cwd)), branch)
        for cwd in cwds
        if (branch := current_branch(cwd)) and branch.startswith("ChaosEngine/")
    ]
    task_locations = list(dict.fromkeys(task_locations))
    if len(task_locations) > 1:
        return "R31 blocked: one mutation call spans multiple task branches. Split it into inspectable calls.", outer_cwd, None
    if task_locations:
        return None, *task_locations[0]
    return None, outer_cwd, current_branch(outer_cwd)


def recovery_reason(
    commands: tuple[str, ...], exact: bool, cwd: object, branch: str, repository: str | None
) -> str | None:
    if not exact or len(commands) != 1:
        return None
    if initial_draft_recovery(commands[0], cwd, branch, repository):
        return "allowed"
    stages = command_stages(commands[0])
    head, arguments = command_head(stages[0]) if len(stages) == 1 else ("", [])
    if (head == "git" and arguments[:1] in (["push"], ["commit"])) or (
        head == "gh" and arguments[:2] in (["pr", "create"], ["pr", "edit"])
    ):
        return "R31 blocked: the command is not a permitted initial-draft recovery."
    return None


def verified_initial_draft_reason(event: dict, cwd: object, branch: str, effective, tool_name: str, tool_input: object) -> str | None:
    head = git_output(cwd, "rev-parse", "HEAD")
    remote = git_output(cwd, "config", "--get", "remote.origin.url")
    repository_name = github_repository(remote)
    marker = hashlib.sha256(f"{remote}|{branch}|{head}".encode()).hexdigest()
    session_id = str(event.get("session_id") or event.get("sessionId") or "")
    if any(item.get("kind") == "initial-draft" and item.get("marker") == marker for item in reflection.entries(session_id)):
        return None
    commands = tuple(command for command, _workdir in effective)
    exact = not (tool_name == "functions.exec" and isinstance(tool_input, str) and (wrapped_patch_call_count(tool_input) or wrapped_exec_call_count(tool_input) != 1))
    recovery = recovery_reason(commands, exact, cwd, branch, repository_name)
    if recovery == "allowed":
        return None
    if recovery:
        return recovery
    if not head:
        return "R31 blocked: the planning checkpoint must be identifiable."
    gh = shutil.which("gh")
    if not gh:
        return "R31 blocked: GitHub status is unavailable."
    try:
        repository_result = subprocess.run([gh, "repo", "view", "--json", "nameWithOwner,defaultBranchRef"], cwd=str(cwd), capture_output=True, text=True, timeout=3, check=False)  # nosec B603
        repository = json.loads(repository_result.stdout) if repository_result.returncode == 0 else {}
        name = repository.get("nameWithOwner")
        default = (repository.get("defaultBranchRef") or {}).get("name")
        merge_base = git_output(cwd, "merge-base", "HEAD", f"origin/{default}") if default else None
        changed = git_output(cwd, "diff", "--name-only", str(merge_base), "HEAD", "--") if merge_base else None
        if changed:
            return None
        if merge_base is None or changed is None:
            return "R31 blocked: the default-base tree state is unavailable."
        if git_output(cwd, "status", "--porcelain", "--untracked-files=all") != "":
            return "R31 blocked: the planning checkpoint must remain clean until draft verification."
        pull_result = subprocess.run([gh, "pr", "list", "--repo", str(name), "--head", branch, "--state", "open", "--json", "isDraft,headRefOid,baseRefName,changedFiles,body"], cwd=str(cwd), capture_output=True, text=True, timeout=3, check=False)  # nosec B603
        pulls = json.loads(pull_result.stdout) if pull_result.returncode == 0 else []
    except (OSError, subprocess.TimeoutExpired, json.JSONDecodeError):
        return "R31 blocked: GitHub status is unavailable."
    pull = pulls[0] if isinstance(pulls, list) and len(pulls) == 1 and isinstance(pulls[0], dict) else None
    if not pull or pull.get("isDraft") is not True or pull.get("headRefOid") != head or pull.get("baseRefName") != default or pull.get("changedFiles") != 0:
        return "R31 blocked: verify an exact-head, default-base, zero-file draft before the first mutation."
    if not initial_plan_complete(pull.get("body")):
        return "R31 blocked: the initial draft body needs substantive Plan, Scope, and Proof sections."
    if not reflection.append_entry(session_id, {"schemaVersion": 1, "kind": "initial-draft", "marker": marker}):
        return "R31 blocked: verified initial draft state could not be recorded."
    return None


def lifecycle_reason(event: dict, tool_name: str, tool_input: object, mutation: bool) -> str | None:
    if not mutation:
        return None
    commands = wrapped_exec_commands(tool_input) if tool_name == "functions.exec" and isinstance(tool_input, str) else ()
    if not commands and isinstance(tool_input, dict):
        command = str(tool_input.get("command") or tool_input.get("cmd") or "")
        commands = (command,) if command else ()
    if commands and all(knowledge_write_command(command) for command in commands):
        return None
    reason, cwds, effective = mutation_contexts(event, tool_name, tool_input)
    if reason:
        return reason
    reason, cwd, branch = task_location(cwds, event.get("cwd") or ".")
    if reason or not branch or not branch.startswith("ChaosEngine/"):
        return reason
    return verified_initial_draft_reason(event, cwd, branch, effective, tool_name, tool_input)


def knowledge_write_command(command: str) -> bool:
    head, arguments = command_head(shell_tokens(command))
    return bool(
        (head == "memory" and arguments[:1] in (["remember"], ["delete"], ["supersede"], ["patch"]))
        or (head == "mempalace" and arguments[:1] in (["add"], ["delete"], ["update"]))
    )


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
            print(json.dumps({"additionalContext": checkpoint_reason(checkpoint)}))
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
    mutation = tool_name in {"Write", "Edit", "NotebookEdit", "apply_patch"} or (
        tool_name == "functions.exec" and isinstance(tool_input, str) and wrapped_patch_call_count(tool_input)
    ) or any(
        mutation_command(candidate) and not tracker_command(candidate) for candidate in commands
    )
    if event_name == "PreToolUse" and checkpoint and not receipt_command and (mutation or unchanged_test):
        print(json.dumps({"decision": "block", "reason": checkpoint_reason(checkpoint)}))
        return 2
    if event_name == "PreToolUse":
        reason = lifecycle_reason(event, tool_name, tool_input, mutation)
        if reason:
            print(json.dumps({"decision": "block", "reason": reason}))
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
