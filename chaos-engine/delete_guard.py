"""
#7251: refuse deletes of work that exists nowhere else.

Used by the PreToolUse hook (hooks/guard.py) and by hookless bots through
``tool.py delete-check <paths>`` or ``tool.py delete-check --command "<cmd>"``.
Inside git a target must be tracked, clean, and pushed (an upstream exists and
HEAD is not ahead). Outside git a target must be on the scratch allow-list.
When the command cannot be parsed with confidence, the guard refuses.
#7343: the owner overrides a refusal with a one-time chat code bound to the
exact path list; a hard never-touch list refuses even with a code.
"""

from __future__ import annotations

import glob
import hashlib
import json
import os
import re
import secrets
import shlex
from fnmatch import fnmatchcase
import tempfile
import subprocess  # nosec B404 - fixed git argv only.
import sys
import time
from pathlib import Path

STATE_DIR = ".chaos-engine-state"
ALLOW_FILE = "delete-allow.txt"
ACK_ENV = "CHAOS_DELETE_ACK"
CODE_TTL = 600
CODE_ALPHABET = "ABCDEFGHJKMNPQRSTUVWXYZ23456789"
# No code unlocks these: a path segment matching a name, or a browser profile dir.
NEVER_NAMES = ("ROG*", "wt-relblock", "wt-ls", "ce-src-*", "yt", "designer-ws.git.retired")
NEVER_PATTERNS = ("**/.config/google-chrome*/**", "**/.config/chromium/**", "**/*chrome*-profile*/**")
NEVER_FILE = Path("~/.config/chaos-engine/never-delete.txt")
# Generic scratch: temp, caches, build output, video render output.
DEFAULT_ALLOW = (
    str(Path(tempfile.gettempdir()).resolve() / "**"),
    "~/.cache/**",
    "**/__pycache__/**",
    "**/.pytest_cache/**",
    "**/.mypy_cache/**",
    "**/.ruff_cache/**",
    "**/node_modules/.cache/**",
    "**/.gradle/caches/**",
    "**/target/**",
    "**/build/**",
    "**/dist/**",
    "**/.next/**",
    "**/.turbo/**",
    "**/*video*/**/render/**",
    "**/*video*/**/renders/**",
    "**/*video*/**/out/**",
    "**/*video*/**/frames/**",
    "**/*video*/**/clips/**",
)
DELETE_WORD = re.compile(r"(?:^|[^\w.-])(?:rm|unlink|shred|rmtree|clean)\b|-delete\b|-exec(?:dir)?\s+\S*rm\b")
UNSURE = re.compile(r"\$|`|<\(|>\(")
RMTREE = re.compile(
    r"(?:shutil\.rmtree|os\.remove|os\.unlink|os\.rmdir|\.unlink|\.rmtree)\s*\(\s*(?P<arg>[^)]*)\)"
)
LITERAL = re.compile(r"""^\s*(?:Path\()?\s*(?:r|R)?(['"])(?P<path>[^'"]+)\1\s*\)?\s*(?:,.*)?$""")
WRAPPERS = frozenset({"command", "env", "sudo", "timeout", "nice", "nohup", "exec", "builtin"})
SHELLS = frozenset({"sh", "bash", "zsh", "dash"})
ENV_ASSIGNMENT = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*=")


class Unsure(ValueError):
    """The command deletes something the parser cannot pin down."""


def _head(arguments: list[str]) -> tuple[str, list[str]]:
    index = 0
    while index < len(arguments):
        item = arguments[index]
        if ENV_ASSIGNMENT.match(item):
            index += 1
            continue
        head = os.path.basename(item)
        if head in WRAPPERS:
            index += 1
            while index < len(arguments) and (arguments[index].startswith("-") or ENV_ASSIGNMENT.match(arguments[index])):
                index += 1
            if head == "timeout" and index < len(arguments) and re.fullmatch(r"\d+[smhd]?", arguments[index]):
                index += 1
            continue
        return head, arguments[index + 1 :]
    return "", []


def _rm_targets(arguments: list[str]) -> list[str]:
    targets, options = [], True
    for item in arguments:
        if options and item == "--":
            options = False
        elif options and item.startswith("-") and item != "-":
            continue
        else:
            targets.append(item)
    if not targets:
        raise Unsure("rm without a target")
    return targets


def _find_targets(arguments: list[str]) -> list[str]:
    deletes = "-delete" in arguments or any(
        item == "-exec" and index + 1 < len(arguments) and os.path.basename(arguments[index + 1]) in {"rm", "rmdir", "unlink", "shred"}
        for index, item in enumerate(arguments)
    ) or "-execdir" in arguments
    if not deletes:
        return []
    roots = []
    for item in arguments:
        if item.startswith(("-", "(", "!")):
            break
        roots.append(item)
    return roots or ["."]


def _drop_git_options(arguments: list[str]) -> list[str]:
    kept, skip = [], False
    for item in arguments:
        if skip:
            skip = False
        elif item in {"-C", "-c", "--git-dir", "--work-tree"}:
            skip = True
        else:
            kept.append(item)
    return kept


def _git_clean_targets(arguments: list[str]) -> list[str] | None:
    words = [item for item in arguments if not item.startswith("-")]
    if not words or words[0] != "clean":
        return None
    flags = "".join(item for item in arguments if item.startswith("-") and not item.startswith("--"))
    if "n" in flags or "--dry-run" in arguments:
        return []
    if "--" in arguments:
        return arguments[arguments.index("--") + 1 :] or ["."]
    return words[1:] or ["."]


def _python_targets(segment: str) -> list[str]:
    targets = []
    for match in RMTREE.finditer(segment):
        literal = LITERAL.match(match.group("arg"))
        if literal is None:
            raise Unsure("non-literal path in a Python delete call")
        targets.append(literal.group("path"))
    return targets


def _segment_targets(arguments: list[str], cwd: Path) -> list[Path]:
    head, rest = _head(arguments)
    raw: list[str] = []
    if head in {"rm", "unlink", "shred"}:
        raw = _rm_targets(rest)
    elif head == "find":
        raw = _find_targets(rest)
    elif head == "git":
        raw = _git_clean_targets(_drop_git_options(rest)) or []
    elif head in {"xargs", "parallel"} and DELETE_WORD.search(" ".join(rest)):
        raise Unsure("xargs/parallel delete: targets come from stdin")
    elif head in SHELLS and "-c" in rest and rest.index("-c") + 1 < len(rest):
        return _command_targets(rest[rest.index("-c") + 1], cwd)[0]
    return [path for item in raw for path in _expand(item, cwd)]


def _segments(line: str) -> list[list[str]]:
    lexer = shlex.shlex(line, posix=True, punctuation_chars=";&|")
    lexer.whitespace_split = True
    segments: list[list[str]] = [[]]
    for token in lexer:
        if token and set(token) <= set(";&|"):
            segments.append([])
        else:
            segments[-1].append(token)
    return [segment for segment in segments if segment]


def _command_targets(command: str, cwd: Path) -> tuple[list[Path], Path]:
    """Every path a shell command would delete, resolved against a tracked cwd."""
    found: list[Path] = []
    for line in command.splitlines():
        if RMTREE.search(line):
            found.extend(path for item in _python_targets(line) for path in _expand(item, cwd))
        try:
            segments = _segments(line)
        except ValueError as error:
            if DELETE_WORD.search(line):
                raise Unsure(f"unparsable delete: {error}") from error
            continue
        for arguments in segments:
            head, rest = _head(arguments)
            if head in {"cd", "pushd"}:
                target = rest[0] if rest else "~"
                if UNSURE.search(target):
                    raise Unsure("cd to a computed path before a delete")
                cwd = (cwd / os.path.expanduser(target)).resolve()
                continue
            if head == "git" and "-C" in rest and rest.index("-C") + 1 < len(rest):
                cwd = (cwd / os.path.expanduser(rest[rest.index("-C") + 1])).resolve()
            found.extend(_segment_targets(arguments, cwd))
    return found, cwd


def _expand(raw: str, cwd: Path) -> list[Path]:
    if UNSURE.search(raw):
        raise Unsure(f"computed delete target: {raw}")
    expanded = os.path.expanduser(raw)
    base = expanded if os.path.isabs(expanded) else str(cwd / expanded)
    if glob.has_magic(base):
        return [Path(item).resolve() for item in glob.glob(base, recursive=True)]
    return [Path(os.path.normpath(base))]


def command_targets(command: str, cwd: Path | None = None) -> list[Path]:
    return _command_targets(command, (cwd or Path.cwd()).resolve())[0]


def _git(cwd: Path, *argv: str) -> subprocess.CompletedProcess:
    return subprocess.run(  # nosec B603 B607 - fixed git argv.
        ["git", "-C", str(cwd), *argv], capture_output=True, text=True, check=False, timeout=30
    )


def _existing(path: Path) -> Path:
    while not path.exists() and path != path.parent:
        path = path.parent
    return path if path.is_dir() else path.parent


def _patterns(project: Path | None) -> list[str]:
    patterns = list(DEFAULT_ALLOW)
    for root in {project, Path.home()} - {None}:
        allow = Path(root) / STATE_DIR / ALLOW_FILE
        if allow.is_file():
            patterns += [line.strip() for line in allow.read_text(encoding="utf-8").splitlines() if line.strip() and not line.startswith("#")]
    return [os.path.expanduser(item) for item in patterns]


def _pattern_regex(pattern: str) -> re.Pattern:
    """`**/` spans any directories, `*` stays inside one segment; a trailing `/**` also matches the dir."""
    out, index = "", 0
    while index < len(pattern):
        if pattern.startswith("**/", index):
            out, index = out + "(?:.*/)?", index + 3
        elif pattern.startswith("/**", index) and index + 3 == len(pattern):
            out, index = out + "(?:/.*)?", index + 3
        elif pattern.startswith("**", index):
            out, index = out + ".*", index + 2
        elif pattern[index] == "*":
            out, index = out + "[^/]*", index + 1
        elif pattern[index] == "?":
            out, index = out + "[^/]", index + 1
        else:
            out, index = out + re.escape(pattern[index]), index + 1
    return re.compile(out + r"\Z")


def allow_listed(path: Path, project: Path | None = None) -> bool:
    text = str(path)
    return any(_pattern_regex(pattern).match(text) for pattern in _patterns(project))


def _git_reason(path: Path, top: Path) -> str:
    status = _git(top, "status", "--porcelain", "--ignored", "--untracked-files=all", "--", str(path))
    if status.returncode != 0:
        return "git status failed"
    dirty = [line for line in status.stdout.splitlines() if line.strip()]
    if dirty:
        return f"{len(dirty)} untracked/modified/ignored file(s), e.g. {dirty[0][3:]}"
    if _git(top, "rev-parse", "--abbrev-ref", "@{u}").returncode != 0:
        return "branch has no upstream (nothing pushed)"
    ahead = _git(top, "rev-list", "--count", "@{u}..HEAD").stdout.strip()
    if ahead not in {"", "0"}:
        return f"branch is {ahead} commit(s) ahead of its upstream"
    if top == path or top.is_relative_to(path):
        refs = _git(top, "for-each-ref", "--format=%(refname:short) %(upstream:track) %(upstream)", "refs/heads")
        for line in refs.stdout.splitlines():
            parts = line.split()
            if len(parts) < 2 or "ahead" in line:
                return f"local branch {parts[0]} is unpushed"
        if _git(top, "stash", "list").stdout.strip():
            return "repo has stash entries"
    return ""


def path_reason(path: Path, project: Path | None = None) -> str:
    """Empty string when deleting ``path`` loses nothing; else the refusal reason."""
    if allow_listed(path, project):
        return ""
    if not path.exists() and not path.is_symlink():
        return ""
    if path.is_symlink():
        return ""  # rm removes the link, not its target.
    probe = _existing(path)
    top = _git(probe, "rev-parse", "--show-toplevel")
    if top.returncode != 0:
        return "outside git and not on the delete allow-list"
    return _git_reason(path, Path(top.stdout.strip()).resolve())


def _ack_store() -> Path:
    return Path(os.environ.get("XDG_STATE_HOME") or Path.home() / ".local/state") / "chaos-engine" / "delete-acks.json"


def _log(entry: dict) -> None:
    log = _ack_store().with_name("delete-overrides.jsonl")
    log.parent.mkdir(parents=True, exist_ok=True)
    with log.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps({"at": int(time.time()), **entry}, sort_keys=True) + "\n")


def _never_rules() -> tuple[list[str], list[str]]:
    names, patterns = list(NEVER_NAMES), list(NEVER_PATTERNS)
    extra = NEVER_FILE.expanduser()
    if extra.is_file():
        for line in extra.read_text(encoding="utf-8").splitlines():
            item = line.strip()
            if item and not item.startswith("#"):
                (patterns if "/" in item else names).append(os.path.expanduser(item))
    return names, patterns


def never_touch(path: Path) -> str:
    """Non-empty when no code may unlock deleting ``path`` (itself, inside, or directly containing a protected dir)."""
    names, patterns = _never_rules()
    for part in path.parts:
        hit = next((name for name in names if fnmatchcase(part, name)), "")
        if hit:
            return f"never-touch `{hit}`"
    text = str(path)
    hit = next((item for item in patterns if _pattern_regex(item).match(text)), "")
    if hit:
        return f"never-touch `{hit}`"
    if path.is_dir() and not path.is_symlink():
        for depth in ("", "*/"):
            for name in names:
                if any(path.glob(depth + name)):
                    return f"contains never-touch `{name}`"
        if any((path / ".config").glob("google-chrome*")) or (path / ".config" / "chromium").exists():
            return "contains a browser profile"
    return ""


def _paths_digest(paths: list[Path]) -> str:
    return hashlib.sha256("\n".join(sorted({str(path) for path in paths})).encode("utf-8")).hexdigest()


def _code_digest(code: str) -> str:
    return hashlib.sha256(code.strip().upper().encode("utf-8")).hexdigest()


def _load_codes() -> dict:
    store = _ack_store()
    try:
        codes = json.loads(store.read_text(encoding="utf-8")) if store.is_file() else {}
    except ValueError:
        codes = {}
    now = time.time()
    return {key: value for key, value in codes.items() if isinstance(value, dict) and value.get("expires", 0) >= now and "paths" in value}


def _save_codes(codes: dict) -> None:
    store = _ack_store()
    store.parent.mkdir(parents=True, exist_ok=True)
    store.write_text(json.dumps(codes, sort_keys=True), encoding="utf-8")
    store.chmod(0o600)


def request_code(paths: list[Path], ttl: float = CODE_TTL) -> str:
    """Mint a one-time code for exactly ``paths``; the bot shows it to the owner in chat."""
    if not paths:
        raise PermissionError("no paths")
    for path in paths:
        hit = never_touch(path)
        if hit:
            raise PermissionError(f"`{path}` is {hit}; no code unlocks it")
    code = "".join(secrets.choice(CODE_ALPHABET) for _ in range(6))
    codes = _load_codes()
    codes[_code_digest(code)] = {"paths": _paths_digest(paths), "count": len(set(paths)), "expires": time.time() + ttl}
    _save_codes(codes)
    _log({"issued": True, "paths": _paths_digest(paths)[:12], "count": len(set(paths))})
    return code


def consume_code(code: str, paths: list[Path]) -> str:
    """'' when ``code`` matches ``paths`` (then it is spent); else why it was rejected."""
    if not code:
        return "no code"
    codes = _load_codes()
    entry = codes.get(_code_digest(code))
    if entry is None:
        return "code unknown, expired or already used"
    if entry.get("paths") != _paths_digest(paths):
        return "code was issued for a different path list"
    del codes[_code_digest(code)]
    _save_codes(codes)
    _log({"used": True, "paths": entry["paths"][:12], "targets": [str(path) for path in paths][:20]})
    return ""


def check(command: str, cwd: Path | None = None, project: Path | None = None, code: str | None = None) -> tuple[str, list[str]]:
    """(refusal or '', warnings). A matching owner code turns git/allow-list refusals into warnings."""
    if not (DELETE_WORD.search(command) or RMTREE.search(command)):
        return "", []
    try:
        targets = command_targets(command, cwd)
    except Unsure as error:
        return f"Delete guard (#7251): refused, cannot tell what this deletes ({error}). Name literal paths and run `tool.py delete-check <paths>`.", []
    for target in targets:
        hit = never_touch(target)
        if hit:
            return f"Delete guard (#7343): refused `{target}`: {hit}. No code unlocks it.", []
    reasons = [(target, reason) for target in targets for reason in [path_reason(target, project)] if reason]
    if not reasons:
        return "", []
    code = code if code is not None else os.environ.get(ACK_ENV, "")
    rejected = consume_code(code, targets) if code else "no code"
    if not rejected:
        return "", [f"`{target}`: {reason} (owner code accepted)" for target, reason in reasons]
    target, reason = reasons[0]
    hint = f"owner code rejected: {rejected}. " if code else ""
    return (
        f"Delete guard (#7251): refused `{target}`: {reason}. {hint}Commit and push it, back it up, "
        f"add a scratch pattern to {STATE_DIR}/{ALLOW_FILE}, or run `tool.py delete-check --request-code <same paths>` "
        f"and ask the owner to paste the code back in chat."
    ), []


def block_reason(command: str, cwd: Path | None = None, project: Path | None = None) -> str:
    """Hook entry point: '' to allow, else a refusal message."""
    return check(command, cwd, project)[0]


def doctor_check(root: Path) -> tuple[str, str]:
    """`delete-guard` row: the hook calls this module and tool.py exposes delete-check."""
    hook = root / "hooks" / "guard.py"
    tool = root / "tool.py"
    if not hook.is_file() or "delete_guard" not in hook.read_text(encoding="utf-8"):
        return "fail", "hook not wired"
    if not tool.is_file() or "delete-check" not in tool.read_text(encoding="utf-8"):
        return "fail", "tool.py delete-check missing"
    return "pass", ""


def _workspaces(project: Path) -> list[Path]:
    listed = project / STATE_DIR / "backup-workspaces.txt"
    extra = os.environ.get("CHAOS_BACKUP_WORKSPACES", "")
    paths = [project]
    if listed.is_file():
        paths += [Path(line.strip()).expanduser() for line in listed.read_text(encoding="utf-8").splitlines() if line.strip() and not line.startswith("#")]
    paths += [Path(item).expanduser() for item in extra.split(os.pathsep) if item]
    return [path for path in dict.fromkeys(paths) if path.is_dir()]


CONSUMER_MARKER = ".chaos-engine-state/consumer-mode"


def backup_remote_issue(path: Path) -> str:
    if _git(path, "rev-parse", "--show-toplevel").returncode != 0:
        return "not in git"
    # #7263: consumer mode (#6237) hides .memory/mempalace via info/exclude, so a clean status proves nothing.
    if (path / CONSUMER_MARKER).is_file():
        return "consumer mode hides harness state from git"
    if not _git(path, "remote").stdout.strip():
        return "no remote"
    if _git(path, "rev-parse", "--abbrev-ref", "@{u}").returncode != 0:
        return "no upstream"
    if _git(path, "rev-list", "--count", "@{u}..HEAD").stdout.strip() not in {"", "0"}:
        return "unpushed commits"
    if _git(path, "status", "--porcelain", "--untracked-files=normal").stdout.strip():
        return "uncommitted files"
    return ""


def backup_remote_status(project: Path) -> tuple[str, str]:
    """Advisory only: never fails doctor."""
    issues = []
    for path in _workspaces(project):
        issue = backup_remote_issue(path)
        if issue:
            issues.append(f"{path.name}: {issue}")
    if not issues:
        return "pass", ""
    return "warn", "; ".join(issues[:3]) + (f" (+{len(issues) - 3})" if len(issues) > 3 else "") + "; push or back up"


def _rm(paths: list[str]) -> str:
    return "rm -rf -- " + " ".join(shlex.quote(item) for item in paths)


def main(arguments: list[str]) -> int:
    code = None
    if arguments[:1] == ["--code"] and len(arguments) >= 2:
        code, arguments = arguments[1], arguments[2:]
    if arguments[:1] == ["--request-code"] and len(arguments) >= 2 and code is None:
        try:
            paths = list(dict.fromkeys(command_targets(_rm(arguments[1:]), Path.cwd())))
            code = request_code(paths)
        except (PermissionError, Unsure) as error:
            print(f"delete-code: refused: {error}", file=sys.stderr)
            return 2
        print(f"delete-code: {code} (one use, {CODE_TTL // 60} min, {len(paths)} path(s)). Show it to the owner; "
              f"after they paste it back run: tool.py delete-check --code <code> <same paths>")
        return 0
    if arguments[:1] == ["--command"] and len(arguments) == 2:
        reason, warnings = check(arguments[1], Path.cwd(), Path.cwd(), code)
    elif arguments and not arguments[0].startswith("--"):
        reason, warnings = check(_rm(arguments), Path.cwd(), Path.cwd(), code)
    else:
        print("usage: tool.py delete-check [--code CODE] <paths...> | [--code CODE] --command '<cmd>' | --request-code <paths...>", file=sys.stderr)
        return 2
    if reason:
        print(reason, file=sys.stderr)
        return 1
    for warning in warnings:
        print(f"delete-check: warning: {warning}", file=sys.stderr)
    print("delete-check: allowed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
