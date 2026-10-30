"""
#7251: refuse deletes of work that exists nowhere else.

Used by the PreToolUse hook (hooks/guard.py) and by hookless bots through
``tool.py delete-check <paths>`` or ``tool.py delete-check --command "<cmd>"``.
Inside git a target must be tracked, clean, and pushed (an upstream exists and
HEAD is not ahead). Outside git a target must be on the scratch allow-list.
When the command cannot be parsed with confidence, the guard refuses.
#7343: the owner overrides a refusal with a one-time chat code bound to the
exact path list; a hard never-touch list refuses even with a code.
#7525: verified uploaded yt files on a local/env allowlist (path + video id + sha256)
may take the owner code after live sha + oEmbed author proof; yt/meta and unverified stay locked.
oEmbed HTTP 403 means private (not a box IP block); optional CHAOS_YT_EXPECTED_AUTHORS binds channel.
Also covers raw filesystem deletes that previously bypassed the shell-word trigger:
``mv`` of a source path (including to trash/outside), ``rsync --delete*`` /
``--remove-source-files``, and ``Path(...).unlink|rmdir|rmtree`` constructor forms.
Tree ops that follow links (``find -delete``, ``rsync --delete*``, ``rm -r`` with a
trailing slash, ``shutil.rmtree`` / ``Path.rmtree``, ``mv`` of ``src/``) resolve the
realpath and apply ``path_reason`` to the target; plain ``rm`` of a symlink stays a
link-only delete. Symlink resolution failures refuse closed.
A delete-check refusal is final: do not bypass with raw ``rm``, ``unlink``,
encoded argv, or a wrapper script; the PreToolUse hook and ``block_reason``
enforce the same gate. Self-created paths may be deleted without a code only
when recorded by the tool in the HMAC-bound per-session created-paths
manifest and contained in the session scoped dir or worktree; CE-managed
dirt is restored via ``git checkout <primary-sha> -- <paths>`` in the current
worktree only (never deleted, never other worktrees). Never-touch stays locked.
"""

from __future__ import annotations

import glob
import hashlib
import hmac
import json
import os
import re
import secrets
import stat
import shlex
from fnmatch import fnmatchcase
import tempfile
import subprocess  # nosec B404 - fixed git argv only.
import sys
import time
from pathlib import Path
from typing import Callable
import urllib.error
import urllib.request

STATE_DIR = ".chaos-engine-state"
ALLOW_FILE = "delete-allow.txt"
# Per-session created-paths manifest (scoped temp). Exact paths listed here may be
# deleted without an owner code when the HMAC verifies and the path is contained;
# never-touch still wins. Env overrides the path. HMAC key stays outside the tree.
SESSION_CREATED_MANIFEST_ENV = "CHAOS_SESSION_CREATED_MANIFEST"
SESSION_CREATED_MANIFEST_NAME = "created-paths.json"
SESSION_MANIFEST_HMAC_ENV = "CHAOS_SESSION_MANIFEST_HMAC"
SESSION_CONTAINMENT_ROOTS_ENV = "CHAOS_SESSION_CONTAINMENT_ROOTS"
SESSION_WORKTREE_ENV = "CHAOS_SESSION_WORKTREE"
SESSION_MANIFEST_SCHEMA = 2
# Relative roots treated as CE-managed for primary-SHA restore (checkout, never delete).
CE_MANAGED_PREFIXES = (".chaos-engine/",)
ACK_ENV = "CHAOS_DELETE_ACK"
CODE_TTL = 600
CODE_ALPHABET = "ABCDEFGHJKMNPQRSTUVWXYZ23456789"
# No code unlocks these: a path segment matching a name, or a browser profile dir.
NEVER_NAMES = ("ROG*", "yt", "designer-ws.git.retired")
# Old renders inside yt/ (not yt/meta/) such as clip.r3.mp4 go through the owner-code flow.
YT_RENDER = re.compile(r".+\.r\d+\.mp4")
NEVER_PATTERNS = (
    "**/.config/google-chrome*/**",
    "**/.config/chromium/**",
    "**/*chrome*-profile*/**",
    "**/.ssh/**",
    "**/.env",
    "**/.env.*",
    "**/credentials.json",
    "**/id_rsa",
    "**/id_ed25519",
)
NEVER_FILE = Path("~/.config/chaos-engine/never-delete.txt")
# #7525: exact-file allowlist of verified uploaded yt files (local/env only; not committed).
YT_VERIFIED_ENV = "CHAOS_YT_VERIFIED_DELETE"
YT_VERIFIED_FILE = Path("~/.config/chaos-engine/yt-verified-delete.txt")
YT_VIDEO_ID = re.compile(r"^[A-Za-z0-9_-]{11}$")
YT_SHA256 = re.compile(r"^[a-f0-9]{64}$")
YT_OEMBED = "https://www.youtube.com/oembed?url=https://www.youtube.com/watch?v={id}&format=json"
YT_OEMBED_TTL = 86400
YT_OEMBED_SOFT_TTL = 60  # network/5xx: short negative cache so outages do not lock for a day
YT_EXPECTED_AUTHORS_ENV = "CHAOS_YT_EXPECTED_AUTHORS"  # optional comma-separated oEmbed author_name values
YT_OEMBED_UA = "Mozilla/5.0 (compatible; ChaosEngine-delete-guard/2)"
_youtube_video_verifier: Callable[[str], bool] | None = None
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
DELETE_WORD = re.compile(
    r"(?:^|[^\w.-])(?:rm|unlink|shred|rmtree|clean|mv)\b|-delete\b|--remove-source-files\b|"
    r"-exec(?:dir)?\s+\S*rm\b"
)
UNSURE = re.compile(r"\$|`|<\(|>\(")
RMTREE = re.compile(
    r"(?:shutil\.rmtree|os\.remove|os\.unlink|os\.rmdir|\.unlink|\.rmtree|\.rmdir)\s*\(\s*(?P<arg>[^)]*)\)"
)
# Path('literal').unlink() / .rmdir() / .rmtree() — path is in the constructor, not the call.
PATHLIB_DELETE = re.compile(
    r"""(?:pathlib\.)?Path\s*\(\s*(?:r|R)?(['"])(?P<path>[^'"]+)\1\s*\)\s*\.\s*(?:unlink|rmdir|rmtree)\b"""
)
LITERAL = re.compile(r"""^\s*(?:Path\()?\s*(?:r|R)?(['"])(?P<path>[^'"]+)\1\s*\)?\s*(?:,.*)?$""")
RS_DELETE = re.compile(r"^--delete(?:-|$)|^-delete$|^--remove-source-files$")
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


def _python_entries(segment: str) -> list[tuple[str, bool]]:
    """Return (path, follow_symlinks) for Python delete calls in ``segment``."""
    entries: list[tuple[str, bool]] = []
    for match in PATHLIB_DELETE.finditer(segment):
        method = match.group(0).rsplit(".", 1)[-1]
        # Path.rmtree follows; unlink/rmdir operate on the named inode (link itself).
        follow = method.startswith("rmtree")
        entries.append((match.group("path"), follow))
    for match in RMTREE.finditer(segment):
        arg = match.group("arg")
        # Method calls already captured via PATHLIB_DELETE have an empty arg list.
        if not arg.strip() and match.group(0).lstrip().startswith("."):
            continue
        literal = LITERAL.match(arg)
        if literal is None:
            raise Unsure("non-literal path in a Python delete call")
        token = match.group(0)
        follow = "rmtree" in token
        entries.append((literal.group("path"), follow))
    return entries


def _operands(arguments: list[str]) -> list[str]:
    """Non-option operands after optional ``--``."""
    targets, options = [], True
    for item in arguments:
        if options and item == "--":
            options = False
        elif options and item.startswith("-") and item != "-":
            continue
        else:
            targets.append(item)
    return targets


def _rm_recursive(arguments: list[str]) -> bool:
    for item in arguments:
        if item in {"-r", "-R", "--recursive"}:
            return True
        if item.startswith("-") and not item.startswith("--") and any(flag in item for flag in "rR"):
            return True
    return False


def _mv_entries(arguments: list[str]) -> list[tuple[str, bool]]:
    """``mv`` sources; trailing slash means move-through-contents (follow)."""
    operands = _operands(arguments)
    if len(operands) < 2:
        raise Unsure("mv without source and destination")
    return [(item, item.endswith(("/", os.sep))) for item in operands[:-1]]


def _rsync_entries(arguments: list[str]) -> list[tuple[str, bool]]:
    """``rsync --delete*`` / ``--remove-source-files`` always follow symlink operands."""
    deleteish = any(RS_DELETE.match(item) or item.startswith("--delete-") for item in arguments)
    remove_sources = "--remove-source-files" in arguments
    if not deleteish and not remove_sources:
        return []
    operands = _operands(arguments)
    if len(operands) < 2:
        raise Unsure("rsync delete without source and destination")
    sources, dest = operands[:-1], operands[-1]
    raw = [dest] if deleteish else []
    if remove_sources:
        raw = list(sources) + raw
    if not raw:
        raise Unsure("rsync delete without resolvable targets")
    return [(item, True) for item in raw]


def _resolve_follow(path: Path) -> Path:
    """Realpath for tree ops that follow links; refuse closed when resolution fails."""
    try:
        if path.is_symlink() or path.exists():
            return path.resolve(strict=True)
    except (OSError, RuntimeError) as error:
        raise Unsure(f"cannot resolve symlink delete target: {path}") from error
    return path


def _expand_entries(raw: list[tuple[str, bool]], cwd: Path) -> list[tuple[Path, bool]]:
    found: list[tuple[Path, bool]] = []
    for item, follow in raw:
        for path in _expand(item, cwd):
            found.append((path, follow))
    return found


def _segment_targets(arguments: list[str], cwd: Path) -> list[tuple[Path, bool]]:
    head, rest = _head(arguments)
    if head in {"rm", "unlink", "shred"}:
        recursive = _rm_recursive(rest)
        # Trailing slash + recursive: GNU rm follows the symlink and deletes contents.
        return _expand_entries(
            [(item, recursive and item.endswith(("/", os.sep))) for item in _rm_targets(rest)],
            cwd,
        )
    if head == "mv":
        return _expand_entries(_mv_entries(rest), cwd)
    if head == "rsync":
        return _expand_entries(_rsync_entries(rest), cwd)
    if head == "find":
        # find follows a symlink start point when searching/deleting under it.
        return _expand_entries([(item, True) for item in _find_targets(rest)], cwd)
    if head == "git":
        raw = _git_clean_targets(_drop_git_options(rest)) or []
        return _expand_entries([(item, False) for item in raw], cwd)
    if head in {"xargs", "parallel"} and DELETE_WORD.search(" ".join(rest)):
        raise Unsure("xargs/parallel delete: targets come from stdin")
    if head in SHELLS and "-c" in rest and rest.index("-c") + 1 < len(rest):
        return _command_targets(rest[rest.index("-c") + 1], cwd)[0]
    return []


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


def _command_targets(command: str, cwd: Path) -> tuple[list[tuple[Path, bool]], Path]:
    """Every (path, follow_symlinks) a shell command would delete, against a tracked cwd."""
    found: list[tuple[Path, bool]] = []
    for line in command.splitlines():
        if RMTREE.search(line) or PATHLIB_DELETE.search(line):
            found.extend(_expand_entries(_python_entries(line), cwd))
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
    """Paths a command would delete; tree ops resolve symlink operands to their realpath."""
    entries, _ = _command_targets(command, (cwd or Path.cwd()).resolve())
    resolved: list[Path] = []
    for path, follow in entries:
        resolved.append(_resolve_follow(path) if follow else path)
    return list(dict.fromkeys(resolved))


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



def session_manifest_path(explicit: Path | None = None) -> Path | None:
    """
    Resolve the per-session created-paths manifest, or None when unset/missing."""
    if explicit is not None:
        return Path(explicit)
    raw = os.environ.get(SESSION_CREATED_MANIFEST_ENV, "").strip()
    if not raw:
        return None
    return Path(os.path.expanduser(raw))


def _session_hmac_key_path(manifest: Path) -> Path:
    """Per-manifest HMAC key file outside any checkout (XDG config)."""
    digest = hashlib.sha256(str(manifest.resolve()).encode("utf-8")).hexdigest()[:24]
    return (
        Path(os.environ.get("XDG_CONFIG_HOME") or Path.home() / ".config")
        / "chaos-engine"
        / "session-manifest-hmac-keys"
        / f"{digest}.hmac-key"
    )


def _read_session_hmac_key(manifest: Path) -> bytes | None:
    """
    Return the per-session HMAC key, or None when unset (forge-closed on load)."""
    raw = os.environ.get(SESSION_MANIFEST_HMAC_ENV, "").strip()
    if raw:
        return raw.encode("utf-8")
    store = _session_hmac_key_path(manifest)
    if store.is_file():
        return store.read_bytes()
    return None


def _load_or_create_session_hmac_key(manifest: Path) -> bytes:
    """
    Return the per-session HMAC key; create one outside the tree when missing."""
    existing = _read_session_hmac_key(manifest)
    if existing is not None:
        return existing
    store = _session_hmac_key_path(manifest)
    store.parent.mkdir(parents=True, exist_ok=True)
    hmac_key = secrets.token_bytes(32)
    store.write_bytes(hmac_key)
    store.chmod(0o600)
    return hmac_key


def _manifest_hmac_payload(roots: list[str], paths: list[str]) -> bytes:
    body = (
        f"schema={SESSION_MANIFEST_SCHEMA}\n"
        + "roots=" + "\0".join(sorted(roots))
        + "\npaths=" + "\0".join(sorted(paths))
    )
    return body.encode("utf-8")


def _sign_manifest(hmac_key: bytes, roots: list[str], paths: list[str]) -> str:
    return hmac.new(hmac_key, _manifest_hmac_payload(roots, paths), hashlib.sha256).hexdigest()


def _verify_manifest_hmac(hmac_key: bytes, roots: list[str], paths: list[str], supplied: str) -> bool:
    if not supplied or not isinstance(supplied, str):
        return False
    expected = _sign_manifest(hmac_key, roots, paths)
    return hmac.compare_digest(expected, supplied.strip().lower())


def _resolve_containment_roots(
    *,
    manifest: Path,
    explicit_roots: list[Path] | None = None,
    data_roots: list | None = None,
) -> list[Path]:
    """
    Roots that may contain allowlisted session-created paths."""
    roots: list[Path] = []
    if explicit_roots:
        roots.extend(Path(r) for r in explicit_roots)
    env_raw = os.environ.get(SESSION_CONTAINMENT_ROOTS_ENV, "").strip()
    if env_raw:
        for part in env_raw.split(os.pathsep):
            part = part.strip()
            if part:
                roots.append(Path(os.path.expanduser(part)))
    wt = os.environ.get(SESSION_WORKTREE_ENV, "").strip()
    if wt:
        roots.append(Path(os.path.expanduser(wt)))
    if data_roots:
        for item in data_roots:
            if isinstance(item, str) and item.strip():
                roots.append(Path(os.path.expanduser(item.strip())))
    # Scoped temp = manifest parent is always a containment root.
    roots.append(manifest.parent)
    out: list[Path] = []
    seen: set[str] = set()
    for root in roots:
        try:
            resolved = Path(os.path.realpath(root))
        except OSError:
            continue
        key = str(resolved)
        if key in seen:
            continue
        seen.add(key)
        out.append(resolved)
    return out


def _is_under_root(real: Path, root: Path) -> bool:
    try:
        real.relative_to(root)
        return True
    except ValueError:
        return False


def _path_escape_or_symlink_reason(path: Path, roots: list[Path]) -> str:
    """
    Non-empty when path is a symlink, escapes containment, or cannot be resolved."""
    try:
        expanded = Path(os.path.expanduser(str(path)))
    except OSError:
        return "unresolvable path"
    # Reject symlink leafs and any symlink in the recorded path chain.
    probe = expanded if expanded.is_absolute() else Path.cwd() / expanded
    try:
        # Walk parents for symlinks; also reject the leaf when it is a symlink.
        if probe.is_symlink():
            return "symlink refused"
        for parent in [probe, *probe.parents]:
            if parent.is_symlink():
                return "symlink refused"
            if parent == parent.anchor or parent == Path(parent.anchor):
                break
    except OSError:
        return "unresolvable path"
    try:
        real = Path(os.path.realpath(probe))
    except OSError:
        return "unresolvable path"
    if not any(_is_under_root(real, root) for root in roots):
        return "outside session containment"
    return ""


def load_session_created_paths(manifest: Path | None = None) -> set[str]:
    """
    Absolute realpath strings recorded as self-created by this session.

    Forge-closed: missing/invalid HMAC yields an empty set. Paths that escape
    containment or are symlinks are dropped.
    """
    path = session_manifest_path(manifest)
    if path is None or not path.is_file():
        return set()
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return set()
    if not isinstance(data, dict):
        return set()
    entries = data.get("paths")
    roots_raw = data.get("roots")
    supplied = data.get("hmac")
    if not isinstance(entries, list) or not isinstance(roots_raw, list):
        return set()
    root_strs = [item for item in roots_raw if isinstance(item, str) and item.strip()]
    path_strs = [item for item in entries if isinstance(item, str) and item.strip()]
    try:
        hmac_key = _read_session_hmac_key(path)
    except OSError:
        return set()
    if hmac_key is None:
        return set()
    # Forged JSON without the outside-tree HMAC key fails closed.
    if not _verify_manifest_hmac(hmac_key, root_strs, path_strs, supplied if isinstance(supplied, str) else ""):
        return set()
    roots = _resolve_containment_roots(manifest=path, data_roots=root_strs)
    out: set[str] = set()
    for item in path_strs:
        candidate = Path(os.path.expanduser(item.strip()))
        reason = _path_escape_or_symlink_reason(candidate, roots)
        if reason:
            continue
        out.add(str(Path(os.path.realpath(candidate))))
    return out


def record_session_created_paths(
    paths: list,
    *,
    manifest: Path | None = None,
    roots: list | None = None,
) -> Path:
    """
    Append contained absolute paths to the HMAC-bound session manifest.

    Tool-only writer. Rejects symlinks, traversal, and absolute paths outside
    the session scoped dir / worktree containment roots. Creates parent dirs.
    """
    path = session_manifest_path(manifest)
    if path is None:
        path = (
            Path(tempfile.gettempdir())
            / "chaos-engine-sessions"
            / str(os.getpid())
            / SESSION_CREATED_MANIFEST_NAME
        )
        os.environ[SESSION_CREATED_MANIFEST_ENV] = str(path)
    path = Path(path)
    explicit_roots = [Path(r) for r in (roots or [])]
    containment = _resolve_containment_roots(manifest=path, explicit_roots=explicit_roots or None)
    accepted: set[str] = set()
    # Keep previously verified entries that still pass containment.
    for prior in load_session_created_paths(path):
        if not _path_escape_or_symlink_reason(Path(prior), containment):
            accepted.add(prior)
    for item in paths:
        candidate = Path(os.path.expanduser(str(item)))
        reason = _path_escape_or_symlink_reason(candidate, containment)
        if reason:
            raise PermissionError(f"session-manifest refused `{candidate}`: {reason}")
        # never-touch cannot be recorded (path_reason also refuses)
        hit = never_touch(Path(os.path.realpath(candidate)))
        if hit:
            raise PermissionError(f"session-manifest refused `{candidate}`: {hit}")
        accepted.add(str(Path(os.path.realpath(candidate))))
    root_strs = sorted({str(r) for r in containment})
    path_strs = sorted(accepted)
    hmac_key = _load_or_create_session_hmac_key(path)
    payload = {
        "schema": SESSION_MANIFEST_SCHEMA,
        "roots": root_strs,
        "paths": path_strs,
        "hmac": _sign_manifest(hmac_key, root_strs, path_strs),
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    path.chmod(0o600)
    return path


def session_created_allows(path: Path, *, manifest: Path | None = None) -> bool:
    """
    True when ``path`` is an exact HMAC-verified, contained manifest entry."""
    allowed = load_session_created_paths(manifest)
    if not allowed:
        return False
    try:
        real = str(Path(os.path.realpath(path)))
    except OSError:
        return False
    if Path(path).is_symlink():
        return False
    return real in allowed


def restore_ce_managed_to_primary(
    paths: list,
    *,
    session: Path,
    primary: Path | None = None,
) -> tuple[str, list[str]]:
    """
    Restore CE-managed paths via ``git checkout <primary-sha> -- <paths>``.

    Only paths under ``session/.chaos-engine/``. Never deletes. Never touches
    other worktrees. Returns ``(refusal_or_empty, restored_relpaths)``.
    """
    session = Path(session).resolve()
    session_ce = session / ".chaos-engine"
    if not session_ce.is_dir():
        return f"no .chaos-engine under session `{session}`", []
    if primary is None:
        raw = os.environ.get("CHAOS_ENGINE_PRIMARY", "").strip()
        primary = Path(raw) if raw else None
    if primary is None:
        return "CHAOS_ENGINE_PRIMARY unset", []
    primary = Path(primary).resolve()
    if primary == session:
        return "primary and session are the same checkout; refuse cross-ambiguity", []
    sha = _git(primary, "rev-parse", "HEAD")
    if sha.returncode != 0 or not sha.stdout.strip():
        return f"cannot read primary SHA at `{primary}`", []
    primary_sha = sha.stdout.strip()
    rels: list[str] = []
    for item in paths:
        abs_path = Path(os.path.abspath(os.path.expanduser(str(item))))
        try:
            rel = abs_path.resolve().relative_to(session)
        except ValueError:
            return f"`{abs_path}` is outside session worktree `{session}`", []
        if not str(rel).replace("\\", "/").startswith(".chaos-engine/"):
            return f"`{abs_path}` is not under session .chaos-engine/", []
        # Refuse if the same relative path would be applied against another worktree root.
        if abs_path.resolve().parts[: len(session.parts)] != session.parts:
            return f"`{abs_path}` escapes session worktree", []
        hit = never_touch(abs_path)
        if hit:
            return f"`{abs_path}` is {hit}", []
        rels.append(rel.as_posix())
    if not rels:
        return "no CE-managed paths", []
    git = _git(session, "checkout", primary_sha, "--", *rels)
    if git.returncode != 0:
        detail = (git.stderr or git.stdout or "checkout failed").strip().splitlines()
        return f"git checkout failed: {detail[-1] if detail else 'unknown'}", []
    return "", rels


def ce_managed_revert_allows(path: Path, *, primary: Path | None = None) -> bool:
    """
    Deprecated delete allowlist — always False.

    CE-managed dirt must be restored via ``restore_ce_managed_to_primary``
    (git checkout), never deleted through delete-check.
    """
    return False


def path_reason(path: Path, project: Path | None = None) -> str:
    """
    Empty string when deleting ``path`` loses nothing; else the refusal reason.

    Never-touch and protected checks run first so callers of ``path_reason``
    alone cannot bypass them via a forged session manifest.
    """
    hit = never_touch(path)
    if hit:
        return hit
    if allow_listed(path, project):
        return ""
    if session_created_allows(path):
        return ""
    if not path.exists() and not path.is_symlink():
        return ""
    if path.is_symlink():
        # Link-only deletes (plain rm/unlink/mv of the symlink inode). Tree ops
        # that follow links resolve via command_targets before path_reason runs.
        return ""
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


def _yt_render(path: Path, part: str) -> bool:
    """An old ``*.rN.mp4`` render under yt/ but outside yt/meta/."""
    after = path.parts[path.parts.index(part) + 1 :]
    return bool(after) and "meta" not in after and YT_RENDER.fullmatch(path.name) is not None


def set_youtube_video_verifier(fn: Callable[[str], bool] | None) -> None:
    """Tests inject a verifier; production uses oEmbed (+ cache)."""
    global _youtube_video_verifier
    _youtube_video_verifier = fn


def _oembed_cache_path() -> Path:
    return _ack_store().with_name("yt-oembed-cache.json")


def _load_oembed_cache() -> dict:
    store = _oembed_cache_path()
    try:
        data = json.loads(store.read_text(encoding="utf-8")) if store.is_file() else {}
    except ValueError:
        data = {}
    return data if isinstance(data, dict) else {}


def _save_oembed_cache(cache: dict) -> None:
    store = _oembed_cache_path()
    store.parent.mkdir(parents=True, exist_ok=True)
    store.write_text(json.dumps(cache, sort_keys=True), encoding="utf-8")
    store.chmod(0o600)


def _expected_oembed_authors() -> frozenset[str] | None:
    """Optional channel bind: non-empty CHAOS_YT_EXPECTED_AUTHORS → require oEmbed author_name match."""
    raw = os.environ.get(YT_EXPECTED_AUTHORS_ENV, "")
    if not raw or not raw.strip():
        return None
    names = frozenset(part.strip() for part in raw.split(",") if part.strip())
    return names or None


def _oembed_author_ok(author: str) -> bool:
    expected = _expected_oembed_authors()
    if expected is None:
        return bool(author)
    return author in expected


def _parse_oembed_proof(body: bytes) -> tuple[bool, str]:
    """Positive proof: JSON with non-empty author_name (and optional expected-authors match)."""
    try:
        payload = json.loads(body.decode("utf-8"))
    except (UnicodeDecodeError, ValueError):
        return False, ""
    if not isinstance(payload, dict):
        return False, ""
    author = payload.get("author_name")
    if not isinstance(author, str):
        return False, ""
    author = author.strip()
    if not author:
        return False, ""
    return _oembed_author_ok(author), author


def _oembed_cache_hit_result(entry: object, now: float) -> bool | None:
    """Return cached bool on a valid hit; None = miss. Never raises on poison/malformation.

    Positive hits re-validate stored author (non-empty str) against the current
    CHAOS_YT_EXPECTED_AUTHORS bind. Bare ok:true is never trusted alone.
    """
    if not isinstance(entry, dict):
        return None
    expires = entry.get("expires", 0)
    if not isinstance(expires, (int, float)):
        return None
    if expires < now:
        return None
    ok = entry.get("ok")
    if not isinstance(ok, bool):
        return None
    if ok is False:
        return False
    author = entry.get("author")
    if not isinstance(author, str):
        return None
    author = author.strip()
    if not author:
        return False
    return _oembed_author_ok(author)


def _default_youtube_video_exists(video_id: str) -> bool:
    """True only on oEmbed HTTP 200 + JSON author_name proof (optional expected-authors bind).

    HTTP 403/401 from oEmbed means private/unlisted-private (not a box IP block): stay locked.
    Network/5xx use a short negative cache so a brief outage does not lock for a day.
    Warm positive cache hits re-apply author proof and the current expected-authors bind;
    malformed cache entries are treated as misses and never raise into never_touch.
    """
    cache = _load_oembed_cache()
    entry = cache.get(video_id)
    now = time.time()
    hit = _oembed_cache_hit_result(entry, now)
    if hit is not None:
        return hit
    ok = False
    ttl = YT_OEMBED_TTL
    status: int | None = None
    author = ""
    try:
        request = urllib.request.Request(
            YT_OEMBED.format(id=video_id),
            headers={"User-Agent": YT_OEMBED_UA, "Accept": "application/json"},
            method="GET",
        )
        with urllib.request.urlopen(request, timeout=10) as response:  # nosec B310 - fixed YouTube oEmbed URL.
            status = int(getattr(response, "status", 200) or 200)
            body = response.read()
        if status == 200:
            ok, author = _parse_oembed_proof(body)
        else:
            ok = False
            ttl = YT_OEMBED_SOFT_TTL if status >= 500 else YT_OEMBED_TTL
    except urllib.error.HTTPError as error:
        status = int(error.code)
        ok = False
        # 4xx (incl. 403 private) is definitive; 5xx is soft.
        ttl = YT_OEMBED_SOFT_TTL if status >= 500 else YT_OEMBED_TTL
    except (urllib.error.URLError, TimeoutError, OSError, ValueError):
        ok = False
        ttl = YT_OEMBED_SOFT_TTL
    cache[video_id] = {
        "ok": ok,
        "expires": now + ttl,
        "checked": int(now),
        "status": status,
        "author": author[:80],
        "ttl": ttl,
    }
    _save_oembed_cache(cache)
    return ok


def youtube_video_exists(video_id: str) -> bool:
    if _youtube_video_verifier is not None:
        return bool(_youtube_video_verifier(video_id))
    return _default_youtube_video_exists(video_id)


def _file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while True:
            chunk = handle.read(1024 * 1024)
            if not chunk:
                break
            digest.update(chunk)
    return digest.hexdigest()


def _owner_only_file(path: Path) -> bool:
    try:
        info = os.stat(path)
    except OSError:
        return False
    if not stat.S_ISREG(info.st_mode):
        return False
    if info.st_uid != os.getuid():
        return False
    return (info.st_mode & 0o777) == 0o600


def _symlink_in_path(path: Path) -> bool:
    """True if any existing component of path is a symlink (lstat)."""
    abs_path = Path(os.path.abspath(os.path.expanduser(path)))
    abs_path = Path(os.path.normpath(abs_path))
    current = Path(abs_path.anchor) if abs_path.anchor else Path("/")
    parts = abs_path.parts[1:] if abs_path.is_absolute() else abs_path.parts
    for part in parts:
        current = current / part
        try:
            if stat.S_ISLNK(os.lstat(current).st_mode):
                return True
        except OSError:
            return False
    return False


def _normalized_abs(path: Path | str) -> Path:
    return Path(os.path.normpath(os.path.abspath(os.path.expanduser(str(path)))))


def _safe_listed_realpath(raw: str) -> str | None:
    """Normalize an allowlist path; reject '..', globs, dirs, escapes, and symlink components."""
    if glob.has_magic(raw) or raw.endswith(("/", os.sep)):
        return None
    expanded = Path(os.path.expanduser(raw))
    if ".." in expanded.parts:
        return None
    abs_path = _normalized_abs(expanded)
    if ".." in abs_path.parts or not abs_path.suffix:
        return None
    if _symlink_in_path(abs_path):
        return None
    if abs_path.exists() and (abs_path.is_dir() or abs_path.is_symlink()):
        return None
    real = Path(os.path.realpath(abs_path))
    if str(real) != str(abs_path):
        return None
    if "yt" not in real.parts:
        return None
    after = real.parts[real.parts.index("yt") + 1 :]
    if not after or "meta" in after:
        return None
    return str(real)


def _yt_verified_path() -> Path | None:
    """Env override (owner-only 0600) or default config when present and owner-only."""
    override = os.environ.get(YT_VERIFIED_ENV, "").strip()
    if override:
        candidate = Path(os.path.expanduser(override))
        if not _owner_only_file(candidate):
            _log({"yt_verified_source": "env-rejected", "path": str(candidate), "reason": "not-owner-only-0600"})
            return None
        _log({"yt_verified_source": "env", "path": str(candidate)})
        return candidate
    default = YT_VERIFIED_FILE.expanduser()
    if not default.is_file():
        return None
    if not _owner_only_file(default):
        _log({"yt_verified_source": "default-rejected", "path": str(default), "reason": "not-owner-only-0600"})
        return None
    _log({"yt_verified_source": "default", "path": str(default)})
    return default


def _load_yt_verified() -> dict[str, dict[str, str]]:
    """realpath -> {video_id, sha256}. Globs, dirs, escapes, and bad proofs are skipped."""
    config = _yt_verified_path()
    if config is None:
        return {}
    out: dict[str, dict[str, str]] = {}
    for line in config.read_text(encoding="utf-8").splitlines():
        item = line.strip()
        if not item or item.startswith("#"):
            continue
        parts = item.split()
        if len(parts) != 3:
            continue
        raw, video_id, digest = parts
        if not YT_VIDEO_ID.fullmatch(video_id) or not YT_SHA256.fullmatch(digest):
            continue
        real = _safe_listed_realpath(raw)
        if real is None:
            continue
        out[real] = {"video_id": video_id, "sha256": digest}
    return out


def _yt_containment_ok(real: Path) -> bool:
    if "yt" not in real.parts:
        return False
    after = real.parts[real.parts.index("yt") + 1 :]
    return bool(after) and "meta" not in after


def _yt_verified_uploaded(path: Path, part: str | None = None) -> bool:
    """#7525: allowlisted yt file with matching sha256 + oEmbed author proof; contained; re-checked live."""
    del part
    if ".." in Path(os.path.expanduser(str(path))).parts:
        return False
    if _symlink_in_path(path):
        return False
    abs_norm = _normalized_abs(path)
    if ".." in abs_norm.parts:
        return False
    try:
        real = Path(os.path.realpath(abs_norm))
    except OSError:
        return False
    if str(real) != str(abs_norm):
        return False
    if not _yt_containment_ok(real):
        return False
    if not real.is_file() or real.is_symlink():
        return False
    entries = _load_yt_verified()
    entry = entries.get(str(real))
    if entry is None:
        return False
    try:
        if _file_sha256(real) != entry["sha256"]:
            return False
    except OSError:
        return False
    return bool(youtube_video_exists(entry["video_id"]))


def never_touch(path: Path) -> str:
    """Non-empty when no code may unlock deleting ``path`` (itself, inside, or directly containing a protected dir)."""
    names, patterns = _never_rules()
    for part in path.parts:
        hit = next((name for name in names if fnmatchcase(part, name)), "")
        if hit == "yt" and (_yt_render(path, part) or _yt_verified_uploaded(path, part)):
            continue
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
    if not (DELETE_WORD.search(command) or RMTREE.search(command) or PATHLIB_DELETE.search(command)):
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


def _only_chaos_engine_dirt(porcelain: str) -> bool:
    """True when every porcelain path is under .chaos-engine/ (uncommitted CE tree)."""
    lines = [line for line in porcelain.splitlines() if line.strip()]
    if not lines:
        return False
    for line in lines:
        rel = line[3:] if len(line) > 3 else line
        rel = rel.strip().strip('"')
        if " -> " in rel:
            rel = rel.split(" -> ", 1)[-1]
        if not (rel == ".chaos-engine" or rel.startswith(".chaos-engine/") or rel.startswith(".chaos-engine\\")):
            return False
    return True


def _is_linked_worktree(path: Path) -> bool:
    result = _git(path, "rev-parse", "--path-format=absolute", "--git-dir", "--git-common-dir")
    if result.returncode != 0:
        return False
    lines = result.stdout.strip().splitlines()
    if len(lines) != 2:
        return False
    return Path(lines[0]).resolve() != Path(lines[1]).resolve()


def backup_remote_issue(path: Path) -> str:
    if _git(path, "rev-parse", "--show-toplevel").returncode != 0:
        return "not in git"
    # #7263: consumer mode (#6237) hides .memory/mempalace via info/exclude, so a
    # clean status proves nothing — still warn. Silence is only for linked
    # worktrees whose sole dirt is an uncommitted CE tree (below).
    if (path / CONSUMER_MARKER).is_file():
        return "consumer mode hides harness state from git"
    if not _git(path, "remote").stdout.strip():
        return "no remote"
    if _git(path, "rev-parse", "--abbrev-ref", "@{u}").returncode != 0:
        return "no upstream"
    if _git(path, "rev-list", "--count", "@{u}..HEAD").stdout.strip() not in {"", "0"}:
        return "unpushed commits"
    porcelain = _git(path, "status", "--porcelain", "--untracked-files=normal").stdout.strip()
    if porcelain:
        # Silence on linked worktrees whose only dirt is an uncommitted CE tree.
        if _is_linked_worktree(path) and _only_chaos_engine_dirt(porcelain):
            return ""
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
    if arguments[:1] == ["--restore-ce"] and len(arguments) >= 2:
        session = Path(os.environ.get(SESSION_WORKTREE_ENV) or Path.cwd()).resolve()
        reason, restored = restore_ce_managed_to_primary(arguments[1:], session=session)
        if reason:
            print(f"ce-restore: refused: {reason}", file=sys.stderr)
            return 1
        print("ce-restore: restored " + ", ".join(restored))
        return 0
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
        print("usage: tool.py delete-check [--code CODE] <paths...> | [--code CODE] --command '<cmd>' | --request-code <paths...> | --restore-ce <CE-paths...>", file=sys.stderr)
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
