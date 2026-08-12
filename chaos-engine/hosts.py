#!/usr/bin/env python3
"""Install and remove receipt-bound project adapters for coding agents."""

from __future__ import annotations

import base64
import hashlib
import hmac
import json
import os
import re
import secrets
import stat
from pathlib import Path


RECEIPT_NAME = ".chaos-engine-hosts.json"
ANCHOR_NAME = ".chaos-engine-hosts.anchor"
ACTIVE_ANCHOR_PREFIX = ".chaos-engine-hosts.active-"
REMOVING_ANCHOR_PREFIX = ".chaos-engine-hosts.removing-"
ANCHOR_TOKEN = re.compile(r"^[0-9a-f]{64}$")
SCHEMA_VERSION = 1
START = "<!-- CHAOSENGINE:START -->"
END = "<!-- CHAOSENGINE:END -->"
DIRECTORY_MARKER = ".chaos-engine-owned-directory"
DIRECTORY_CLAIM_PREFIX = ".chaos-engine-directory-claim-"
INSTRUCTION = (
    f"{START}\nBefore every task, follow the canonical "
    "[ChaosEngine](.chaos-engine/skills/chaos-engine/SKILL.md). "
    "Use `.chaos-engine/tool.py` for the project-local Memory, MemPalace, and Graphify tools.\n"
    f"{END}\n"
)


def interpreter(platform_name: str | None = None) -> tuple[str, list[str]]:
    platform_name = platform_name or os.name
    return ("py", ["-3"]) if platform_name == "nt" else ("python3", [])


def owned_servers(platform_name: str | None = None) -> dict[str, dict[str, object]]:
    command, prefix = interpreter(platform_name)
    return {
        "chaosengine-memory": {
            "command": command,
            "args": [*prefix, ".chaos-engine/tool.py", "memory-mcp"],
            "cwd": ".",
        },
        "chaosengine-mempalace": {
            "command": command,
            "args": [*prefix, ".chaos-engine/tool.py", "mempalace-mcp"],
            "cwd": ".",
            "env": {"MEMPALACE_EMBEDDING_MODEL": "minilm"},
        },
    }


def managed_paths() -> tuple[str, ...]:
    return (
        ".agents/skills/chaos-engine/SKILL.md",
        ".claude/skills/chaos-engine/SKILL.md",
        ".gemini/skills/chaos-engine/SKILL.md",
        ".github/skills/chaos-engine/SKILL.md",
        "AGENTS.md",
        "CLAUDE.md",
        "GEMINI.md",
        ".github/copilot-instructions.md",
        ".mcp.json",
        ".gemini/settings.json",
        ".codex/config.toml",
    )


def host_routes() -> dict[str, str]:
    return {
        "codex": ".agents/skills/chaos-engine/SKILL.md",
        "claude": ".claude/skills/chaos-engine/SKILL.md",
        "grok": "AGENTS.md",
        "gemini": ".gemini/skills/chaos-engine/SKILL.md",
        "copilot": ".github/skills/chaos-engine/SKILL.md",
    }


def created_directories(project: Path) -> list[str]:
    directories: set[Path] = set()
    for relative in managed_paths():
        current = (project / relative).parent
        while current != project:
            if not current.exists() and not is_link_or_reparse(current):
                directories.add(current)
            current = current.parent
    return [
        path.relative_to(project).as_posix()
        for path in sorted(directories, key=lambda item: (len(item.parts), item.as_posix()))
    ]


def allowed_managed_directories() -> set[str]:
    directories: set[str] = set()
    for relative in managed_paths():
        current = Path(relative).parent
        while current != Path("."):
            directories.add(current.as_posix())
            current = current.parent
    return directories


def sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def installed_host_token(project: Path) -> str | None:
    manifest = project / ".chaos-engine/manifest.json"
    raw = read_file(project, manifest)
    if raw is None:
        return None
    try:
        value = json.loads(raw.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("ChaosEngine core manifest is invalid") from error
    token = value.get("hostToken") if isinstance(value, dict) else None
    if not isinstance(token, str) or ANCHOR_TOKEN.fullmatch(token) is None:
        raise ValueError("ChaosEngine core host token is invalid")
    return token


def host_anchor_paths(project: Path, *, allow_unbound: bool = True) -> list[Path]:
    legacy = project / ANCHOR_NAME
    validate_path(project, legacy)
    if legacy.exists() or is_link_or_reparse(legacy):
        raise ValueError(f"ChaosEngine host anchor collision: {legacy}")
    found: list[Path] = []
    expected_token = installed_host_token(project)
    if not project.is_dir():
        return found
    for path in project.iterdir():
        token = None
        for prefix in (ACTIVE_ANCHOR_PREFIX, REMOVING_ANCHOR_PREFIX):
            if path.name.startswith(prefix):
                token = path.name[len(prefix) :]
                break
        if token is None:
            continue
        validate_path(project, path)
        if not ANCHOR_TOKEN.fullmatch(token) or read_file(project, path) != b"":
            raise ValueError(f"ChaosEngine host anchor collision: {path}")
        if (
            path.name.startswith(ACTIVE_ANCHOR_PREFIX)
            and not allow_unbound
            and (expected_token is None or token != expected_token)
        ):
            raise ValueError(f"ChaosEngine host anchor collision: {path}")
        found.append(path)
    if len(found) > 1:
        raise ValueError("ChaosEngine host anchor collision")
    return found


def host_anchor_path(project: Path, *, create: bool = False) -> Path:
    found = host_anchor_paths(project)
    if found:
        return found[0]
    if not create:
        raise ValueError("ChaosEngine host anchor is missing")
    token = installed_host_token(project)
    if token is None:
        token = secrets.token_hex(32)
    while True:
        path = project / f"{ACTIVE_ANCHOR_PREFIX}{token}"
        validate_path(project, path)
        try:
            descriptor = os.open(
                path,
                os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0),
                0o600,
            )
        except FileExistsError as error:
            raise ValueError(f"ChaosEngine host anchor collision: {path}") from error
        os.close(descriptor)
        return path


def host_anchor(project: Path, *, create: bool = False) -> bytes:
    path = host_anchor_path(project, create=create)
    for prefix in (ACTIVE_ANCHOR_PREFIX, REMOVING_ANCHOR_PREFIX):
        if path.name.startswith(prefix):
            return bytes.fromhex(path.name[len(prefix) :])
    raise ValueError("ChaosEngine host anchor is invalid")


def anchor_token(path: Path) -> str:
    for prefix in (ACTIVE_ANCHOR_PREFIX, REMOVING_ANCHOR_PREFIX):
        if path.name.startswith(prefix):
            token = path.name[len(prefix) :]
            if ANCHOR_TOKEN.fullmatch(token):
                return token
    raise ValueError("ChaosEngine host anchor is invalid")


def move_anchor(project: Path, source: Path, prefix: str) -> Path:
    destination = project / f"{prefix}{anchor_token(source)}"
    validate_path(project, destination)
    if destination.exists() or is_link_or_reparse(destination):
        raise ValueError(f"ChaosEngine host anchor collision: {destination}")
    source.replace(destination)
    if read_file(project, destination) != b"":
        raise ValueError("ChaosEngine host anchor is invalid")
    return destination


def authenticate(project: Path, purpose: str, payload: bytes) -> str:
    return hmac.new(host_anchor(project), purpose.encode() + b"\0" + payload, hashlib.sha256).hexdigest()


def transaction_auth(project: Path, purpose: str, payload: bytes) -> bytes:
    return (f"chaos-engine-{purpose}-v1:" + authenticate(project, purpose, payload) + "\n").encode()


def expected_claim_path(project: Path, base: Path, purpose: str, payload: bytes) -> Path:
    token = authenticate(project, purpose, payload)
    return base.with_name(f"{base.name}.{token}")


def ensure_claim(project: Path, base: Path, purpose: str, payload: bytes) -> Path:
    claim = expected_claim_path(project, base, purpose, payload)
    validate_path(project, base)
    candidates = [path for path in base.parent.glob(f"{base.name}.*")]
    if base.exists() or is_link_or_reparse(base) or any(path != claim for path in candidates):
        raise ValueError(f"ChaosEngine transaction claim collision: {base}")
    current = read_file(project, claim)
    if current == b"":
        return claim
    if current is not None or is_link_or_reparse(claim):
        raise ValueError(f"ChaosEngine transaction claim collision: {claim}")
    descriptor = os.open(
        claim,
        os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0),
        0o600,
    )
    os.close(descriptor)
    return claim


def is_link_or_reparse(path: Path) -> bool:
    if path.is_symlink():
        return True
    try:
        attributes = getattr(os.lstat(path), "st_file_attributes", 0)
    except FileNotFoundError:
        return False
    return bool(attributes & 0x400)


def validate_path(project: Path, path: Path) -> None:
    current = path
    while current != project:
        if is_link_or_reparse(current):
            raise ValueError(f"ChaosEngine host path is a link or reparse point: {current}")
        parent = current.parent
        if parent == current:
            raise ValueError(f"ChaosEngine host path escapes the project: {path}")
        current = parent
    if is_link_or_reparse(project):
        raise ValueError(f"ChaosEngine host path escapes the project: {path}")


def read_file(project: Path, path: Path) -> bytes | None:
    validate_path(project, path)
    if not path.exists():
        return None
    if not path.is_file():
        raise ValueError(f"ChaosEngine host path is not a file: {path}")
    flags = os.O_RDONLY | getattr(os, "O_BINARY", 0) | getattr(os, "O_NOFOLLOW", 0)
    descriptor = os.open(path, flags)
    with os.fdopen(descriptor, "rb") as stream:
        opened = os.fstat(stream.fileno())
        named = os.stat(path, follow_symlinks=False)
        if not stat.S_ISREG(opened.st_mode) or (opened.st_dev, opened.st_ino) != (
            named.st_dev,
            named.st_ino,
        ):
            raise ValueError(f"ChaosEngine host path changed during read: {path}")
        return stream.read()


def instruction_content(before: bytes | None, instruction: str) -> bytes:
    try:
        existing = before.decode("utf-8") if before is not None else ""
    except UnicodeDecodeError as error:
        raise ValueError("invalid host instruction file") from error
    if START in existing or END in existing:
        if instruction not in existing:
            raise ValueError("ChaosEngine instruction collision")
        return before  # type: ignore[return-value]
    separator = "\n" if existing and not existing.endswith("\n") else ""
    return (existing + separator + instruction).encode()


def json_content(before: bytes | None) -> bytes:
    try:
        value = json.loads(before.decode("utf-8")) if before is not None else {}
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("invalid host JSON configuration") from error
    if not isinstance(value, dict):
        raise ValueError("invalid host JSON configuration")
    servers = value.setdefault("mcpServers", {})
    if not isinstance(servers, dict):
        raise ValueError("invalid MCP server configuration")
    for name, desired in owned_servers().items():
        if name in servers and servers[name] != desired:
            raise ValueError(f"ChaosEngine MCP server collision: {name}")
        servers[name] = desired
    return (json.dumps(value, indent=2, sort_keys=True) + "\n").encode()


def codex_content(before: bytes | None, platform_name: str | None = None) -> bytes:
    try:
        existing = before.decode("utf-8") if before is not None else ""
    except UnicodeDecodeError as error:
        raise ValueError("invalid Codex configuration") from error
    command, prefix = interpreter(platform_name)
    prefix_text = '"-3", ' if prefix else ""
    block = (
        "# CHAOSENGINE:START\n"
        f'[mcp_servers."chaosengine-memory"]\ncommand = "{command}"\n'
        f'args = [{prefix_text}".chaos-engine/tool.py", "memory-mcp"]\ncwd = ".."\n\n'
        f'[mcp_servers."chaosengine-mempalace"]\ncommand = "{command}"\n'
        f'args = [{prefix_text}".chaos-engine/tool.py", "mempalace-mcp"]\ncwd = ".."\n'
        'env = { MEMPALACE_EMBEDDING_MODEL = "minilm" }\n# CHAOSENGINE:END\n'
    )
    if "# CHAOSENGINE:START" in existing or "# CHAOSENGINE:END" in existing:
        if block not in existing:
            raise ValueError("ChaosEngine Codex configuration collision")
        return before  # type: ignore[return-value]
    for name in owned_servers():
        if f'mcp_servers."{name}"' in existing or f"mcp_servers.{name}" in existing:
            raise ValueError(f"ChaosEngine Codex server collision: {name}")
    separator = "\n" if existing and not existing.endswith("\n") else ""
    return (existing + separator + block).encode()


def desired_content(before: dict[str, bytes | None]) -> dict[str, bytes]:
    adapters = managed_paths()[:4]
    skill = (
        "---\nname: chaos-engine\ndescription: Load the canonical installed ChaosEngine before every task.\n---\n\n"
        "Follow the [canonical ChaosEngine](../../../.chaos-engine/skills/chaos-engine/SKILL.md).\n"
    ).encode()
    after = {relative: skill for relative in adapters}
    for relative in ("AGENTS.md", "CLAUDE.md", "GEMINI.md"):
        after[relative] = instruction_content(before[relative], INSTRUCTION)
    after[".github/copilot-instructions.md"] = instruction_content(
        before[".github/copilot-instructions.md"],
        INSTRUCTION.replace(".chaos-engine/", "../.chaos-engine/"),
    )
    after[".mcp.json"] = json_content(before[".mcp.json"])
    after[".gemini/settings.json"] = json_content(before[".gemini/settings.json"])
    after[".codex/config.toml"] = codex_content(before[".codex/config.toml"])
    return after


def current_images(project: Path) -> dict[str, bytes | None]:
    return {relative: read_file(project, project / relative) for relative in managed_paths()}


def encode_images(images: dict[str, bytes | None]) -> dict[str, str | None]:
    return {
        relative: None if content is None else base64.b64encode(content).decode("ascii")
        for relative, content in images.items()
    }


def decode_images(value: object, *, nullable: bool) -> dict[str, bytes | None]:
    if not isinstance(value, dict) or set(value) != set(managed_paths()):
        raise ValueError("ChaosEngine host receipt ownership is invalid")
    result: dict[str, bytes | None] = {}
    try:
        for relative, content in value.items():
            if content is None and nullable:
                result[str(relative)] = None
            elif isinstance(content, str):
                result[str(relative)] = base64.b64decode(content, validate=True)
            else:
                raise ValueError
    except (ValueError, TypeError) as error:
        raise ValueError("ChaosEngine host receipt content is invalid") from error
    return result


def receipt_directories(receipt: dict[str, object]) -> list[str]:
    value = receipt.get("createdDirectories")
    if not isinstance(value, list) or any(not isinstance(item, str) for item in value):
        raise ValueError("ChaosEngine host receipt directory ownership is invalid")
    allowed = allowed_managed_directories()
    expected_order = sorted(value, key=lambda item: (len(Path(item).parts), item))
    if len(value) != len(set(value)) or value != expected_order:
        raise ValueError("ChaosEngine host receipt directory ownership is invalid")
    for relative in value:
        path = Path(relative)
        if (
            path.is_absolute()
            or ".." in path.parts
            or path.as_posix() != relative
            or relative not in allowed
        ):
            raise ValueError("ChaosEngine host receipt directory ownership is invalid")
    return value


def directory_marker(project: Path, receipt: dict[str, object], relative: str) -> bytes:
    nonce = receipt.get("directoryNonce")
    if not isinstance(nonce, str) or len(nonce) != 32:
        raise ValueError("ChaosEngine host receipt directory nonce is invalid")
    body = f"chaos-engine-host-directory-v1:{nonce}:{relative}\n".encode()
    return body + authenticate(project, "directory", body).encode() + b"\n"


def directory_claim_base(project: Path, relative: str) -> Path:
    digest = hashlib.sha256(relative.encode()).hexdigest()[:24]
    return project / f"{DIRECTORY_CLAIM_PREFIX}{digest}"


def directory_claim_path(project: Path, receipt: dict[str, object], relative: str) -> Path:
    expected = directory_marker(project, receipt, relative)
    return expected_claim_path(
        project,
        directory_claim_base(project, relative),
        "host-directory",
        expected,
    )


def write_directory_claim(project: Path, receipt: dict[str, object], relative: str) -> Path:
    expected = directory_marker(project, receipt, relative)
    return ensure_claim(
        project,
        directory_claim_base(project, relative),
        "host-directory",
        expected,
    )


def prepare_created_directories(project: Path, receipt: dict[str, object]) -> None:
    for relative in receipt_directories(receipt):
        path = project / relative
        marker = path / DIRECTORY_MARKER
        validate_path(project, path)
        expected = directory_marker(project, receipt, relative)
        expected_claim = directory_claim_path(project, receipt, relative)
        recovering_claim = read_file(project, expected_claim) == b""
        claim = write_directory_claim(project, receipt, relative)
        try:
            path.mkdir()
        except FileExistsError as error:
            marker_content = read_file(project, marker) if path.is_dir() else None
            if (
                not path.is_dir()
                or marker_content not in {None, expected}
                or (marker_content is None and not recovering_claim)
            ):
                if read_file(project, claim) == b"":
                    claim.unlink()
                raise ValueError(f"ChaosEngine host directory claim collision: {path}") from error
        validate_path(project, path)
        expected = directory_marker(project, receipt, relative)
        current = read_file(project, marker)
        if current is None:
            descriptor = os.open(
                marker,
                os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0),
                0o600,
            )
            with os.fdopen(descriptor, "wb") as stream:
                stream.write(expected)
                stream.flush()
                os.fsync(stream.fileno())
        elif current != expected:
            raise ValueError(f"ChaosEngine host directory marker collision: {marker}")
        if read_file(project, claim) != b"":
            raise ValueError(f"ChaosEngine host directory claim drift detected: {claim}")
        claim.unlink()


def verify_created_directories(project: Path, receipt: dict[str, object]) -> None:
    for relative in receipt_directories(receipt):
        path = project / relative
        marker = path / DIRECTORY_MARKER
        if not path.is_dir() or read_file(project, marker) != directory_marker(project, receipt, relative):
            raise ValueError(f"ChaosEngine host directory ownership drift detected: {path}")


def receipt_bytes(receipt: dict[str, object], project: Path | None = None) -> bytes:
    body = {key: value for key, value in receipt.items() if key not in {"integritySha256", "authenticationHmac"}}
    encoded = json.dumps(body, sort_keys=True, separators=(",", ":")).encode()
    body["integritySha256"] = sha256_bytes(encoded)
    if project is not None:
        body["authenticationHmac"] = authenticate(project, "receipt", encoded)
    return (json.dumps(body, indent=2, sort_keys=True) + "\n").encode()


def atomic_write(  # noqa: MC0001 - one descriptor-bound transaction protects user files.
    project: Path, path: Path, content: bytes, expected: bytes | None
) -> None:
    validate_path(project, path)
    scratch = path.with_name(f".{path.name}.chaos-engine-old")
    temporary = path.with_name(f".{path.name}.chaos-engine-new")
    claim_base = path.with_name(f".{path.name}.chaos-engine-write-claim")
    claim_payload = (
        path.relative_to(project).as_posix().encode()
        + b"\0"
        + (b"absent" if expected is None else sha256_bytes(expected).encode())
        + b"\0"
        + sha256_bytes(content).encode()
    )
    claim = expected_claim_path(project, claim_base, "host-write", claim_payload)
    recovering = any(
        candidate.exists() or is_link_or_reparse(candidate)
        for candidate in (scratch, temporary)
    )
    if recovering:
        if read_file(project, claim) != b"":
            kind = "publication" if temporary.exists() or is_link_or_reparse(temporary) else "scratch"
            raise ValueError(f"ChaosEngine host {kind} scratch collision: {temporary if kind == 'publication' else scratch}")
    else:
        claim = ensure_claim(project, claim_base, "host-write", claim_payload)
    if scratch.exists() or is_link_or_reparse(scratch):
        if is_link_or_reparse(scratch) or read_file(project, scratch) != expected:
            raise ValueError(f"ChaosEngine host scratch path collision: {scratch}")
        current = read_file(project, path)
        if current is None:
            scratch.replace(path)
        elif current == content:
            scratch.unlink()
            claim.unlink()
            return
        else:
            raise ValueError(f"ChaosEngine host scratch recovery collision: {path}")
    if temporary.exists() or is_link_or_reparse(temporary):
        partial = read_file(project, temporary)
        if is_link_or_reparse(temporary) or partial is None or not content.startswith(partial):
            raise ValueError(f"ChaosEngine host publication scratch collision: {temporary}")
        if partial != content:
            temporary.unlink()
        current = read_file(project, path)
        if partial == content and current == content:
            temporary.unlink()
            if scratch.exists():
                scratch.unlink()
            claim.unlink()
            return
        if current != expected:
            raise ValueError(f"ChaosEngine host publication scratch recovery collision: {path}")
    if read_file(project, path) != expected:
        raise ValueError(f"ChaosEngine host path changed before publication: {path}")
    path.parent.mkdir(parents=True, exist_ok=True)
    validate_path(project, path.parent)
    if not temporary.exists():
        descriptor = os.open(
            temporary,
            os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0),
            0o600,
        )
    else:
        descriptor = None
    try:
        if descriptor is not None:
            with os.fdopen(descriptor, "wb") as stream:
                stream.write(content)
                stream.flush()
                os.fsync(stream.fileno())
                opened = os.fstat(stream.fileno())
                named = os.stat(temporary, follow_symlinks=False)
                if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino):
                    raise ValueError(f"ChaosEngine host scratch path collision: {temporary}")
        if read_file(project, path) != expected:
            raise ValueError(f"ChaosEngine host path changed before publication: {path}")
        if expected is None:
            try:
                os.link(temporary, path)
            except FileExistsError as error:
                raise ValueError(f"ChaosEngine host path changed before publication: {path}") from error
            except OSError as error:
                raise ValueError("ChaosEngine host filesystem cannot publish with hard links") from error
        else:
            validate_path(project, scratch)
            if scratch.exists() or is_link_or_reparse(scratch):
                raise ValueError(f"ChaosEngine host scratch path collision: {scratch}")
            path.replace(scratch)
            moved = read_file(project, scratch)
            if moved != expected:
                try:
                    os.link(scratch, path)
                except FileExistsError:
                    pass
                if read_file(project, path) == moved:
                    scratch.unlink()
                raise ValueError(f"ChaosEngine host path changed before publication: {path}")
            try:
                os.link(temporary, path)
            except BaseException as error:
                if not path.exists() and not is_link_or_reparse(path):
                    scratch.replace(path)
                raise ValueError("ChaosEngine host filesystem cannot publish with hard links") from error
            scratch.unlink()
        claim.unlink()
    finally:
        if temporary.exists() and not is_link_or_reparse(temporary):
            temporary.unlink()


def atomic_remove(project: Path, path: Path, expected: bytes) -> None:
    validate_path(project, path)
    scratch = path.with_name(f".{path.name}.chaos-engine-removed")
    claim_base = path.with_name(f".{path.name}.chaos-engine-remove-claim")
    claim_payload = path.relative_to(project).as_posix().encode() + b"\0" + sha256_bytes(expected).encode()
    claim = expected_claim_path(project, claim_base, "host-remove", claim_payload)
    recovering = scratch.exists() or is_link_or_reparse(scratch)
    if recovering:
        if read_file(project, claim) != b"":
            raise ValueError(f"ChaosEngine host removal scratch collision: {scratch}")
    else:
        claim = ensure_claim(project, claim_base, "host-remove", claim_payload)
    if scratch.exists() or is_link_or_reparse(scratch):
        if is_link_or_reparse(scratch) or read_file(project, scratch) != expected:
            raise ValueError(f"ChaosEngine host removal scratch collision: {scratch}")
        if read_file(project, path) is None:
            scratch.unlink()
            claim.unlink()
            return
        raise ValueError(f"ChaosEngine host removal scratch recovery collision: {path}")
    if read_file(project, path) != expected:
        raise ValueError(f"ChaosEngine host path changed before removal: {path}")
    validate_path(project, scratch)
    if scratch.exists() or is_link_or_reparse(scratch):
        raise ValueError(f"ChaosEngine host removal scratch path collision: {scratch}")
    path.replace(scratch)
    moved = read_file(project, scratch)
    if moved != expected:
        if read_file(project, path) is None:
            scratch.replace(path)
        raise ValueError(f"ChaosEngine host path changed before removal: {path}")
    scratch.unlink()
    claim.unlink()


def write_receipt(project: Path, receipt: dict[str, object], expected: bytes | None) -> bytes:
    content = receipt_bytes(receipt, project)
    atomic_write(project, project / RECEIPT_NAME, content, expected)
    return content


def read_receipt(project: Path) -> tuple[dict[str, object], bytes]:
    raw = read_file(project, project / RECEIPT_NAME)
    if raw is None:
        raise ValueError("ChaosEngine host receipt is missing or invalid")
    try:
        value = json.loads(raw.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("ChaosEngine host receipt is missing or invalid") from error
    if not isinstance(value, dict) or value.get("schemaVersion") != SCHEMA_VERSION:
        raise ValueError("ChaosEngine host receipt schema is unsupported")
    integrity = value.get("integritySha256")
    authentication = value.get("authenticationHmac")
    body = {key: item for key, item in value.items() if key not in {"integritySha256", "authenticationHmac"}}
    encoded = json.dumps(body, sort_keys=True, separators=(",", ":")).encode()
    if integrity != sha256_bytes(encoded) or authentication != authenticate(project, "receipt", encoded):
        raise ValueError("ChaosEngine host receipt integrity drift detected")
    if value.get("phase") not in {"installing", "installed", "removing"}:
        raise ValueError("ChaosEngine host receipt phase is invalid")
    if value.get("hosts") != host_routes():
        raise ValueError("ChaosEngine host receipt routes are invalid")
    decode_images(value.get("before"), nullable=True)
    decode_images(value.get("after"), nullable=False)
    directories = receipt_directories(value)
    if directories:
        directory_marker(project, value, directories[0])
    intent = value.get("rollbackIntent")
    if intent is not None and (
        not isinstance(intent, dict)
        or set(intent) != {"desiredCommit", "priorCommit"}
        or any(not isinstance(item, str) or len(item) != 40 for item in intent.values())
    ):
        raise ValueError("ChaosEngine host rollback intent is invalid")
    return value, raw


def reconcile(  # noqa: MC0001 - one ordered pass retains rollback images for every host.
    project: Path,
    desired: dict[str, bytes | None],
    allowed: tuple[dict[str, bytes | None], ...],
) -> None:
    snapshots = current_images(project)
    for relative, current in snapshots.items():
        if not any(current == candidate[relative] for candidate in allowed):
            raise ValueError(f"ChaosEngine host adapter drift detected: {project / relative}")
    changed: list[tuple[str, bytes | None, bytes | None]] = []
    try:
        for relative in managed_paths():
            current = read_file(project, project / relative)
            wanted = desired[relative]
            if current == wanted:
                continue
            if wanted is None:
                if current is None:
                    continue
                atomic_remove(project, project / relative, current)
            else:
                atomic_write(project, project / relative, wanted, current)
            changed.append((relative, current, wanted))
    except BaseException as mutation_error:
        rollback_errors: list[BaseException] = []
        for relative, prior, published in reversed(changed):
            path = project / relative
            current = read_file(project, path)
            if current != published:
                rollback_errors.append(
                    ValueError(f"ChaosEngine host path changed during rollback: {path}")
                )
                continue
            try:
                if prior is None:
                    if current is not None:
                        atomic_remove(project, path, current)
                elif current != prior:
                    atomic_write(project, path, prior, current)
            except BaseException as rollback_error:
                rollback_errors.append(rollback_error)
        if rollback_errors:
            raise rollback_errors[0] from mutation_error
        raise


def install(project: Path, core_commit: str | None = None) -> dict[str, object]:
    project = project.resolve()
    receipt_path = project / RECEIPT_NAME
    receipt_exists = receipt_path.exists() or is_link_or_reparse(receipt_path)
    existing_anchors = host_anchor_paths(project, allow_unbound=receipt_exists)
    anchor_existed = bool(existing_anchors)
    canonical = project / ".chaos-engine/skills/chaos-engine/SKILL.md"
    if not canonical.is_file() or is_link_or_reparse(canonical):
        raise ValueError(f"installed ChaosEngine skill is missing: {canonical}")
    validate_path(project, receipt_path)
    if receipt_path.exists() or is_link_or_reparse(receipt_path):
        host_anchor(project)
        receipt, raw = read_receipt(project)
        before = decode_images(receipt["before"], nullable=True)
        after = decode_images(receipt["after"], nullable=False)
        if receipt["phase"] == "installed":
            verify(project, receipt)
            wanted = desired_content(before)
            if after == wanted and receipt.get("coreCommit") == core_commit:
                return receipt
            next_receipt = dict(receipt)
            next_receipt["phase"] = "installing"
            next_receipt["coreCommit"] = core_commit
            next_receipt["after"] = encode_images(wanted)
            next_raw = write_receipt(project, next_receipt, raw)
            try:
                reconcile(project, wanted, (after, wanted))
                next_receipt["phase"] = "installed"
                write_receipt(project, next_receipt, next_raw)
                return next_receipt
            except BaseException:
                reconcile(project, after, (after, wanted))
                atomic_write(project, receipt_path, raw, read_file(project, receipt_path))
                raise
        prepare_created_directories(project, receipt)
        reconcile(project, after, (before, after))
        receipt["phase"] = "installed"
        write_receipt(project, receipt, raw)
        return receipt

    before = current_images(project)
    after = desired_content(before)
    if existing_anchors and existing_anchors[0].name.startswith(REMOVING_ANCHOR_PREFIX):
        raise ValueError("ChaosEngine host removal recovery is required")
    anchor_path = host_anchor_path(project, create=True)
    receipt: dict[str, object] = {
        "schemaVersion": SCHEMA_VERSION,
        "phase": "installing",
        "hosts": host_routes(),
        "coreCommit": core_commit,
        "createdDirectories": created_directories(project),
        "directoryNonce": secrets.token_hex(16),
        "rollbackIntent": None,
        "before": encode_images(before),
        "after": encode_images(after),
    }
    raw = write_receipt(project, receipt, None)
    try:
        prepare_created_directories(project, receipt)
        reconcile(project, after, (before, after))
        receipt["phase"] = "installed"
        write_receipt(project, receipt, raw)
        return receipt
    except BaseException:
        reconcile(project, before, (before, after))
        remove_created_directories(project, receipt)
        if read_file(project, receipt_path) is not None:
            receipt_path.unlink()
        if not anchor_existed and anchor_path.exists() and read_file(project, anchor_path) == b"":
            anchor_path.unlink()
        raise


def verify(
    project: Path,
    receipt: dict[str, object] | None = None,
    core_commit: str | None = None,
) -> dict[str, str]:
    project = project.resolve()
    if receipt is None:
        receipt, _ = read_receipt(project)
    if receipt.get("phase") != "installed":
        return {"status": "recovery-required"}
    verify_created_directories(project, receipt)
    if core_commit is not None and receipt.get("coreCommit") != core_commit:
        raise ValueError("ChaosEngine host receipt does not match the installed core")
    after = decode_images(receipt.get("after"), nullable=False)
    current = current_images(project)
    for relative in managed_paths():
        if current[relative] != after[relative]:
            raise ValueError(f"ChaosEngine host adapter drift detected: {project / relative}")
    return {"status": "healthy"}


def snapshot(project: Path) -> dict[str, object]:
    project = project.resolve()
    receipt, raw = read_receipt(project)
    verify(project, receipt)
    return {"receipt": receipt, "raw": raw}


def set_rollback_intent(project: Path, desired_commit: str, prior_commit: str) -> None:
    project = project.resolve()
    receipt, raw = read_receipt(project)
    wanted = {"desiredCommit": desired_commit, "priorCommit": prior_commit}
    current = receipt.get("rollbackIntent")
    if current is not None and current != wanted:
        raise ValueError("ChaosEngine host rollback intent collision")
    if current == wanted:
        return
    receipt["rollbackIntent"] = wanted
    write_receipt(project, receipt, raw)


def clear_rollback_intent(project: Path, desired_commit: str, prior_commit: str) -> None:
    project = project.resolve()
    receipt, raw = read_receipt(project)
    wanted = {"desiredCommit": desired_commit, "priorCommit": prior_commit}
    if receipt.get("rollbackIntent") != wanted:
        raise ValueError("ChaosEngine host rollback intent drift detected")
    receipt["rollbackIntent"] = None
    write_receipt(project, receipt, raw)


def restore_snapshot(project: Path, saved: dict[str, object]) -> None:
    project = project.resolve()
    previous = saved.get("receipt")
    raw = saved.get("raw")
    if not isinstance(previous, dict) or not isinstance(raw, bytes):
        raise ValueError("ChaosEngine host snapshot is invalid")
    current, current_raw = read_receipt(project)
    verify(project, current)
    previous_after = decode_images(previous.get("after"), nullable=False)
    current_after = decode_images(current.get("after"), nullable=False)
    reconcile(project, previous_after, (current_after, previous_after))
    atomic_write(project, project / RECEIPT_NAME, raw, current_raw)


def prepare_uninstall(project: Path) -> dict[str, object]:
    project = project.resolve()
    receipt, raw = read_receipt(project)
    before = decode_images(receipt["before"], nullable=True)
    after = decode_images(receipt["after"], nullable=False)
    if receipt["phase"] == "installed":
        verify(project, receipt)
        receipt["phase"] = "removing"
        raw = write_receipt(project, receipt, raw)
    elif receipt["phase"] != "removing":
        raise ValueError("ChaosEngine host installation recovery is required")
    reconcile(project, before, (before, after))
    return receipt


def remove_created_directories(project: Path, receipt: dict[str, object]) -> None:
    for relative in reversed(receipt_directories(receipt)):
        path = project / relative
        marker = path / DIRECTORY_MARKER
        claim = directory_claim_path(project, receipt, relative)
        expected = directory_marker(project, receipt, relative)
        validate_path(project, path)
        if path.exists():
            if not path.is_dir():
                raise ValueError(f"ChaosEngine host directory ownership drift detected: {path}")
            marker_content = read_file(project, marker)
            claim_content = read_file(project, claim)
            if marker_content is None and claim_content is None:
                continue
            if marker_content is None and claim_content == b"" and not any(path.iterdir()):
                path.rmdir()
                claim.unlink()
                continue
            if marker_content != expected:
                raise ValueError(f"ChaosEngine host directory ownership drift detected: {path}")
            write_directory_claim(project, receipt, relative)
            marker.unlink()
            try:
                path.rmdir()
            except OSError as error:
                if any(path.iterdir()):
                    atomic_write(project, marker, directory_marker(project, receipt, relative), None)
                    if claim.exists():
                        claim.unlink()
                    raise ValueError(
                        f"ChaosEngine host directory ownership drift detected: {path}"
                    ) from error
                raise
            claim.unlink()


def cancel_uninstall(project: Path) -> None:
    project = project.resolve()
    receipt, raw = read_receipt(project)
    if receipt["phase"] != "removing":
        raise ValueError("ChaosEngine host removal is not prepared")
    before = decode_images(receipt["before"], nullable=True)
    after = decode_images(receipt["after"], nullable=False)
    prepare_created_directories(project, receipt)
    reconcile(project, after, (before, after))
    receipt["phase"] = "installed"
    write_receipt(project, receipt, raw)
    anchor = host_anchor_path(project)
    if anchor.name.startswith(REMOVING_ANCHOR_PREFIX):
        move_anchor(project, anchor, ACTIVE_ANCHOR_PREFIX)


def finalize_uninstall(project: Path) -> None:
    project = project.resolve()
    receipt_path = project / RECEIPT_NAME
    validate_path(project, receipt_path)
    if read_file(project, receipt_path) is None:
        anchors = host_anchor_paths(project)
        if len(anchors) == 1 and anchors[0].name.startswith(REMOVING_ANCHOR_PREFIX):
            anchors[0].unlink()
            return
        raise ValueError("ChaosEngine host receipt is missing or invalid")
    receipt, _ = read_receipt(project)
    if receipt["phase"] != "removing":
        raise ValueError("ChaosEngine host removal is not prepared")
    before = decode_images(receipt["before"], nullable=True)
    if current_images(project) != before:
        raise ValueError("ChaosEngine host removal state drift detected")
    remove_created_directories(project, receipt)
    anchor = host_anchor_path(project)
    if anchor.name.startswith(ACTIVE_ANCHOR_PREFIX):
        anchor = move_anchor(project, anchor, REMOVING_ANCHOR_PREFIX)
    receipt_path.unlink()
    anchor.unlink()


def uninstall(project: Path) -> None:
    project = project.resolve()
    receipt_path = project / RECEIPT_NAME
    if read_file(project, receipt_path) is None:
        finalize_uninstall(project)
        return
    prepare_uninstall(project)
    try:
        finalize_uninstall(project)
    except BaseException:
        if read_file(project, receipt_path) is not None:
            cancel_uninstall(project)
        raise
