#!/usr/bin/env python3
"""Install and remove receipt-bound project adapters for coding agents."""

from __future__ import annotations

import base64
from contextlib import contextmanager
import ctypes
import errno
import hashlib
import hmac
import json
import os
import platform
import queue
import re
import secrets
import shutil
import sqlite3
import stat
import subprocess  # nosec B404 - probes a resolved local Java executable.
import sys
import threading
from pathlib import Path, PurePosixPath


RECEIPT_NAME = ".chaos-engine-hosts.json"
ANCHOR_NAME = ".chaos-engine-hosts.anchor"
ACTIVE_ANCHOR_PREFIX = ".chaos-engine-hosts.active-"
REMOVING_ANCHOR_PREFIX = ".chaos-engine-hosts.removing-"
ANCHOR_TOKEN = re.compile(r"^[0-9a-f]{64}$")
SCHEMA_VERSION = 1
PLUGIN_NAME = "chaos-engine"
CAVEMAN_PLUGIN_NAME = "caveman"
CAVEMAN_PLUGIN_VERSION = "0.1.0"
CAVEMAN_UPSTREAM_COMMIT = "766dce6b1394ebb56a3090748d5a0240a5aefb36"
PONYTAIL_PLUGIN_NAME = "ponytail"
PONYTAIL_PLUGIN_VERSION = "0.1.0"
PONYTAIL_UPSTREAM_COMMIT = "2ed6c52c9d7e5e56942508591085fd45dea277d3"
COMPANION_PLUGIN_NAMES = (CAVEMAN_PLUGIN_NAME, PONYTAIL_PLUGIN_NAME)
MEMORY_SCHEMA_FILES = (
    "config.schema.json",
    "object.schema.json",
    "relation.schema.json",
    "event.schema.json",
    "patch.schema.json",
)
SQLITE_EXACT_SCHEMA = {
    "meta": {"key": ("TEXT", 0, 1), "value": ("TEXT", 1, 0)},
    "collections": {
        "id": ("INTEGER", 0, 1),
        "name": ("TEXT", 1, 0),
        "dimension": ("INTEGER", 0, 0),
        "created_at": ("TEXT", 1, 0),
    },
    "documents": {
        "collection_id": ("INTEGER", 1, 1),
        "id": ("TEXT", 1, 2),
        "document": ("TEXT", 1, 0),
        "metadata_json": ("TEXT", 1, 0),
        "embedding": ("BLOB", 1, 0),
        "dim": ("INTEGER", 1, 0),
        "created_at": ("TEXT", 1, 0),
        "updated_at": ("TEXT", 1, 0),
    },
}
SQLITE_EXACT_INDEXES = {
    "collections": {(1, ("name",))},
    "documents": {
        (0, ("collection_id",)),
        (1, ("collection_id", "id")),
    },
}
MEMPALACE_MCP_ENV = {
    "MEMPALACE_EMBEDDING_MODEL": "minilm",
    "MEMPALACE_BACKEND": "sqlite_exact",
}
MEMPALACE_MCP_ENV_TOML = (
    'env = { MEMPALACE_EMBEDDING_MODEL = "minilm", '
    'MEMPALACE_BACKEND = "sqlite_exact" }\n'
)
MEMORY_PACKAGE_PREFIX = "@aictx/memory@"
UNGUARDED_MEMPALACE_COMMANDS = frozenset({"mempalace-mcp", "mempalace-mcp.exe"})
STALE_MEMORY_PACKAGE_RE = re.compile(r"@aictx/memory@[0-9A-Za-z][0-9A-Za-z._-]*")
CODEX_SERVER_HEADER_RE = re.compile(r"(?m)(^\[mcp_servers[^\]]*\])")
CHROMA_SCHEMA = {
    "collections": {
        "id": ("TEXT", 0, 1), "name": ("TEXT", 1, 0),
        "dimension": ("INTEGER", 0, 0), "database_id": ("TEXT", 1, 0),
        "config_json_str": ("TEXT", 0, 0), "schema_str": ("TEXT", 0, 0),
    },
    "segments": {
        "id": ("TEXT", 0, 1), "type": ("TEXT", 1, 0),
        "scope": ("TEXT", 1, 0), "collection": ("TEXT", 1, 0),
    },
    "embeddings_queue": {
        "seq_id": ("INTEGER", 0, 1), "created_at": ("TIMESTAMP", 1, 0),
        "operation": ("INTEGER", 1, 0), "topic": ("TEXT", 1, 0),
        "id": ("TEXT", 1, 0), "vector": ("BLOB", 0, 0),
        "encoding": ("TEXT", 0, 0), "metadata": ("TEXT", 0, 0),
    },
}
LEGACY_MANAGED_PATHS = (
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


def default_mempalace_wing(project_name: str) -> str:
    """Return the shared `{repository}_main` wing for a new MemPalace config."""
    safe = re.sub(r"[^a-z0-9]+", "_", project_name.casefold()).strip("_") or "project"
    return f"{safe}_main"


def project_identity_name(project: Path) -> str:
    """Return the repository identity, independent of a checkout/worktree folder name."""
    try:
        result = subprocess.run(  # nosec B603 B607 - fixed git query, no shell.
            ["git", "-C", str(project), "config", "--get", "remote.origin.url"],
            capture_output=True,
            text=True,
            check=False,
            timeout=5,
        )
    except (OSError, subprocess.SubprocessError):
        return project.name
    if result.returncode == 0:
        remote = result.stdout.strip().rstrip("/\\")
        candidate = re.split(r"[/\\:]", remote)[-1]
        if candidate.casefold().endswith(".git"):
            candidate = candidate[:-4]
        if candidate:
            return candidate
    return project.name


def _memory_project_valid(project_config: object) -> bool:
    return (
        isinstance(project_config, dict)
        and set(project_config) == {"id", "name"}
        and isinstance(project_config.get("id"), str)
        and re.fullmatch(r"project\.[a-z0-9][a-z0-9-]*", project_config["id"]) is not None
        and isinstance(project_config.get("name"), str)
        and bool(project_config["name"].strip())
    )


def _memory_options_valid(memory_options: object, required: set[str]) -> bool:
    budget = memory_options.get("defaultTokenBudget") if isinstance(memory_options, dict) else None
    return (
        isinstance(memory_options, dict)
        and set(memory_options) == required
        and isinstance(memory_options.get("autoIndex"), bool)
        and isinstance(budget, int)
        and not isinstance(budget, bool)
        and 501 <= budget <= 50000
        and (
            "saveContextPacks" not in required
            or isinstance(memory_options.get("saveContextPacks"), bool)
        )
    )


def validate_memory_config(content: bytes) -> None:
    try:
        config = json.loads(content)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("invalid Memory configuration") from error
    if not isinstance(config, dict) or not _memory_project_valid(config.get("project")):
        raise ValueError("invalid Memory configuration")
    version = config.get("version")
    if version == 5:
        valid = set(config) == {"version", "project", "memory"} and _memory_options_valid(
            config.get("memory"), {"autoIndex", "defaultTokenBudget"}
        )
    elif version == 4:
        git_options = config.get("git")
        valid = (
            set(config) == {"version", "project", "memory", "git"}
            and _memory_options_valid(
                config.get("memory"),
                {"autoIndex", "defaultTokenBudget", "saveContextPacks"},
            )
            and isinstance(git_options, dict)
            and set(git_options) == {"trackContextPacks"}
            and isinstance(git_options.get("trackContextPacks"), bool)
        )
    else:
        valid = False
    if not valid:
        raise ValueError("invalid Memory configuration")


def migrate_memory_config(content: bytes) -> bytes:
    validate_memory_config(content)
    config = json.loads(content)
    if config.get("version") == 5:
        return content if content.endswith(b"\n") else content + b"\n"
    memory = config["memory"]
    migrated = {
        "version": 5,
        "project": config["project"],
        "memory": {
            "autoIndex": memory["autoIndex"],
            "defaultTokenBudget": memory["defaultTokenBudget"],
        },
    }
    payload = (json.dumps(migrated, indent=2, sort_keys=True) + "\n").encode()
    validate_memory_config(payload)
    return payload


def memory_schema_assets() -> Path:
    return Path(__file__).resolve().parent / "assets/memory-v5"


def validate_memory_storage(project: Path) -> None:
    schema_root = project / ".memory/schema"
    for name in MEMORY_SCHEMA_FILES:
        try:
            schema = json.loads((schema_root / name).read_bytes())
        except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
            raise ValueError("invalid Memory storage") from error
        if not isinstance(schema, (dict, bool)):
            raise ValueError("invalid Memory storage")
    for relative in (".memory/memory", ".memory/relations"):
        if not (project / relative).is_dir():
            raise ValueError("invalid Memory storage")
    try:
        events = (project / ".memory/events.jsonl").read_text(encoding="utf-8")
        for line in events.splitlines():
            if line.strip() and not isinstance(json.loads(line), dict):
                raise ValueError("invalid Memory storage")
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("invalid Memory storage") from error


def validate_mempalace_config(content: bytes) -> None:
    try:
        text = content.decode("utf-8")
    except UnicodeDecodeError as error:
        raise ValueError("invalid MemPalace configuration") from error
    wing_matches = re.findall(r"(?m)^wing:\s*([A-Za-z0-9_.-]+)\s*$", text)
    rooms = re.search(
        r"(?ms)^rooms:\s*\n(?P<body>.*?)(?=^[A-Za-z_][\w-]*:\s*$|\Z)",
        text,
    )
    excludes = re.search(
        r"(?ms)^exclude_patterns:\s*\n(?P<body>.*?)(?=^[A-Za-z_][\w-]*:\s*$|\Z)",
        text,
    )
    if (
        len(wing_matches) != 1
        or rooms is None
        or re.search(r"(?m)^\s*- name:\s*\S+", rooms.group("body")) is None
        or re.search(r"(?m)^\s*description:\s*\S+", rooms.group("body")) is None
        or excludes is None
        or re.search(r"(?m)^\s*-\s+\S+", excludes.group("body")) is None
    ):
        raise ValueError("invalid MemPalace configuration")


def retrieval_configs_healthy(project: Path) -> bool:
    try:
        validate_memory_config((project / ".memory/config.json").read_bytes())
        validate_memory_storage(project)
        validate_mempalace_config((project / "mempalace.yaml").read_bytes())
    except (OSError, ValueError):
        return False
    return True


def retrieval_runtime_status(project: Path) -> dict[str, str]:
    tool = project / ".chaos-engine/tool.py"
    for arguments in (("status", "--json"), ("check", "--json")):
        result = subprocess.run(  # nosec B603 - fixed owned launcher and arguments.
            [sys.executable, str(tool), "memory", *arguments],
            cwd=project,
            capture_output=True,
            text=True,
            check=False,
            timeout=30,
        )
        if result.returncode != 0:
            detail = (result.stderr or result.stdout or "memory tool exited non-zero").strip()
            return {
                "status": "recovery-required",
                "reason": f"memory {' '.join(arguments)} failed: {detail[:240]}",
            }
        try:
            payload = json.loads(result.stdout)
        except json.JSONDecodeError:
            return {
                "status": "recovery-required",
                "reason": f"memory {' '.join(arguments)} did not return JSON",
            }
        if not isinstance(payload, dict) or payload.get("ok") is not True:
            return {
                "status": "recovery-required",
                "reason": f"memory {' '.join(arguments)} reported not ok",
            }
        if arguments[0] == "check" and payload.get("data", {}).get("valid") is not True:
            return {
                "status": "recovery-required",
                "reason": "memory check reported invalid store",
            }
    return {"status": "healthy"}


def retrieval_runtime_healthy(project: Path) -> bool:
    return retrieval_runtime_status(project).get("status") == "healthy"


def _sqlite_runtime_valid(
    database: Path,
    *,
    required_schema: dict[str, dict[str, tuple[str, int, int]]] | None = None,
    required_indexes: dict[str, set[tuple[int, tuple[str, ...]]]] | None = None,
    collection: str | None = None,
) -> bool:
    wal = Path(f"{database}-wal")
    shared_memory = Path(f"{database}-shm")
    if not database.is_file() or any(
        is_link_or_reparse(path) for path in (database, wal, shared_memory)
    ):
        return False
    wal_exists = wal.exists()
    shared_memory_exists = shared_memory.exists()
    if wal_exists != shared_memory_exists or (
        wal_exists and (not wal.is_file() or not shared_memory.is_file())
    ):
        return False
    connection = None
    try:
        query = "mode=ro" if wal_exists else "mode=ro&immutable=1"
        connection = sqlite3.connect(
            f"{database.resolve().as_uri()}?{query}",
            uri=True,
        )
        connection.execute("PRAGMA trusted_schema=OFF")
        if connection.execute("PRAGMA quick_check(1)").fetchone() != ("ok",):
            return False
        if required_schema is not None and not all(
            expected
            == {
                str(row[1]): (str(row[2]).upper(), int(row[3]), int(row[5]))
                for row in connection.execute(f"PRAGMA table_info({table})")
            }
            for table, expected in required_schema.items()
        ):
            return False
        if required_indexes is not None:
            for table, expected in required_indexes.items():
                actual = set()
                for row in connection.execute(f"PRAGMA index_list({table})"):
                    columns = tuple(
                        str(column[2])
                        for column in connection.execute(
                            f"PRAGMA index_info({str(row[1])})"
                        )
                    )
                    actual.add((int(row[2]), columns))
                if not expected <= actual:
                    return False
        if collection is not None and connection.execute(
            "SELECT 1 FROM collections WHERE name = ?",
            (collection,),
        ).fetchone() != (1,):
            return False
        return True
    except (OSError, sqlite3.DatabaseError):
        return False
    finally:
        if connection is not None:
            connection.close()


def repository_map_resolver_present(project: Path) -> bool:
    return (project / "tools/repository-map/resolve_mempalace.py").is_file()


def centralized_mempalace_status() -> dict[str, str]:
    return {
        "status": "degraded",
        "detail": (
            "Centralized MemPalace is the operator path; "
            "use py -3 scripts/agents/knowledge_stores.py status"
        ),
    }


def resolved_central_palace(project: Path) -> Path | None:
    """Return the resolver palace path when it is absolute and printable."""
    resolver = project / "tools/repository-map/resolve_mempalace.py"
    if not resolver.is_file():
        return None
    try:
        completed = subprocess.run(  # nosec B603 - owned resolver, no shell.
            [sys.executable, str(resolver)],
            cwd=project,
            capture_output=True,
            text=True,
            check=False,
            timeout=5,
        )
    except (OSError, subprocess.TimeoutExpired):
        return None
    palace = completed.stdout.strip()
    if completed.returncode != 0 or not palace:
        return None
    path = Path(palace)
    return path if path.is_absolute() else None


def mempalace_directory_status(palace: Path) -> dict[str, str]:
    """Classify one MemPalace directory without importing its native backend."""
    if is_link_or_reparse(palace):
        return {
            "status": "recovery-required",
            "detail": "MemPalace state is a link or reparse point",
        }
    if not palace.exists():
        return {"status": "initialization-required", "backend": "sqlite_exact"}
    if not palace.is_dir():
        return {
            "status": "recovery-required",
            "detail": "MemPalace state path is not a directory",
        }

    chroma = palace / "chroma.sqlite3"
    exact = palace / "sqlite_exact.sqlite3"
    try:
        children = list(palace.iterdir())
    except OSError:
        return {
            "status": "recovery-required",
            "detail": "MemPalace state is unreadable or contains a link or reparse point",
        }
    if any(is_link_or_reparse(child) for child in children):
        return {
            "status": "recovery-required",
            "detail": "MemPalace state is unreadable or contains a link or reparse point",
        }
    if chroma.exists():
        if not _sqlite_runtime_valid(
            chroma,
            required_schema=CHROMA_SCHEMA,
        ):
            return {
                "status": "recovery-required",
                "detail": "Legacy Chroma MemPalace state is unreadable or malformed",
            }
        chroma_names = {
            chroma.name,
            f"{chroma.name}-wal",
            f"{chroma.name}-shm",
        }
        segment = re.compile(
            r"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
        )
        if any(
            child.name not in chroma_names
            and (not child.is_dir() or segment.fullmatch(child.name) is None)
            for child in children
        ):
            return {
                "status": "recovery-required",
                "detail": "Legacy Chroma MemPalace state is mixed or unrecognized",
            }
        return {
            "status": "migration-required",
            "detail": (
                "Legacy Chroma/HNSW MemPalace state requires migration; "
                "ChaosEngine will not open its native index"
            ),
        }

    wal = Path(f"{exact}-wal")
    shared_memory = Path(f"{exact}-shm")
    sidecar = palace / ".mempalace"
    allowed_names = {path.name for path in (exact, wal, shared_memory, sidecar)}
    if any(child.name not in allowed_names for child in children):
        return {
            "status": "recovery-required",
            "detail": "MemPalace state contains unrecognized recoverable data",
        }
    if sidecar.exists():
        if not sidecar.is_dir() or is_link_or_reparse(sidecar):
            return {
                "status": "recovery-required",
                "detail": "MemPalace state contains unrecognized recoverable data",
            }
        try:
            sidecar_children = list(sidecar.iterdir())
        except OSError:
            return {
                "status": "recovery-required",
                "detail": "MemPalace state is unreadable or contains a link or reparse point",
            }
        if any(
            child.name != "origin.json" or is_link_or_reparse(child)
            for child in sidecar_children
        ):
            return {
                "status": "recovery-required",
                "detail": "MemPalace state contains unrecognized recoverable data",
            }
    wal_exists = wal.exists()
    shared_memory_exists = shared_memory.exists()
    if not exact.exists() and (wal_exists or shared_memory_exists):
        return {
            "status": "recovery-required",
            "detail": "SQLite-exact MemPalace WAL state has no database",
        }
    if exact.exists():
        if not _sqlite_runtime_valid(
            exact,
            required_schema=SQLITE_EXACT_SCHEMA,
            required_indexes=SQLITE_EXACT_INDEXES,
            collection="mempalace_drawers",
        ):
            return {
                "status": "recovery-required",
                "detail": "SQLite-exact MemPalace state is unreadable or malformed",
            }
        return {"status": "healthy", "backend": "sqlite_exact"}
    return {"status": "initialization-required", "backend": "sqlite_exact"}


def mempalace_runtime_status(project: Path) -> dict[str, str]:
    """Classify project-local or centralized MemPalace state."""
    palace = project / ".chaos-engine-state/mempalace"
    if is_link_or_reparse(palace):
        return {
            "status": "recovery-required",
            "detail": "MemPalace state is a link or reparse point",
        }
    if not palace.exists():
        central = resolved_central_palace(project)
        if central is not None and central.exists():
            status = mempalace_directory_status(central)
            if status.get("status") != "initialization-required":
                return status
        if repository_map_resolver_present(project):
            return centralized_mempalace_status()
        return {"status": "initialization-required", "backend": "sqlite_exact"}
    status = mempalace_directory_status(palace)
    if (
        status.get("status") == "initialization-required"
        and repository_map_resolver_present(project)
    ):
        central = resolved_central_palace(project)
        if central is not None and central.exists():
            central_status = mempalace_directory_status(central)
            if central_status.get("status") != "initialization-required":
                return central_status
        return centralized_mempalace_status()
    return status


def _cleanup_failed_mempalace_initialization(
    *,
    connection,
    descriptor: int | None,
    database: Path,
    identity: tuple[int, int] | None,
    palace: Path,
    palace_created: bool,
    state_root: Path,
    state_root_created: bool,
) -> None:
    """Remove only state created by the failed initializer transaction."""
    if connection is not None:
        connection.close()
    if descriptor is not None:
        os.close(descriptor)
    try:
        current = os.stat(database, follow_symlinks=False)
    except OSError:
        current = None
    if current is not None and identity == (current.st_dev, current.st_ino):
        database.unlink()
    if palace_created and palace.exists() and not any(palace.iterdir()):
        palace.rmdir()
    if state_root_created and state_root.exists() and not any(state_root.iterdir()):
        state_root.rmdir()


def initialize_mempalace_runtime(project: Path) -> None:
    """Create only a fresh empty sqlite_exact collection; never migrate user state."""
    project = project.resolve()
    state_root = project / ".chaos-engine-state"
    palace = state_root / "mempalace"
    status = mempalace_runtime_status(project)["status"]
    if status == "healthy":
        return
    if status != "initialization-required":
        return

    validate_path(project, palace)
    state_root_created = not state_root.exists()
    palace_created = not palace.exists()
    state_root.mkdir(exist_ok=True)
    validate_path(project, palace)
    palace.mkdir(exist_ok=True)
    validate_path(project, palace)
    palace_stat = os.stat(palace, follow_symlinks=False)
    if not stat.S_ISDIR(palace_stat.st_mode):
        raise ValueError("ChaosEngine MemPalace state path is not a directory")
    palace_identity = (palace_stat.st_dev, palace_stat.st_ino)
    database = palace / "sqlite_exact.sqlite3"
    descriptor = None
    connection = None
    identity: tuple[int, int] | None = None
    try:
        if any(palace.iterdir()):
            raise ValueError("ChaosEngine will not initialize over existing MemPalace state")
        validate_path(project, palace)
        named_palace = os.stat(palace, follow_symlinks=False)
        if palace_identity != (named_palace.st_dev, named_palace.st_ino):
            raise ValueError("ChaosEngine MemPalace state path changed before initialization")
        descriptor = os.open(
            database,
            os.O_RDWR | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0),
            0o600,
        )
        opened = os.fstat(descriptor)
        identity = (opened.st_dev, opened.st_ino)
        named_palace = os.stat(palace, follow_symlinks=False)
        if palace_identity != (named_palace.st_dev, named_palace.st_ino):
            raise ValueError("ChaosEngine MemPalace state path changed during initialization")
        os.close(descriptor)
        descriptor = None
        connection = sqlite3.connect(database)
        connection.executescript(
            """
            PRAGMA journal_mode=WAL;
            CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT NOT NULL);
            CREATE TABLE collections (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT NOT NULL UNIQUE,
                dimension INTEGER,
                created_at TEXT NOT NULL
            );
            CREATE TABLE documents (
                collection_id INTEGER NOT NULL,
                id TEXT NOT NULL,
                document TEXT NOT NULL,
                metadata_json TEXT NOT NULL,
                embedding BLOB NOT NULL,
                dim INTEGER NOT NULL,
                created_at TEXT NOT NULL,
                updated_at TEXT NOT NULL,
                PRIMARY KEY (collection_id, id),
                FOREIGN KEY(collection_id) REFERENCES collections(id) ON DELETE CASCADE
            );
            CREATE INDEX idx_documents_collection ON documents(collection_id);
            INSERT INTO collections(name, created_at)
            VALUES ('mempalace_drawers', CURRENT_TIMESTAMP);
            """
        )
        connection.commit()
        connection.close()
        connection = None
        if mempalace_runtime_status(project)["status"] != "healthy":
            raise ValueError("fresh SQLite-exact MemPalace state failed validation")
    except BaseException:
        _cleanup_failed_mempalace_initialization(
            connection=connection,
            descriptor=descriptor,
            database=database,
            identity=identity,
            palace=palace,
            palace_created=palace_created,
            state_root=state_root,
            state_root_created=state_root_created,
        )
        raise


def mcp_runtime_healthy(project: Path, managed_python: Path | None = None) -> bool:
    if mempalace_runtime_status(project)["status"] != "healthy":
        return False
    skip_checkout_mempalace = (
        repository_map_resolver_present(project)
        and not (project / ".chaos-engine-state/mempalace/sqlite_exact.sqlite3").is_file()
    )
    initialize = json.dumps(
        {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2025-06-18",
                "capabilities": {},
                "clientInfo": {"name": "chaos-engine-doctor", "version": "1"},
            },
        }
    ) + "\n"
    mempalace_probe = (
        initialize
        + json.dumps(
            {"jsonrpc": "2.0", "method": "notifications/initialized", "params": {}}
        )
        + "\n"
        + json.dumps(
            {
                "jsonrpc": "2.0",
                "id": 2,
                "method": "tools/call",
                "params": {"name": "mempalace_status", "arguments": {}},
            }
        )
        + "\n"
    )
    tool = project / ".chaos-engine/tool.py"
    environment = os.environ.copy()
    environment["PYTHONDONTWRITEBYTECODE"] = "1"
    environment.update(MEMPALACE_MCP_ENV)
    python = str(managed_python) if managed_python is not None else sys.executable
    commands = (
        [python, str(tool), "memory-mcp"],
        [
            python,
            str(tool),
            "mempalace-mcp",
            "--palace",
            ".chaos-engine-state/mempalace",
            "--backend",
            "sqlite_exact",
        ],
    )
    for index, command in enumerate(commands):
        if index == 1 and skip_checkout_mempalace:
            continue
        result = subprocess.run(  # nosec B603 - fixed owned launcher and arguments.
            command,
            cwd=project,
            input=mempalace_probe if index == 1 else initialize,
            capture_output=True,
            text=True,
            check=False,
            env=environment,
            timeout=30,
        )
        if result.returncode != 0:
            return False
        try:
            responses = [
                json.loads(line)
                for line in result.stdout.splitlines()
                if line.strip().startswith("{")
            ]
        except json.JSONDecodeError:
            return False
        if not any(
            isinstance(response, dict)
            and response.get("id") == 1
            and isinstance(response.get("result"), dict)
            for response in responses
        ):
            return False
        if index == 1:
            status_responses = [
                response
                for response in responses
                if isinstance(response, dict) and response.get("id") == 2
            ]
            if len(status_responses) != 1:
                return False
            result_payload = status_responses[0].get("result")
            content = result_payload.get("content") if isinstance(result_payload, dict) else None
            if not isinstance(content, list):
                return False
            try:
                status_payloads = [
                    json.loads(item["text"])
                    for item in content
                    if isinstance(item, dict)
                    and item.get("type") == "text"
                    and isinstance(item.get("text"), str)
                ]
            except json.JSONDecodeError:
                return False
            if not any(
                isinstance(payload, dict)
                and payload.get("backend") == "sqlite_exact"
                and isinstance(payload.get("total_drawers"), int)
                and "error" not in payload
                for payload in status_payloads
            ):
                return False
    return True


def hook_runtime_healthy(project: Path, managed_python: Path) -> bool:
    """Run changed-sensitive hook events through generated managed Python."""
    guard = project / ".chaos-engine/hooks/guard.py"
    if not managed_python.is_file() or not guard.is_file():
        return False
    for event in ("UserPromptSubmit", "PreToolUse", "PostToolUse"):
        payload = {"hook_event_name": event, "session_id": "chaos-engine-doctor"}
        if event != "UserPromptSubmit":
            payload.update({"tool_name": "Bash", "tool_input": {"command": "true"}})
        try:
            result = subprocess.run(  # nosec B603 - receipt-owned interpreter and hook.
                [str(managed_python), str(guard)], cwd=project, input=json.dumps(payload),
                capture_output=True, text=True, check=False, timeout=30,
                env={**os.environ, "PYTHONDONTWRITEBYTECODE": "1"},
            )
            if result.returncode or not isinstance(json.loads(result.stdout or "{}"), dict):
                return False
        except (OSError, subprocess.TimeoutExpired, json.JSONDecodeError):
            return False
    return True


def client_command(
    executable: str,
    arguments: list[str],
    project: Path,
    runner=subprocess.run,
    timeout: int = 30,
) -> subprocess.CompletedProcess[str]:
    result = runner(  # nosec B603 - executable is resolved by shutil.which.
        [executable, *arguments],
        cwd=project,
        capture_output=True,
        text=True,
        check=False,
        timeout=timeout,
    )
    if result.returncode != 0:
        detail = (result.stderr or result.stdout).strip()
        raise RuntimeError(f"client plugin command failed: {detail}")
    return result


def client_json(
    executable: str,
    arguments: list[str],
    project: Path,
    runner=subprocess.run,
) -> object:
    result = client_command(executable, arguments, project, runner=runner)
    try:
        return json.loads(result.stdout)
    except json.JSONDecodeError as error:
        raise RuntimeError("client plugin command returned invalid JSON") from error


_STALE_MARKETPLACE_ERROR = re.compile(
    r"^(?:client plugin command failed:\s*)?(?:Error:\s*)?"
    r"failed to load (?:configured )?marketplace(?: snapshot)?\(s\):\s*-\s*"
    r"`(?P<name>chaos-engine-[0-9a-f]{12})`\s+at\s+(?P<root>.+?)"
    r"(?::\s*|\s+)marketplace root does not contain a supported manifest\.?\s*$",
    re.DOTALL,
)
_SUPPORTED_MARKETPLACE_MANIFESTS = (
    ".agents/plugins/marketplace.json",
    ".agents/plugins/api_marketplace.json",
    ".claude-plugin/marketplace.json",
    ".cursor-plugin/marketplace.json",
)


def stale_owned_marketplace(error: RuntimeError) -> str | None:
    match = _STALE_MARKETPLACE_ERROR.fullmatch(str(error))
    if match is None:
        return None
    name = match.group("name")
    root = Path(match.group("root").strip())
    if not root.is_absolute():
        return None
    try:
        if any(is_link_or_reparse(path) for path in (root, *root.parents)):
            return None
    except OSError:
        return None
    parts = tuple(os.path.normcase(part) for part in root.parts)
    durable = parts[-3:] == (
        os.path.normcase("ChaosEngine"),
        os.path.normcase("client-marketplaces"),
        os.path.normcase(name),
    )
    legacy = parts[-2:] == (
        os.path.normcase(".chaos-engine-state"),
        os.path.normcase("client-marketplace"),
    )
    if legacy:
        try:
            project = root.parent.parent.resolve()
        except OSError:
            return None
        digest = hashlib.sha256(os.path.normcase(str(project)).encode()).hexdigest()[:12]
        legacy = name == f"chaos-engine-{digest}"
    if not durable and not legacy:
        return None
    try:
        supported_manifest = any(
            (root / relative).exists() or is_link_or_reparse(root / relative)
            for relative in _SUPPORTED_MARKETPLACE_MANIFESTS
        )
    except OSError:
        return None
    if supported_manifest:
        return None
    return name


def remove_stale_marketplace_before_activation(
    client: str,
    executable: str,
    project: Path,
    *,
    runner=subprocess.run,
) -> None:
    arguments = ["plugin", "marketplace", "list", "--json"]
    try:
        client_json(executable, arguments, project, runner=runner)
    except RuntimeError as error:
        marketplace_name = stale_owned_marketplace(error)
        if marketplace_name is None:
            raise
        remove = ["plugin", "marketplace", "remove", marketplace_name]
        if client == "claude":
            remove.extend(["--scope", "local"])
        client_command(executable, remove, project, runner=runner)
        client_json(executable, arguments, project, runner=runner)


def same_path(left: object, right: Path) -> bool:
    if not isinstance(left, str):
        return False
    try:
        return os.path.normcase(str(Path(left).resolve())) == os.path.normcase(str(right.resolve()))
    except OSError:
        return False


def activation_contract(project: Path) -> tuple[Path, str, str, str]:
    project = project.resolve()
    digest = hashlib.sha256(os.path.normcase(str(project)).encode()).hexdigest()[:12]
    marketplace_name = f"chaos-engine-{digest}"
    root = maven_tools_data_root() / "ChaosEngine/client-marketplaces" / marketplace_name
    manifest_path = project / "plugins/chaos-engine/.codex-plugin/plugin.json"
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("ChaosEngine plugin manifest is unavailable") from error
    version = manifest.get("version") if isinstance(manifest, dict) else None
    if not isinstance(version, str) or re.fullmatch(r"\d+\.\d+\.\d+", version) is None:
        raise ValueError("ChaosEngine plugin version is invalid")
    return root, marketplace_name, f"{PLUGIN_NAME}@{marketplace_name}", version


def activation_bundle_root(activation: dict[str, object]) -> Path:
    """Return the exact receipt-owned durable marketplace path."""
    name = activation.get("marketplaceName")
    encoded_root = activation.get("bundleRoot")
    if not isinstance(name, str) or re.fullmatch(r"chaos-engine-[0-9a-f]{12}", name) is None:
        raise ValueError("ChaosEngine client activation receipt is invalid")
    if not isinstance(encoded_root, str):
        raise ValueError("ChaosEngine client activation receipt has no bundle root")
    root = Path(encoded_root)
    if not root.is_absolute() or root.name != name or root.parent.name != "client-marketplaces":
        raise ValueError("ChaosEngine client activation receipt bundle root is invalid")
    return root


def activation_plugins(project: Path, marketplace_name: str) -> dict[str, dict[str, object]]:
    plugins: dict[str, dict[str, object]] = {}
    for name in (PLUGIN_NAME, *COMPANION_PLUGIN_NAMES):
        manifest_path = project / f"plugins/{name}/.codex-plugin/plugin.json"
        try:
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
            raise ValueError(f"{name} plugin manifest is unavailable") from error
        version = manifest.get("version") if isinstance(manifest, dict) else None
        if (
            not isinstance(version, str)
            or re.fullmatch(r"\d+\.\d+\.\d+", version) is None
            or manifest.get("name") != name
        ):
            raise ValueError(f"{name} plugin manifest is invalid")
        plugins[name] = {
            "id": f"{name}@{marketplace_name}",
            "version": version,
            "source": project / f"plugins/{name}",
        }
    return plugins


def activation_plugins_from_root(root: Path, marketplace_name: str) -> dict[str, dict[str, object]]:
    plugins: dict[str, dict[str, object]] = {}
    source_root = root / "plugins"
    for name in (PLUGIN_NAME, *COMPANION_PLUGIN_NAMES):
        manifest_path = source_root / name / ".codex-plugin/plugin.json"
        if not manifest_path.is_file():
            continue
        try:
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
            raise ValueError(f"{name} plugin manifest is unavailable") from error
        version = manifest.get("version") if isinstance(manifest, dict) else None
        if (
            not isinstance(version, str)
            or re.fullmatch(r"\d+\.\d+\.\d+", version) is None
            or manifest.get("name") != name
        ):
            raise ValueError(f"{name} plugin manifest is invalid")
        plugins[name] = {
            "id": f"{name}@{marketplace_name}",
            "version": version,
            "source": root / f"plugins/{name}",
        }
    return plugins


def prepare_activation_bundle(project: Path) -> tuple[Path, str, str, str]:
    """Publish one path-unique generated marketplace without tracked machine paths."""
    project = project.resolve()
    root, marketplace_name, plugin_id, version = activation_contract(project)
    plugins = activation_plugins(project, marketplace_name)
    for name, contract in plugins.items():
        source_plugin = contract["source"]
        if (
            not isinstance(source_plugin, Path)
            or not source_plugin.is_dir()
            or is_link_or_reparse(source_plugin)
        ):
            raise ValueError(f"{name} plugin source is unavailable")
    state_root = root.parent
    state_root.mkdir(parents=True, exist_ok=True)
    building = state_root / f".{root.name}.building-{secrets.token_hex(8)}"
    backup = state_root / f".{root.name}.backup-{secrets.token_hex(8)}"
    building.mkdir()
    try:
        for name, contract in plugins.items():
            shutil.copytree(contract["source"], building / f"plugins/{name}")
        codex_marketplace = {
            "name": marketplace_name,
            "interface": {"displayName": "ChaosEngine Project"},
            "plugins": [
                {
                    "name": PLUGIN_NAME,
                    "source": {"source": "local", "path": "./plugins/chaos-engine"},
                    "policy": {"installation": "INSTALLED_BY_DEFAULT", "authentication": "ON_INSTALL"},
                    "category": "Developer Tools",
                },
                {
                    "name": CAVEMAN_PLUGIN_NAME,
                    "source": {"source": "local", "path": "./plugins/caveman"},
                    "policy": {
                        "installation": "INSTALLED_BY_DEFAULT",
                        "authentication": "ON_INSTALL",
                    },
                    "category": "Productivity",
                },
                {
                    "name": PONYTAIL_PLUGIN_NAME,
                    "source": {"source": "local", "path": "./plugins/ponytail"},
                    "policy": {
                        "installation": "INSTALLED_BY_DEFAULT",
                        "authentication": "ON_INSTALL",
                    },
                    "category": "Productivity",
                },
            ],
        }
        claude_marketplace = {
            "name": marketplace_name,
            "owner": {"name": "ChaosEngine contributors"},
            "description": "Neutral project-local agent harness.",
            "plugins": [
                {
                    "name": PLUGIN_NAME,
                    "source": "./plugins/chaos-engine",
                    "description": "Neutral project-local agent harness.",
                    "version": version,
                },
                {
                    "name": CAVEMAN_PLUGIN_NAME,
                    "source": "./plugins/caveman",
                    "description": "Ultra-compressed communication mode.",
                    "version": CAVEMAN_PLUGIN_VERSION,
                },
                {
                    "name": PONYTAIL_PLUGIN_NAME,
                    "source": "./plugins/ponytail",
                    "description": "Laziest solution that actually works.",
                    "version": PONYTAIL_PLUGIN_VERSION,
                },
            ],
        }
        for relative, document in (
            (".agents/plugins/marketplace.json", codex_marketplace),
            (".codex-plugin/marketplace.json", codex_marketplace),
            (".claude-plugin/marketplace.json", claude_marketplace),
        ):
            path = building / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        if root.exists() or is_link_or_reparse(root):
            if is_link_or_reparse(root) or not root.is_dir():
                raise ValueError("ChaosEngine activation marketplace collision")
            root.replace(backup)
        building.replace(root)
        if backup.exists():
            shutil.rmtree(backup)
    except BaseException:
        if building.exists():
            shutil.rmtree(building)
        if backup.exists() and not root.exists():
            backup.replace(root)
        raise
    return root, marketplace_name, plugin_id, version


def detected_plugin_status(
    project: Path,
    *,
    runner=subprocess.run,
    which=shutil.which,
) -> dict[str, dict[str, object]]:
    """Read back native plugin registration for every client installed on the host."""
    project = project.resolve()
    root, marketplace_name, plugin_id, version = activation_contract(project)
    plugins = activation_plugins(project, marketplace_name)
    status: dict[str, dict[str, object]] = {}
    for client in ("codex", "claude"):
        executable = which(client)
        if executable is None:
            continue
        if client == "codex":
            marketplace_document = client_json(
                executable, ["plugin", "marketplace", "list", "--json"], project, runner=runner
            )
            marketplaces = (
                marketplace_document.get("marketplaces", [])
                if isinstance(marketplace_document, dict)
                else []
            )
            plugin_document = client_json(
                executable, ["plugin", "list", "--available", "--json"], project, runner=runner
            )
            records = plugin_document.get("installed", []) if isinstance(plugin_document, dict) else []
            marketplace_ok = any(
                isinstance(item, dict)
                and item.get("name") == marketplace_name
                and same_path(item.get("root"), root)
                for item in marketplaces
            )
            plugin_states = {}
            for name, contract in plugins.items():
                present = any(
                    isinstance(item, dict)
                    and item.get("pluginId") == contract["id"]
                    and item.get("installed") is True
                    and item.get("enabled") is True
                    and isinstance(item.get("source"), dict)
                    and same_path(item["source"].get("path"), root / f"plugins/{name}")
                    for item in records
                )
                healthy = present and any(
                    isinstance(item, dict)
                    and item.get("pluginId") == contract["id"]
                    and item.get("version") == contract["version"]
                    for item in records
                )
                plugin_states[name] = "healthy" if healthy else ("stale" if present else "absent")
        else:
            marketplaces = client_json(
                executable, ["plugin", "marketplace", "list", "--json"], project, runner=runner
            )
            plugin_document = client_json(
                executable, ["plugin", "list", "--available", "--json"], project, runner=runner
            )
            records = plugin_document.get("installed", []) if isinstance(plugin_document, dict) else []
            marketplace_ok = isinstance(marketplaces, list) and any(
                isinstance(item, dict)
                and item.get("name") == marketplace_name
                and same_path(item.get("path"), root)
                for item in marketplaces
            )
            plugin_states = {}
            for name, contract in plugins.items():
                present = any(
                    isinstance(item, dict)
                    and item.get("id") == contract["id"]
                    and item.get("enabled") is True
                    and same_path(item.get("projectPath"), project)
                    for item in records
                )
                healthy = present and any(
                    isinstance(item, dict)
                    and item.get("id") == contract["id"]
                    and item.get("version") == contract["version"]
                    and same_path(item.get("projectPath"), project)
                    and cached_plugin_matches(item.get("installPath"), root / f"plugins/{name}")
                    for item in records
                )
                plugin_states[name] = "healthy" if healthy else ("stale" if present else "absent")
        plugin_ok = all(item == "healthy" for item in plugin_states.values())
        plugin_present = any(item != "absent" for item in plugin_states.values())
        status[client] = {
            "status": "healthy" if marketplace_ok and plugin_ok else "absent",
            "marketplace": "healthy" if marketplace_ok else "absent",
            "plugin": "healthy" if plugin_ok else ("stale" if plugin_present else "absent"),
            "plugins": plugin_states,
        }
    return status


def publish_vendor_plugin(
    after: dict[str, bytes],
    *,
    name: str,
    vendor: str,
    repository: str,
    commit: str,
    version: str,
) -> None:
    root = Path(__file__).resolve().parent / "vendor" / vendor
    skip = {"PIN.json", "INVENTORY.md"}
    for path in root.rglob("*"):
        if not path.is_file() or path.name in skip:
            continue
        relative = path.relative_to(root).as_posix()
        after[f"plugins/{name}/{relative}"] = path.read_bytes()
    after[f"plugins/{name}/UPSTREAM.md"] = (
        f"# {name.capitalize()} provenance\n\n"
        f"Bundled from `{repository}` under the MIT license.\n\n"
        f"- Upstream commit: `{commit}`\n"
        f"- Skill version: `{version}`\n"
        "- Install is project-local. Companion hooks do not deny tools or hold completion.\n"
    ).encode()


def companion_managed_paths() -> tuple[str, ...]:
    paths: list[str] = []
    for name, vendor in (
        (CAVEMAN_PLUGIN_NAME, "caveman"),
        (PONYTAIL_PLUGIN_NAME, "ponytail"),
    ):
        paths.extend(
            (
                f"plugins/{name}/.codex-plugin/plugin.json",
                f"plugins/{name}/.claude-plugin/plugin.json",
                f"plugins/{name}/UPSTREAM.md",
            )
        )
        root = Path(__file__).resolve().parent / "vendor" / vendor
        for path in sorted(root.rglob("*")):
            if path.is_file() and path.name not in {"PIN.json", "INVENTORY.md"}:
                paths.append(f"plugins/{name}/{path.relative_to(root).as_posix()}")
    return tuple(paths)


def companion_required_files(name: str) -> tuple[str, ...]:
    if name == CAVEMAN_PLUGIN_NAME:
        return (
            "skills/caveman/SKILL.md",
            "LICENSE",
            "src/hooks/caveman-activate.js",
            "UPSTREAM.md",
        )
    if name == PONYTAIL_PLUGIN_NAME:
        return (
            "skills/ponytail/SKILL.md",
            "LICENSE",
            "hooks/ponytail-activate.js",
            "UPSTREAM.md",
        )
    raise ValueError(f"unknown companion plugin: {name}")


def cached_plugin_matches(installed_path: object, source: Path) -> bool:
    if not isinstance(installed_path, str):
        return False
    installed = Path(installed_path)
    required = (
        (
            "hooks/guard.py",
            "hooks/kernel.py",
            "hooks/launch.js",
            "hooks/lifecycle.py",
            "hooks/matchers.json",
            "hooks/reflection.py",
            "skills/chaos-engine/SKILL.md",
        )
        if source.name == PLUGIN_NAME
        else companion_required_files(source.name)
    )
    for relative in required:
        cached = installed / relative
        expected = source / relative
        try:
            if not cached.is_file() or cached.read_bytes() != expected.read_bytes():
                return False
        except OSError:
            return False
    return True


def activation_commands(root: Path, plugin_id: str) -> dict[str, dict[str, list[str]]]:
    return {
        "codex": {
            "marketplace": ["plugin", "marketplace", "add", str(root), "--json"],
            "install": ["plugin", "add", plugin_id, "--json"],
            "remove": ["plugin", "remove", plugin_id, "--json"],
            "removeMarketplace": ["plugin", "marketplace", "remove", plugin_id.split("@", 1)[1]],
        },
        "claude": {
            "marketplace": ["plugin", "marketplace", "add", "--scope", "local", str(root)],
            "install": ["plugin", "install", plugin_id, "--scope", "local"],
            "remove": ["plugin", "uninstall", plugin_id, "--scope", "local"],
            "removeMarketplace": [
                "plugin", "marketplace", "remove", plugin_id.split("@", 1)[1], "--scope", "local"
            ],
        },
    }


def record_client_activation(project: Path, activation: dict[str, object]) -> None:
    receipt_path = project / RECEIPT_NAME
    if read_file(project, receipt_path) is None:
        return
    receipt, raw = read_receipt(project)
    if receipt.get("phase") != "installed":
        raise ValueError("ChaosEngine host activation requires an installed receipt")
    receipt["clientActivation"] = {
        "marketplaceName": activation["marketplaceName"],
        "bundleRoot": activation["bundleRoot"],
        "ownedClients": activation["ownedClients"],
        "pluginVersion": activation["pluginVersion"],
        "claudeLocalBefore": activation["claudeLocalBefore"],
    }
    write_receipt(project, receipt, raw)


def restore_claude_local_before(project: Path, activation: dict[str, object]) -> None:
    encoded = activation.get("claudeLocalBefore")
    if encoded is not None and not isinstance(encoded, str):
        raise ValueError("ChaosEngine client activation receipt is invalid")
    before = base64.b64decode(encoded, validate=True) if isinstance(encoded, str) else None
    path = project / ".claude/settings.local.json"
    current = read_file(project, path)
    if before is None:
        if current is not None:
            atomic_remove(project, path, current)
    elif current != before:
        atomic_write(project, path, before, current)


def remove_client_activation(
    project: Path,
    clients: list[str],
    *,
    runner=subprocess.run,
    which=shutil.which,
) -> None:
    root, marketplace_name, _, _ = activation_contract(project)
    plugins = activation_plugins(project, marketplace_name)
    for client in reversed(clients):
        executable = which(client)
        if executable is None:
            continue
        selected = lambda name, chosen=client, path=executable: path if name == chosen else None
        current = detected_plugin_status(project, runner=runner, which=selected).get(client, {})
        plugin_states = current.get("plugins", {})
        for name in reversed(tuple(plugins)):
            if isinstance(plugin_states, dict) and plugin_states.get(name) in {"healthy", "stale"}:
                commands = activation_commands(root, str(plugins[name]["id"]))
                client_command(executable, commands[client]["remove"], project, runner=runner)
        current = detected_plugin_status(project, runner=runner, which=selected).get(client, {})
        if current.get("marketplace") == "healthy":
            commands = activation_commands(root, str(plugins[PLUGIN_NAME]["id"]))
            client_command(executable, commands[client]["removeMarketplace"], project, runner=runner)


def restore_client_activation(
    project: Path,
    activation: dict[str, object] | None,
    *,
    runner=subprocess.run,
    which=shutil.which,
) -> None:
    if activation is None:
        return
    clients = activation.get("ownedClients")
    if not isinstance(clients, list) or not all(item in {"codex", "claude"} for item in clients):
        raise ValueError("ChaosEngine client activation receipt is invalid")
    root, marketplace_name, _, _ = activation_contract(project)
    plugins = activation_plugins_from_root(root, marketplace_name)
    if PLUGIN_NAME not in plugins:
        raise ValueError(f"{PLUGIN_NAME} plugin manifest is unavailable")
    for client in clients:
        executable = which(client)
        if executable is None:
            continue
        commands = activation_commands(root, str(plugins[PLUGIN_NAME]["id"]))
        client_command(executable, commands[client]["marketplace"], project, runner=runner)
        for contract in plugins.values():
            plugin_commands = activation_commands(root, str(contract["id"]))
            client_command(executable, plugin_commands[client]["install"], project, runner=runner)


def activate_detected_plugins(
    project: Path,
    *,
    runner=subprocess.run,
    which=shutil.which,
    confirmer=None,
) -> dict[str, object]:
    """Register and install the project plugin for detected native clients."""
    project = project.resolve()
    old_root, _, _, _ = activation_contract(project)
    prior_activation = None
    if read_file(project, project / RECEIPT_NAME) is not None:
        prior_receipt, _ = read_receipt(project)
        candidate = prior_receipt.get("clientActivation")
        prior_activation = candidate if isinstance(candidate, dict) else None
    claude_local_before = read_file(project, project / ".claude/settings.local.json")
    original_local = (
        prior_activation.get("claudeLocalBefore")
        if prior_activation is not None and "claudeLocalBefore" in prior_activation
        else (
            base64.b64encode(claude_local_before).decode("ascii")
            if claude_local_before is not None
            else None
        )
    )
    snapshot = old_root.parent / f".{old_root.name}.activation-backup-{secrets.token_hex(8)}"
    if old_root.exists():
        if is_link_or_reparse(old_root) or not old_root.is_dir():
            raise ValueError("ChaosEngine activation marketplace collision")
        shutil.copytree(old_root, snapshot)
    root, marketplace_name, plugin_id, _ = prepare_activation_bundle(project)
    created_marketplaces: list[str] = []
    created_plugins: list[str] = []
    plugins = activation_plugins(project, marketplace_name)
    marketplace_commands = activation_commands(root, str(plugins[PLUGIN_NAME]["id"]))
    touched_clients: list[str] = []
    receipt: dict[str, object] = {
        "createdMarketplaces": created_marketplaces,
        "createdPlugins": created_plugins,
        "marketplaceName": marketplace_name,
        "bundleRoot": str(root),
    }
    try:
        for client in ("codex", "claude"):
            executable = which(client)
            if executable is None:
                continue
            selected_client = lambda name, selected=client, path=executable: path if name == selected else None
            remove_stale_marketplace_before_activation(
                client, executable, project, runner=runner
            )
            current = detected_plugin_status(project, runner=runner, which=selected_client)[client]
            touched_clients.append(client)
            if current["marketplace"] != "healthy":
                if confirmer is not None:
                    confirmer(f"Register {client} plugin marketplace")
                client_command(
                    executable,
                    marketplace_commands[client]["marketplace"],
                    project,
                    runner=runner,
                )
                created_marketplaces.append(client)
            current = detected_plugin_status(project, runner=runner, which=selected_client)[client]
            plugin_states = current.get("plugins", {})
            for name, contract in plugins.items():
                state = plugin_states.get(name) if isinstance(plugin_states, dict) else "absent"
                commands = activation_commands(root, str(contract["id"]))
                if state == "absent":
                    if confirmer is not None:
                        confirmer(f"Activate {name} plugin for {client}")
                    client_command(executable, commands[client]["install"], project, runner=runner)
                    created_plugins.append(f"{client}:{name}")
                elif state == "stale":
                    if confirmer is not None:
                        confirmer(f"Replace stale {name} plugin for {client}")
                    client_command(executable, commands[client]["remove"], project, runner=runner)
                    client_command(executable, commands[client]["install"], project, runner=runner)
            verified = detected_plugin_status(project, runner=runner, which=selected_client)[client]
            if verified["status"] != "healthy":
                raise RuntimeError(f"{client} plugin activation did not verify")
        verified_clients = detected_plugin_status(project, runner=runner, which=which)
        receipt["ownedClients"] = touched_clients
        receipt["pluginVersion"] = activation_contract(project)[3]
        receipt["clients"] = verified_clients
        receipt["claudeLocalBefore"] = original_local
        record_client_activation(project, receipt)
        if snapshot.exists():
            shutil.rmtree(snapshot)
    except BaseException:
        rollback_errors: list[BaseException] = []
        try:
            remove_client_activation(project, touched_clients, runner=runner, which=which)
        except BaseException as error:
            rollback_errors.append(error)
        try:
            if snapshot.exists():
                if root.exists():
                    shutil.rmtree(root)
                snapshot.replace(root)
                restore_client_activation(
                    project, prior_activation, runner=runner, which=which
                )
            elif root.exists():
                shutil.rmtree(root)
            if prior_activation is None:
                restore_claude_local_before(
                    project, {"claudeLocalBefore": original_local}
                )
        except BaseException as error:
            rollback_errors.append(error)
        if rollback_errors:
            raise rollback_errors[0]
        raise
    return receipt


def deactivate_created_plugins(
    project: Path,
    receipt: dict[str, object],
    *,
    runner=subprocess.run,
    which=shutil.which,
) -> None:
    """Compensate only native registrations created by one activation call."""
    project = project.resolve()
    marketplace_name = receipt.get("marketplaceName")
    if not isinstance(marketplace_name, str):
        _, marketplace_name, _, _ = activation_contract(project)
    plugin_id = f"{PLUGIN_NAME}@{marketplace_name}"
    created_plugins = receipt.get("createdPlugins", [])
    created_marketplaces = receipt.get("createdMarketplaces", [])
    for client in reversed(created_plugins if isinstance(created_plugins, list) else []):
        executable = which(str(client))
        if executable is None:
            continue
        arguments = (
            ["plugin", "remove", plugin_id, "--json"]
            if client == "codex"
            else ["plugin", "uninstall", plugin_id, "--scope", "local"]
        )
        client_command(executable, arguments, project, runner=runner)
    for client in reversed(created_marketplaces if isinstance(created_marketplaces, list) else []):
        executable = which(str(client))
        if executable is None:
            continue
        arguments = ["plugin", "marketplace", "remove", marketplace_name]
        if client == "claude":
            arguments.extend(["--scope", "local"])
        client_command(executable, arguments, project, runner=runner)
MAVEN_TOOLS_MCP_VERSION = "3.2.0"
MAVEN_TOOLS_MCP_COMMIT = "4475ff6c61f23ea9a93cb6d5665a63235ef2ef36"
MAVEN_TOOLS_MCP_RECEIPT = "install-receipt.json"
MAVEN_TOOLS_MCP_PROFILE = "docker,no-context7"
MAVEN_TOOLS_CACHE_LOCK = ".cache.lock"
MAVEN_TOOLS_CACHE_LOCK_MAGIC = b"chaos-engine-maven-tools-cache-lock-v1\n"
TEMURIN_RECEIPT = "runtime-receipt.json"
LEGACY_MAVEN_TOOLS_SERVER = {
    "command": "docker",
    "args": ["run", "-i", "--rm", "arvindand/maven-tools-mcp:3.2.0"],
}
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
GITIGNORE_START = "# CHAOSENGINE-RUNTIME:START"
GITIGNORE_END = "# CHAOSENGINE-RUNTIME:END"
GITATTRIBUTES_START = "# CHAOSENGINE-EOL:START"
GITATTRIBUTES_END = "# CHAOSENGINE-EOL:END"


def interpreter(platform_name: str | None = None) -> tuple[str, list[str]]:
    platform_name = platform_name or os.name
    return ("py", ["-3"]) if platform_name == "nt" else ("python3", [])


def plugin_cache_version(core_commit: str | None) -> str:
    """Return a cache-busting SemVer; stale activation is reinstalled, never ordered."""
    if core_commit and re.fullmatch(r"[0-9a-f]{40}", core_commit):
        return f"1.0.{int(core_commit[:8], 16)}"
    return "1.0.0"


def java_major(java: Path) -> int | None:
    try:
        result = subprocess.run(  # nosec B603 - executable is resolved before use.
            [str(java), "-version"],
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return None
    match = re.search(r'version "(?P<major>\d+)', result.stderr + result.stdout)
    return int(match.group("major")) if match else None


def verified_maven_tools_jar(candidate: Path) -> Path | None:
    if not candidate.is_file() or is_link_or_reparse(candidate):
        return None
    jar = candidate.resolve()
    receipt_path = jar.parent / MAVEN_TOOLS_MCP_RECEIPT
    if not receipt_path.is_file() or is_link_or_reparse(receipt_path):
        return None
    try:
        if os.stat(jar, follow_symlinks=False).st_nlink != 1 or os.stat(receipt_path, follow_symlinks=False).st_nlink != 1:
            return None
    except OSError:
        return None
    try:
        receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return None
    try:
        digest = hashlib.sha256(jar.read_bytes()).hexdigest()
    except OSError:
        return None
    expected = {
        "version": MAVEN_TOOLS_MCP_VERSION,
        "commit": MAVEN_TOOLS_MCP_COMMIT,
        "jar": jar.name,
        "sha256": digest,
    }
    return jar if receipt == expected else None


def maven_tools_data_root() -> Path:
    configured = os.environ.get("LOCALAPPDATA" if os.name == "nt" else "XDG_DATA_HOME", "")
    return Path(configured or Path.home() / ".local/share").absolute()


def maven_tools_cache_root() -> Path:
    return maven_tools_data_root() / "ChaosEngine/tools/maven-tools-mcp"


def _validate_cache_path(path: Path, anchor: Path) -> None:
    path = path.absolute()
    anchor = anchor.absolute()
    try:
        relative = path.relative_to(anchor)
    except ValueError as error:
        raise ValueError("Maven Tools MCP cache path escapes its data root") from error
    current = anchor
    for part in (Path(), *relative.parts):
        current = current / part
        if is_link_or_reparse(current):
            raise ValueError(f"Maven Tools MCP cache path is linked: {current}")


def _cache_anchor(root: Path | None) -> Path:
    selected = maven_tools_cache_root() if root is None else Path(root).absolute()
    return Path(selected.anchor)


def _rename_no_replace(source: Path, target: Path) -> None:
    """Atomically rename a directory and fail if the target exists."""
    if os.name == "nt":
        os.rename(source, target)
        return
    libc = ctypes.CDLL(None, use_errno=True)
    source_bytes = os.fsencode(source)
    target_bytes = os.fsencode(target)
    if sys.platform == "darwin":
        rename = getattr(libc, "renamex_np", None)
        if rename is None:
            raise RuntimeError("atomic no-overwrite rename is unavailable")
        rename.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_uint]
        rename.restype = ctypes.c_int
        result = rename(source_bytes, target_bytes, 0x00000004)
    else:
        rename = getattr(libc, "renameat2", None)
        if rename is None:
            raise RuntimeError("atomic no-overwrite rename is unavailable")
        rename.argtypes = [
            ctypes.c_int,
            ctypes.c_char_p,
            ctypes.c_int,
            ctypes.c_char_p,
            ctypes.c_uint,
        ]
        rename.restype = ctypes.c_int
        result = rename(-100, source_bytes, -100, target_bytes, 1)
    if result != 0:
        error_number = ctypes.get_errno()
        raise OSError(error_number, os.strerror(error_number), str(target))


def _maven_tools_version_directory(root: Path, version: str) -> Path:
    if version != MAVEN_TOOLS_MCP_VERSION:
        raise ValueError(f"unsupported Maven Tools MCP cache version: {version}")
    return root.absolute() / version


@contextmanager
def maven_tools_cache_lock(root: Path | None = None, *, anchor: Path | None = None):
    root = (root or maven_tools_cache_root()).absolute()
    anchor = anchor or _cache_anchor(root)
    _validate_cache_path(root, anchor)
    root.mkdir(parents=True, exist_ok=True)
    _validate_cache_path(root, anchor)
    lock_path = root / MAVEN_TOOLS_CACHE_LOCK
    flags = os.O_RDWR | getattr(os, "O_BINARY", 0)
    created = False
    try:
        descriptor = os.open(lock_path, flags | os.O_CREAT | os.O_EXCL, 0o600)
        created = True
    except FileExistsError:
        if is_link_or_reparse(lock_path):
            raise ValueError(f"Maven Tools MCP cache lock is linked: {lock_path}")
        descriptor = os.open(lock_path, flags)
    try:
        stream = os.fdopen(descriptor, "r+b", closefd=True)
    except BaseException:
        os.close(descriptor)
        raise
    try:
        opened = os.fstat(stream.fileno())
        named = os.stat(lock_path, follow_symlinks=False)
        if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino) or named.st_nlink != 1:
            raise ValueError(f"Maven Tools MCP cache lock collision: {lock_path}")
        if created:
            stream.write(MAVEN_TOOLS_CACHE_LOCK_MAGIC)
            stream.flush()
            os.fsync(stream.fileno())
        else:
            stream.seek(0)
            if stream.read() != MAVEN_TOOLS_CACHE_LOCK_MAGIC:
                raise ValueError(f"Maven Tools MCP cache lock collision: {lock_path}")
        stream.seek(0)
        if os.name == "nt":
            import msvcrt  # pylint: disable=import-outside-toplevel

            msvcrt.locking(stream.fileno(), msvcrt.LK_NBLCK, 1)
        else:
            import fcntl  # pylint: disable=import-outside-toplevel,import-error

            fcntl.flock(stream.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError as error:
        stream.close()
        raise RuntimeError("another Maven Tools MCP cache operation is already running") from error
    except BaseException:
        stream.close()
        raise
    try:
        yield
    finally:
        try:
            stream.seek(0)
            if os.name == "nt":
                msvcrt.locking(stream.fileno(), msvcrt.LK_UNLCK, 1)
            else:
                fcntl.flock(stream.fileno(), fcntl.LOCK_UN)
        finally:
            stream.close()


def _maven_tools_cache_status_unlocked(
    root: Path, version: str, *, anchor: Path
) -> dict[str, str]:
    version_root = _maven_tools_version_directory(root, version)
    result = {"component": "maven-tools-mcp", "version": version, "path": str(version_root)}
    _validate_cache_path(version_root, anchor)
    tombstone = root / f".purging-{version}"
    purge_claims = tuple(root.glob(f".purged-{version}-*")) if root.is_dir() else ()
    if tombstone.exists() or is_link_or_reparse(tombstone) or purge_claims:
        return {**result, "status": "invalid", "reason": "cache purge recovery is required"}
    if not version_root.exists() and not is_link_or_reparse(version_root):
        return {**result, "status": "absent"}
    if is_link_or_reparse(root) or is_link_or_reparse(version_root) or not version_root.is_dir():
        return {**result, "status": "invalid", "reason": "cache path is linked or invalid"}
    expected_names = {
        f"maven-tools-mcp-{version}.jar",
        MAVEN_TOOLS_MCP_RECEIPT,
    }
    try:
        names = {path.name for path in version_root.iterdir()}
    except OSError:
        return {**result, "status": "invalid", "reason": "cache directory is inaccessible"}
    if names != expected_names:
        return {**result, "status": "invalid", "reason": "cache contains unknown or missing files"}
    jar = version_root / f"maven-tools-mcp-{version}.jar"
    if verified_maven_tools_jar(jar) is None:
        return {**result, "status": "invalid", "reason": "JAR receipt validation failed"}
    return {**result, "status": "healthy", "commit": MAVEN_TOOLS_MCP_COMMIT}


def _unlink_stable_cache_file(path: Path, expected: os.stat_result) -> None:
    current = os.stat(path, follow_symlinks=False)
    if (
        (current.st_dev, current.st_ino, current.st_nlink)
        != (expected.st_dev, expected.st_ino, 1)
        or is_link_or_reparse(path)
    ):
        raise ValueError("Maven Tools MCP cache changed before purge")
    path.unlink()


def _rmdir_stable_cache_directory(path: Path, expected: os.stat_result) -> None:
    current = os.stat(path, follow_symlinks=False)
    if (
        (current.st_dev, current.st_ino) != (expected.st_dev, expected.st_ino)
        or is_link_or_reparse(path)
    ):
        raise ValueError("Maven Tools MCP cache directory changed before purge")
    path.rmdir()


def maven_tools_cache_status(
    version: str = MAVEN_TOOLS_MCP_VERSION, *, root: Path | None = None
) -> dict[str, str]:
    cache_root = (root or maven_tools_cache_root()).absolute()
    anchor = _cache_anchor(cache_root)
    version_root = _maven_tools_version_directory(cache_root, version)
    try:
        _validate_cache_path(version_root, anchor)
        if not cache_root.exists() and not is_link_or_reparse(cache_root):
            return {"component": "maven-tools-mcp", "version": version, "path": str(version_root), "status": "absent"}
        with maven_tools_cache_lock(cache_root, anchor=anchor):
            return _maven_tools_cache_status_unlocked(cache_root, version, anchor=anchor)
    except RuntimeError:
        return {"component": "maven-tools-mcp", "version": version, "path": str(version_root), "status": "busy"}
    except (OSError, ValueError):
        return {"component": "maven-tools-mcp", "version": version, "path": str(version_root), "status": "invalid", "reason": "cache lock is linked or invalid"}


def purge_maven_tools_cache(
    version: str, *, root: Path | None = None
) -> dict[str, str]:
    cache_root = (root or maven_tools_cache_root()).absolute()
    anchor = _cache_anchor(cache_root)
    version_root = _maven_tools_version_directory(cache_root, version)
    if not cache_root.exists() and not is_link_or_reparse(cache_root):
        return {"component": "maven-tools-mcp", "version": version, "path": str(version_root), "status": "absent"}
    with maven_tools_cache_lock(cache_root, anchor=anchor):
        observed = _maven_tools_cache_status_unlocked(cache_root, version, anchor=anchor)
        if observed["status"] == "absent":
            return observed
        if observed["status"] != "healthy":
            raise ValueError(f"Maven Tools MCP cache purge refused: {observed.get('reason', 'invalid cache')}")
        jar = version_root / f"maven-tools-mcp-{version}.jar"
        receipt = version_root / MAVEN_TOOLS_MCP_RECEIPT
        identities = {
            jar.name: os.stat(jar, follow_symlinks=False),
            receipt.name: os.stat(receipt, follow_symlinks=False),
        }
        directory_identity = os.stat(version_root, follow_symlinks=False)
        tombstone = cache_root / f".purging-{version}"
        if tombstone.exists() or is_link_or_reparse(tombstone):
            raise ValueError("Maven Tools MCP cache purge recovery is required")
        try:
            _rename_no_replace(version_root, tombstone)
        except FileExistsError as error:
            raise ValueError("Maven Tools MCP cache purge recovery is required") from error
        removed_any = False
        try:
            tombstone_jar = tombstone / jar.name
            tombstone_receipt = tombstone / receipt.name
            if (
                verified_maven_tools_jar(tombstone_jar) is None
                or {path.name for path in tombstone.iterdir()} != {jar.name, receipt.name}
            ):
                raise ValueError("Maven Tools MCP cache changed before purge")
            _unlink_stable_cache_file(tombstone_jar, identities[jar.name])
            removed_any = True
            _unlink_stable_cache_file(tombstone_receipt, identities[receipt.name])
            claim = cache_root / f".purged-{version}-{secrets.token_hex(16)}"
            _rename_no_replace(tombstone, claim)
            _rmdir_stable_cache_directory(claim, directory_identity)
        except BaseException:
            if not removed_any and tombstone.exists() and not version_root.exists():
                _rename_no_replace(tombstone, version_root)
            raise
        return {**observed, "status": "purged"}


def publish_maven_tools_cache(staging: Path, *, root: Path | None = None) -> Path:
    staging = staging.absolute()
    cache_root = (root or maven_tools_cache_root()).absolute()
    anchor = _cache_anchor(cache_root)
    version = MAVEN_TOOLS_MCP_VERSION
    common_root = Path(os.path.commonpath((staging, cache_root)))
    _validate_cache_path(staging, common_root)
    _validate_cache_path(cache_root, common_root)
    if is_link_or_reparse(staging) or not staging.is_dir():
        raise ValueError("Maven Tools MCP staging directory is invalid")
    jar = staging / f"maven-tools-mcp-{version}.jar"
    expected_names = {jar.name, MAVEN_TOOLS_MCP_RECEIPT}
    try:
        names = {path.name for path in staging.iterdir()}
    except OSError as error:
        raise ValueError("Maven Tools MCP staging pair is inaccessible") from error
    if names != expected_names or verified_maven_tools_jar(jar) is None:
        raise ValueError("Maven Tools MCP staging pair is invalid")
    cache_root.mkdir(parents=True, exist_ok=True)
    with maven_tools_cache_lock(cache_root, anchor=anchor):
        target = _maven_tools_version_directory(cache_root, version)
        if target.exists() or is_link_or_reparse(target):
            raise ValueError(f"Maven Tools MCP cache version already exists: {target}")
        if os.stat(staging).st_dev != os.stat(cache_root).st_dev:
            raise ValueError("Maven Tools MCP staging directory must use the cache filesystem")
        try:
            _rename_no_replace(staging, target)
        except FileExistsError as error:
            raise ValueError(f"Maven Tools MCP cache version already exists: {target}") from error
        except OSError as error:
            if error.errno == errno.EEXIST:
                raise ValueError(f"Maven Tools MCP cache version already exists: {target}") from error
            raise
        return target


def discover_maven_tools_runtime() -> tuple[Path, Path] | None:
    configured_jar = os.environ.get("CHAOSENGINE_MAVEN_TOOLS_MCP_JAR")
    jar_candidates = [
        Path(configured_jar).expanduser() if configured_jar else None,
        maven_tools_cache_root()
        / MAVEN_TOOLS_MCP_VERSION
        / f"maven-tools-mcp-{MAVEN_TOOLS_MCP_VERSION}.jar",
    ]
    jar = next(
        (
            verified
            for candidate in jar_candidates
            if candidate is not None
            for verified in (verified_maven_tools_jar(candidate),)
            if verified is not None
        ),
        None,
    )
    if jar is None:
        return None

    configured_java = os.environ.get("CHAOSENGINE_JAVA")
    java_home = os.environ.get("JAVA_HOME")
    path_java = shutil.which("java")
    system = "windows" if os.name == "nt" else "macos" if sys.platform == "darwin" else "linux"
    machine = platform.machine().lower()
    architecture = "arm64" if machine in {"arm64", "aarch64"} else "x64"
    managed_root = maven_tools_cache_root().parent / "temurin" / "25.0.4+7" / f"{system}-{architecture}"
    managed_java = managed_root / (
        "bin/java.exe" if os.name == "nt" else
        "Contents/Home/bin/java" if sys.platform == "darwin" else "bin/java"
    )
    java_candidates = [
        Path(configured_java).expanduser() if configured_java else None,
        Path(java_home) / "bin" / ("java.exe" if os.name == "nt" else "java")
        if java_home
        else None,
        Path(path_java) if path_java else None,
        verified_managed_temurin(managed_java, f"{system}-{architecture}"),
    ]
    for candidate in java_candidates:
        if candidate is None or not candidate.is_file():
            continue
        try:
            resolved = candidate.resolve(strict=True)
        except OSError:
            continue
        if is_link_or_reparse(resolved):
            continue
        if (java_major(resolved) or 0) >= 17:
            return resolved, jar
    return None


def verified_managed_temurin(candidate: Path, host_platform: str) -> Path | None:
    if not candidate.is_file() or is_link_or_reparse(candidate):
        return None
    receipt_path = candidate.parents[3 if sys.platform == "darwin" else 1] / TEMURIN_RECEIPT
    if not receipt_path.is_file() or is_link_or_reparse(receipt_path):
        return None
    try:
        receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
        digest = hashlib.sha256(candidate.read_bytes()).hexdigest()
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return None
    expected_architecture = "x64" if host_platform == "windows-arm64" else host_platform.split("-", 1)[1]
    expected = {
        "schemaVersion": 1,
        "runtime": "temurin",
        "version": "25.0.4+7",
        "hostPlatform": host_platform,
        "artifactArchitecture": expected_architecture,
        "emulated": host_platform == "windows-arm64",
        "java": candidate.relative_to(receipt_path.parent).as_posix(),
        "javaSha256": digest,
    }
    return candidate.resolve() if receipt == expected and java_major(candidate) == 25 else None


def probe_maven_tools_runtime(
    java: Path,
    jar: Path,
    *,
    popen=subprocess.Popen,
    timeout: float = 30.0,
) -> bool:
    """Require a real MCP initialize and non-empty tools/list exchange."""
    process = None
    try:
        process = popen(  # nosec B603 - both executables are receipt-verified owned paths.
            [str(java), "-jar", str(jar), f"--spring.profiles.active={MAVEN_TOOLS_MCP_PROFILE}"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            encoding="utf-8",
        )
        if process.stdin is None or process.stdout is None:
            return False

        def exchange(requests: list[dict[str, object]]) -> dict[str, object]:
            for request in requests:
                process.stdin.write(json.dumps(request, separators=(",", ":")) + "\n")
            process.stdin.flush()
            received: queue.Queue[str] = queue.Queue(maxsize=1)
            threading.Thread(
                target=lambda: received.put(process.stdout.readline()), daemon=True
            ).start()
            response = json.loads(received.get(timeout=timeout))
            return response if isinstance(response, dict) else {}

        initialized = exchange([{
            "jsonrpc": "2.0", "id": 1, "method": "initialize",
            "params": {"protocolVersion": "2025-03-26", "capabilities": {},
                       "clientInfo": {"name": "chaosengine-installer", "version": "1"}},
        }])
        if initialized.get("id") != 1 or not isinstance(initialized.get("result"), dict):
            return False
        listed = exchange([
            {"jsonrpc": "2.0", "method": "notifications/initialized", "params": {}},
            {"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}},
        ])
        result = listed.get("result")
        return listed.get("id") == 2 and isinstance(result, dict) and bool(result.get("tools"))
    except (OSError, ValueError, json.JSONDecodeError, queue.Empty):
        return False
    finally:
        if process is not None and process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=5)


_DEPENDENCY_SPECIFICATION_PATH = Path(__file__).parent / "dependencies.json"


def memory_package_pin() -> str:
    """Return the pinned Memory package from the colocated specification."""
    specification = json.loads(_DEPENDENCY_SPECIFICATION_PATH.read_text(encoding="utf-8"))
    package = specification["tools"]["memory"]["package"]
    if not isinstance(package, str) or not package.startswith(MEMORY_PACKAGE_PREFIX):
        raise ValueError("ChaosEngine Memory package pin is invalid")
    return package


def unguarded_mempalace_server(existing: object) -> bool:
    if not isinstance(existing, dict):
        return False
    command = Path(str(existing.get("command") or "")).name.casefold()
    args = existing.get("args") if isinstance(existing.get("args"), list) else []
    arg_names = {Path(str(item)).name.casefold() for item in args if isinstance(item, str)}
    launches = command in UNGUARDED_MEMPALACE_COMMANDS or bool(
        arg_names & UNGUARDED_MEMPALACE_COMMANDS
    )
    return launches and "--palace" not in args


def repair_legacy_store_servers(
    servers: dict[str, object],
    *,
    maven_runtime: tuple[Path, Path] | None = None,
    managed_python: Path | None = None,
) -> None:
    """Rewrite drifted Memory/MemPalace MCP aliases without colliding custom servers."""
    pin = memory_package_pin()
    owned_mempalace = owned_servers(
        maven_runtime=maven_runtime, managed_python=managed_python
    )["chaosengine-mempalace"]
    for name, existing in list(servers.items()):
        if unguarded_mempalace_server(existing):
            repaired = dict(owned_mempalace)
            if isinstance(existing, dict) and "required" in existing:
                repaired["required"] = existing["required"]
            servers[name] = repaired
            continue
        if not isinstance(existing, dict):
            continue
        args = existing.get("args")
        if not isinstance(args, list):
            continue
        rewritten = []
        changed = False
        for item in args:
            if isinstance(item, str) and item.startswith(MEMORY_PACKAGE_PREFIX) and item != pin:
                rewritten.append(pin)
                changed = True
            else:
                rewritten.append(item)
        if changed:
            updated = dict(existing)
            updated["args"] = rewritten
            servers[name] = updated


def repair_codex_store_servers(existing: str) -> str:
    """Keep Codex Memory/MemPalace aliases on the pinned sqlite_exact contract."""
    pin = memory_package_pin()
    existing = STALE_MEMORY_PACKAGE_RE.sub(pin, existing)
    parts = CODEX_SERVER_HEADER_RE.split(existing)
    if len(parts) == 1:
        return existing
    owned = owned_servers()["chaosengine-mempalace"]
    args = json.dumps(owned["args"])
    windows_args = json.dumps(owned["argsWindows"])
    rendered = (
        f'command = "{owned["command"]}"\n'
        f"args = {args}\n"
        f'commandWindows = "{owned["commandWindows"]}"\n'
        f"argsWindows = {windows_args}\n"
        f'cwd = "{owned["cwd"]}"\n'
        f"{MEMPALACE_MCP_ENV_TOML}"
    )
    out = [parts[0]]
    index = 1
    while index < len(parts):
        header = parts[index]
        body = parts[index + 1] if index + 1 < len(parts) else ""
        if re.search(r'(?m)^command\s*=\s*"mempalace-mcp"\s*$', body):
            required = re.search(r"(?m)^required\s*=\s*(true|false)\s*$", body)
            body = rendered
            if required:
                body += f"required = {required.group(1)}\n"
            if not body.endswith("\n"):
                body += "\n"
        out.extend((header, body))
        index += 2
    return "".join(out)


def portable_python_server(
    script_args: list[str], extra: dict[str, object] | None = None,
    managed_python: Path | None = None,
) -> dict[str, object]:
    posix_command, posix_prefix = interpreter("posix")
    windows_command, windows_prefix = interpreter("nt")
    server: dict[str, object] = {
        "command": str(managed_python) if managed_python else posix_command,
        "args": script_args if managed_python else [*posix_prefix, *script_args],
        "commandWindows": str(managed_python) if managed_python else windows_command,
        "argsWindows": script_args if managed_python else [*windows_prefix, *script_args],
        "cwd": ".",
    }
    if extra:
        server.update(extra)
    return server


def owned_servers(
    platform_name: str | None = None,
    maven_runtime: tuple[Path, Path] | None = None,
    managed_python: Path | None = None,
) -> dict[str, dict[str, object]]:
    del platform_name
    servers: dict[str, dict[str, object]] = {
        "chaosengine-memory": portable_python_server(
            [".chaos-engine/tool.py", "memory-mcp"], managed_python=managed_python
        ),
        "chaosengine-mempalace": portable_python_server(
            [
                ".chaos-engine/tool.py",
                "mempalace-mcp",
                "--palace",
                ".chaos-engine-state/mempalace",
                "--backend",
                "sqlite_exact",
            ],
            extra={"env": dict(MEMPALACE_MCP_ENV)},
            managed_python=managed_python,
        ),
    }
    if maven_runtime is not None:
        java, jar = maven_runtime
        servers["maven-tools-mcp"] = {
            "command": str(java),
            "args": [
                "-jar",
                str(jar),
                f"--spring.profiles.active={MAVEN_TOOLS_MCP_PROFILE}",
            ],
        }
    return servers


def managed_paths() -> tuple[str, ...]:
    return (
        ".agents/skills/chaos-engine/SKILL.md",
        ".claude/skills/chaos-engine/SKILL.md",
        ".gemini/skills/chaos-engine/SKILL.md",
        ".github/skills/chaos-engine/SKILL.md",
        ".agents/skills/README.md",
        ".agents/plugins/marketplace.json",
        ".claude-plugin/marketplace.json",
        "plugins/chaos-engine/.codex-plugin/plugin.json",
        "plugins/chaos-engine/.claude-plugin/plugin.json",
        "plugins/chaos-engine/hooks/hooks.json",
        "plugins/chaos-engine/hooks/guard.py",
        "plugins/chaos-engine/hooks/kernel.py",
        "plugins/chaos-engine/hooks/launch.js",
        "plugins/chaos-engine/hooks/lifecycle.py",
        "plugins/chaos-engine/hooks/matchers.json",
        "plugins/chaos-engine/hooks/reflection.py",
        "plugins/chaos-engine/skills/chaos-engine/SKILL.md",
        *companion_managed_paths(),
        ".codex/hooks.json",
        ".grok/hooks/lifecycle.json",
        ".github/hooks/chaos-engine.json",
        ".claude/settings.json",
        ".claude/agents/chaos-engine-orchestrator.md",
        ".claude/agents/chaos-engine-implementer.md",
        ".claude/agents/chaos-engine-reviewer.md",
        ".claude/agents/chaos-engine-tester.md",
        ".claude/agents/chaos-engine-mechanical-helper.md",
        ".codex/agents/chaos-engine-orchestrator.toml",
        ".codex/agents/chaos-engine-implementer.toml",
        ".codex/agents/chaos-engine-reviewer.toml",
        ".codex/agents/chaos-engine-tester.toml",
        ".codex/agents/chaos-engine-mechanical-helper.toml",
        "AGENTS.md",
        "CLAUDE.md",
        "GEMINI.md",
        ".github/copilot-instructions.md",
        ".mcp.json",
        ".gemini/settings.json",
        ".codex/config.toml",
        ".memory/config.json",
        ".memory/schema/config.schema.json",
        ".memory/schema/object.schema.json",
        ".memory/schema/relation.schema.json",
        ".memory/schema/event.schema.json",
        ".memory/schema/patch.schema.json",
        ".memory/events.jsonl",
        ".memory/memory/.gitkeep",
        ".memory/relations/.gitkeep",
        "mempalace.yaml",
        ".gitignore",
        ".gitattributes",
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


def directories_for_paths(relatives: object) -> set[str]:
    directories: set[str] = set()
    if not isinstance(relatives, (list, tuple, set, frozenset)):
        return directories
    for relative in relatives:
        if not isinstance(relative, str):
            continue
        current = Path(relative).parent
        while current != Path("."):
            directories.add(current.as_posix())
            current = current.parent
    return directories


def allowed_managed_directories() -> set[str]:
    return directories_for_paths(managed_paths())


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


def legacy_owned_python_server(name: str, platform_name: str) -> dict[str, object]:
    command, prefix = interpreter(platform_name)
    args = {
        "chaosengine-memory": [*prefix, ".chaos-engine/tool.py", "memory-mcp"],
        "chaosengine-mempalace": [
            *prefix,
            ".chaos-engine/tool.py",
            "mempalace-mcp",
            "--palace",
            ".chaos-engine-state/mempalace",
            "--backend",
            "sqlite_exact",
        ],
    }[name]
    server: dict[str, object] = {"command": command, "args": args, "cwd": "."}
    if name == "chaosengine-mempalace":
        server["env"] = dict(MEMPALACE_MCP_ENV)
    return server


def replaceable_owned_server(name: str, existing: object, desired: dict[str, object]) -> bool:
    if existing == desired:
        return True
    if name not in {"chaosengine-memory", "chaosengine-mempalace"}:
        return False
    return existing in (
        legacy_owned_python_server(name, "nt"),
        legacy_owned_python_server(name, "posix"),
    )


def legacy_codex_python_block(platform_name: str) -> str:
    command, prefix = interpreter(platform_name)
    prefix_text = '"-3", ' if prefix else ""
    return (
        "# CHAOSENGINE:START\n"
        f'[mcp_servers."chaosengine-memory"]\ncommand = "{command}"\n'
        f'args = [{prefix_text}".chaos-engine/tool.py", "memory-mcp"]\ncwd = ".."\n\n'
        f'[mcp_servers."chaosengine-mempalace"]\ncommand = "{command}"\n'
        f'args = [{prefix_text}".chaos-engine/tool.py", "mempalace-mcp", "--palace", '
        '".chaos-engine-state/mempalace", "--backend", "sqlite_exact"]\ncwd = ".."\n'
        f"{MEMPALACE_MCP_ENV_TOML}# CHAOSENGINE:END\n"
    )


def json_content(
    before: bytes | None, maven_runtime: tuple[Path, Path] | None = None,
    managed_python: Path | None = None,
) -> bytes:
    try:
        value = json.loads(before.decode("utf-8")) if before is not None else {}
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("invalid host JSON configuration") from error
    if not isinstance(value, dict):
        raise ValueError("invalid host JSON configuration")
    servers = value.setdefault("mcpServers", {})
    if not isinstance(servers, dict):
        raise ValueError("invalid MCP server configuration")
    if servers.get("maven-tools-mcp") == LEGACY_MAVEN_TOOLS_SERVER:
        del servers["maven-tools-mcp"]
    repair_legacy_store_servers(
        servers, maven_runtime=maven_runtime, managed_python=managed_python
    )
    for name, desired in owned_servers(maven_runtime=maven_runtime, managed_python=managed_python).items():
        if name in servers and not replaceable_owned_server(name, servers[name], desired):
            raise ValueError(f"ChaosEngine MCP server collision: {name}")
        servers[name] = desired
    return (json.dumps(value, indent=2, sort_keys=True) + "\n").encode()


def codex_content(
    before: bytes | None,
    platform_name: str | None = None,
    maven_runtime: tuple[Path, Path] | None = None,
    managed_python: Path | None = None,
) -> bytes:
    try:
        existing = before.decode("utf-8") if before is not None else ""
    except UnicodeDecodeError as error:
        raise ValueError("invalid Codex configuration") from error
    legacy_blocks = (
        '[mcp_servers.maven-tools-mcp]\ncommand = "docker"\n'
        'args = ["run", "-i", "--rm", "arvindand/maven-tools-mcp:3.2.0"]\n'
        "required = false\n",
        '[mcp_servers."maven-tools-mcp"]\ncommand = "docker"\n'
        'args = ["run", "-i", "--rm", "arvindand/maven-tools-mcp:3.2.0"]\n'
        "required = false\n",
    )
    for legacy in legacy_blocks:
        for newline_variant in (legacy, legacy.replace("\n", "\r\n")):
            existing = existing.replace(newline_variant, "")
    existing = repair_codex_store_servers(existing)
    del platform_name
    posix_command, _posix_prefix = interpreter("posix")
    windows_command, _windows_prefix = interpreter("nt")
    if managed_python is not None:
        posix_command = windows_command = str(managed_python).replace("\\", "\\\\")
    windows_prefix = "" if managed_python is not None else '"-3", '
    memory_args = '".chaos-engine/tool.py", "memory-mcp"'
    mempalace_args = (
        '".chaos-engine/tool.py", "mempalace-mcp", "--palace", '
        '".chaos-engine-state/mempalace", "--backend", "sqlite_exact"'
    )
    block = (
        "# CHAOSENGINE:START\n"
        f'[mcp_servers."chaosengine-memory"]\ncommand = "{posix_command}"\n'
        f"args = [{memory_args}]\n"
        f'commandWindows = "{windows_command}"\n'
        f'argsWindows = [{windows_prefix}{memory_args}]\n'
        'cwd = "."\n\n'
        f'[mcp_servers."chaosengine-mempalace"]\ncommand = "{posix_command}"\n'
        f"args = [{mempalace_args}]\n"
        f'commandWindows = "{windows_command}"\n'
        f'argsWindows = [{windows_prefix}{mempalace_args}]\n'
        'cwd = "."\n'
        f"{MEMPALACE_MCP_ENV_TOML}# CHAOSENGINE:END\n"
    )
    if maven_runtime is not None:
        java, jar = maven_runtime
        block = block.replace(
            "# CHAOSENGINE:END\n",
            "\n"
            '[mcp_servers."maven-tools-mcp"]\n'
            f"command = {json.dumps(str(java))}\n"
            f'args = ["-jar", {json.dumps(str(jar))}, '
            f'"--spring.profiles.active={MAVEN_TOOLS_MCP_PROFILE}"]\n'
            "# CHAOSENGINE:END\n",
        )
    if "# CHAOSENGINE:START" in existing or "# CHAOSENGINE:END" in existing:
        if block in existing:
            return existing.encode()
        for platform in ("nt", "posix"):
            legacy = legacy_codex_python_block(platform)
            for candidate in (legacy, legacy.replace("\n", "\r\n")):
                if candidate in existing:
                    return existing.replace(candidate, block).encode()
        raise ValueError("ChaosEngine Codex configuration collision")
    for name in owned_servers(maven_runtime=maven_runtime, managed_python=managed_python):
        if f'mcp_servers."{name}"' in existing or f"mcp_servers.{name}" in existing:
            raise ValueError(f"ChaosEngine Codex server collision: {name}")
    separator = "\n" if existing and not existing.endswith("\n") else ""
    return (existing + separator + block).encode()


def hook_content(before: bytes | None, rendered: bytes, label: str) -> bytes:
    try:
        existing = json.loads(before) if before is not None else {"hooks": {}}
        desired = json.loads(rendered)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError(f"invalid {label} hook configuration") from error
    if not isinstance(existing, dict) or not isinstance(existing.get("hooks"), dict):
        raise ValueError(f"invalid {label} hook configuration")
    for event, groups in desired["hooks"].items():
        current = existing["hooks"].setdefault(event, [])
        if not isinstance(current, list):
            raise ValueError(f"invalid {label} hook configuration")
        for group in groups:
            if group not in current:
                current.append(group)
    return (json.dumps(existing, indent=2, sort_keys=True) + "\n").encode()


REQUIRED_HOOK_EVENTS = (
    "SessionStart",
    "UserPromptSubmit",
    "PreToolUse",
    "PostToolUse",
    "PostToolUseFailure",
    "Stop",
    "SubagentStop",
    "SessionEnd",
)
CLAUDE_HOOK_EVENTS = (*REQUIRED_HOOK_EVENTS, "PreCompact")


def _tool_matchers() -> tuple[str, str]:
    policy = json.loads(
        (Path(__file__).resolve().parent / "hooks/matchers.json").read_text(encoding="utf-8")
    )
    preventive = tuple(policy["preventive"])
    observational = tuple(policy["observational"])
    return "|".join(preventive), "|".join(observational)


PRE_TOOL_MATCHER, POST_TOOL_MATCHER = _tool_matchers()


def chaos_guard_locator_command(*, windows: bool, host: str, managed_python: Path | None = None) -> str:
    interpreter = json.dumps(str(managed_python)) if managed_python else ("py -3" if windows else "python3")
    return (
        f'{interpreter} -c "import os,pathlib,runpy;'
        f"os.environ['CHAOS_ENGINE_HOST']='{host}';"
        "cands=('.chaos-engine/hooks/guard.py','plugins/chaos-engine/hooks/guard.py','chaos-engine/hooks/guard.py');"
        "p=next((root/rel for root in (pathlib.Path.cwd(),*pathlib.Path.cwd().parents) "
        "for rel in cands if (root/rel).is_file()),None);"
        "runpy.run_path(str(p),run_name='__main__') if p else print('{}')\""
    )


def lifecycle_hooks_document(host: str, events: dict[str, str] | None = None, managed_python: Path | None = None) -> bytes:
    handler = {
        "type": "command",
        "command": chaos_guard_locator_command(windows=False, host=host, managed_python=managed_python),
        "commandWindows": chaos_guard_locator_command(windows=True, host=host, managed_python=managed_python),
        "timeout": 30,
    }
    defaults = CLAUDE_HOOK_EVENTS if host == "claude" else REQUIRED_HOOK_EVENTS
    selected = events or {event: event for event in defaults}
    hooks = {}
    for native in selected:
        command = dict(handler)
        if native == "SessionEnd" and host in {"codex", "grok"}:
            command["timeout"] = 3
        group = {"hooks": [command]}
        if native == "PreToolUse":
            group["matcher"] = PRE_TOOL_MATCHER
        elif native in {"PostToolUse", "PostToolUseFailure"}:
            group["matcher"] = POST_TOOL_MATCHER
        hooks[native] = [group]
    return (json.dumps({"hooks": hooks}, indent=2, sort_keys=True) + "\n").encode()


def copilot_hooks_document(managed_node: Path | None = None) -> bytes:
    node = json.dumps(str(managed_node)) if managed_node else "node"
    handler = {
        "type": "command",
        "bash": f"{node} .chaos-engine/hooks/launch.js copilot",
        "powershell": f"{node} .chaos-engine/hooks/launch.js copilot",
        "timeoutSec": 30,
    }
    hooks = {
        event: [handler]
        for event in (
            "sessionStart",
            "userPromptSubmitted",
            "preToolUse",
            "postToolUse",
            "postToolUseFailure",
            "agentStop",
            "subagentStop",
            "preCompact",
            "sessionEnd",
        )
    }
    return (json.dumps({"version": 1, "hooks": hooks}, indent=2, sort_keys=True) + "\n").encode()


def gemini_hooks_document(managed_node: Path | None = None) -> bytes:
    node = json.dumps(str(managed_node)) if managed_node else "node"
    handler = {
        "type": "command",
        "command": f"{node} .chaos-engine/hooks/launch.js gemini",
        "name": "ChaosEngine lifecycle",
        "timeout": 30000,
    }
    hooks = {}
    for event in (
            "SessionStart",
            "BeforeAgent",
            "BeforeTool",
            "AfterTool",
            "AfterAgent",
            "PreCompress",
            "SessionEnd",
    ):
        group = {"hooks": [handler]}
        if event == "BeforeTool":
            group["matcher"] = PRE_TOOL_MATCHER
        elif event == "AfterTool":
            group["matcher"] = POST_TOOL_MATCHER
        hooks[event] = [group]
    return (json.dumps({"hooks": hooks}, indent=2, sort_keys=True) + "\n").encode()


def copilot_hook_content(before: bytes | None, managed_node: Path | None = None) -> bytes:
    desired = json.loads(copilot_hooks_document(managed_node))
    if before is None:
        return (json.dumps(desired, indent=2, sort_keys=True) + "\n").encode()
    try:
        existing = json.loads(before)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("invalid Copilot hook configuration") from error
    hooks = existing.get("hooks") if isinstance(existing, dict) else None
    if existing.get("version") != 1 or not isinstance(hooks, dict):
        raise ValueError("ChaosEngine Copilot hook collision")
    for event, entries in list(hooks.items()):
        if not isinstance(entries, list) or not all(isinstance(entry, dict) for entry in entries):
            raise ValueError("ChaosEngine Copilot hook collision")
        hooks[event] = [
            entry
            for entry in entries
            if not any(
                chaos_hook_command(entry.get(field))
                for field in ("bash", "powershell", "command")
            )
        ]
        if not hooks[event]:
            del hooks[event]
    for event, entries in desired["hooks"].items():
        current = hooks.setdefault(event, [])
        for entry in entries:
            if entry not in current:
                current.append(entry)
    return (json.dumps(existing, indent=2, sort_keys=True) + "\n").encode()


def chaos_hook_command(command: object) -> bool:
    if not isinstance(command, str):
        return False
    tokens = re.findall(r'"([^"]*)"|\'([^\']*)\'|(\S+)', command)
    owned_suffixes = (
        "scripts/agents/guard.py",
        ".chaos-engine/hooks/guard.py",
        "plugins/chaos-engine/hooks/guard.py",
        ".chaos-engine/hooks/launch.js",
        "plugins/chaos-engine/hooks/launch.js",
        "${CLAUDE_PLUGIN_ROOT}/hooks/guard.py",
    )
    for token_parts in tokens:
        token = next((part for part in token_parts if part), "").replace("\\", "/")
        token = token.rstrip(";,)")
        for suffix in owned_suffixes:
            found = token.find(suffix)
            if found < 0:
                continue
            after = found + len(suffix)
            if after == len(token) or token[after] in "/\"';, )|":
                return True
    return False


def without_chaos_hooks(before: bytes | None, label: str) -> bytes:
    """Remove only owned command handlers while preserving foreign handlers and metadata."""
    try:
        existing = json.loads(before) if before is not None else {"hooks": {}}
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError(f"invalid {label} hook configuration") from error
    if not isinstance(existing, dict):
        raise ValueError(f"invalid {label} hook configuration")
    hooks = existing.setdefault("hooks", {})
    if not isinstance(hooks, dict):
        raise ValueError(f"invalid {label} hook configuration")
    for event, groups in list(hooks.items()):
        if not isinstance(groups, list):
            raise ValueError(f"invalid {label} hook configuration")
        retained_groups = []
        for group in groups:
            if not isinstance(group, dict) or not isinstance(group.get("hooks"), list):
                retained_groups.append(group)
                continue
            retained_hooks = [
                hook
                for hook in group["hooks"]
                if not (isinstance(hook, dict) and chaos_hook_command(hook.get("command")))
            ]
            if retained_hooks:
                retained_group = dict(group)
                retained_group["hooks"] = retained_hooks
                retained_groups.append(retained_group)
        if retained_groups:
            hooks[event] = retained_groups
        else:
            del hooks[event]
    return (json.dumps(existing, indent=2, sort_keys=True) + "\n").encode()


def replace_owned_text_block(
    existing: str, start: str, end: str, block: str, label: str
) -> bytes:
    """Upgrade one marker-owned block while preserving all foreign text."""
    start_count = existing.count(start)
    end_count = existing.count(end)
    if start_count == end_count == 0:
        separator = "\n" if existing and not existing.endswith("\n") else ""
        return (existing + separator + block).encode()
    if start_count != 1 or end_count != 1:
        raise ValueError(f"ChaosEngine {label} collision")
    begin = existing.index(start)
    finish = existing.index(end, begin) + len(end)
    if finish < begin:
        raise ValueError(f"ChaosEngine {label} collision")
    if finish < len(existing) and existing[finish] == "\r":
        finish += 1
    if finish < len(existing) and existing[finish] == "\n":
        finish += 1
    return (existing[:begin] + block + existing[finish:]).encode()


def gitignore_content(before: bytes | None) -> bytes:
    try:
        existing = before.decode("utf-8") if before is not None else ""
    except UnicodeDecodeError as error:
        raise ValueError("invalid gitignore configuration") from error
    block = (
        f"{GITIGNORE_START}\n"
        ".chaos-engine-runtime/\n.chaos-engine-runtime.lock\n.chaos-engine-runtime.*\n.chaos-engine-state/\n"
        ".chaos-engine-runtime-current.json\n.chaos-engine-runtime-current.json.*\n"
        ".chaos-engine-runtime-generations/\n.chaos-engine-runtime-transactions/\n"
        ".chaos-engine.lock\n.chaos-engine.transaction.json\n"
        ".chaos-engine.backup/\n.chaos-engine.backup.*/\n"
        ".chaos-engine-cross-rollback/\n.chaos-engine-uninstall-*\n"
        ".chaos-engine-hosts.json\n.chaos-engine-hosts.*\n"
        ".chaos-engine-directory-claim-*\ngraphify-out/\n"
        ".memory/*\n!.memory/\n!.memory/config.json\n!.memory/events.jsonl\n"
        "!.memory/schema/\n!.memory/schema/*.schema.json\n"
        "!.memory/memory/\n.memory/memory/*\n!.memory/memory/.gitkeep\n"
        "!.memory/relations/\n.memory/relations/*\n!.memory/relations/.gitkeep\n"
        "!.chaos-engine/\n!.chaos-engine/**\n.chaos-engine/**/__pycache__/\n"
        "!.agents/\n!.agents/plugins/\n!.agents/plugins/marketplace.json\n"
        "!.agents/skills/\n!.agents/skills/README.md\n"
        "!.agents/skills/chaos-engine/\n!.agents/skills/chaos-engine/**\n"
        "!.claude/\n!.claude/**\n.claude/settings.local.json\n!.codex/\n!.codex/**\n"
        "!.grok/\n!.grok/hooks/\n!.grok/hooks/*.json\n"
        "!.gemini/\n!.gemini/settings.json\n!.gemini/skills/\n"
        "!.gemini/skills/chaos-engine/\n!.gemini/skills/chaos-engine/**\n"
        "!.github/\n!.github/copilot-instructions.md\n!.github/hooks/\n"
        "!.github/hooks/chaos-engine.json\n!.github/skills/\n"
        "!.github/skills/chaos-engine/\n!.github/skills/chaos-engine/**\n"
        "!plugins/\n!plugins/chaos-engine/\n!plugins/chaos-engine/**\n"
        "!plugins/caveman/\n!plugins/caveman/**\n"
        "!plugins/ponytail/\n!plugins/ponytail/**\n"
        "!.mcp.json\n!mempalace.yaml\n!AGENTS.md\n!CLAUDE.md\n!GEMINI.md\n!.gitattributes\n"
        ".chaos-engine-owned-directory\n"
        f"{GITIGNORE_END}\n"
    )
    return replace_owned_text_block(
        existing, GITIGNORE_START, GITIGNORE_END, block, "gitignore"
    )


def gitattributes_content(before: bytes | None) -> bytes:
    try:
        existing = before.decode("utf-8") if before is not None else ""
    except UnicodeDecodeError as error:
        raise ValueError("invalid gitattributes configuration") from error
    repository_root_anchor = "/"
    block = (
        f"{GITATTRIBUTES_START}\n"
        f"{repository_root_anchor}.agents/** text eol=lf\n"
        f"{repository_root_anchor}.chaos-engine/** text eol=lf\n"
        f"{repository_root_anchor}.claude-plugin/** text eol=lf\n"
        f"{repository_root_anchor}.claude/** text eol=lf\n"
        f"{repository_root_anchor}.codex/** text eol=lf\n"
        f"{repository_root_anchor}.grok/hooks/** text eol=lf\n"
        f"{repository_root_anchor}.gemini/** text eol=lf\n"
        f"{repository_root_anchor}.github/hooks/** text eol=lf\n"
        f"{repository_root_anchor}.github/copilot-instructions.md text eol=lf\n"
        f"{repository_root_anchor}.github/skills/chaos-engine/** text eol=lf\n"
        f"{repository_root_anchor}.mcp.json text eol=lf\n"
        f"{repository_root_anchor}.memory/** text eol=lf\n"
        f"{repository_root_anchor}plugins/chaos-engine/** text eol=lf\n"
        f"{repository_root_anchor}plugins/caveman/** text eol=lf\n"
        f"{repository_root_anchor}plugins/ponytail/** text eol=lf\n"
        f"{repository_root_anchor}AGENTS.md text eol=lf\n"
        f"{repository_root_anchor}CLAUDE.md text eol=lf\n"
        f"{repository_root_anchor}GEMINI.md text eol=lf\n"
        f"{repository_root_anchor}mempalace.yaml text eol=lf\n"
        f"{repository_root_anchor}.gitignore text eol=lf\n"
        f"{repository_root_anchor}.gitattributes text eol=lf\n"
        f"{GITATTRIBUTES_END}\n"
    )
    return replace_owned_text_block(
        existing, GITATTRIBUTES_START, GITATTRIBUTES_END, block, "gitattributes"
    )


def desired_content(
    before: dict[str, bytes | None],
    maven_runtime: tuple[Path, Path] | None | bool = False,
    project_name: str = "project",
    plugin_version: str = "1.0.0",
    dependency_runtime: Path | None = None,
) -> dict[str, bytes]:
    if maven_runtime is False:
        maven_runtime = discover_maven_tools_runtime()
    managed_python = None
    managed_node = None
    if dependency_runtime is not None:
        scripts = "Scripts" if os.name == "nt" else "bin"
        managed_python = dependency_runtime / "uv-tools/mempalace" / scripts / ("python.exe" if os.name == "nt" else "python")
        managed_node = dependency_runtime / ("node/node.exe" if os.name == "nt" else "node/bin/node")
    adapters = managed_paths()[:4]
    skill = (
        "---\nname: chaos-engine\ndescription: Load the canonical installed ChaosEngine before every task.\n---\n\n"
        "Follow the [canonical ChaosEngine](../../../.chaos-engine/skills/chaos-engine/SKILL.md).\n"
    ).encode()
    after = {relative: skill for relative in adapters}
    after[".agents/skills/README.md"] = (
        "# Installed agent harness\n\n"
        "- `chaos-engine/`: canonical skill adapter.\n"
        "- `../../plugins/chaos-engine/`: installed plugin and lifecycle hook.\n"
        "- `../../plugins/caveman/`: pinned Caveman skill and hooks.\n"
        "- `../../plugins/ponytail/`: pinned Ponytail skill and hooks.\n"
        "- `.chaos-engine/`: canonical skills, playbooks, tools, and policy.\n"
    ).encode()
    plugin_entry = {
        "name": "chaos-engine",
        "source": {"source": "local", "path": "./plugins/chaos-engine"},
        "policy": {
            "installation": "INSTALLED_BY_DEFAULT",
            "authentication": "ON_INSTALL",
        },
        "category": "Developer Tools",
    }
    marketplace_before = before[".agents/plugins/marketplace.json"]
    if marketplace_before is None:
        marketplace = {
            "name": "chaos-engine-project",
            "interface": {"displayName": "ChaosEngine Project"},
            "plugins": [],
        }
    else:
        try:
            marketplace = json.loads(marketplace_before)
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise ValueError("invalid plugin marketplace configuration") from error
        if not isinstance(marketplace, dict) or not isinstance(marketplace.get("plugins"), list):
            raise ValueError("invalid plugin marketplace configuration")
    existing_plugin = next(
        (item for item in marketplace["plugins"] if isinstance(item, dict) and item.get("name") == "chaos-engine"),
        None,
    )
    if existing_plugin is not None and existing_plugin != plugin_entry:
        raise ValueError("ChaosEngine plugin marketplace collision")
    if existing_plugin is None:
        marketplace["plugins"].append(plugin_entry)
    caveman_entry = {
        "name": CAVEMAN_PLUGIN_NAME,
        "source": {"source": "local", "path": "./plugins/caveman"},
        "policy": {
            "installation": "INSTALLED_BY_DEFAULT",
            "authentication": "ON_INSTALL",
        },
        "category": "Productivity",
    }
    existing_caveman = next(
        (
            item
            for item in marketplace["plugins"]
            if isinstance(item, dict) and item.get("name") == CAVEMAN_PLUGIN_NAME
        ),
        None,
    )
    if existing_caveman is not None and existing_caveman != caveman_entry:
        raise ValueError("Caveman plugin marketplace collision")
    if existing_caveman is None:
        marketplace["plugins"].append(caveman_entry)
    ponytail_entry = {
        "name": PONYTAIL_PLUGIN_NAME,
        "source": {"source": "local", "path": "./plugins/ponytail"},
        "policy": {
            "installation": "INSTALLED_BY_DEFAULT",
            "authentication": "ON_INSTALL",
        },
        "category": "Productivity",
    }
    existing_ponytail = next(
        (
            item
            for item in marketplace["plugins"]
            if isinstance(item, dict) and item.get("name") == PONYTAIL_PLUGIN_NAME
        ),
        None,
    )
    if existing_ponytail is not None and existing_ponytail != ponytail_entry:
        raise ValueError("Ponytail plugin marketplace collision")
    if existing_ponytail is None:
        marketplace["plugins"].append(ponytail_entry)
    after[".agents/plugins/marketplace.json"] = (
        json.dumps(marketplace, indent=2, sort_keys=True) + "\n"
    ).encode()
    claude_plugin_entry = {
        "name": "chaos-engine",
        "source": "./plugins/chaos-engine",
        "description": "Neutral project-local agent harness.",
        "version": plugin_version,
    }
    claude_marketplace_before = before[".claude-plugin/marketplace.json"]
    if claude_marketplace_before is None:
        claude_marketplace = {
            "name": "chaos-engine-project",
            "owner": {"name": "ChaosEngine contributors"},
            "description": "Neutral project-local agent harness.",
            "plugins": [],
        }
    else:
        try:
            claude_marketplace = json.loads(claude_marketplace_before)
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise ValueError("invalid Claude marketplace configuration") from error
        if (
            not isinstance(claude_marketplace, dict)
            or not isinstance(claude_marketplace.get("name"), str)
            or not claude_marketplace["name"].strip()
            or not isinstance(claude_marketplace.get("plugins"), list)
            or any(
                not isinstance(item, dict) or not isinstance(item.get("name"), str)
                for item in claude_marketplace.get("plugins", [])
            )
        ):
            raise ValueError("invalid Claude marketplace configuration")
    existing_claude_plugin = next(
        (
            item
            for item in claude_marketplace["plugins"]
            if isinstance(item, dict) and item.get("name") == "chaos-engine"
        ),
        None,
    )
    if existing_claude_plugin is not None and existing_claude_plugin != claude_plugin_entry:
        raise ValueError("ChaosEngine Claude marketplace collision")
    if existing_claude_plugin is None:
        claude_marketplace["plugins"].append(claude_plugin_entry)
    caveman_claude_entry = {
        "name": CAVEMAN_PLUGIN_NAME,
        "source": "./plugins/caveman",
        "description": "Ultra-compressed communication mode.",
        "version": CAVEMAN_PLUGIN_VERSION,
    }
    existing_caveman = next(
        (
            item
            for item in claude_marketplace["plugins"]
            if isinstance(item, dict) and item.get("name") == CAVEMAN_PLUGIN_NAME
        ),
        None,
    )
    if existing_caveman is not None and existing_caveman != caveman_claude_entry:
        raise ValueError("Caveman Claude plugin collision")
    if existing_caveman is None:
        claude_marketplace["plugins"].append(caveman_claude_entry)
    ponytail_claude_entry = {
        "name": PONYTAIL_PLUGIN_NAME,
        "source": "./plugins/ponytail",
        "description": "Laziest solution that actually works.",
        "version": PONYTAIL_PLUGIN_VERSION,
    }
    existing_ponytail = next(
        (
            item
            for item in claude_marketplace["plugins"]
            if isinstance(item, dict) and item.get("name") == PONYTAIL_PLUGIN_NAME
        ),
        None,
    )
    if existing_ponytail is not None and existing_ponytail != ponytail_claude_entry:
        raise ValueError("Ponytail Claude plugin collision")
    if existing_ponytail is None:
        claude_marketplace["plugins"].append(ponytail_claude_entry)
    after[".claude-plugin/marketplace.json"] = (
        json.dumps(claude_marketplace, indent=2, sort_keys=True) + "\n"
    ).encode()
    plugin_manifest = {
        "name": "chaos-engine",
        "version": plugin_version,
        "description": "Neutral project-local software agent working harness.",
        "author": {"name": "ChaosEngine contributors"},
        "skills": "./skills",
        "interface": {
            "displayName": "ChaosEngine",
            "shortDescription": "Neutral project working harness",
            "longDescription": "A neutral project-local harness for research, planning, implementation, verification, and durable learning.",
            "developerName": "ChaosEngine contributors",
            "category": "Developer Tools",
            "capabilities": ["Instructions", "MCP servers"],
            "defaultPrompt": ["Use ChaosEngine for this task."],
        },
    }
    after["plugins/chaos-engine/.codex-plugin/plugin.json"] = (
        json.dumps(plugin_manifest, indent=2, sort_keys=True) + "\n"
    ).encode()
    after["plugins/chaos-engine/.claude-plugin/plugin.json"] = (
        json.dumps(
            {
                "name": "chaos-engine",
                "version": plugin_version,
                "description": "Neutral project-local software agent working harness.",
                "author": {"name": "ChaosEngine contributors"},
            },
            indent=2,
            sort_keys=True,
        )
        + "\n"
    ).encode()
    desired_hooks = lifecycle_hooks_document("codex", managed_python=managed_python)
    after["plugins/chaos-engine/hooks/hooks.json"] = (
        json.dumps({"hooks": {}}, indent=2, sort_keys=True) + "\n"
    ).encode()
    after[".codex/hooks.json"] = hook_content(
        without_chaos_hooks(before[".codex/hooks.json"], "Codex"),
        desired_hooks,
        "Codex",
    )
    after[".grok/hooks/lifecycle.json"] = hook_content(
        without_chaos_hooks(before[".grok/hooks/lifecycle.json"], "Grok"),
        lifecycle_hooks_document("grok", managed_python=managed_python),
        "Grok",
    )
    after[".github/hooks/chaos-engine.json"] = copilot_hook_content(
        before[".github/hooks/chaos-engine.json"], managed_node
    )
    after["plugins/chaos-engine/hooks/guard.py"] = (
        Path(__file__).resolve().parent / "hooks/guard.py"
    ).read_bytes()
    after["plugins/chaos-engine/hooks/kernel.py"] = (
        Path(__file__).resolve().parent / "hooks/kernel.py"
    ).read_bytes()
    after["plugins/chaos-engine/hooks/launch.js"] = (
        Path(__file__).resolve().parent / "hooks/launch.js"
    ).read_bytes()
    after["plugins/chaos-engine/hooks/lifecycle.py"] = (
        Path(__file__).resolve().parent / "hooks/lifecycle.py"
    ).read_bytes()
    after["plugins/chaos-engine/hooks/matchers.json"] = (
        Path(__file__).resolve().parent / "hooks/matchers.json"
    ).read_bytes()
    after["plugins/chaos-engine/hooks/reflection.py"] = (
        Path(__file__).resolve().parent / "hooks/reflection.py"
    ).read_bytes()
    after["plugins/chaos-engine/skills/chaos-engine/SKILL.md"] = (
        "---\nname: chaos-engine\ndescription: Load the canonical installed ChaosEngine before every task.\n---\n\n"
        "From the active project root, load `.chaos-engine/skills/chaos-engine/SKILL.md` before every task.\n"
        "That router decides whether to load the bundled Caveman and Ponytail companions.\n"
    ).encode()
    claude_settings = hook_content(
        without_chaos_hooks(before[".claude/settings.json"], "Claude"),
        lifecycle_hooks_document("claude", managed_python=managed_python),
        "Claude",
    )
    try:
        settings = json.loads(claude_settings) if claude_settings is not None else {}
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("invalid Claude settings") from error
    if not isinstance(settings, dict):
        raise ValueError("invalid Claude settings")
    enabled = settings.setdefault("enabledPlugins", {})
    marketplaces = settings.setdefault("extraKnownMarketplaces", {})
    if not isinstance(enabled, dict) or not isinstance(marketplaces, dict):
        raise ValueError("invalid Claude settings")
    claude_marketplace_name = claude_marketplace["name"]
    plugin_id = f"chaos-engine@{claude_marketplace_name}"
    if plugin_id in enabled and enabled[plugin_id] is not True:
        raise ValueError("ChaosEngine Claude plugin collision")
    desired_marketplace = {
        "source": {"source": "directory", "path": "."}
    }
    if claude_marketplace_name in marketplaces and marketplaces[claude_marketplace_name] != desired_marketplace:
        raise ValueError("ChaosEngine Claude marketplace collision")
    enabled[plugin_id] = True
    enabled[f"caveman@{claude_marketplace_name}"] = True
    enabled[f"ponytail@{claude_marketplace_name}"] = True
    marketplaces[claude_marketplace_name] = desired_marketplace
    after[".claude/settings.json"] = (
        json.dumps(settings, indent=2, sort_keys=True) + "\n"
    ).encode()
    caveman_manifest = {
        "name": CAVEMAN_PLUGIN_NAME,
        "version": CAVEMAN_PLUGIN_VERSION,
        "description": "Ultra-compressed communication mode. Cut filler. Keep technical accuracy.",
        "author": {
            "name": "Julius Brussee",
            "url": "https://github.com/JuliusBrussee",
        },
        "homepage": "https://github.com/JuliusBrussee/caveman",
        "repository": "https://github.com/JuliusBrussee/caveman",
        "license": "MIT",
        "skills": "./skills/",
        "interface": {
            "displayName": "Caveman",
            "shortDescription": "Talk like caveman. Cut filler. Keep technical accuracy.",
            "longDescription": "Ultra-compressed communication mode for coding agents.",
            "developerName": "Julius Brussee",
            "category": "Productivity",
            "capabilities": ["Write"],
            "websiteURL": "https://github.com/JuliusBrussee/caveman",
            "defaultPrompt": ["Use caveman mode. Cut filler. Keep technical accuracy."],
        },
    }
    after["plugins/caveman/.codex-plugin/plugin.json"] = (
        json.dumps(caveman_manifest, indent=2, sort_keys=True) + "\n"
    ).encode()
    after["plugins/caveman/.claude-plugin/plugin.json"] = (
        json.dumps(
            {
                "name": CAVEMAN_PLUGIN_NAME,
                "version": CAVEMAN_PLUGIN_VERSION,
                "description": caveman_manifest["description"],
                "author": caveman_manifest["author"],
                "hooks": {
                    "UserPromptSubmit": [
                        {
                            "hooks": [
                                {
                                    "type": "command",
                                    "command": 'node "${CLAUDE_PLUGIN_ROOT}/src/hooks/caveman-mode-tracker.js"',
                                    "timeout": 5,
                                    "statusMessage": "Tracking caveman mode...",
                                }
                            ]
                        }
                    ],
                },
            },
            indent=2,
            sort_keys=True,
        )
        + "\n"
    ).encode()
    publish_vendor_plugin(
        after,
        name=CAVEMAN_PLUGIN_NAME,
        vendor="caveman",
        repository="JuliusBrussee/caveman",
        commit=CAVEMAN_UPSTREAM_COMMIT,
        version=CAVEMAN_PLUGIN_VERSION,
    )
    ponytail_manifest = {
        "name": PONYTAIL_PLUGIN_NAME,
        "version": PONYTAIL_PLUGIN_VERSION,
        "description": "Forces the laziest solution that actually works.",
        "author": {
            "name": "DietrichGebert",
            "url": "https://github.com/DietrichGebert",
        },
        "homepage": "https://github.com/DietrichGebert/ponytail",
        "repository": "https://github.com/DietrichGebert/ponytail",
        "license": "MIT",
        "skills": "./skills/",
    }
    after["plugins/ponytail/.codex-plugin/plugin.json"] = (
        json.dumps(ponytail_manifest, indent=2, sort_keys=True) + "\n"
    ).encode()
    ponytail_hooks = json.loads(
        (
            Path(__file__).resolve().parent / "vendor/ponytail/hooks/claude-codex-hooks.json"
        ).read_text(encoding="utf-8")
    )
    published_ponytail_hooks = dict(ponytail_hooks.get("hooks", ponytail_hooks))
    published_ponytail_hooks.pop("SessionStart", None)
    after["plugins/ponytail/.claude-plugin/plugin.json"] = (
        json.dumps(
            {
                "name": PONYTAIL_PLUGIN_NAME,
                "version": PONYTAIL_PLUGIN_VERSION,
                "description": ponytail_manifest["description"],
                "author": ponytail_manifest["author"],
                "hooks": published_ponytail_hooks,
            },
            indent=2,
            sort_keys=True,
        )
        + "\n"
    ).encode()
    publish_vendor_plugin(
        after,
        name=PONYTAIL_PLUGIN_NAME,
        vendor="ponytail",
        repository="DietrichGebert/ponytail",
        commit=PONYTAIL_UPSTREAM_COMMIT,
        version=PONYTAIL_PLUGIN_VERSION,
    )
    roles = {
        "orchestrator": "Own planning, architecture, synthesis, and final verification.",
        "implementer": "Implement one bounded specification before consolidated validation.",
        "reviewer": "Perform an independent read-only adversarial review; never edit.",
        "tester": "Reproduce behavior and produce regression and acceptance evidence.",
        "mechanical-helper": "Perform deterministic reversible spec-exact work; stop on ambiguity.",
    }
    for role, responsibility in roles.items():
        slug = f"chaos-engine-{role}"
        tools = "Read, Grep, Glob, Bash" if role == "reviewer" else "Read, Grep, Glob, Bash, Write, Edit"
        after[f".claude/agents/{slug}.md"] = (
            f"---\nname: {slug}\ndescription: {responsibility}\n"
            f"tools: {tools}\n---\n\n"
            f"Load `.chaos-engine/skills/chaos-engine/SKILL.md` and follow "
            f"`.chaos-engine/references/roles.md#{role}`. {responsibility}\n"
        ).encode()
        sandbox = 'sandbox_mode = "read-only"\n' if role == "reviewer" else ""
        after[f".codex/agents/{slug}.toml"] = (
            f'name = "{slug}"\n'
            f'description = {json.dumps(responsibility)}\n'
            f'developer_instructions = {json.dumps(f"Load .chaos-engine/skills/chaos-engine/SKILL.md and follow .chaos-engine/references/roles.md#{role}. {responsibility}")}\n'
            f"{sandbox}"
        ).encode()
    memory_before = before[".memory/config.json"]
    if memory_before is None:
        normalized_name = re.sub(r"[^a-z0-9]+", "-", project_name.casefold()).strip("-") or "project"
        memory_config = {
            "version": 5,
            "project": {"id": f"project.{normalized_name}", "name": project_name},
            "memory": {
                "autoIndex": True,
                "defaultTokenBudget": 6000,
            },
        }
        after[".memory/config.json"] = (
            json.dumps(memory_config, indent=2, sort_keys=True) + "\n"
        ).encode()
    else:
        after[".memory/config.json"] = migrate_memory_config(memory_before)
    schema_assets = memory_schema_assets()
    for name in MEMORY_SCHEMA_FILES:
        relative = f".memory/schema/{name}"
        after[relative] = (schema_assets / name).read_bytes()
    events = before[".memory/events.jsonl"]
    if events is None:
        after[".memory/events.jsonl"] = b""
    else:
        try:
            for line in events.decode("utf-8").splitlines():
                if line.strip() and not isinstance(json.loads(line), dict):
                    raise ValueError("invalid Memory storage")
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise ValueError("invalid Memory storage") from error
        after[".memory/events.jsonl"] = events
    for relative in (".memory/memory/.gitkeep", ".memory/relations/.gitkeep"):
        existing = before[relative]
        if existing not in (None, b""):
            raise ValueError("invalid Memory storage")
        after[relative] = b""
    mempalace_before = before["mempalace.yaml"]
    if mempalace_before is None:
        after["mempalace.yaml"] = (
            f"wing: {default_mempalace_wing(project_name)}\n"
            "rooms:\n  - name: general\n    description: Project source and documentation\n"
            "    keywords: [project, source, documentation]\n"
            "exclude_patterns:\n  - mempalace.yaml\n  - .memory/**\n"
            "  - graphify-out/**\n  - .chaos-engine-runtime/**\n  - .chaos-engine-state/**\n"
        ).encode()
    else:
        validate_mempalace_config(mempalace_before)
        after["mempalace.yaml"] = mempalace_before
    after[".gitignore"] = gitignore_content(before[".gitignore"])
    for relative in ("AGENTS.md", "CLAUDE.md", "GEMINI.md"):
        after[relative] = instruction_content(before[relative], INSTRUCTION)
    after[".github/copilot-instructions.md"] = instruction_content(
        before[".github/copilot-instructions.md"],
        INSTRUCTION.replace(".chaos-engine/", "../.chaos-engine/"),
    )
    after[".mcp.json"] = json_content(before[".mcp.json"], maven_runtime, managed_python)
    gemini_settings = json_content(before[".gemini/settings.json"], maven_runtime, managed_python)
    after[".gemini/settings.json"] = hook_content(
        without_chaos_hooks(gemini_settings, "Gemini"),
        gemini_hooks_document(managed_node),
        "Gemini",
    )
    after[".codex/config.toml"] = codex_content(
        before[".codex/config.toml"], maven_runtime=maven_runtime, managed_python=managed_python
    )
    after[".gitattributes"] = gitattributes_content(before[".gitattributes"])
    return after


def current_images(project: Path) -> dict[str, bytes | None]:
    return {relative: read_file(project, project / relative) for relative in managed_paths()}


def encode_images(images: dict[str, bytes | None]) -> dict[str, str | None]:
    return {
        relative: None if content is None else base64.b64encode(content).decode("ascii")
        for relative, content in images.items()
    }


def receipt_image_key(relative: object) -> str:
    if not isinstance(relative, str) or not relative or relative.startswith(("/", "\\")):
        raise ValueError("ChaosEngine host receipt contains an unsafe receipt path")
    if "\\" in relative or ":" in relative:
        raise ValueError("ChaosEngine host receipt contains an unsafe receipt path")
    parts = PurePosixPath(relative).parts
    if not parts or any(part in {"", ".", ".."} for part in parts):
        raise ValueError("ChaosEngine host receipt contains an unsafe receipt path")
    return relative


def decode_images(value: object, *, nullable: bool) -> dict[str, bytes | None]:
    if not isinstance(value, dict):
        raise ValueError("ChaosEngine host receipt ownership is invalid")
    keys = frozenset(value)
    if not frozenset(LEGACY_MANAGED_PATHS) <= keys:
        raise ValueError("ChaosEngine host receipt is missing required adapter paths")
    for relative in keys:
        receipt_image_key(relative)
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
    for relative in managed_paths():
        result.setdefault(relative, None)
    return result


def receipt_directories(receipt: dict[str, object]) -> list[str]:
    value = receipt.get("createdDirectories")
    if not isinstance(value, list) or any(not isinstance(item, str) for item in value):
        raise ValueError("ChaosEngine host receipt directory ownership is invalid")
    recorded: set[str] = set()
    for key in ("before", "after"):
        images = receipt.get(key)
        if isinstance(images, dict):
            recorded.update(str(item) for item in images)
    allowed = allowed_managed_directories() | directories_for_paths(recorded)
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
                    # A concurrent publisher now owns the destination; preserve it.
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
    capability_digest = value.get("capabilityPolicySha256")
    if capability_digest is not None and (
        not isinstance(capability_digest, str)
        or re.fullmatch(r"[0-9a-f]{64}", capability_digest) is None
    ):
        raise ValueError("ChaosEngine host receipt capability policy is invalid")
    if value.get("hosts") != host_routes():
        raise ValueError("ChaosEngine host receipt routes are invalid")
    decode_images(value.get("before"), nullable=True)
    decode_images(value.get("after"), nullable=False)
    before_value = value.get("before")
    after_value = value.get("after")
    if isinstance(before_value, dict) and isinstance(after_value, dict):
        missing = set(managed_paths()) - set(after_value)
        if missing:
            current = current_images(project)
            for relative in missing:
                if current[relative] is not None:
                    encoded = base64.b64encode(current[relative]).decode("ascii")
                    before_value[relative] = encoded
                    after_value[relative] = encoded
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


def install(
    project: Path,
    core_commit: str | None = None,
    capability_policy_digest: str | None = None,
    dependency_runtime: Path | None = None,
) -> dict[str, object]:
    project = project.resolve()
    if capability_policy_digest is not None and re.fullmatch(r"[0-9a-f]{64}", capability_policy_digest) is None:
        raise ValueError("ChaosEngine capability policy digest is invalid")
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
            desired_capability_digest = capability_policy_digest or receipt.get("capabilityPolicySha256")
            version = plugin_cache_version(core_commit)
            wanted = desired_content(
                before,
                project_name=project_identity_name(project),
                plugin_version=version,
                dependency_runtime=dependency_runtime,
            )
            current = current_images(project)
            for relative in managed_paths():
                if current[relative] not in (after[relative], wanted[relative]):
                    raise ValueError(
                        f"ChaosEngine host adapter drift detected: {project / relative}"
                    )
            if (
                after == wanted
                and receipt.get("coreCommit") == core_commit
                and receipt.get("capabilityPolicySha256") == desired_capability_digest
            ):
                return receipt
            next_receipt = dict(receipt)
            next_receipt["phase"] = "installing"
            next_receipt["coreCommit"] = core_commit
            if desired_capability_digest is not None:
                next_receipt["capabilityPolicySha256"] = desired_capability_digest
            next_receipt["after"] = encode_images(wanted)
            new_directories = created_directories(project)
            next_receipt["createdDirectories"] = sorted(
                set(receipt_directories(receipt)) | set(new_directories),
                key=lambda item: (len(Path(item).parts), item),
            )
            next_raw = write_receipt(project, next_receipt, raw)
            try:
                prepare_created_directories(project, next_receipt)
                reconcile(project, wanted, (after, wanted))
                next_receipt["phase"] = "installed"
                write_receipt(project, next_receipt, next_raw)
                return next_receipt
            except BaseException:
                reconcile(project, after, (after, wanted))
                if new_directories:
                    cleanup_receipt = dict(next_receipt)
                    cleanup_receipt["createdDirectories"] = new_directories
                    remove_created_directories(project, cleanup_receipt)
                atomic_write(project, receipt_path, raw, read_file(project, receipt_path))
                raise
        prepare_created_directories(project, receipt)
        reconcile(project, after, (before, after))
        receipt["phase"] = "installed"
        write_receipt(project, receipt, raw)
        return receipt

    before = current_images(project)
    version = plugin_cache_version(core_commit)
    after = desired_content(
        before,
        project_name=project_identity_name(project),
        plugin_version=version,
        dependency_runtime=dependency_runtime,
    )
    if existing_anchors and existing_anchors[0].name.startswith(REMOVING_ANCHOR_PREFIX):
        raise ValueError("ChaosEngine host removal recovery is required")
    anchor_path = host_anchor_path(project, create=True)
    receipt: dict[str, object] = {
        "schemaVersion": SCHEMA_VERSION,
        "phase": "installing",
        "hosts": host_routes(),
        "coreCommit": core_commit,
        **({"capabilityPolicySha256": capability_policy_digest} if capability_policy_digest else {}),
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


def grok_runtime_status(
    project: Path, *, executable: str | None = None, runner=None
) -> dict[str, str]:
    """Verify detected Grok project trust and loaded lifecycle hooks without mutation."""
    command = executable or shutil.which("grok")
    if not command:
        return {"status": "not-detected"}
    run = subprocess.run if runner is None else runner
    try:
        completed = run(
            [command, "inspect", "--json"],
            cwd=project.resolve(),
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
        payload = json.loads(completed.stdout or "{}") if completed.returncode == 0 else {}
    except (OSError, subprocess.SubprocessError, ValueError):
        payload = {}
    recovery = (
        "Run `grok inspect --json` from the project. If projectTrusted is false, "
        "review the project then run `/hooks-trust`; reload hooks and rerun doctor."
    )
    if not isinstance(payload, dict) or payload.get("projectTrusted") is not True:
        return {"status": "recovery-required", "detail": recovery}
    hooks = payload.get("hooks")
    loaded = {
        str(item.get("event"))
        for item in hooks if isinstance(item, dict)
        if "guard.py" in str(item.get("target") or "")
    } if isinstance(hooks, list) else set()
    required = {
        "session_start", "user_prompt_submit", "pre_tool_use", "post_tool_use",
        "post_tool_use_failure", "stop", "subagent_stop", "session_end",
    }
    if not required.issubset(loaded):
        return {"status": "recovery-required", "detail": recovery}
    return {"status": "healthy"}


def snapshot(project: Path) -> dict[str, object]:
    project = project.resolve()
    receipt, raw = read_receipt(project)
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


def prepare_uninstall(
    project: Path,
    *,
    runner=subprocess.run,
    which=shutil.which,
) -> dict[str, object]:
    project = project.resolve()
    receipt, raw = read_receipt(project)
    before = decode_images(receipt["before"], nullable=True)
    after = decode_images(receipt["after"], nullable=False)
    if receipt["phase"] == "installed":
        verify(project, receipt)
        activation = receipt.get("clientActivation")
        removed_activation = False
        try:
            if isinstance(activation, dict):
                clients = activation.get("ownedClients")
                if not isinstance(clients, list):
                    raise ValueError("ChaosEngine client activation receipt is invalid")
                remove_client_activation(project, clients, runner=runner, which=which)
                removed_activation = True
                restore_claude_local_before(project, activation)
            receipt["clientActivationRemoved"] = removed_activation
            receipt["phase"] = "removing"
            write_receipt(project, receipt, raw)
        except BaseException:
            if removed_activation and isinstance(activation, dict):
                restore_client_activation(project, activation, runner=runner, which=which)
            raise
    elif receipt["phase"] != "removing":
        raise ValueError("ChaosEngine host installation recovery is required")
    try:
        reconcile(project, before, (before, after))
    except BaseException:
        cancel_uninstall(project, runner=runner, which=which)
        raise
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


def cancel_uninstall(
    project: Path,
    *,
    runner=subprocess.run,
    which=shutil.which,
) -> None:
    project = project.resolve()
    receipt, raw = read_receipt(project)
    if receipt["phase"] != "removing":
        raise ValueError("ChaosEngine host removal is not prepared")
    before = decode_images(receipt["before"], nullable=True)
    after = decode_images(receipt["after"], nullable=False)
    prepare_created_directories(project, receipt)
    reconcile(project, after, (before, after))
    activation = receipt.get("clientActivation")
    if receipt.get("clientActivationRemoved") is True and isinstance(activation, dict):
        restore_client_activation(project, activation, runner=runner, which=which)
    receipt["clientActivationRemoved"] = False
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
    observed = {relative: read_file(project, project / relative) for relative in before}
    if observed != before:
        raise ValueError("ChaosEngine host removal state drift detected")
    remove_created_directories(project, receipt)
    anchor = host_anchor_path(project)
    if anchor.name.startswith(ACTIVE_ANCHOR_PREFIX):
        anchor = move_anchor(project, anchor, REMOVING_ANCHOR_PREFIX)
    activation = receipt.get("clientActivation")
    activation_root = activation_bundle_root(activation) if isinstance(activation, dict) else None
    if activation_root is not None and activation_root.exists():
        if is_link_or_reparse(activation_root) or not activation_root.is_dir():
            raise ValueError("ChaosEngine activation marketplace collision")
        activation_plugins_from_root(activation_root, str(activation["marketplaceName"]))
        shutil.rmtree(activation_root)
    receipt_path.unlink()
    anchor.unlink()


def uninstall(
    project: Path,
    *,
    runner=subprocess.run,
    which=shutil.which,
) -> None:
    project = project.resolve()
    receipt_path = project / RECEIPT_NAME
    if read_file(project, receipt_path) is None:
        finalize_uninstall(project)
        return
    prepare_uninstall(project, runner=runner, which=which)
    try:
        finalize_uninstall(project)
    except BaseException:
        if read_file(project, receipt_path) is not None:
            cancel_uninstall(project, runner=runner, which=which)
        raise
