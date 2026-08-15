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
import shutil
import stat
import subprocess  # nosec B404 - probes a resolved local Java executable.
import sys
from pathlib import Path


RECEIPT_NAME = ".chaos-engine-hosts.json"
ANCHOR_NAME = ".chaos-engine-hosts.anchor"
ACTIVE_ANCHOR_PREFIX = ".chaos-engine-hosts.active-"
REMOVING_ANCHOR_PREFIX = ".chaos-engine-hosts.removing-"
ANCHOR_TOKEN = re.compile(r"^[0-9a-f]{64}$")
SCHEMA_VERSION = 1
PLUGIN_NAME = "chaos-engine"
MEMORY_SCHEMA_FILES = (
    "config.schema.json",
    "object.schema.json",
    "relation.schema.json",
    "event.schema.json",
    "patch.schema.json",
)
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


def project_identity_name(project: Path) -> str:
    """Return the repository identity, independent of a checkout/worktree folder name."""
    result = subprocess.run(  # nosec B603 B607 - fixed git query, no shell.
        ["git", "-C", str(project), "config", "--get", "remote.origin.url"],
        capture_output=True,
        text=True,
        check=False,
        timeout=5,
    )
    if result.returncode == 0:
        remote = result.stdout.strip().rstrip("/\\")
        candidate = re.split(r"[/\\:]", remote)[-1]
        if candidate.casefold().endswith(".git"):
            candidate = candidate[:-4]
        if candidate:
            return candidate
    return project.name


def validate_memory_config(content: bytes) -> None:
    try:
        config = json.loads(content)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("invalid Memory configuration") from error
    project_config = config.get("project") if isinstance(config, dict) else None
    memory_options = config.get("memory") if isinstance(config, dict) else None
    if (
        not isinstance(config, dict)
        or set(config) != {"version", "project", "memory"}
        or config.get("version") != 5
        or not isinstance(project_config, dict)
        or set(project_config) != {"id", "name"}
        or not isinstance(project_config.get("id"), str)
        or re.fullmatch(r"project\.[a-z0-9][a-z0-9-]*", project_config["id"]) is None
        or not isinstance(project_config.get("name"), str)
        or not project_config["name"].strip()
        or not isinstance(memory_options, dict)
        or set(memory_options) != {"autoIndex", "defaultTokenBudget"}
        or not isinstance(memory_options.get("autoIndex"), bool)
        or not isinstance(memory_options.get("defaultTokenBudget"), int)
        or isinstance(memory_options.get("defaultTokenBudget"), bool)
        or not 501 <= memory_options["defaultTokenBudget"] <= 50000
    ):
        raise ValueError("invalid Memory configuration")


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
    rooms = re.search(r"(?ms)^rooms:\s*\n(?P<body>.*?)(?=^exclude_patterns:\s*$)", text)
    excludes = re.search(r"(?ms)^exclude_patterns:\s*\n(?P<body>.*)\Z", text)
    if (
        len(wing_matches) != 1
        or rooms is None
        or re.search(r"(?m)^\s{2}- name:\s*\S+\s*$", rooms.group("body")) is None
        or re.search(r"(?m)^\s{4}description:\s*\S+.*$", rooms.group("body")) is None
        or excludes is None
        or re.search(r"(?m)^\s{2}-\s+\S+\s*$", excludes.group("body")) is None
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


def retrieval_runtime_healthy(project: Path) -> bool:
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
            return False
        try:
            payload = json.loads(result.stdout)
        except json.JSONDecodeError:
            return False
        if not isinstance(payload, dict) or payload.get("ok") is not True:
            return False
        if arguments[0] == "check" and payload.get("data", {}).get("valid") is not True:
            return False
    return True


def mcp_runtime_healthy(project: Path) -> bool:
    request = json.dumps(
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
    tool = project / ".chaos-engine/tool.py"
    environment = os.environ.copy()
    environment["PYTHONDONTWRITEBYTECODE"] = "1"
    environment["MEMPALACE_EMBEDDING_MODEL"] = "minilm"
    commands = (
        [sys.executable, str(tool), "memory-mcp"],
        [
            sys.executable,
            str(tool),
            "mempalace-mcp",
            "--palace",
            ".chaos-engine-state/mempalace",
        ],
    )
    for command in commands:
        result = subprocess.run(  # nosec B603 - fixed owned launcher and arguments.
            command,
            cwd=project,
            input=request,
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
    return True


def client_command(
    executable: str,
    arguments: list[str],
    project: Path,
    runner=subprocess.run,
) -> subprocess.CompletedProcess[str]:
    result = runner(  # nosec B603 - executable is resolved by shutil.which.
        [executable, *arguments],
        cwd=project,
        capture_output=True,
        text=True,
        check=False,
        timeout=30,
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
    root = project / ".chaos-engine-state/client-marketplace"
    manifest_path = project / "plugins/chaos-engine/.codex-plugin/plugin.json"
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("ChaosEngine plugin manifest is unavailable") from error
    version = manifest.get("version") if isinstance(manifest, dict) else None
    if not isinstance(version, str) or re.fullmatch(r"\d+\.\d+\.\d+", version) is None:
        raise ValueError("ChaosEngine plugin version is invalid")
    return root, marketplace_name, f"{PLUGIN_NAME}@{marketplace_name}", version


def prepare_activation_bundle(project: Path) -> tuple[Path, str, str, str]:
    """Publish one path-unique generated marketplace without tracked machine paths."""
    project = project.resolve()
    root, marketplace_name, plugin_id, version = activation_contract(project)
    source_plugin = project / "plugins/chaos-engine"
    if not source_plugin.is_dir() or is_link_or_reparse(source_plugin):
        raise ValueError("ChaosEngine plugin source is unavailable")
    state_root = root.parent
    state_root.mkdir(parents=True, exist_ok=True)
    building = state_root / f".{root.name}.building-{secrets.token_hex(8)}"
    backup = state_root / f".{root.name}.backup-{secrets.token_hex(8)}"
    building.mkdir()
    try:
        shutil.copytree(source_plugin, building / "plugins/chaos-engine")
        codex_marketplace = {
            "name": marketplace_name,
            "interface": {"displayName": "ChaosEngine Project"},
            "plugins": [
                {
                    "name": PLUGIN_NAME,
                    "source": {"source": "local", "path": "./plugins/chaos-engine"},
                    "policy": {"installation": "INSTALLED_BY_DEFAULT", "authentication": "ON_INSTALL"},
                    "category": "Developer Tools",
                }
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
                }
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
) -> dict[str, dict[str, str]]:
    """Read back native plugin registration for every client installed on the host."""
    project = project.resolve()
    root, marketplace_name, plugin_id, version = activation_contract(project)
    status: dict[str, dict[str, str]] = {}
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
            plugin_present = any(
                isinstance(item, dict)
                and item.get("pluginId") == plugin_id
                and item.get("installed") is True
                and item.get("enabled") is True
                and isinstance(item.get("source"), dict)
                and same_path(item["source"].get("path"), root / "plugins/chaos-engine")
                for item in records
            )
            plugin_ok = plugin_present and any(
                isinstance(item, dict)
                and item.get("pluginId") == plugin_id
                and item.get("version") == version
                for item in records
            )
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
            plugin_present = any(
                isinstance(item, dict)
                and item.get("id") == plugin_id
                and item.get("enabled") is True
                and same_path(item.get("projectPath"), project)
                for item in records
            )
            plugin_ok = plugin_present and any(
                isinstance(item, dict)
                and item.get("id") == plugin_id
                and item.get("version") == version
                and same_path(item.get("projectPath"), project)
                and cached_plugin_matches(item.get("installPath"), root / "plugins/chaos-engine")
                for item in records
            )
        status[client] = {
            "status": "healthy" if marketplace_ok and plugin_ok else "absent",
            "marketplace": "healthy" if marketplace_ok else "absent",
            "plugin": "healthy" if plugin_ok else ("stale" if plugin_present else "absent"),
        }
    return status


def cached_plugin_matches(installed_path: object, source: Path) -> bool:
    if not isinstance(installed_path, str):
        return False
    installed = Path(installed_path)
    for relative in ("hooks/guard.py", "skills/chaos-engine/SKILL.md"):
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
    root, _, plugin_id, _ = activation_contract(project)
    commands = activation_commands(root, plugin_id)
    for client in reversed(clients):
        executable = which(client)
        if executable is None:
            continue
        selected = lambda name, chosen=client, path=executable: path if name == chosen else None
        current = detected_plugin_status(project, runner=runner, which=selected).get(client, {})
        if current.get("plugin") in {"healthy", "stale"}:
            client_command(executable, commands[client]["remove"], project, runner=runner)
        current = detected_plugin_status(project, runner=runner, which=selected).get(client, {})
        if current.get("marketplace") == "healthy":
            client_command(
                executable, commands[client]["removeMarketplace"], project, runner=runner
            )


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
    root, _, plugin_id, _ = activation_contract(project)
    commands = activation_commands(root, plugin_id)
    for client in clients:
        executable = which(client)
        if executable is None:
            continue
        client_command(executable, commands[client]["marketplace"], project, runner=runner)
        client_command(executable, commands[client]["install"], project, runner=runner)


def activate_detected_plugins(
    project: Path,
    *,
    runner=subprocess.run,
    which=shutil.which,
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
    commands = activation_commands(root, plugin_id)
    touched_clients: list[str] = []
    receipt: dict[str, object] = {
        "createdMarketplaces": created_marketplaces,
        "createdPlugins": created_plugins,
        "marketplaceName": marketplace_name,
    }
    try:
        for client in ("codex", "claude"):
            executable = which(client)
            if executable is None:
                continue
            touched_clients.append(client)
            selected_client = lambda name, selected=client, path=executable: path if name == selected else None
            current = detected_plugin_status(project, runner=runner, which=selected_client)[client]
            if current["marketplace"] != "healthy":
                client_command(executable, commands[client]["marketplace"], project, runner=runner)
                created_marketplaces.append(client)
            current = detected_plugin_status(project, runner=runner, which=selected_client)[client]
            if current["plugin"] == "absent":
                client_command(executable, commands[client]["install"], project, runner=runner)
                created_plugins.append(client)
            elif current["plugin"] == "stale":
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


def discover_maven_tools_runtime() -> tuple[Path, Path] | None:
    configured_jar = os.environ.get("CHAOSENGINE_MAVEN_TOOLS_MCP_JAR")
    configured_data_root = os.environ.get(
        "LOCALAPPDATA" if os.name == "nt" else "XDG_DATA_HOME", ""
    )
    data_root = Path(configured_data_root or Path.home() / ".local/share")
    jar_candidates = [
        Path(configured_jar).expanduser() if configured_jar else None,
        data_root
        / "ChaosEngine/tools/maven-tools-mcp"
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
    java_candidates = [
        Path(configured_java).expanduser() if configured_java else None,
        Path(java_home) / "bin" / ("java.exe" if os.name == "nt" else "java")
        if java_home
        else None,
        Path(path_java) if path_java else None,
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
        if java_major(resolved) == 25:
            return resolved, jar
    return None


def owned_servers(
    platform_name: str | None = None,
    maven_runtime: tuple[Path, Path] | None = None,
) -> dict[str, dict[str, object]]:
    command, prefix = interpreter(platform_name)
    servers: dict[str, dict[str, object]] = {
        "chaosengine-memory": {
            "command": command,
            "args": [*prefix, ".chaos-engine/tool.py", "memory-mcp"],
            "cwd": ".",
        },
        "chaosengine-mempalace": {
            "command": command,
            "args": [
                *prefix,
                ".chaos-engine/tool.py",
                "mempalace-mcp",
                "--palace",
                ".chaos-engine-state/mempalace",
            ],
            "cwd": ".",
            "env": {"MEMPALACE_EMBEDDING_MODEL": "minilm"},
        },
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
        "plugins/chaos-engine/skills/chaos-engine/SKILL.md",
        ".codex/hooks.json",
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


def json_content(
    before: bytes | None, maven_runtime: tuple[Path, Path] | None = None
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
    for name, desired in owned_servers(maven_runtime=maven_runtime).items():
        if name in servers and servers[name] != desired:
            raise ValueError(f"ChaosEngine MCP server collision: {name}")
        servers[name] = desired
    return (json.dumps(value, indent=2, sort_keys=True) + "\n").encode()


def codex_content(
    before: bytes | None,
    platform_name: str | None = None,
    maven_runtime: tuple[Path, Path] | None = None,
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
    command, prefix = interpreter(platform_name)
    prefix_text = '"-3", ' if prefix else ""
    block = (
        "# CHAOSENGINE:START\n"
        f'[mcp_servers."chaosengine-memory"]\ncommand = "{command}"\n'
        f'args = [{prefix_text}".chaos-engine/tool.py", "memory-mcp"]\ncwd = ".."\n\n'
        f'[mcp_servers."chaosengine-mempalace"]\ncommand = "{command}"\n'
        f'args = [{prefix_text}".chaos-engine/tool.py", "mempalace-mcp", "--palace", '
        '".chaos-engine-state/mempalace"]\ncwd = ".."\n'
        'env = { MEMPALACE_EMBEDDING_MODEL = "minilm" }\n# CHAOSENGINE:END\n'
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
        if block not in existing:
            raise ValueError("ChaosEngine Codex configuration collision")
        return existing.encode()
    for name in owned_servers(maven_runtime=maven_runtime):
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


def gitignore_content(before: bytes | None) -> bytes:
    try:
        existing = before.decode("utf-8") if before is not None else ""
    except UnicodeDecodeError as error:
        raise ValueError("invalid gitignore configuration") from error
    block = (
        f"{GITIGNORE_START}\n"
        ".chaos-engine-runtime/\n.chaos-engine-runtime.lock\n.chaos-engine-runtime.*\n.chaos-engine-state/\n"
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
        "!.gemini/\n!.gemini/settings.json\n!.gemini/skills/\n"
        "!.gemini/skills/chaos-engine/\n!.gemini/skills/chaos-engine/**\n"
        "!.github/\n!.github/copilot-instructions.md\n!.github/skills/\n"
        "!.github/skills/chaos-engine/\n!.github/skills/chaos-engine/**\n"
        "!plugins/\n!plugins/chaos-engine/\n!plugins/chaos-engine/**\n"
        "!.mcp.json\n!mempalace.yaml\n!AGENTS.md\n!CLAUDE.md\n!GEMINI.md\n!.gitattributes\n"
        ".chaos-engine-owned-directory\n"
        f"{GITIGNORE_END}\n"
    )
    if GITIGNORE_START in existing or GITIGNORE_END in existing:
        if block not in existing:
            raise ValueError("ChaosEngine gitignore collision")
        return before  # type: ignore[return-value]
    separator = "\n" if existing and not existing.endswith("\n") else ""
    return (existing + separator + block).encode()


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
        f"{repository_root_anchor}.gemini/** text eol=lf\n"
        f"{repository_root_anchor}.github/copilot-instructions.md text eol=lf\n"
        f"{repository_root_anchor}.github/skills/chaos-engine/** text eol=lf\n"
        f"{repository_root_anchor}.mcp.json text eol=lf\n"
        f"{repository_root_anchor}.memory/** text eol=lf\n"
        f"{repository_root_anchor}plugins/chaos-engine/** text eol=lf\n"
        f"{repository_root_anchor}AGENTS.md text eol=lf\n"
        f"{repository_root_anchor}CLAUDE.md text eol=lf\n"
        f"{repository_root_anchor}GEMINI.md text eol=lf\n"
        f"{repository_root_anchor}mempalace.yaml text eol=lf\n"
        f"{repository_root_anchor}.gitignore text eol=lf\n"
        f"{repository_root_anchor}.gitattributes text eol=lf\n"
        f"{GITATTRIBUTES_END}\n"
    )
    if GITATTRIBUTES_START in existing or GITATTRIBUTES_END in existing:
        if block not in existing:
            raise ValueError("ChaosEngine gitattributes collision")
        return before  # type: ignore[return-value]
    separator = "\n" if existing and not existing.endswith("\n") else ""
    return (existing + separator + block).encode()


def desired_content(
    before: dict[str, bytes | None],
    maven_runtime: tuple[Path, Path] | None | bool = False,
    project_name: str = "project",
    plugin_version: str = "1.0.0",
) -> dict[str, bytes]:
    if maven_runtime is False:
        maven_runtime = discover_maven_tools_runtime()
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
            or claude_marketplace.get("name") != "chaos-engine-project"
            or not isinstance(claude_marketplace.get("plugins"), list)
        ):
            raise ValueError("ChaosEngine Claude marketplace collision")
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
            "capabilities": ["Instructions", "Lifecycle hooks", "MCP servers"],
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
    command, prefix = interpreter()
    hook_command = " ".join([command, *prefix, '"${CLAUDE_PLUGIN_ROOT}/hooks/guard.py"'])
    lifecycle_events = {
        "SessionStart": "startup|resume|clear|compact",
        "UserPromptSubmit": None,
        "PreToolUse": "Bash|PowerShell|shell_command",
        "PostToolUse": "Bash|PowerShell|shell_command",
        "Stop": None,
        "SubagentStop": None,
    }
    hooks: dict[str, list[dict[str, object]]] = {}
    for event, matcher in lifecycle_events.items():
        group: dict[str, object] = {
            "hooks": [{"type": "command", "command": hook_command, "timeout": 5}]
        }
        if matcher is not None:
            group["matcher"] = matcher
        hooks[event] = [group]
    rendered_plugin_hooks = (
        json.dumps({"hooks": hooks}, indent=2, sort_keys=True) + "\n"
    ).encode()
    project_command = " ".join([command, *prefix, ".chaos-engine/hooks/guard.py"])
    project_hooks = json.loads(rendered_plugin_hooks)
    for groups in project_hooks["hooks"].values():
        for group in groups:
            for hook in group["hooks"]:
                hook["command"] = project_command
    rendered_project_hooks = (json.dumps(project_hooks, indent=2, sort_keys=True) + "\n").encode()
    after["plugins/chaos-engine/hooks/hooks.json"] = hook_content(
        before["plugins/chaos-engine/hooks/hooks.json"], rendered_plugin_hooks, "plugin"
    )
    after[".codex/hooks.json"] = hook_content(
        before[".codex/hooks.json"], rendered_project_hooks, "Codex"
    )
    after["plugins/chaos-engine/hooks/guard.py"] = (
        Path(__file__).resolve().parent / "hooks/guard.py"
    ).read_bytes()
    after["plugins/chaos-engine/skills/chaos-engine/SKILL.md"] = (
        "---\nname: chaos-engine\ndescription: Load the canonical installed ChaosEngine before every task.\n---\n\n"
        "From the active project root, load `.chaos-engine/skills/chaos-engine/SKILL.md` before every task.\n"
    ).encode()
    claude_settings = before[".claude/settings.json"]
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
    plugin_id = "chaos-engine@chaos-engine-project"
    if plugin_id in enabled and enabled[plugin_id] is not True:
        raise ValueError("ChaosEngine Claude plugin collision")
    desired_marketplace = {
        "source": {"source": "directory", "path": "."}
    }
    if "chaos-engine-project" in marketplaces and marketplaces["chaos-engine-project"] != desired_marketplace:
        raise ValueError("ChaosEngine Claude marketplace collision")
    enabled[plugin_id] = True
    marketplaces["chaos-engine-project"] = desired_marketplace
    after[".claude/settings.json"] = (
        json.dumps(settings, indent=2, sort_keys=True) + "\n"
    ).encode()
    roles = {
        "orchestrator": "Own planning, architecture, synthesis, and final verification.",
        "implementer": "Implement one bounded specification with test-driven development.",
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
        validate_memory_config(memory_before)
        after[".memory/config.json"] = memory_before
    schema_assets = memory_schema_assets()
    for name in MEMORY_SCHEMA_FILES:
        relative = f".memory/schema/{name}"
        existing = before[relative]
        if existing is None:
            after[relative] = (schema_assets / name).read_bytes()
        else:
            try:
                schema = json.loads(existing)
            except (UnicodeDecodeError, json.JSONDecodeError) as error:
                raise ValueError("invalid Memory storage") from error
            if not isinstance(schema, (dict, bool)):
                raise ValueError("invalid Memory storage")
            after[relative] = existing
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
        safe_name = re.sub(r"[^A-Za-z0-9_.-]+", "-", project_name).strip("-") or "project"
        after["mempalace.yaml"] = (
            f"wing: {safe_name}\n"
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
    after[".mcp.json"] = json_content(before[".mcp.json"], maven_runtime)
    after[".gemini/settings.json"] = json_content(
        before[".gemini/settings.json"], maven_runtime
    )
    after[".codex/config.toml"] = codex_content(
        before[".codex/config.toml"], maven_runtime=maven_runtime
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


def decode_images(value: object, *, nullable: bool) -> dict[str, bytes | None]:
    keys = frozenset(value) if isinstance(value, dict) else frozenset()
    current_keys = frozenset(managed_paths())
    if (
        not isinstance(value, dict)
        or not frozenset(LEGACY_MANAGED_PATHS) <= keys
        or not keys <= current_keys
    ):
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
    for relative in managed_paths():
        result.setdefault(relative, None)
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
            version = plugin_cache_version(core_commit)
            wanted = desired_content(
                before,
                project_name=project_identity_name(project),
                plugin_version=version,
            )
            if after == wanted and receipt.get("coreCommit") == core_commit:
                return receipt
            next_receipt = dict(receipt)
            next_receipt["phase"] = "installing"
            next_receipt["coreCommit"] = core_commit
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
    )
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
    if current_images(project) != before:
        raise ValueError("ChaosEngine host removal state drift detected")
    remove_created_directories(project, receipt)
    anchor = host_anchor_path(project)
    if anchor.name.startswith(ACTIVE_ANCHOR_PREFIX):
        anchor = move_anchor(project, anchor, REMOVING_ANCHOR_PREFIX)
    activation_root = project / ".chaos-engine-state/client-marketplace"
    if activation_root.exists():
        if is_link_or_reparse(activation_root) or not activation_root.is_dir():
            raise ValueError("ChaosEngine activation marketplace collision")
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
