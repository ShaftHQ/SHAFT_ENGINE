"""Portable ChaosEngine host adapter tests (#4795)."""

from __future__ import annotations

import importlib.util
import errno
import hashlib
import json
import os
import re
import shutil
import sqlite3
import subprocess  # nosec B404 - fixed Git acceptance commands.
import tempfile
import unittest
import unittest.mock as mock
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
HOSTS = ROOT / "chaos-engine/hosts.py"
TOOL = ROOT / "chaos-engine/tool.py"


def load(path: Path, name: str):
    specification = importlib.util.spec_from_file_location(name, path)
    if specification is None or specification.loader is None:
        raise RuntimeError("host controller test module could not be loaded")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


def create_sqlite_exact_state(path: Path) -> None:
    database = sqlite3.connect(path)
    try:
        database.executescript(
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
            VALUES ('mempalace_drawers', '2026-08-15T00:00:00Z');
            """
        )
        database.commit()
    finally:
        database.close()


def create_chroma_state(path: Path) -> None:
    database = sqlite3.connect(path)
    try:
        database.executescript(
            """
            CREATE TABLE collections (
                id TEXT PRIMARY KEY, name TEXT NOT NULL, dimension INTEGER,
                database_id TEXT NOT NULL, config_json_str TEXT, schema_str TEXT
            );
            CREATE TABLE segments (
                id TEXT PRIMARY KEY, type TEXT NOT NULL, scope TEXT NOT NULL,
                collection TEXT NOT NULL
            );
            CREATE TABLE embeddings_queue (
                seq_id INTEGER PRIMARY KEY, created_at TIMESTAMP NOT NULL,
                operation INTEGER NOT NULL, topic TEXT NOT NULL, id TEXT NOT NULL,
                vector BLOB, encoding TEXT, metadata TEXT
            );
            """
        )
        database.commit()
    finally:
        database.close()


class ChaosEngineHostsTest(unittest.TestCase):
    def test_hook_receipt_reports_exact_hash_trust_and_restart_only_on_change(self):
        module = load(HOSTS, "chaos_engine_hook_receipt")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            canonical = project / ".chaos-engine/skills/chaos-engine/SKILL.md"
            canonical.parent.mkdir(parents=True)
            canonical.write_text("# ChaosEngine\n", encoding="utf-8")

            first = module.install(project, core_commit="1" * 40)
            self.assertTrue(first["restartRequired"])
            self.assertEqual("review-required", first["hookTrust"])
            self.assertTrue(first["changedHooks"])
            self.assertEqual(
                set(first["hookHashes"]), set(first["changedHooks"])
            )
            for digest in first["hookHashes"].values():
                self.assertRegex(digest, r"^[0-9a-f]{64}$")

            second = module.install(project, core_commit="1" * 40)
            self.assertFalse(second["restartRequired"])
            self.assertEqual([], second["changedHooks"])
            self.assertEqual(first["hookHashes"], second["hookHashes"])
            status = module.verify(project)
            self.assertEqual("review-required", status["hookTrust"])
            self.assertFalse(status["restartRequired"])
            self.assertEqual(second["hookHashes"], status["hookHashes"])

    def test_grok_runtime_requires_trust_and_complete_loaded_lifecycle(self):
        module = load(HOSTS, "chaos_engine_grok_runtime")
        events = (
            "session_start", "user_prompt_submit", "pre_tool_use", "post_tool_use",
            "post_tool_use_failure", "stop", "subagent_stop", "session_end",
        )
        healthy = {"projectTrusted": True, "hooks": [
            {"event": event, "target": "python3 .chaos-engine/hooks/guard.py"}
            for event in events
        ]}
        def result(payload, returncode=0):
            return subprocess.CompletedProcess([], returncode, json.dumps(payload), "")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            for payload, expected in (
                (healthy, "healthy"),
                ({**healthy, "projectTrusted": False}, "recovery-required"),
                ({**healthy, "hooks": healthy["hooks"][:-1]}, "recovery-required"),
            ):
                calls = []
                state = module.grok_runtime_status(
                    project, executable="grok",
                    runner=lambda command, **kwargs: calls.append((command, kwargs)) or result(payload),
                )
                self.assertEqual(expected, state["status"])
                self.assertEqual(["grok", "inspect", "--json"], calls[0][0])
                self.assertNotIn("--trust", calls[0][0])
            failed = module.grok_runtime_status(
                project, executable="grok", runner=lambda *_a, **_k: result({}, 1)
            )
            self.assertEqual("recovery-required", failed["status"])
            self.assertIn("/hooks-trust", failed["detail"])

    def test_detected_client_plugins_are_registered_installed_and_verified(self):
        module = load(HOSTS, "chaos_engine_plugin_activation")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            canonical = project / ".chaos-engine/skills/chaos-engine/SKILL.md"
            canonical.parent.mkdir(parents=True)
            canonical.write_text("# ChaosEngine\n", encoding="utf-8")
            module.install(project, core_commit="1" * 40)
            activation_root, marketplace_name, plugin_id, version = module.activation_contract(project)
            state = {
                "codex_marketplace": False,
                "codex_plugins": set(),
                "claude_marketplace": False,
                "claude_plugins": set(),
            }
            calls = []

            def runner(command, **options):
                calls.append((command, options.get("cwd")))
                client = Path(command[0]).stem
                joined = " ".join(command[1:])
                key = f"{client}_marketplace"
                plugin_key = f"{client}_plugins"
                if "marketplace list" in joined:
                    if client == "codex":
                        value = {"marketplaces": [{"name": marketplace_name, "root": str(activation_root)}]} if state[key] else {"marketplaces": []}
                    else:
                        value = [{"name": marketplace_name, "path": str(activation_root)}] if state[key] else []
                elif "marketplace add" in joined:
                    state[key] = True
                    value = {}
                elif "marketplace remove" in joined:
                    state[key] = False
                    value = {}
                elif "plugin list" in joined:
                    records = []
                    for name in state[plugin_key]:
                        contract = module.activation_plugins(project, marketplace_name)[name]
                        if client == "codex":
                            records.append({"pluginId": contract["id"], "version": contract["version"], "installed": True, "enabled": True, "source": {"path": str(activation_root / f"plugins/{name}")}})
                        else:
                            records.append({"id": contract["id"], "version": contract["version"], "enabled": True, "projectPath": str(project), "installPath": str(activation_root / f"plugins/{name}")})
                    value = {"installed": records, "available": []}
                elif "plugin add" in joined or "plugin install" in joined:
                    installed_id = next(item for item in command if "@" in item)
                    state[plugin_key].add(installed_id.split("@", 1)[0])
                    value = {}
                elif "plugin remove" in joined or "plugin uninstall" in joined:
                    removed_id = next(item for item in command if "@" in item)
                    state[plugin_key].discard(removed_id.split("@", 1)[0])
                    value = {}
                elif command[0] in {"npm", "caveman"}:
                    raise AssertionError(command)
                else:
                    raise AssertionError(command)
                return mock.Mock(returncode=0, stdout=json.dumps(value), stderr="")

            receipt = module.activate_detected_plugins(
                project,
                runner=runner,
                which=lambda name: name,
            )
            status = module.detected_plugin_status(
                project,
                runner=runner,
                which=lambda name: name,
            )

            self.assertEqual(
                {
                    "codex:chaos-engine",
                    "codex:caveman",
                    "codex:ponytail",
                    "claude:chaos-engine",
                    "claude:caveman",
                    "claude:ponytail",
                },
                set(receipt["createdPlugins"]),
            )
            self.assertNotIn("cavemanProxy", receipt)
            self.assertFalse(
                any(command and command[0] in {"npm", "caveman"} for command, _ in calls)
            )
            self.assertTrue(all(item["status"] == "healthy" for item in status.values()))
            self.assertTrue(all(cwd == project for _, cwd in calls))

            module.uninstall(project, runner=runner, which=lambda name: name)
            self.assertFalse(any(state.values()))
            self.assertFalse(activation_root.exists())

    def test_restore_client_activation_uses_snapshot_plugin_set(self):
        module = load(HOSTS, "chaos_engine_restore_plugin_set")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            canonical = project / ".chaos-engine/skills/chaos-engine/SKILL.md"
            canonical.parent.mkdir(parents=True)
            canonical.write_text("# ChaosEngine\n", encoding="utf-8")
            module.install(project, core_commit="1" * 40)
            root, marketplace_name, _, _ = module.prepare_activation_bundle(project)
            shutil.rmtree(root / "plugins/caveman")
            shutil.rmtree(root / "plugins/ponytail")

            activation = {
                "marketplaceName": marketplace_name,
                "bundleRoot": str(root),
                "ownedClients": ["codex"],
                "pluginVersion": "1.0.0",
                "claudeLocalBefore": None,
            }
            calls = []

            def runner(command, **_options):
                calls.append(command)
                return mock.Mock(returncode=0, stdout=json.dumps({}), stderr="")

            module.restore_client_activation(
                project,
                activation,
                runner=runner,
                which=lambda name: name if name == "codex" else None,
            )

            self.assertIn(
                ["plugin", "marketplace", "add", str(root), "--json"],
                [command[1:] for command in calls if command[0] == "codex"],
            )
            self.assertIn(
                ["plugin", "add", f"{module.PLUGIN_NAME}@{marketplace_name}", "--json"],
                [command[1:] for command in calls if command[0] == "codex"],
            )
            joined = " ".join(" ".join(command) for command in calls if command[0] == "codex")
            self.assertNotIn("caveman", joined)
            self.assertNotIn("ponytail", joined)

    def test_activation_marketplace_identity_is_collision_safe_across_projects(self):
        module = load(HOSTS, "chaos_engine_plugin_identity")
        with tempfile.TemporaryDirectory() as temporary:
            first = Path(temporary) / "first"
            second = Path(temporary) / "second"
            for project in (first, second):
                manifest = project / "plugins/chaos-engine/.codex-plugin/plugin.json"
                manifest.parent.mkdir(parents=True)
                manifest.write_text(
                    json.dumps({"name": "chaos-engine", "version": "1.0.7"}),
                    encoding="utf-8",
                )

            self.assertNotEqual(
                module.activation_contract(first)[1],
                module.activation_contract(second)[1],
            )

    def test_activation_bundle_uses_durable_user_data_root(self):
        module = load(HOSTS, "chaos_engine_durable_activation_root")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            manifest = project / "plugins/chaos-engine/.codex-plugin/plugin.json"
            manifest.parent.mkdir(parents=True)
            manifest.write_text(
                json.dumps({"name": "chaos-engine", "version": "1.0.7"}),
                encoding="utf-8",
            )
            data_root = Path(temporary) / "user-data"
            with mock.patch.dict(module.os.environ, {"XDG_DATA_HOME": str(data_root)}):
                root, marketplace_name, _, _ = module.activation_contract(project)

            self.assertEqual(
                data_root / "ChaosEngine/client-marketplaces" / marketplace_name,
                root,
            )
            self.assertNotIn(".chaos-engine-state", str(root))

    def test_activation_removes_only_proven_stale_owned_marketplace(self):
        module = load(HOSTS, "chaos_engine_stale_owned_marketplace")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            canonical = project / ".chaos-engine/skills/chaos-engine/SKILL.md"
            canonical.parent.mkdir(parents=True)
            canonical.write_text("# ChaosEngine\n", encoding="utf-8")
            module.install(project, core_commit="1" * 40)
            root, marketplace_name, _, _ = module.activation_contract(project)
            stale_root = project / ".chaos-engine-state/client-marketplace"
            state = {"stale": True, "marketplace": False, "plugins": set()}
            calls = []

            def runner(command, **_options):
                calls.append(command)
                joined = " ".join(command[1:])
                if "marketplace list" in joined and state["stale"]:
                    return mock.Mock(
                        returncode=1,
                        stdout="",
                        stderr=(
                            "Error: failed to load marketplace(s): "
                            f"- `{marketplace_name}` at {stale_root} "
                            "marketplace root does not contain a supported manifest"
                        ),
                    )
                if "marketplace list" in joined:
                    value = {
                        "marketplaces": [
                            {"name": marketplace_name, "root": str(root)}
                        ]
                        if state["marketplace"]
                        else []
                    }
                elif "marketplace remove" in joined:
                    state["stale"] = False
                    value = {}
                elif "marketplace add" in joined:
                    state["marketplace"] = True
                    value = {}
                elif "plugin list" in joined:
                    contracts = module.activation_plugins(project, marketplace_name)
                    value = {
                        "installed": [
                            {
                                "pluginId": contracts[name]["id"],
                                "version": contracts[name]["version"],
                                "installed": True,
                                "enabled": True,
                                "source": {"path": str(root / f"plugins/{name}")},
                            }
                            for name in state["plugins"]
                        ],
                        "available": [],
                    }
                elif "plugin add" in joined:
                    plugin_id = next(item for item in command if "@" in item)
                    state["plugins"].add(plugin_id.split("@", 1)[0])
                    value = {}
                else:
                    raise AssertionError(command)
                return mock.Mock(returncode=0, stdout=json.dumps(value), stderr="")

            receipt = module.activate_detected_plugins(
                project,
                runner=runner,
                which=lambda name: name if name == "codex" else None,
            )

            self.assertEqual("healthy", receipt["clients"]["codex"]["status"])
            self.assertEqual(
                1,
                sum(
                    command[1:] == ["plugin", "marketplace", "remove", marketplace_name]
                    for command in calls
                ),
            )

    def test_activation_preserves_unclassified_broken_marketplace(self):
        module = load(HOSTS, "chaos_engine_unclassified_broken_marketplace")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            cases = []
            for label in ("foreign", "unknown", "valid", "linked"):
                project = base / label / "consumer"
                canonical = project / ".chaos-engine/skills/chaos-engine/SKILL.md"
                canonical.parent.mkdir(parents=True)
                canonical.write_text("# ChaosEngine\n", encoding="utf-8")
                module.install(project, core_commit="1" * 40)
                _, marketplace_name, _, _ = module.activation_contract(project)
                stale_root = project / ".chaos-engine-state/client-marketplace"
                reported_name = marketplace_name
                reported_root = stale_root
                if label == "foreign":
                    reported_name = "foreign-marketplace"
                elif label == "unknown":
                    reported_root = base / "unknown-marketplace-root"
                elif label == "valid":
                    manifest = stale_root / ".agents/plugins/marketplace.json"
                    manifest.parent.mkdir(parents=True)
                    manifest.write_text("{}\n", encoding="utf-8")
                cases.append((label, project, reported_name, reported_root))

            for label, project, reported_name, reported_root in cases:
                calls = []

                def runner(command, **_options):
                    calls.append(command)
                    return mock.Mock(
                        returncode=1,
                        stdout="",
                        stderr=(
                            "Error: failed to load marketplace(s): "
                            f"- `{reported_name}` at {reported_root} "
                            "marketplace root does not contain a supported manifest"
                        ),
                    )

                real_link_check = module.is_link_or_reparse
                with self.subTest(case=label), mock.patch.object(
                    module,
                    "is_link_or_reparse",
                    side_effect=lambda path: (
                        label == "linked" and Path(path) == reported_root
                    ) or real_link_check(path),
                ), self.assertRaisesRegex(
                    RuntimeError, "marketplace root does not contain a supported manifest"
                ):
                    module.activate_detected_plugins(
                        project,
                        runner=runner,
                        which=lambda name: name if name == "codex" else None,
                    )
                self.assertFalse(
                    any("marketplace remove" in " ".join(command[1:]) for command in calls)
                )

    def test_failed_plugin_upgrade_restores_prior_bundle_and_activation_receipt(self):
        module = load(HOSTS, "chaos_engine_plugin_upgrade_rollback")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            canonical = project / ".chaos-engine/skills/chaos-engine/SKILL.md"
            canonical.parent.mkdir(parents=True)
            canonical.write_text("# ChaosEngine\n", encoding="utf-8")
            module.install(project, core_commit="f" * 40)
            root, marketplace_name, plugin_id, old_version = module.prepare_activation_bundle(project)
            prior = {
                "marketplaceName": marketplace_name,
                "bundleRoot": str(root),
                "ownedClients": [],
                "pluginVersion": old_version,
                "claudeLocalBefore": None,
            }
            module.record_client_activation(project, prior)
            old_manifest = root.joinpath("plugins/chaos-engine/.codex-plugin/plugin.json").read_bytes()
            module.install(project, core_commit="0" * 40)
            state = {"marketplace": True, "plugin": True}

            def runner(command, **_options):
                joined = " ".join(command[1:])
                if "marketplace list" in joined:
                    value = {
                        "marketplaces": [
                            {"name": marketplace_name, "root": str(root)}
                        ]
                        if state["marketplace"]
                        else []
                    }
                elif "plugin list" in joined:
                    value = {
                        "installed": [
                            {
                                "pluginId": plugin_id,
                                "installed": True,
                                "enabled": True,
                                "version": old_version,
                                "source": {"path": str(root / "plugins/chaos-engine")},
                            }
                        ]
                        if state["plugin"]
                        else [],
                        "available": [],
                    }
                elif "plugin remove" in joined:
                    state["plugin"] = False
                    value = {}
                elif "marketplace remove" in joined:
                    state["marketplace"] = False
                    value = {}
                elif "plugin add" in joined:
                    return mock.Mock(
                        returncode=1,
                        stdout="",
                        stderr="injected plugin upgrade failure",
                    )
                else:
                    raise AssertionError(command)
                return mock.Mock(returncode=0, stdout=json.dumps(value), stderr="")

            with self.assertRaisesRegex(RuntimeError, "injected plugin upgrade failure"):
                module.activate_detected_plugins(
                    project,
                    runner=runner,
                    which=lambda name: name if name == "codex" else None,
                )

            receipt, _ = module.read_receipt(project)
            self.assertEqual(old_version, receipt["clientActivation"]["pluginVersion"])
            self.assertEqual(
                old_manifest,
                root.joinpath("plugins/chaos-engine/.codex-plugin/plugin.json").read_bytes(),
            )

    def test_failed_host_upgrade_restores_runtime_modules_and_preserves_foreign_files(self):
        module = load(HOSTS, "chaos_engine_runtime_upgrade_rollback")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            canonical = project / ".chaos-engine/skills/chaos-engine/SKILL.md"
            canonical.parent.mkdir(parents=True)
            canonical.write_text("# ChaosEngine\n", encoding="utf-8")
            module.install(project, core_commit="1" * 40)
            hooks = project / "plugins/chaos-engine/hooks"
            runtime_paths = (hooks / "kernel.py", hooks / "lifecycle.py")
            prior = {path: path.read_bytes() for path in runtime_paths}
            foreign = hooks / "foreign.py"
            foreign.write_bytes(b"foreign-owned\n")
            real_desired_content = module.desired_content
            real_atomic_write = module.atomic_write

            def changed_content(*args, **kwargs):
                desired = real_desired_content(*args, **kwargs)
                desired["plugins/chaos-engine/hooks/kernel.py"] = b"new kernel\n"
                desired["plugins/chaos-engine/hooks/lifecycle.py"] = b"new lifecycle\n"
                desired["plugins/chaos-engine/skills/chaos-engine/SKILL.md"] = b"trigger\n"
                return desired

            def fail_after_runtime_modules(root, path, content, before):
                relative = path.relative_to(root).as_posix()
                if relative == "plugins/chaos-engine/skills/chaos-engine/SKILL.md":
                    raise OSError("injected host upgrade failure")
                return real_atomic_write(root, path, content, before)

            with mock.patch.object(module, "desired_content", side_effect=changed_content):
                with mock.patch.object(module, "atomic_write", side_effect=fail_after_runtime_modules):
                    with self.assertRaisesRegex(OSError, "injected host upgrade failure"):
                        module.install(project, core_commit="2" * 40)

            for path, expected in prior.items():
                self.assertEqual(expected, path.read_bytes(), path.name)
            self.assertEqual(b"foreign-owned\n", foreign.read_bytes())

    def test_cached_chaos_plugin_requires_both_runtime_modules(self):
        module = load(HOSTS, "chaos_engine_cached_runtime_inventory")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source/chaos-engine"
            installed = root / "installed/chaos-engine"
            required = (
                "hooks/guard.py",
                "hooks/kernel.py",
                "hooks/launch.js",
                "hooks/lifecycle.py",
                "hooks/matchers.json",
                "hooks/reflection.py",
                "skills/chaos-engine/SKILL.md",
            )
            for relative in required:
                source_path = source / relative
                installed_path = installed / relative
                source_path.parent.mkdir(parents=True, exist_ok=True)
                installed_path.parent.mkdir(parents=True, exist_ok=True)
                source_path.write_text(relative, encoding="utf-8")
                installed_path.write_text(relative, encoding="utf-8")

            self.assertTrue(module.cached_plugin_matches(str(installed), source))
            for relative in (
                "hooks/kernel.py",
                "hooks/launch.js",
                "hooks/lifecycle.py",
                "hooks/matchers.json",
            ):
                missing = installed / relative
                missing.unlink()
                self.assertFalse(module.cached_plugin_matches(str(installed), source), relative)
                missing.write_text(relative, encoding="utf-8")

    def setUp(self):
        self.runtime_state = tempfile.TemporaryDirectory()
        self.runtime_environment = mock.patch.dict(
            os.environ,
            {
                "LOCALAPPDATA": self.runtime_state.name,
                "XDG_DATA_HOME": self.runtime_state.name,
                "CHAOSENGINE_JAVA": "",
                "CHAOSENGINE_MAVEN_TOOLS_MCP_JAR": "",
                "JAVA_HOME": "",
            },
            clear=False,
        )
        self.runtime_environment.start()

    def tearDown(self):
        self.runtime_environment.stop()
        self.runtime_state.cleanup()

    def test_validate_path_rejects_a_path_outside_the_project(self):
        module = load(HOSTS, "chaos_engine_hosts_outside_path")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            with self.assertRaisesRegex(ValueError, "escapes the project"):
                module.validate_path(project, root / "outside.txt")

    def test_five_host_adapters_route_to_one_installed_skill(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            core = project / ".chaos-engine"
            core.joinpath("skills/chaos-engine").mkdir(parents=True)
            core.joinpath("skills/chaos-engine/SKILL.md").write_text("# ChaosEngine\n")

            receipt = module.install(project)

            expected = {
                "codex": project / ".agents/skills/chaos-engine/SKILL.md",
                "claude": project / ".claude/skills/chaos-engine/SKILL.md",
                "grok": project / "AGENTS.md",
                "gemini": project / ".gemini/skills/chaos-engine/SKILL.md",
                "copilot": project / ".github/skills/chaos-engine/SKILL.md",
            }
            self.assertEqual(set(expected), set(receipt["hosts"]))
            for path in expected.values():
                self.assertIn(".chaos-engine", path.read_text(encoding="utf-8"))

    def test_host_configs_use_project_relative_local_runtime_launchers(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            module.install(project)

            claude = json.loads(project.joinpath(".mcp.json").read_text(encoding="utf-8"))
            gemini = json.loads(
                project.joinpath(".gemini/settings.json").read_text(encoding="utf-8")
            )
            codex = project.joinpath(".codex/config.toml").read_text(encoding="utf-8")

            for config in (claude, gemini):
                servers = config["mcpServers"]
                self.assertEqual(
                    {"chaosengine-memory", "chaosengine-mempalace", "context7"},
                    set(servers),
                )
                self.assertEqual(
                    {"url": "https://mcp.context7.com/mcp"}, servers["context7"]
                )
                for name in ("chaosengine-memory", "chaosengine-mempalace"):
                    server = servers[name]
                    self.assertEqual("python3", server["command"])
                    self.assertNotEqual("-3", server["args"][0])
                    self.assertEqual("py", server["commandWindows"])
                    self.assertEqual("-3", server["argsWindows"][0])
                    self.assertIn(".chaos-engine/tool.py", server["args"])
                    self.assertIn(".chaos-engine/tool.py", server["argsWindows"])
                mempalace = servers["chaosengine-mempalace"]
                self.assertEqual(
                    ["--backend", "sqlite_exact"],
                    mempalace["args"][-2:],
                )
                self.assertEqual(module.MEMPALACE_MCP_ENV, mempalace["env"])
            self.assertIn('[mcp_servers."chaosengine-memory"]', codex)
            self.assertIn('".chaos-engine/tool.py", "memory-mcp"]', codex)
            self.assertIn(".chaos-engine-state/mempalace", str(claude))
            self.assertIn('"--backend", "sqlite_exact"', codex)
            self.assertIn("MEMPALACE_BACKEND = \"sqlite_exact\"", codex)
            self.assertIn("MEMPALACE_EMBEDDING_MODEL = \"minilm\"", codex)

    def test_complete_host_harness_installs_inventory_roles_hooks_and_plugin(self):
        module = load(HOSTS, "chaos_engine_complete_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            legacy_plugin_hooks = project / "plugins/chaos-engine/hooks/hooks.json"
            legacy_plugin_hooks.parent.mkdir(parents=True)
            legacy_plugin_hooks.write_bytes(module.lifecycle_hooks_document("codex"))

            receipt = module.install(project)

            required = {
                ".agents/skills/README.md",
                ".agents/plugins/marketplace.json",
                ".claude-plugin/marketplace.json",
                "plugins/chaos-engine/.codex-plugin/plugin.json",
                "plugins/chaos-engine/.claude-plugin/plugin.json",
                "plugins/chaos-engine/hooks/hooks.json",
                "plugins/chaos-engine/hooks/guard.py",
                "plugins/chaos-engine/hooks/kernel.py",
                "plugins/chaos-engine/hooks/lifecycle.py",
                "plugins/chaos-engine/hooks/reflection.py",
                "plugins/chaos-engine/skills/chaos-engine/SKILL.md",
                ".grok/hooks/lifecycle.json",
                ".github/hooks/chaos-engine.json",
                "plugins/caveman/.codex-plugin/plugin.json",
                "plugins/caveman/.claude-plugin/plugin.json",
                "plugins/caveman/skills/caveman/SKILL.md",
                "plugins/caveman/LICENSE",
                "plugins/caveman/UPSTREAM.md",
                "plugins/caveman/src/hooks/caveman-activate.js",
                "plugins/ponytail/.codex-plugin/plugin.json",
                "plugins/ponytail/.claude-plugin/plugin.json",
                "plugins/ponytail/skills/ponytail/SKILL.md",
                "plugins/ponytail/LICENSE",
                "plugins/ponytail/UPSTREAM.md",
                "plugins/ponytail/hooks/ponytail-activate.js",
                ".codex/hooks.json",
                ".claude/settings.json",
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
            }
            for role in ("orchestrator", "implementer", "reviewer", "tester", "mechanical-helper"):
                required.add(f".claude/agents/chaos-engine-{role}.md")
                required.add(f".codex/agents/chaos-engine-{role}.toml")
            self.assertLessEqual(required, set(receipt["after"]))
            for relative in required:
                self.assertTrue(project.joinpath(relative).is_file(), relative)
            self.assertEqual(
                {},
                json.loads(
                    project.joinpath(
                        "plugins/chaos-engine/hooks/hooks.json"
                    ).read_text()
                )["hooks"],
            )
            marketplace = json.loads(
                project.joinpath(".agents/plugins/marketplace.json").read_text()
            )
            self.assertEqual(
                "INSTALLED_BY_DEFAULT",
                marketplace["plugins"][0]["policy"]["installation"],
            )
            self.assertNotIn(
                "act-as-mohab",
                "\n".join(
                    project.joinpath(path).read_text(errors="ignore") for path in required
                ),
            )
            for name in ("kernel.py", "lifecycle.py"):
                self.assertEqual(
                    (ROOT / "chaos-engine/hooks" / name).read_bytes(),
                    (project / "plugins/chaos-engine/hooks" / name).read_bytes(),
                )
            memory_config = json.loads(project.joinpath(".memory/config.json").read_text())
            self.assertEqual(5, memory_config["version"])
            self.assertEqual({"version", "project", "memory"}, set(memory_config))
            self.assertEqual("consumer", memory_config["project"]["name"])
            self.assertTrue(module.retrieval_configs_healthy(project))
            self.assertEqual("consumer_main", module.default_mempalace_wing("consumer"))
            self.assertEqual("shaft_engine_main", module.default_mempalace_wing("SHAFT_ENGINE"))
            self.assertIn("wing: consumer_main", project.joinpath("mempalace.yaml").read_text())
            ignores = project.joinpath(".gitignore").read_text()
            self.assertIn(".chaos-engine-runtime/", ignores)
            self.assertIn(".chaos-engine.lock", ignores)
            self.assertIn(".chaos-engine-runtime.lock", ignores)
            self.assertIn(".chaos-engine-runtime-current.json", ignores)
            self.assertIn(".chaos-engine-runtime-generations/", ignores)
            self.assertIn(".chaos-engine-runtime-transactions/", ignores)
            self.assertIn(".chaos-engine.backup/", ignores)
            self.assertIn(".chaos-engine-owned-directory", ignores)
            self.assertGreater(
                ignores.rindex(".chaos-engine-owned-directory"),
                ignores.index("!.codex/**"),
            )
            self.assertIn("!.memory/config.json", ignores)
            self.assertIn("!.memory/schema/*.schema.json", ignores)
            self.assertIn("!.memory/events.jsonl", ignores)
            self.assertIn("!.claude/**", ignores)
            self.assertIn("!.codex/**", ignores)
            self.assertIn(".claude/settings.local.json", ignores)
            for name, vendor in (("caveman", "caveman"), ("ponytail", "ponytail")):
                pin = json.loads(
                    (ROOT / "chaos-engine/vendor" / vendor / "PIN.json").read_text(
                        encoding="utf-8"
                    )
                )
                for relative in pin["files"]:
                    published = project / "plugins" / name / relative
                    self.assertTrue(published.is_file(), published)
                    self.assertEqual(
                        published.read_bytes(),
                        (ROOT / "chaos-engine/vendor" / vendor / relative).read_bytes(),
                    )
                manifest = json.loads(
                    project.joinpath(f"plugins/{name}/.claude-plugin/plugin.json").read_text()
                )
                self.assertIn("hooks", manifest)
                self.assertNotIn("SessionStart", manifest.get("hooks", {}))

            required_events = {
                "SessionStart",
                "UserPromptSubmit",
                "PreToolUse",
                "PostToolUse",
                "PostToolUseFailure",
                "Stop",
                "SubagentStop",
                "SessionEnd",
            }
            lifecycle = json.loads(project.joinpath(".codex/hooks.json").read_text())["hooks"]
            grok_lifecycle = json.loads(
                project.joinpath(".grok/hooks/lifecycle.json").read_text()
            )["hooks"]
            claude_lifecycle = json.loads(
                project.joinpath(".claude/settings.json").read_text()
            )["hooks"]
            for document in (lifecycle, grok_lifecycle):
                self.assertEqual(required_events, set(document))
                for event in required_events:
                    self.assertEqual(1, len(document[event]), event)
                    self.assertEqual(1, len(document[event][0]["hooks"]), event)
                    command = document[event][0]["hooks"][0]["command"]
                    self.assertIn(" ", command, event)
                    self.assertTrue(
                        ".chaos-engine/hooks/guard.py" in command
                        or "plugins/chaos-engine/hooks/guard.py" in command,
                        event,
                    )
                self.assertEqual(3, document["SessionEnd"][0]["hooks"][0]["timeout"])
            claude_events = required_events | {"PreCompact"}
            self.assertEqual(claude_events, set(claude_lifecycle))
            for event in claude_events:
                self.assertEqual(1, len(claude_lifecycle[event]), event)
                self.assertEqual(1, len(claude_lifecycle[event][0]["hooks"]), event)
            for manifest_path in (
                "plugins/chaos-engine/.codex-plugin/plugin.json",
                "plugins/chaos-engine/.claude-plugin/plugin.json",
            ):
                manifest = json.loads(project.joinpath(manifest_path).read_text())
                self.assertNotIn("hooks", manifest)
            gemini_lifecycle = json.loads(
                project.joinpath(".gemini/settings.json").read_text()
            )["hooks"]
            self.assertEqual(
                {
                    "SessionStart",
                    "BeforeAgent",
                    "BeforeTool",
                    "AfterTool",
                    "AfterAgent",
                    "PreCompress",
                    "SessionEnd",
                },
                set(gemini_lifecycle),
            )
            copilot_lifecycle = json.loads(
                project.joinpath(".github/hooks/chaos-engine.json").read_text()
            )
            self.assertEqual(1, copilot_lifecycle["version"])
            self.assertEqual(
                {
                    "sessionStart",
                    "userPromptSubmitted",
                    "preToolUse",
                    "postToolUse",
                    "postToolUseFailure",
                    "agentStop",
                    "subagentStop",
                    "preCompact",
                    "sessionEnd",
                },
                set(copilot_lifecycle["hooks"]),
            )
            installed_hook = project / "plugins/chaos-engine/hooks/guard.py"
            hook_environment = {**os.environ, "TMPDIR": temporary, "TEMP": temporary}
            failure = {
                "hook_event_name": "PostToolUseFailure",
                "tool_name": "PowerShell",
                "tool_input": {"command": "py -3 -m unittest installed.focused"},
                "session_id": "installed-reflection",
            }
            first = subprocess.run(  # nosec B603 - fixed interpreter and installed local hook.
                [os.sys.executable, str(installed_hook)],
                input=json.dumps(failure), capture_output=True, text=True,
                env=hook_environment, check=False,
            )
            second = subprocess.run(  # nosec B603 - fixed interpreter and installed local hook.
                [os.sys.executable, str(installed_hook)],
                input=json.dumps(failure), capture_output=True, text=True,
                env=hook_environment, check=False,
            )
            wrapped_destruction = subprocess.run(  # nosec B603 - installed local hook.
                [os.sys.executable, str(installed_hook)],
                input=json.dumps(
                    {
                        "hook_event_name": "PreToolUse",
                        "tool_name": "functions.exec",
                        "tool_input": {"cmd": "git reset --hard HEAD~1"},
                        "session_id": "installed-object-wrapper",
                    }
                ),
                capture_output=True,
                text=True,
                env=hook_environment,
                check=False,
            )
            wrapped_source_destruction = subprocess.run(  # nosec B603 - installed local hook.
                [os.sys.executable, str(installed_hook)],
                input=json.dumps(
                    {
                        "hook_event_name": "PreToolUse",
                        "tool_name": "functions.exec",
                        "tool_input": {
                            "input": (
                                'await tools.exec_command('
                                '{cmd:"git reset --hard HEAD~1"});'
                            )
                        },
                        "session_id": "installed-source-wrapper",
                    }
                ),
                capture_output=True,
                text=True,
                env=hook_environment,
                check=False,
            )
            self.assertEqual(0, first.returncode, first.stderr)
            self.assertEqual(0, second.returncode, second.stderr)
            self.assertIn("Reflection required", second.stdout)
            self.assertEqual(2, wrapped_destruction.returncode)
            self.assertEqual(
                "block", json.loads(wrapped_destruction.stdout)["decision"]
            )
            self.assertEqual(2, wrapped_source_destruction.returncode)
            self.assertEqual(
                "block",
                json.loads(wrapped_source_destruction.stdout)["decision"],
            )

    def test_lifecycle_hook_is_a_noop_outside_an_installed_project(self):
        module = load(HOSTS, "chaos_engine_hook_noop")
        document = json.loads(module.lifecycle_hooks_document("codex"))
        handler = document["hooks"]["PreToolUse"][0]["hooks"][0]
        command = handler["commandWindows"] if os.name == "nt" else handler["command"]

        with tempfile.TemporaryDirectory() as temporary:
            completed = subprocess.run(  # nosec B602 - generated fixed hook command.
                command,
                shell=True,
                cwd=temporary,
                input=json.dumps({"hook_event_name": "PreToolUse"}),
                capture_output=True,
                text=True,
                check=False,
            )

        self.assertEqual(0, completed.returncode, completed.stderr)
        self.assertEqual({}, json.loads(completed.stdout))

    def test_generated_host_hooks_share_preventive_and_observational_matchers(self):
        module = load(HOSTS, "chaos_engine_hook_matchers")
        preventive = module.PRE_TOOL_MATCHER
        observational = module.POST_TOOL_MATCHER

        for tool in ("Read", "Grep", "WebSearch", "WebFetch", "web__run", "update_plan"):
            self.assertIsNone(re.fullmatch(preventive, tool), tool)
            self.assertIsNone(re.fullmatch(observational, tool), tool)
        for tool in ("Bash", "PowerShell", "apply_patch", "Write", "spawn_agent"):
            self.assertIsNotNone(re.fullmatch(preventive, tool), tool)

        for host in ("codex", "claude", "grok"):
            hooks = json.loads(module.lifecycle_hooks_document(host))["hooks"]
            self.assertEqual(preventive, hooks["PreToolUse"][0]["matcher"])
            self.assertEqual(observational, hooks["PostToolUse"][0]["matcher"])

        for relative in (".codex/hooks.json", ".claude/settings.json"):
            hooks = json.loads((ROOT / relative).read_text(encoding="utf-8"))["hooks"]
            self.assertEqual(preventive, hooks["PreToolUse"][0]["matcher"])
            self.assertEqual(observational, hooks["PostToolUse"][0]["matcher"])
            self.assertEqual(observational, hooks["PostToolUseFailure"][0]["matcher"])

        gemini = json.loads(module.gemini_hooks_document())["hooks"]
        self.assertEqual(preventive, gemini["BeforeTool"][0]["matcher"])
        self.assertEqual(observational, gemini["AfterTool"][0]["matcher"])

        launcher = (ROOT / "chaos-engine/hooks/launch.js").read_text(encoding="utf-8")
        self.assertLess(launcher.index("matchesHook()"), launcher.index("guardPath();"))
        self.assertIn("matchers.json", launcher)

    def test_plugin_marketplace_preserves_unrelated_entries(self):
        module = load(HOSTS, "chaos_engine_marketplace_merge")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            marketplace_path = project / ".agents/plugins/marketplace.json"
            marketplace_path.parent.mkdir(parents=True)
            original = {
                "name": "project",
                "interface": {"displayName": "Project"},
                "plugins": [
                    {
                        "name": "unrelated",
                        "source": {"source": "local", "path": "vendor/unrelated"},
                        "policy": {"installation": "AVAILABLE", "authentication": "ON_USE"},
                        "category": "Developer Tools",
                    }
                ],
            }
            marketplace_path.write_text(json.dumps(original), encoding="utf-8")

            module.install(project)
            merged = json.loads(marketplace_path.read_text())
            self.assertEqual(
                ["unrelated", "chaos-engine", "caveman", "ponytail"],
                [item["name"] for item in merged["plugins"]],
            )
            self.assertEqual("./plugins/chaos-engine", merged["plugins"][1]["source"]["path"])
            module.uninstall(project)
            self.assertEqual(original, json.loads(marketplace_path.read_text()))

    def test_legacy_host_receipt_updates_to_complete_harness(self):
        module = load(HOSTS, "chaos_engine_legacy_receipt")
        legacy_paths = (
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
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            module.install(project, core_commit="1" * 40)
            receipt, _ = module.read_receipt(project)
            for field in ("before", "after"):
                receipt[field] = {
                    path: value for path, value in receipt[field].items() if path in legacy_paths
                }
            receipt["createdDirectories"] = []
            for relative in set(module.managed_paths()) - set(legacy_paths):
                path = project / relative
                if path.is_file():
                    path.unlink()
            project.joinpath(".gitignore").write_text("user-owned-rule/\n", encoding="utf-8")
            project.joinpath(module.RECEIPT_NAME).write_bytes(
                module.receipt_bytes(receipt, project)
            )

            receipt = module.install(project, core_commit="2" * 40)

            self.assertEqual(set(module.managed_paths()), set(receipt["after"]))
            self.assertEqual("healthy", module.verify(project, core_commit="2" * 40)["status"])
            self.assertTrue(project.joinpath(".gitignore").read_text().startswith("user-owned-rule/"))

    def test_codex_hooks_preserve_unrelated_events(self):
        module = load(HOSTS, "chaos_engine_hook_merge")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            hook_path = project / ".codex/hooks.json"
            hook_path.parent.mkdir(parents=True)
            original = {"hooks": {"SessionStart": [{"hooks": [{"type": "command", "command": "user-hook"}]}]}}
            hook_path.write_text(json.dumps(original), encoding="utf-8")

            module.install(project)
            merged = json.loads(hook_path.read_text())
            self.assertEqual("user-hook", merged["hooks"]["SessionStart"][0]["hooks"][0]["command"])
            self.assertGreater(len(merged["hooks"]["SessionStart"]), 1)
            self.assertIn("Stop", merged["hooks"])
            module.uninstall(project)
            self.assertEqual(original, json.loads(hook_path.read_text()))

    def test_source_repository_registers_copilot_hooks_through_kernel_launcher(self):
        document = json.loads(
            (ROOT / ".github/hooks/chaos-engine.json").read_text(encoding="utf-8")
        )
        expected = {
            "sessionStart",
            "userPromptSubmitted",
            "preToolUse",
            "postToolUse",
            "postToolUseFailure",
            "agentStop",
            "subagentStop",
            "preCompact",
            "sessionEnd",
        }

        self.assertEqual(1, document["version"])
        self.assertEqual(expected, set(document["hooks"]))
        for handlers in document["hooks"].values():
            self.assertEqual(1, len(handlers))
            self.assertEqual(
                "node chaos-engine/hooks/launch.js copilot", handlers[0]["bash"]
            )
            self.assertEqual(handlers[0]["bash"], handlers[0]["powershell"])

    def test_grok_hooks_preserve_unrelated_events(self):
        module = load(HOSTS, "chaos_engine_grok_hook_merge")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            hook_path = project / ".grok/hooks/lifecycle.json"
            hook_path.parent.mkdir(parents=True)
            original = {"hooks": {"SessionStart": [{"hooks": [{"type": "command", "command": "user-hook"}]}]}}
            hook_path.write_text(json.dumps(original), encoding="utf-8")

            module.install(project)
            merged = json.loads(hook_path.read_text())
            self.assertEqual("user-hook", merged["hooks"]["SessionStart"][0]["hooks"][0]["command"])
            self.assertGreater(len(merged["hooks"]["SessionStart"]), 1)
            self.assertIn("Stop", merged["hooks"])
            module.uninstall(project)
            self.assertEqual(original, json.loads(hook_path.read_text()))

    def test_copilot_cli_hooks_preserve_foreign_handlers_and_metadata(self):
        module = load(HOSTS, "chaos_engine_copilot_hook_merge")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            hook_path = project / ".github/hooks/chaos-engine.json"
            hook_path.parent.mkdir(parents=True)
            original = {
                "version": 1,
                "ownerMetadata": {"preserve": True},
                "hooks": {
                    "SessionStart": [
                        {"type": "command", "bash": "python tools/user-hook.py"}
                    ]
                },
            }
            hook_path.write_text(json.dumps(original), encoding="utf-8")

            module.install(project)
            merged = json.loads(hook_path.read_text())
            self.assertEqual({"preserve": True}, merged["ownerMetadata"])
            self.assertEqual(
                "python tools/user-hook.py", merged["hooks"]["SessionStart"][0]["bash"]
            )
            self.assertEqual(1, len(merged["hooks"]["SessionStart"]))
            self.assertIn("sessionStart", merged["hooks"])
            self.assertIn("preToolUse", merged["hooks"])
            module.uninstall(project)
            self.assertEqual(original, json.loads(hook_path.read_text()))

    def test_hook_cleanup_removes_only_owned_handlers_from_mixed_group(self):
        module = load(HOSTS, "chaos_engine_hook_cleanup")
        source = {
            "hooks": {
                "PreToolUse": [{
                    "matcher": "Bash",
                    "hooks": [
                        {"type": "command", "command": "python .chaos-engine/hooks/guard.py"},
                        {"type": "command", "command": "python tools/my-guard.py"},
                    ],
                }],
                "Custom": [{"hooks": [{"type": "command", "command": "python tools/scripts/agents/guard.py.backup"}]}],
            }
        }
        cleaned = json.loads(module.without_chaos_hooks(json.dumps(source).encode(), "test"))
        self.assertEqual("python tools/my-guard.py", cleaned["hooks"]["PreToolUse"][0]["hooks"][0]["command"])
        self.assertIn("Custom", cleaned["hooks"])

    def test_unrelated_claude_marketplace_coexists_and_uninstall_restores_exact_bytes(self):
        module = load(HOSTS, "chaos_engine_claude_marketplace_coexistence")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            path = project / ".claude-plugin/marketplace.json"
            path.parent.mkdir(parents=True)
            original = b'{\n  "name": "user-marketplace",\n  "owner": {"name": "User"},\n  "custom": true,\n  "plugins": [{"name": "user-plugin", "source": "./user"}]\n}\n'
            path.write_bytes(original)

            module.install(project)
            installed = json.loads(path.read_text(encoding="utf-8"))
            self.assertEqual("user-marketplace", installed["name"])
            self.assertEqual({"name": "User"}, installed["owner"])
            self.assertTrue(installed["custom"])
            self.assertEqual("user-plugin", installed["plugins"][0]["name"])
            self.assertEqual(
                ["chaos-engine", module.CAVEMAN_PLUGIN_NAME, module.PONYTAIL_PLUGIN_NAME],
                [item["name"] for item in installed["plugins"][1:]],
            )
            module.uninstall(project)
            self.assertEqual(original, path.read_bytes())

    def test_conflicting_claude_plugin_fails_closed_without_mutation(self):
        module = load(HOSTS, "chaos_engine_claude_marketplace_collision")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            path = project / ".claude-plugin/marketplace.json"
            path.parent.mkdir(parents=True)
            original = b'{"name":"user-marketplace","plugins":[{"name":"chaos-engine","source":"./foreign"}]}'
            path.write_bytes(original)
            with self.assertRaisesRegex(ValueError, "Claude marketplace collision"):
                module.install(project)
            self.assertEqual(original, path.read_bytes())
            self.assertFalse(project.joinpath(module.RECEIPT_NAME).exists())

    def test_gitignore_reincludes_tracked_memory_config_under_existing_parent_rule(self):
        module = load(HOSTS, "chaos_engine_gitignore_memory")
        before = {relative: None for relative in module.managed_paths()}
        before[".gitignore"] = b".memory/\n"

        rendered = module.desired_content(before)[".gitignore"].decode()

        self.assertLess(rendered.index("!.memory/"), rendered.index("!.memory/config.json"))

    def test_gitignore_reincludes_every_canonical_harness_root(self):
        module = load(HOSTS, "chaos_engine_gitignore_canonical_roots")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            ignored_roots = (".chaos-engine", ".agents", ".github", "plugins")
            project.joinpath(".gitignore").write_bytes(
                module.gitignore_content(
                    "".join(f"{root}/\n" for root in ignored_roots).encode()
                )
            )
            candidates = (
                ".chaos-engine/install.py",
                ".agents/skills/chaos-engine/SKILL.md",
                ".github/skills/chaos-engine/SKILL.md",
                "plugins/chaos-engine/.codex-plugin/plugin.json",
            )
            for relative in candidates:
                path = project / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("tracked\n", encoding="utf-8")
            subprocess.run(["git", "init", "-q", str(project)], check=True)

            result = subprocess.run(
                ["git", "-C", str(project), "check-ignore", *candidates],
                capture_output=True,
                text=True,
                check=False,
            )

            self.assertEqual(1, result.returncode, result.stdout)

    def test_gitignore_keeps_generated_python_bytecode_untracked(self):
        module = load(HOSTS, "chaos_engine_gitignore_bytecode")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            project.joinpath(".gitignore").write_bytes(module.gitignore_content(None))
            generated = ".chaos-engine/__pycache__/hosts.cpython-314.pyc"
            path = project / generated
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(b"generated")
            subprocess.run(["git", "init", "-q", str(project)], check=True)

            result = subprocess.run(
                ["git", "-C", str(project), "check-ignore", generated],
                capture_output=True,
                text=True,
                check=False,
            )

            self.assertEqual(0, result.returncode, result.stdout)

    def test_gitattributes_pins_canonical_harness_text_to_lf(self):
        module = load(HOSTS, "chaos_engine_gitattributes_eol")
        before = {relative: None for relative in module.managed_paths()}

        rendered = module.desired_content(before)

        self.assertIn(".gitattributes", rendered)
        attributes = rendered[".gitattributes"].decode()
        for pattern in (
            "/.chaos-engine/** text eol=lf",
            "/.agents/** text eol=lf",
            "/.memory/** text eol=lf",
            "/plugins/chaos-engine/** text eol=lf",
            "/.gitattributes text eol=lf",
        ):
            self.assertIn(pattern, attributes)

    def test_gitattributes_preserves_unrelated_rules_and_rejects_marker_drift(self):
        module = load(HOSTS, "chaos_engine_gitattributes_merge")
        before = {relative: None for relative in module.managed_paths()}
        before[".gitattributes"] = b"*.png binary\n"

        rendered = module.desired_content(before)[".gitattributes"]

        self.assertTrue(rendered.startswith(b"*.png binary\n"))
        self.assertEqual(rendered, module.gitattributes_content(rendered))
        with self.assertRaisesRegex(ValueError, "gitattributes collision"):
            module.gitattributes_content(b"# CHAOSENGINE-EOL:START\nchanged\n")

    def test_gitattributes_preserves_core_bytes_in_autocrlf_clone(self):
        module = load(HOSTS, "chaos_engine_gitattributes_clone")
        with tempfile.TemporaryDirectory() as temporary:
            source = Path(temporary) / "source"
            clone = Path(temporary) / "clone"
            skill = source / ".chaos-engine/skills/chaos-engine/SKILL.md"
            skill.parent.mkdir(parents=True)
            skill.write_bytes(b"# Canonical\n\nLine two\n")
            core = source / ".chaos-engine/install.py"
            core.write_bytes(b"print('one')\nprint('two')\n")
            module.install(source)
            subprocess.run(  # nosec B603 B607
                ["git", "init", "-q", str(source)], check=True
            )
            subprocess.run(  # nosec B603 B607
                ["git", "-C", str(source), "add", "-A"], check=True
            )
            subprocess.run(  # nosec B603 B607
                [
                    "git",
                    "-C",
                    str(source),
                    "-c",
                    "user.name=ChaosEngine test",
                    "-c",
                    "user.email=chaos-engine@example.invalid",
                    "commit",
                    "-qm",
                    "fixture",
                ],
                check=True,
            )

            subprocess.run(  # nosec B603 B607
                ["git", "-c", "core.autocrlf=true", "clone", "-q", str(source), str(clone)],
                check=True,
            )

            self.assertEqual(core.read_bytes(), clone.joinpath(".chaos-engine/install.py").read_bytes())
            self.assertNotIn(b"\r\n", clone.joinpath(".chaos-engine/install.py").read_bytes())

    def test_invalid_retrieval_configs_fail_before_mutation(self):
        module = load(HOSTS, "chaos_engine_invalid_retrieval")
        for relative, content, message in (
            (".memory/config.json", "{}", "Memory configuration"),
            (
                ".memory/config.json",
                json.dumps(
                    {
                        "version": 4,
                        "project": {"id": "project.consumer", "name": "consumer"},
                        "memory": {},
                        "git": {},
                    }
                ),
                "Memory configuration",
            ),
            ("mempalace.yaml", "arbitrary: value\n", "MemPalace configuration"),
            (
                "mempalace.yaml",
                "wing: project\nrooms:\nexclude_patterns:\n",
                "MemPalace configuration",
            ),
        ):
            with self.subTest(relative=relative), tempfile.TemporaryDirectory() as temporary:
                project = Path(temporary) / "consumer"
                project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
                project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
                path = project / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(content, encoding="utf-8")

                with self.assertRaisesRegex(ValueError, message):
                    module.install(project)
                self.assertFalse(project.joinpath(module.RECEIPT_NAME).exists())

    def test_healthy_adopter_v4_memory_and_unindented_mempalace_installs(self):
        module = load(HOSTS, "chaos_engine_healthy_v4_retrieval")
        v4_config = {
            "version": 4,
            "project": {"id": "project.itestflow-agent", "name": "iTestFlow Agent"},
            "memory": {
                "autoIndex": True,
                "defaultTokenBudget": 600,
                "saveContextPacks": False,
            },
            "git": {"trackContextPacks": False},
        }
        v4_schema = {"$id": "https://aictx.dev/schemas/v4/config.schema.json", "type": "object"}
        object_bytes = json.dumps(
            {
                "id": "architecture.adopter-core",
                "type": "architecture",
                "title": "Adopter core",
            },
            indent=2,
        ).encode() + b"\n"
        yaml_text = (
            "wing: itestflow_agent\n"
            "exclude_patterns:\n"
            "- .git/**\n"
            "- .memory/**\n"
            "rooms:\n"
            "- name: general\n"
            "  description: Project source and documentation\n"
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            project.joinpath(".memory/schema").mkdir(parents=True)
            project.joinpath(".memory/memory").mkdir()
            project.joinpath(".memory/relations").mkdir()
            project.joinpath(".memory/config.json").write_text(
                json.dumps(v4_config, indent=2) + "\n",
                encoding="utf-8",
            )
            for name in module.MEMORY_SCHEMA_FILES:
                project.joinpath(".memory/schema", name).write_text(
                    json.dumps(v4_schema, indent=2) + "\n",
                    encoding="utf-8",
                )
            object_path = project / ".memory/memory/architecture.json"
            object_path.write_bytes(object_bytes)
            project.joinpath("mempalace.yaml").write_bytes(yaml_text.encode())
            before_config = project.joinpath(".memory/config.json").read_bytes()
            before_schemas = {
                name: project.joinpath(".memory/schema", name).read_bytes()
                for name in module.MEMORY_SCHEMA_FILES
            }
            before_yaml = project.joinpath("mempalace.yaml").read_bytes()

            receipt = module.install(project)
            second = module.install(project)

            self.assertTrue(project.joinpath(module.RECEIPT_NAME).exists())
            self.assertEqual("installed", receipt["phase"])
            self.assertEqual("installed", second["phase"])
            self.assertTrue(module.retrieval_configs_healthy(project))
            migrated = json.loads(project.joinpath(".memory/config.json").read_text(encoding="utf-8"))
            self.assertEqual(5, migrated["version"])
            self.assertEqual(v4_config["project"], migrated["project"])
            self.assertEqual(v4_config["memory"]["autoIndex"], migrated["memory"]["autoIndex"])
            self.assertEqual(
                v4_config["memory"]["defaultTokenBudget"],
                migrated["memory"]["defaultTokenBudget"],
            )
            self.assertNotIn("git", migrated)
            self.assertNotIn("saveContextPacks", migrated["memory"])
            schema_root = Path(module.memory_schema_assets())
            for name in module.MEMORY_SCHEMA_FILES:
                self.assertEqual(
                    (schema_root / name).read_bytes(),
                    project.joinpath(".memory/schema", name).read_bytes(),
                )
            self.assertEqual(object_bytes, object_path.read_bytes())
            self.assertEqual(before_yaml, project.joinpath("mempalace.yaml").read_bytes())
            self.assertTrue(before_yaml.startswith(b"wing: itestflow_agent\nexclude_patterns:\n"))
            self.assertEqual(4, json.loads(before_config)["version"])

    def test_repository_remote_defines_identity_in_a_named_worktree(self):
        module = load(HOSTS, "chaos_engine_repository_identity")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "feature-worktree"
            project.mkdir()
            subprocess.run(["git", "init", "-q", str(project)], check=True)
            subprocess.run(
                [
                    "git",
                    "-C",
                    str(project),
                    "remote",
                    "add",
                    "origin",
                    "ssh://git.example/team/actual-project.git",
                ],
                check=True,
            )
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")

            module.install(project)

            memory = json.loads(project.joinpath(".memory/config.json").read_text())
            self.assertEqual("actual-project", memory["project"]["name"])
            self.assertEqual(
                "actual_project_main",
                module.default_mempalace_wing("actual-project"),
            )
            self.assertIn(
                "wing: actual_project_main",
                project.joinpath("mempalace.yaml").read_text(),
            )

    def test_retrieval_runtime_executes_memory_status_and_check(self):
        module = load(HOSTS, "chaos_engine_retrieval_runtime")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            project.joinpath(".chaos-engine").mkdir()
            project.joinpath(".chaos-engine/tool.py").write_text("# owned\n")
            responses = [
                mock.Mock(returncode=0, stdout=json.dumps({"ok": True})),
                mock.Mock(
                    returncode=0,
                    stdout=json.dumps({"ok": True, "data": {"valid": True}}),
                ),
            ]
            with mock.patch.object(module.subprocess, "run", side_effect=responses) as run:
                self.assertTrue(module.retrieval_runtime_healthy(project))

            self.assertEqual(2, run.call_count)
            self.assertEqual(project, run.call_args_list[0].kwargs["cwd"])

            invalid = mock.Mock(
                returncode=0,
                stdout=json.dumps({"ok": True, "data": {"valid": False}}),
            )
            with mock.patch.object(
                module.subprocess,
                "run",
                side_effect=[responses[0], invalid],
            ):
                self.assertFalse(module.retrieval_runtime_healthy(project))

    def test_mcp_runtime_executes_both_initialize_handshakes(self):
        module = load(HOSTS, "chaos_engine_mcp_runtime")
        memory_response = mock.Mock(
            returncode=0,
            stdout="\n".join((
                json.dumps({"jsonrpc": "2.0", "id": 1, "result": {}}),
                json.dumps({"jsonrpc": "2.0", "id": 2, "result": {"tools": []}}),
            )) + "\n",
        )
        mempalace_response = mock.Mock(
            returncode=0,
            stdout="\n".join(
                (
                    json.dumps({"jsonrpc": "2.0", "id": 1, "result": {}}),
                    json.dumps(
                        {
                            "jsonrpc": "2.0",
                            "id": 2,
                            "result": {"tools": []},
                        }
                    ),
                )
            )
            + "\n",
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            project.joinpath(".chaos-engine").mkdir()
            project.joinpath(".chaos-engine/tool.py").write_text("# owned\n")
            module.initialize_mempalace_runtime(project)
            with mock.patch.object(
                module.subprocess,
                "run",
                side_effect=[memory_response, mempalace_response],
            ) as run:
                self.assertTrue(module.mcp_runtime_healthy(project))

            self.assertEqual(2, run.call_count)
            self.assertEqual(
                ["--backend", "sqlite_exact"],
                run.call_args_list[1].args[0][-2:],
            )
            for call in run.call_args_list:
                requests = [
                    json.loads(line) for line in call.kwargs["input"].splitlines()
                ]
                self.assertEqual("initialize", requests[0]["method"])
                self.assertEqual(project, call.kwargs["cwd"])
                self.assertEqual("1", call.kwargs["env"]["PYTHONDONTWRITEBYTECODE"])
            mempalace_requests = run.call_args_list[1].kwargs["input"].splitlines()
            self.assertEqual(3, len(mempalace_requests))
            self.assertEqual(
                "tools/list",
                json.loads(mempalace_requests[2])["method"],
            )

            backend_error = mock.Mock(
                returncode=0,
                stdout="\n".join(
                    (
                        json.dumps({"jsonrpc": "2.0", "id": 1, "result": {}}),
                        json.dumps(
                            {
                                "jsonrpc": "2.0",
                                "id": 2,
                                "result": {
                                    "content": [
                                        {
                                            "type": "text",
                                            "text": json.dumps(
                                                {
                                                    "error": "Backend open failed",
                                                    "backend": "sqlite_exact",
                                                }
                                            ),
                                        }
                                    ]
                                },
                            }
                        ),
                    )
                ),
            )
            with mock.patch.object(
                module.subprocess,
                "run",
                side_effect=[memory_response, backend_error],
            ):
                self.assertFalse(module.mcp_runtime_healthy(project))

            invalid = mock.Mock(returncode=0, stdout="not-json\n")
            with mock.patch.object(module.subprocess, "run", return_value=invalid):
                self.assertFalse(module.mcp_runtime_healthy(project))

    def test_doctor_mcp_probe_uses_configured_cwd_and_managed_python(self):
        module = load(HOSTS, "chaos_engine_doctor_configured_mcp")
        response = mock.Mock(
            returncode=0,
            stdout="\n".join((
                json.dumps({"jsonrpc": "2.0", "id": 1, "result": {}}),
                json.dumps({"jsonrpc": "2.0", "id": 2, "result": {"tools": []}}),
            )) + "\n",
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            tool = project / ".chaos-engine/tool.py"
            tool.parent.mkdir()
            tool.write_text("# owned\n", encoding="utf-8")
            module.initialize_mempalace_runtime(project)
            managed_python = project / "managed/python"
            managed_python.parent.mkdir()
            managed_python.write_bytes(b"python")
            with mock.patch.object(
                module.subprocess,
                "run",
                side_effect=[response, mock.Mock(
                    returncode=0,
                    stdout="\n".join((
                        response.stdout.strip(),
                        json.dumps({"jsonrpc": "2.0", "id": 2,
                                    "result": {"tools": []}}),
                    )) + "\n",
                )],
            ) as run:
                self.assertTrue(module.mcp_runtime_healthy(project, managed_python))

            self.assertEqual(2, run.call_count)
            for call in run.call_args_list:
                self.assertEqual(managed_python, Path(call.args[0][0]))
                self.assertEqual(project, call.kwargs["cwd"])
            self.assertEqual(str(tool), run.call_args_list[0].args[0][1])

    def test_hook_runtime_probe_executes_prompt_pre_and_post_with_managed_python(self):
        module = load(HOSTS, "chaos_engine_doctor_hook_events")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            guard = project / ".chaos-engine/hooks/guard.py"
            guard.parent.mkdir(parents=True)
            guard.write_text("# owned\n", encoding="utf-8")
            managed_python = project / "managed/python"
            managed_python.parent.mkdir()
            managed_python.write_bytes(b"python")
            result = mock.Mock(returncode=0, stdout="{}\n")
            with mock.patch.object(module.subprocess, "run", return_value=result) as run:
                self.assertTrue(module.hook_runtime_healthy(project, managed_python))

            self.assertEqual(3, run.call_count)
            payloads = [json.loads(call.kwargs["input"]) for call in run.call_args_list]
            self.assertEqual(
                ["UserPromptSubmit", "PreToolUse", "PostToolUse"],
                [payload["hook_event_name"] for payload in payloads],
            )
            self.assertNotIn("tool_name", payloads[0])
            for call, payload in zip(run.call_args_list, payloads):
                self.assertEqual([str(managed_python), str(guard)], call.args[0])
                self.assertEqual(project, call.kwargs["cwd"])
                if payload["hook_event_name"] != "UserPromptSubmit":
                    self.assertEqual("Bash", payload["tool_name"])

    def test_mempalace_runtime_state_is_fail_closed_before_native_launch(self):
        module = load(HOSTS, "chaos_engine_mempalace_runtime_state")
        self.assertTrue(hasattr(module, "mempalace_runtime_status"))
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            palace = project / ".chaos-engine-state/mempalace"

            self.assertEqual(
                "initialization-required",
                module.mempalace_runtime_status(project)["status"],
            )

            palace.mkdir(parents=True)
            exact = palace / "sqlite_exact.sqlite3"
            wal = Path(f"{exact}-wal")
            shared_memory = Path(f"{exact}-shm")
            wal.write_bytes(b"orphan WAL")
            shared_memory.write_bytes(b"orphan shared memory")
            self.assertEqual(
                "recovery-required",
                module.mempalace_runtime_status(project)["status"],
            )
            wal.unlink()
            shared_memory.unlink()

            database = sqlite3.connect(exact)
            database.execute("CREATE TABLE unrelated (secret TEXT)")
            database.commit()
            database.close()
            self.assertEqual(
                "recovery-required",
                module.mempalace_runtime_status(project)["status"],
            )
            exact.unlink()

            database = sqlite3.connect(exact)
            database.executescript(
                """
                CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT);
                CREATE TABLE collections (
                    id INTEGER PRIMARY KEY, name TEXT, dimension INTEGER,
                    created_at TEXT
                );
                CREATE TABLE documents (
                    collection_id INTEGER, id TEXT, document TEXT,
                    metadata_json TEXT, embedding BLOB, dim INTEGER,
                    created_at TEXT, updated_at TEXT,
                    PRIMARY KEY (collection_id, id)
                );
                INSERT INTO collections(name, created_at)
                VALUES ('mempalace_drawers', '2026-08-15T00:00:00Z');
                """
            )
            database.commit()
            database.close()
            self.assertEqual(
                "recovery-required",
                module.mempalace_runtime_status(project)["status"],
            )
            exact.unlink()

            create_sqlite_exact_state(exact)
            self.assertFalse(wal.exists())
            self.assertFalse(shared_memory.exists())
            self.assertEqual(
                "healthy",
                module.mempalace_runtime_status(project)["status"],
            )
            self.assertFalse(wal.exists())
            self.assertFalse(shared_memory.exists())

            mined = palace / ".mined"
            mined.write_text("current\n", encoding="utf-8")
            self.assertEqual(
                "healthy",
                module.mempalace_runtime_status(project)["status"],
            )
            mined.write_text("unexpected\n", encoding="utf-8")
            self.assertEqual(
                "recovery-required",
                module.mempalace_runtime_status(project)["status"],
            )
            mined.unlink()

            database = sqlite3.connect(exact)
            database.execute("DROP INDEX idx_documents_collection")
            database.commit()
            database.close()
            self.assertEqual(
                "recovery-required",
                module.mempalace_runtime_status(project)["status"],
            )
            exact.unlink()
            create_sqlite_exact_state(exact)

            wal.write_bytes(b"partial sidecar")
            self.assertEqual(
                "recovery-required",
                module.mempalace_runtime_status(project)["status"],
            )
            self.assertFalse(shared_memory.exists())
            wal.unlink()

            writer = sqlite3.connect(exact)
            try:
                writer.execute("INSERT INTO meta(key, value) VALUES ('live-wal', '1')")
                writer.commit()
                self.assertTrue(wal.is_file())
                self.assertTrue(shared_memory.is_file())
                self.assertEqual(
                    "healthy",
                    module.mempalace_runtime_status(project)["status"],
                )
            finally:
                writer.close()

            exact.write_bytes(b"SQLite format 3\x00" + b"truncated")
            self.assertEqual(
                "recovery-required",
                module.mempalace_runtime_status(project)["status"],
            )

            exact.unlink()
            chroma = palace / "chroma.sqlite3"
            chroma.write_bytes(b"SQLite format 3\x00")
            self.assertEqual(
                "recovery-required",
                module.mempalace_runtime_status(project)["status"],
            )
            chroma.unlink()

            create_chroma_state(chroma)
            (palace / "00000000-0000-0000-0000-000000000001").mkdir()
            state = module.mempalace_runtime_status(project)
            self.assertEqual("migration-required", state["status"])
            self.assertIn("Chroma", state["detail"])

            with mock.patch.object(module.subprocess, "run") as run:
                self.assertFalse(module.mcp_runtime_healthy(project))
            run.assert_not_called()

            unknown = palace / "unknown.sqlite3"
            unknown.write_bytes(b"preserve")
            self.assertEqual(
                "recovery-required",
                module.mempalace_runtime_status(project)["status"],
            )
            unknown.unlink()

            create_sqlite_exact_state(exact)
            self.assertEqual(
                "recovery-required",
                module.mempalace_runtime_status(project)["status"],
            )

    def test_mempalace_runtime_accepts_sqlite_exact_origin_sidecar(self):
        module = load(HOSTS, "chaos_engine_mempalace_origin_sidecar")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            palace = project / ".chaos-engine-state/mempalace"
            palace.mkdir(parents=True)
            create_sqlite_exact_state(palace / "sqlite_exact.sqlite3")
            sidecar = palace / ".mempalace"
            sidecar.mkdir()
            (sidecar / "origin.json").write_text("{}", encoding="utf-8")

            self.assertEqual(
                "healthy",
                module.mempalace_runtime_status(project)["status"],
            )

            (sidecar / "unknown.sqlite3").write_bytes(b"nope")
            self.assertEqual(
                "recovery-required",
                module.mempalace_runtime_status(project)["status"],
            )
            (sidecar / "unknown.sqlite3").unlink()
            (palace / "unknown.sqlite3").write_bytes(b"nope")
            self.assertEqual(
                "recovery-required",
                module.mempalace_runtime_status(project)["status"],
            )

    def test_mempalace_runtime_rejects_reparse_state_before_native_launch(self):
        module = load(HOSTS, "chaos_engine_mempalace_reparse_state")
        self.assertTrue(hasattr(module, "mempalace_runtime_status"))
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            palace = project / ".chaos-engine-state/mempalace"
            palace.mkdir(parents=True)
            exact = palace / "sqlite_exact.sqlite3"
            exact.write_bytes(b"SQLite format 3\x00")

            with mock.patch.object(
                module,
                "is_link_or_reparse",
                side_effect=lambda path: path == exact,
            ):
                state = module.mempalace_runtime_status(project)

            self.assertEqual("recovery-required", state["status"])
            self.assertIn("link or reparse", state["detail"])

    def test_mempalace_runtime_rejects_unrecognized_state_before_native_launch(self):
        module = load(HOSTS, "chaos_engine_mempalace_unknown_state")
        self.assertTrue(hasattr(module, "mempalace_runtime_status"))
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            palace = project / ".chaos-engine-state/mempalace"
            palace.mkdir(parents=True)

            for relative, content in (
                ("legacy-segment-uuid", None),
                ("chroma.sqlite3-wal", b"recoverable legacy WAL"),
                ("unknown.sqlite3", b"recoverable unknown database"),
            ):
                path = palace / relative
                if content is None:
                    path.mkdir()
                else:
                    path.write_bytes(content)
                try:
                    state = module.mempalace_runtime_status(project)
                    self.assertEqual("recovery-required", state["status"])
                    self.assertIn("unrecognized", state["detail"])
                    with mock.patch.object(module.subprocess, "run") as run:
                        self.assertFalse(module.mcp_runtime_healthy(project))
                    run.assert_not_called()
                finally:
                    if path.is_dir():
                        path.rmdir()
                    else:
                        path.unlink()

    def test_fresh_mempalace_state_initializes_once_without_claiming_user_data(self):
        module = load(HOSTS, "chaos_engine_mempalace_initialize")
        self.assertTrue(hasattr(module, "initialize_mempalace_runtime"))
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            self.assertEqual(
                "initialization-required",
                module.mempalace_runtime_status(project)["status"],
            )

            module.initialize_mempalace_runtime(project)
            database = project / ".chaos-engine-state/mempalace/sqlite_exact.sqlite3"
            self.assertTrue(database.is_file())
            self.assertEqual(
                "healthy",
                module.mempalace_runtime_status(project)["status"],
            )
            before = database.read_bytes()
            module.initialize_mempalace_runtime(project)
            self.assertEqual(before, database.read_bytes())

            database.unlink()
            (database.parent / "user-owned.bin").write_bytes(b"preserve")
            module.initialize_mempalace_runtime(project)
            self.assertEqual(
                b"preserve",
                (database.parent / "user-owned.bin").read_bytes(),
            )

    def test_fresh_mempalace_initializer_revalidates_paths_after_creation(self):
        module = load(HOSTS, "chaos_engine_mempalace_initialize_race")
        self.assertTrue(hasattr(module, "initialize_mempalace_runtime"))
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            palace = project / ".chaos-engine-state/mempalace"
            real_validate = module.validate_path
            calls = 0

            def reject_post_creation(root, path):
                nonlocal calls
                calls += 1
                real_validate(root, path)
                if calls == 2:
                    raise ValueError("simulated link or reparse point swap")

            with mock.patch.object(
                module,
                "validate_path",
                side_effect=reject_post_creation,
            ):
                with self.assertRaisesRegex(ValueError, "link or reparse"):
                    module.initialize_mempalace_runtime(project)

            self.assertFalse((palace / "sqlite_exact.sqlite3").exists())

    def test_shaft_resolver_with_healthy_central_palace_is_healthy_and_skips_checkout_mcp(self):
        module = load(HOSTS, "chaos_engine_mempalace_central_healthy")
        with tempfile.TemporaryDirectory() as temporary:
            shaft = Path(temporary) / "shaft"
            palace = Path(temporary) / "shared-palace"
            palace.mkdir(parents=True)
            create_sqlite_exact_state(palace / "sqlite_exact.sqlite3")
            (palace / ".mempalace").mkdir()
            (palace / ".mempalace" / "origin.json").write_text("{}", encoding="utf-8")
            resolver = shaft / "tools/repository-map/resolve_mempalace.py"
            resolver.parent.mkdir(parents=True)
            resolver.write_text(
                "from pathlib import Path\nprint(Path(r'%s').resolve())\n"
                % str(palace).replace("\\", "\\\\"),
                encoding="utf-8",
            )

            state = module.mempalace_runtime_status(shaft)
            self.assertEqual("healthy", state["status"])
            self.assertEqual("sqlite_exact", state["backend"])

            module.initialize_mempalace_runtime(shaft)
            self.assertFalse((shaft / ".chaos-engine-state/mempalace").exists())
            shaft.joinpath(".chaos-engine").mkdir()
            shaft.joinpath(".chaos-engine/tool.py").write_text("# owned\n")
            memory_ok = mock.Mock(
                returncode=0,
                stdout="\n".join((
                    json.dumps({"jsonrpc": "2.0", "id": 1, "result": {}}),
                    json.dumps({"jsonrpc": "2.0", "id": 2,
                                "result": {"tools": []}}),
                )) + "\n",
            )
            real_run = module.subprocess.run

            def runner(command, **kwargs):
                if len(command) >= 2 and str(command[1]).endswith("resolve_mempalace.py"):
                    return real_run(command, **kwargs)
                return memory_ok

            with mock.patch.object(module.subprocess, "run", side_effect=runner) as run:
                self.assertTrue(module.mcp_runtime_healthy(shaft))
            launched = [
                call.args[0]
                for call in run.call_args_list
                if call.args and "memory-mcp" in call.args[0]
            ]
            self.assertEqual(1, len(launched))
            self.assertTrue(
                all("mempalace-mcp" not in call.args[0] for call in run.call_args_list)
            )

    def test_shaft_resolver_with_empty_central_palace_is_degraded_and_does_not_initialize(self):
        module = load(HOSTS, "chaos_engine_mempalace_central_empty")
        with tempfile.TemporaryDirectory() as temporary:
            shaft = Path(temporary) / "shaft"
            palace = Path(temporary) / "shared-palace"
            palace.mkdir(parents=True)
            resolver = shaft / "tools/repository-map/resolve_mempalace.py"
            resolver.parent.mkdir(parents=True)
            resolver.write_text(
                "from pathlib import Path\nprint(Path(r'%s').resolve())\n"
                % str(palace).replace("\\", "\\\\"),
                encoding="utf-8",
            )

            state = module.mempalace_runtime_status(shaft)
            self.assertEqual("degraded", state["status"])
            module.initialize_mempalace_runtime(shaft)
            self.assertFalse((shaft / ".chaos-engine-state/mempalace").exists())
            self.assertEqual([], list(palace.iterdir()))

    def test_shaft_resolver_without_checkout_palace_is_degraded_and_does_not_initialize(self):
        module = load(HOSTS, "chaos_engine_mempalace_shaft_resolver")
        with tempfile.TemporaryDirectory() as temporary:
            shaft = Path(temporary) / "shaft"
            resolver = shaft / "tools/repository-map/resolve_mempalace.py"
            resolver.parent.mkdir(parents=True)
            resolver.write_text("# fixture SHAFT resolver\n", encoding="utf-8")

            state = module.mempalace_runtime_status(shaft)
            self.assertEqual("degraded", state["status"])
            self.assertIn("scripts/agents/knowledge_stores.py status", state["detail"])
            self.assertIn("centralized", state["detail"].lower())
            self.assertNotIn(".git", state["detail"])

            module.initialize_mempalace_runtime(shaft)
            self.assertFalse((shaft / ".chaos-engine-state/mempalace").exists())

            portable = Path(temporary) / "portable"
            portable.mkdir()
            self.assertEqual(
                "initialization-required",
                module.mempalace_runtime_status(portable)["status"],
            )

    def test_shaft_resolver_with_empty_checkout_palace_is_degraded_and_does_not_initialize(self):
        module = load(HOSTS, "chaos_engine_mempalace_empty_checkout")
        with tempfile.TemporaryDirectory() as temporary:
            shaft = Path(temporary) / "shaft"
            resolver = shaft / "tools/repository-map/resolve_mempalace.py"
            resolver.parent.mkdir(parents=True)
            resolver.write_text("# fixture SHAFT resolver\n", encoding="utf-8")
            palace = shaft / ".chaos-engine-state/mempalace"
            palace.mkdir(parents=True)

            state = module.mempalace_runtime_status(shaft)
            self.assertEqual("degraded", state["status"])
            self.assertIn("scripts/agents/knowledge_stores.py status", state["detail"])
            self.assertIn("centralized", state["detail"].lower())
            self.assertNotIn(".git", state["detail"])

            module.initialize_mempalace_runtime(shaft)
            self.assertFalse((palace / "sqlite_exact.sqlite3").exists())
            self.assertEqual([], list(palace.iterdir()))

            portable = Path(temporary) / "portable"
            (portable / ".chaos-engine-state/mempalace").mkdir(parents=True)
            self.assertEqual(
                "initialization-required",
                module.mempalace_runtime_status(portable)["status"],
            )

    def test_tool_guard_refuses_shaft_degraded_mempalace_and_names_knowledge_stores(self):
        hosts = load(HOSTS, "chaos_engine_mempalace_shaft_guard_hosts")
        tool = load(TOOL, "chaos_engine_mempalace_shaft_guard_tool")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            resolver = project / "tools/repository-map/resolve_mempalace.py"
            resolver.parent.mkdir(parents=True)
            resolver.write_text("# fixture SHAFT resolver\n", encoding="utf-8")
            arguments = [
                "--palace",
                ".chaos-engine-state/mempalace",
                "--backend",
                "sqlite_exact",
            ]
            with mock.patch.object(
                tool,
                "load_host_controller",
                return_value=hosts.__dict__,
            ):
                with self.assertRaisesRegex(ValueError, r"knowledge_stores\.py"):
                    tool.guard_mempalace_mcp(project / ".chaos-engine", arguments)

    def test_hosts_module_source_has_no_portable_forbidden_tokens(self):
        catalog = json.loads((ROOT / "chaos-engine/distributions.json").read_text(encoding="utf-8"))
        tokens = catalog["distributions"]["portable"]["forbiddenTokens"]
        text = HOSTS.read_text(encoding="utf-8").casefold()
        for token in tokens:
            with self.subTest(token=token):
                self.assertNotIn(str(token).casefold(), text)

    def test_launcher_rendering_is_explicit_for_windows_and_posix(self):
        module = load(HOSTS, "chaos_engine_hosts")

        windows = module.owned_servers("nt")
        posix = module.owned_servers("posix")

        self.assertEqual(windows, posix)
        for name in ("chaosengine-memory", "chaosengine-mempalace"):
            server = windows[name]
            self.assertEqual("python3", server["command"])
            self.assertNotEqual("-3", server["args"][0])
            self.assertEqual("py", server["commandWindows"])
            self.assertEqual("-3", server["argsWindows"][0])

    def test_windows_install_writes_portable_mcp_launchers(self):
        module = load(HOSTS, "chaos_engine_hosts_portable_mcp")
        with mock.patch.object(module.os, "name", "nt"):
            windows_json = json.loads(module.json_content(None))["mcpServers"]
            windows_codex = module.codex_content(None).decode("utf-8")
        with mock.patch.object(module.os, "name", "posix"):
            posix_json = json.loads(module.json_content(None))["mcpServers"]
            posix_codex = module.codex_content(None).decode("utf-8")

        self.assertEqual(windows_json, posix_json)
        self.assertEqual(windows_codex, posix_codex)
        self.assertEqual(
            {"url": "https://mcp.context7.com/mcp"}, windows_json["context7"]
        )
        for name in ("chaosengine-memory", "chaosengine-mempalace"):
            server = windows_json[name]
            self.assertEqual("python3", server["command"], name)
            self.assertNotEqual("-3", server["args"][0], name)
            self.assertIn(".chaos-engine/tool.py", server["args"], name)
            self.assertEqual("py", server["commandWindows"], name)
            self.assertEqual("-3", server["argsWindows"][0], name)
            self.assertIn(".chaos-engine/tool.py", server["argsWindows"], name)
        self.assertIn('command = "python3"', windows_codex)
        self.assertNotIn('command = "py"', windows_codex)
        self.assertIn('commandWindows = "py"', windows_codex)
        self.assertIn('"-3", ".chaos-engine/tool.py"', windows_codex)
        self.assertIn('cwd = "."', windows_codex)
        self.assertNotIn('cwd = ".."', windows_codex)

    def test_legacy_os_baked_mcp_servers_are_replaced_not_collided(self):
        module = load(HOSTS, "chaos_engine_hosts_legacy_mcp_launch")
        desired = json.loads(module.json_content(None))
        desired_codex = module.codex_content(None)
        for platform in ("nt", "posix"):
            legacy = {
                "mcpServers": {
                    name: module.legacy_owned_python_server(name, platform)
                    for name in ("chaosengine-memory", "chaosengine-mempalace")
                }
            }
            upgraded = json.loads(module.json_content(json.dumps(legacy).encode()))
            self.assertEqual(desired, upgraded)
            legacy_codex = module.legacy_codex_python_block(platform).encode()
            self.assertEqual(desired_codex, module.codex_content(legacy_codex))
            self.assertEqual(
                desired_codex,
                module.codex_content(legacy_codex.replace(b"\n", b"\r\n")),
            )

    def test_exact_legacy_mcp_aliases_migrate_and_unknown_servers_survive(self):
        module = load(HOSTS, "chaos_engine_hosts_legacy_aliases")
        legacy = {
            "mcpServers": {
                "shaft-memory": {"command": "/usr/bin/memory-mcp", "args": []},
                "mempalace": {
                    "command": "/usr/bin/mempalace-mcp",
                    "args": ["--palace", ".chaos-engine-state/mempalace"],
                },
                "context7": {"command": "npx", "args": ["-y", "@upstash/context7-mcp"]},
                "user-owned": {"command": "keep-me", "args": ["--exact"]},
            }
        }

        rendered = json.loads(module.json_content(json.dumps(legacy).encode()))

        self.assertNotIn("shaft-memory", rendered["mcpServers"])
        self.assertNotIn("mempalace", rendered["mcpServers"])
        self.assertEqual(
            {"command": "keep-me", "args": ["--exact"]},
            rendered["mcpServers"]["user-owned"],
        )
        self.assertEqual(
            {"url": "https://mcp.context7.com/mcp"},
            rendered["mcpServers"]["context7"],
        )

    def test_account_mcp_servers_use_resolved_absolute_executables(self):
        module = load(HOSTS, "chaos_engine_hosts_account_mcp")
        commands = {
            "memory-mcp": "/home/user tools/bin/memory-mcp",
            "mempalace-mcp": "/home/user tools/bin/mempalace-mcp",
        }

        servers = module.owned_servers(account_commands=commands)

        self.assertEqual(commands["memory-mcp"], servers["chaosengine-memory"]["command"])
        self.assertEqual([], servers["chaosengine-memory"]["args"])
        self.assertEqual(commands["mempalace-mcp"], servers["chaosengine-mempalace"]["command"])
        self.assertEqual(".", servers["chaosengine-mempalace"]["cwd"])

    def test_native_maven_tools_runtime_uses_resolved_host_paths(self):
        module = load(HOSTS, "chaos_engine_hosts_native_maven")
        java = Path(r"C:\runtime\jdk-25\bin\java.exe")
        jar = Path(r"C:\runtime\maven-tools-mcp\3.2.0\server.jar")

        servers = module.owned_servers("nt", maven_runtime=(java, jar))
        maven = servers["maven-tools-mcp"]

        self.assertEqual(str(java), maven["command"])
        self.assertEqual(
            ["-jar", str(jar)],
            maven["args"],
        )
        self.assertNotEqual("docker", Path(str(maven["command"])).name.casefold())

    def test_explicit_maven_tools_docker_mode_pins_resolved_image(self):
        module = load(HOSTS, "chaos_engine_hosts_docker_maven")
        servers = module.owned_servers(
            maven_docker=("/usr/bin/docker", "arvindand/maven-tools-mcp:3.2.1")
        )
        self.assertEqual(
            {
                "command": "/usr/bin/docker",
                "args": ["run", "-i", "--rm", "arvindand/maven-tools-mcp:3.2.1"],
            },
            servers["maven-tools-mcp"],
        )

    def test_native_maven_tools_runtime_is_rendered_for_both_host_configs(self):
        module = load(HOSTS, "chaos_engine_hosts_native_maven_configs")
        java = Path(r"C:\runtime\jdk-25\bin\java.exe")
        jar = Path(r"C:\runtime\maven-tools-mcp\3.2.0\server.jar")
        before = {relative: None for relative in module.managed_paths()}

        after = module.desired_content(before, maven_runtime=(java, jar))
        claude = json.loads(after[".mcp.json"])
        codex = after[".codex/config.toml"].decode("utf-8")

        self.assertEqual(str(java), claude["mcpServers"]["maven-tools-mcp"]["command"])
        self.assertIn(str(jar).replace("\\", "\\\\"), codex)
        self.assertNotIn("spring.profiles.active", codex)

    def test_native_maven_tools_runtime_discovers_user_paths(self):
        module = load(HOSTS, "chaos_engine_hosts_native_maven_discovery")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            java = root / "jdk-25/bin/java.exe"
            jar = (
                root
                / "data/ChaosEngine/tools/maven-tools-mcp/3.2.0"
                / "maven-tools-mcp-3.2.0.jar"
            )
            java.parent.mkdir(parents=True)
            jar.parent.mkdir(parents=True)
            java.write_bytes(b"java")
            jar.write_bytes(b"jar")
            jar.with_name("install-receipt.json").write_text(
                json.dumps(
                    {
                        "version": "3.2.0",
                        "commit": "4475ff6c61f23ea9a93cb6d5665a63235ef2ef36",
                        "jar": jar.name,
                        "sha256": hashlib.sha256(jar.read_bytes()).hexdigest(),
                    }
                ),
                encoding="utf-8",
            )
            globals_ = module.discover_maven_tools_runtime.__globals__
            prior = globals_["java_major"]
            globals_["java_major"] = lambda candidate: 25 if candidate == java.resolve() else None
            try:
                with mock.patch.dict(
                    os.environ,
                    {
                        "CHAOSENGINE_JAVA": str(java),
                        "CHAOSENGINE_MAVEN_TOOLS_MCP_JAR": "",
                    },
                    clear=False,
                ), mock.patch.object(
                    module, "maven_tools_data_root", return_value=root / "data"
                ):
                    self.assertEqual(
                        (java.resolve(), jar.resolve()),
                        module.discover_maven_tools_runtime(),
                    )
            finally:
                globals_["java_major"] = prior

    def test_posix_runtime_ignores_localappdata_when_xdg_is_set(self):
        module = load(HOSTS, "chaos_engine_hosts_native_maven_posix_data_root")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            java = root / "jdk-25/bin/java"
            jar = (
                root
                / "xdg-data/ChaosEngine/tools/maven-tools-mcp/3.2.0"
                / "maven-tools-mcp-3.2.0.jar"
            )
            java.parent.mkdir(parents=True)
            jar.parent.mkdir(parents=True)
            java.write_bytes(b"java")
            jar.write_bytes(b"jar")
            jar.with_name("install-receipt.json").write_text(
                json.dumps(
                    {
                        "version": "3.2.0",
                        "commit": "4475ff6c61f23ea9a93cb6d5665a63235ef2ef36",
                        "jar": jar.name,
                        "sha256": hashlib.sha256(jar.read_bytes()).hexdigest(),
                    }
                ),
                encoding="utf-8",
            )

            with mock.patch.dict(
                os.environ,
                {
                    "LOCALAPPDATA": str(root / "windows-data"),
                    "XDG_DATA_HOME": str(root / "xdg-data"),
                    "CHAOSENGINE_JAVA": str(java),
                    "CHAOSENGINE_MAVEN_TOOLS_MCP_JAR": "",
                    "JAVA_HOME": "",
                },
                clear=False,
            ), mock.patch.object(module.os, "name", "posix"), mock.patch.object(
                module, "Path", type(root)
            ), mock.patch.object(module, "java_major", return_value=25):
                self.assertEqual(
                    (java.resolve(), jar.resolve()),
                    module.discover_maven_tools_runtime(),
                )

    def test_native_maven_tools_runtime_rejects_unreceipted_or_changed_jar(self):
        module = load(HOSTS, "chaos_engine_hosts_native_maven_receipt")
        with tempfile.TemporaryDirectory() as temporary:
            jar = Path(temporary) / "maven-tools-mcp-3.2.0.jar"
            jar.write_bytes(b"not-a-jar")
            self.assertIsNone(module.verified_maven_tools_jar(jar))

            jar.with_name("install-receipt.json").write_text(
                json.dumps(
                    {
                        "version": "3.2.0",
                        "commit": "4475ff6c61f23ea9a93cb6d5665a63235ef2ef36",
                        "jar": jar.name,
                        "sha256": hashlib.sha256(jar.read_bytes()).hexdigest(),
                    }
                ),
                encoding="utf-8",
            )
            self.assertEqual(jar.resolve(), module.verified_maven_tools_jar(jar))
            jar.write_bytes(b"changed")
            self.assertIsNone(module.verified_maven_tools_jar(jar))

    def test_maven_tools_cache_status_and_purge_are_receipt_bounded(self):
        module = load(HOSTS, "chaos_engine_hosts_maven_cache")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "cache"
            version = root / module.MAVEN_TOOLS_MCP_VERSION
            jar = version / f"maven-tools-mcp-{module.MAVEN_TOOLS_MCP_VERSION}.jar"

            self.assertEqual("absent", module.maven_tools_cache_status(root=root)["status"])
            version.mkdir(parents=True)
            jar.write_bytes(b"jar")
            (version / module.MAVEN_TOOLS_MCP_RECEIPT).write_text(
                json.dumps({
                    "version": module.MAVEN_TOOLS_MCP_VERSION,
                    "commit": module.MAVEN_TOOLS_MCP_COMMIT,
                    "jar": jar.name,
                    "sha256": hashlib.sha256(jar.read_bytes()).hexdigest(),
                }),
                encoding="utf-8",
            )

            self.assertEqual("healthy", module.maven_tools_cache_status(root=root)["status"])
            (version / "unknown.txt").write_text("unknown", encoding="utf-8")
            self.assertEqual("invalid", module.maven_tools_cache_status(root=root)["status"])
            with self.assertRaisesRegex(ValueError, "unknown"):
                module.purge_maven_tools_cache(module.MAVEN_TOOLS_MCP_VERSION, root=root)
            (version / "unknown.txt").unlink()
            self.assertEqual("purged", module.purge_maven_tools_cache(module.MAVEN_TOOLS_MCP_VERSION, root=root)["status"])
            self.assertEqual("absent", module.purge_maven_tools_cache(module.MAVEN_TOOLS_MCP_VERSION, root=root)["status"])

    def test_maven_tools_cache_reports_nonwaiting_lock_contention(self):
        module = load(HOSTS, "chaos_engine_hosts_maven_cache_lock")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "cache"
            root.mkdir()
            with module.maven_tools_cache_lock(root):
                self.assertEqual("busy", module.maven_tools_cache_status(root=root)["status"])
                with self.assertRaisesRegex(RuntimeError, "already running"):
                    module.purge_maven_tools_cache(module.MAVEN_TOOLS_MCP_VERSION, root=root)

    def test_maven_tools_cache_status_maps_inaccessible_lock_to_invalid(self):
        module = load(HOSTS, "chaos_engine_hosts_maven_cache_inaccessible")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "cache"
            root.mkdir()
            with mock.patch.object(module.os, "open", side_effect=PermissionError("denied")):
                observed = module.maven_tools_cache_status(root=root)
            self.assertEqual("invalid", observed["status"])
            self.assertNotIn("denied", json.dumps(observed))

    def test_maven_tools_cache_status_maps_accessibility_race_to_invalid(self):
        module = load(HOSTS, "chaos_engine_hosts_maven_cache_accessibility_race")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "cache"
            with mock.patch.object(module, "_validate_cache_path"), mock.patch.object(
                module, "is_link_or_reparse", side_effect=PermissionError("denied")
            ):
                observed = module.maven_tools_cache_status(root=root)
            self.assertEqual("invalid", observed["status"])
            self.assertNotIn("denied", json.dumps(observed))

    def test_maven_tools_cache_rejects_linked_version_directory(self):
        module = load(HOSTS, "chaos_engine_hosts_maven_cache_link")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "cache"
            outside = Path(temporary) / "outside"
            root.mkdir()
            outside.mkdir()
            version = root / module.MAVEN_TOOLS_MCP_VERSION
            try:
                version.symlink_to(outside, target_is_directory=True)
            except OSError as error:
                self.skipTest(f"directory links unavailable: {error}")

            self.assertEqual("invalid", module.maven_tools_cache_status(root=root)["status"])
            with self.assertRaisesRegex(ValueError, "linked|invalid"):
                module.purge_maven_tools_cache(module.MAVEN_TOOLS_MCP_VERSION, root=root)
            self.assertTrue(outside.exists())

    def test_maven_tools_cache_status_reports_linked_root_as_invalid(self):
        module = load(HOSTS, "chaos_engine_hosts_maven_cache_root_link")
        with tempfile.TemporaryDirectory() as temporary:
            outside = Path(temporary) / "outside"
            root = Path(temporary) / "cache-link"
            outside.mkdir()
            try:
                root.symlink_to(outside, target_is_directory=True)
            except OSError as error:
                self.skipTest(f"directory links unavailable: {error}")

            self.assertEqual("invalid", module.maven_tools_cache_status(root=root)["status"])

    def test_maven_tools_cache_status_reports_foreign_lock_as_invalid(self):
        module = load(HOSTS, "chaos_engine_hosts_maven_cache_foreign_lock")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "cache"
            root.mkdir()
            (root / module.MAVEN_TOOLS_CACHE_LOCK).write_text("foreign", encoding="utf-8")

            self.assertEqual("invalid", module.maven_tools_cache_status(root=root)["status"])

    def test_maven_tools_cache_rejects_linked_ancestor_and_hard_linked_pair(self):
        module = load(HOSTS, "chaos_engine_hosts_maven_cache_link_boundaries")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            data = base / "data"
            outside_component = base / "outside/ChaosEngine"
            version = outside_component / "tools/maven-tools-mcp" / module.MAVEN_TOOLS_MCP_VERSION
            version.mkdir(parents=True)
            jar = version / f"maven-tools-mcp-{module.MAVEN_TOOLS_MCP_VERSION}.jar"
            jar.write_bytes(b"jar")
            receipt = version / module.MAVEN_TOOLS_MCP_RECEIPT
            receipt.write_text(json.dumps({
                "version": module.MAVEN_TOOLS_MCP_VERSION,
                "commit": module.MAVEN_TOOLS_MCP_COMMIT,
                "jar": jar.name,
                "sha256": hashlib.sha256(jar.read_bytes()).hexdigest(),
            }), encoding="utf-8")
            data.mkdir()
            try:
                (data / "ChaosEngine").symlink_to(outside_component, target_is_directory=True)
            except OSError as error:
                self.skipTest(f"directory links unavailable: {error}")
            apparent_root = data / "ChaosEngine/tools/maven-tools-mcp"

            self.assertEqual("invalid", module.maven_tools_cache_status(root=apparent_root)["status"])
            with self.assertRaises(ValueError):
                module.purge_maven_tools_cache(module.MAVEN_TOOLS_MCP_VERSION, root=apparent_root)
            self.assertTrue(jar.exists())

            (data / "ChaosEngine").unlink()
            real_root = data / "ChaosEngine/tools/maven-tools-mcp"
            shutil.copytree(outside_component / "tools/maven-tools-mcp", real_root)
            hard_link = base / "jar-hard-link"
            try:
                os.link(real_root / module.MAVEN_TOOLS_MCP_VERSION / jar.name, hard_link)
            except OSError as error:
                self.skipTest(f"hard links unavailable: {error}")
            self.assertEqual("invalid", module.maven_tools_cache_status(root=real_root)["status"])
            with self.assertRaises(ValueError):
                module.purge_maven_tools_cache(module.MAVEN_TOOLS_MCP_VERSION, root=real_root)
            self.assertTrue(hard_link.exists())

    def test_default_maven_cache_rejects_link_above_configured_data_root(self):
        module = load(HOSTS, "chaos_engine_hosts_maven_cache_default_ancestor")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            outside = base / "outside"
            alias = base / "alias"
            version = outside / "data/ChaosEngine/tools/maven-tools-mcp" / module.MAVEN_TOOLS_MCP_VERSION
            version.mkdir(parents=True)
            jar = version / f"maven-tools-mcp-{module.MAVEN_TOOLS_MCP_VERSION}.jar"
            jar.write_bytes(b"jar")
            (version / module.MAVEN_TOOLS_MCP_RECEIPT).write_text(json.dumps({
                "version": module.MAVEN_TOOLS_MCP_VERSION,
                "commit": module.MAVEN_TOOLS_MCP_COMMIT,
                "jar": jar.name,
                "sha256": hashlib.sha256(jar.read_bytes()).hexdigest(),
            }), encoding="utf-8")
            try:
                alias.symlink_to(outside, target_is_directory=True)
            except OSError as error:
                self.skipTest(f"directory links unavailable: {error}")
            variable = "LOCALAPPDATA" if os.name == "nt" else "XDG_DATA_HOME"
            with mock.patch.dict(os.environ, {variable: str(alias / "data")}, clear=False):
                self.assertEqual("invalid", module.maven_tools_cache_status()["status"])
                with self.assertRaises(ValueError):
                    module.purge_maven_tools_cache(module.MAVEN_TOOLS_MCP_VERSION)
            self.assertTrue(jar.exists())

    def test_manual_maven_cache_publication_is_atomic_and_no_overwrite(self):
        module = load(HOSTS, "chaos_engine_hosts_maven_cache_publish")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "cache"
            staging = Path(temporary) / "staging-unique"
            staging.mkdir()
            jar = staging / f"maven-tools-mcp-{module.MAVEN_TOOLS_MCP_VERSION}.jar"
            jar.write_bytes(b"jar")
            (staging / module.MAVEN_TOOLS_MCP_RECEIPT).write_text(
                json.dumps({
                    "version": module.MAVEN_TOOLS_MCP_VERSION,
                    "commit": module.MAVEN_TOOLS_MCP_COMMIT,
                    "jar": jar.name,
                    "sha256": hashlib.sha256(jar.read_bytes()).hexdigest(),
                }),
                encoding="utf-8",
            )

            target = module.publish_maven_tools_cache(staging, root=root)
            self.assertEqual(root / module.MAVEN_TOOLS_MCP_VERSION, target)
            self.assertFalse(staging.exists())
            replacement = Path(temporary) / "staging-replacement"
            shutil.copytree(target, replacement)
            with self.assertRaisesRegex(ValueError, "already exists"):
                module.publish_maven_tools_cache(replacement, root=root)

    def test_manual_maven_cache_publication_refuses_last_moment_target(self):
        module = load(HOSTS, "chaos_engine_hosts_maven_cache_publish_race")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "cache"
            staging = Path(temporary) / "staging-unique"
            staging.mkdir()
            jar = staging / f"maven-tools-mcp-{module.MAVEN_TOOLS_MCP_VERSION}.jar"
            jar.write_bytes(b"jar")
            (staging / module.MAVEN_TOOLS_MCP_RECEIPT).write_text(json.dumps({
                "version": module.MAVEN_TOOLS_MCP_VERSION,
                "commit": module.MAVEN_TOOLS_MCP_COMMIT,
                "jar": jar.name,
                "sha256": hashlib.sha256(jar.read_bytes()).hexdigest(),
            }), encoding="utf-8")

            def collide(_source, target):
                target.mkdir()
                raise FileExistsError(target)

            with mock.patch.object(module, "_rename_no_replace", side_effect=collide):
                with self.assertRaisesRegex(ValueError, "already exists"):
                    module.publish_maven_tools_cache(staging, root=root)
            self.assertTrue(staging.exists())
            self.assertTrue((root / module.MAVEN_TOOLS_MCP_VERSION).exists())

    def test_native_no_replace_rename_preserves_both_directories(self):
        module = load(HOSTS, "chaos_engine_hosts_native_no_replace")
        with tempfile.TemporaryDirectory() as temporary:
            source = Path(temporary) / "source"
            target = Path(temporary) / "target"
            source.mkdir()
            target.mkdir()

            with self.assertRaises(OSError):
                module._rename_no_replace(source, target)

            self.assertTrue(source.is_dir())
            self.assertTrue(target.is_dir())

    def test_maven_cache_purge_does_not_remove_replacement_directory(self):
        module = load(HOSTS, "chaos_engine_hosts_maven_cache_purge_directory_race")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "cache"
            version = root / module.MAVEN_TOOLS_MCP_VERSION
            version.mkdir(parents=True)
            jar = version / f"maven-tools-mcp-{module.MAVEN_TOOLS_MCP_VERSION}.jar"
            jar.write_bytes(b"jar")
            (version / module.MAVEN_TOOLS_MCP_RECEIPT).write_text(json.dumps({
                "version": module.MAVEN_TOOLS_MCP_VERSION,
                "commit": module.MAVEN_TOOLS_MCP_COMMIT,
                "jar": jar.name,
                "sha256": hashlib.sha256(jar.read_bytes()).hexdigest(),
            }), encoding="utf-8")
            owned_empty = Path(temporary) / "owned-empty"
            original = module._rmdir_stable_cache_directory

            def replace_before_remove(path, expected):
                path.rename(owned_empty)
                path.mkdir()
                return original(path, expected)

            with mock.patch.object(module, "_rmdir_stable_cache_directory", side_effect=replace_before_remove):
                with self.assertRaisesRegex(ValueError, "changed before purge"):
                    module.purge_maven_tools_cache(module.MAVEN_TOOLS_MCP_VERSION, root=root)
            self.assertTrue(owned_empty.is_dir())
            self.assertTrue(any(path.is_dir() for path in root.iterdir() if path.name.startswith(".purged-")))
            self.assertEqual("invalid", module.maven_tools_cache_status(root=root)["status"])

    def test_two_projects_reuse_one_immutable_maven_cache_pair(self):
        module = load(HOSTS, "chaos_engine_hosts_maven_cache_consumers")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            data = root / "data"
            cache = data / "ChaosEngine/tools/maven-tools-mcp"
            version = cache / module.MAVEN_TOOLS_MCP_VERSION
            version.mkdir(parents=True)
            jar = version / f"maven-tools-mcp-{module.MAVEN_TOOLS_MCP_VERSION}.jar"
            jar.write_bytes(b"jar")
            (version / module.MAVEN_TOOLS_MCP_RECEIPT).write_text(
                json.dumps({
                    "version": module.MAVEN_TOOLS_MCP_VERSION,
                    "commit": module.MAVEN_TOOLS_MCP_COMMIT,
                    "jar": jar.name,
                    "sha256": hashlib.sha256(jar.read_bytes()).hexdigest(),
                }), encoding="utf-8"
            )
            java = root / "java.exe"
            java.write_bytes(b"java")
            projects = [root / "one", root / "two"]
            for project in projects:
                skill = project / ".chaos-engine/skills/chaos-engine/SKILL.md"
                skill.parent.mkdir(parents=True)
                skill.write_text("skill", encoding="utf-8")
            data_variable = "LOCALAPPDATA" if os.name == "nt" else "XDG_DATA_HOME"
            before = hashlib.sha256(jar.read_bytes()).hexdigest()

            with mock.patch.dict(
                os.environ,
                {data_variable: str(data), "CHAOSENGINE_JAVA": str(java), "CHAOSENGINE_MAVEN_TOOLS_MCP_JAR": ""},
                clear=False,
            ), mock.patch.object(module, "java_major", return_value=25), mock.patch.object(
                module, "project_identity_name", side_effect=("one", "two")
            ):
                for project in projects:
                    module.install(project, core_commit="1" * 40)

            for project in projects:
                configured = json.loads((project / ".mcp.json").read_text(encoding="utf-8"))
                self.assertEqual(str(jar.resolve()), configured["mcpServers"]["maven-tools-mcp"]["args"][1])
            self.assertEqual(before, hashlib.sha256(jar.read_bytes()).hexdigest())

    def test_native_maven_tools_runtime_resolves_path_java_symlink(self):
        module = load(HOSTS, "chaos_engine_hosts_native_maven_java_symlink")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            real_java = root / "jdk-25/bin/java"
            linked_java = root / "bin/java"
            jar = root / "maven-tools-mcp-3.2.0.jar"
            real_java.parent.mkdir(parents=True)
            linked_java.parent.mkdir(parents=True)
            real_java.write_bytes(b"java")
            try:
                linked_java.symlink_to(real_java)
            except OSError as error:
                self.skipTest(f"symlinks unavailable: {error}")
            jar.write_bytes(b"jar")
            jar.with_name("install-receipt.json").write_text(
                json.dumps(
                    {
                        "version": "3.2.0",
                        "commit": "4475ff6c61f23ea9a93cb6d5665a63235ef2ef36",
                        "jar": jar.name,
                        "sha256": hashlib.sha256(jar.read_bytes()).hexdigest(),
                    }
                ),
                encoding="utf-8",
            )
            globals_ = module.discover_maven_tools_runtime.__globals__
            prior = globals_["java_major"]
            globals_["java_major"] = lambda candidate: 25 if candidate == real_java.resolve() else None
            try:
                with mock.patch.dict(
                    os.environ,
                    {
                        "LOCALAPPDATA": str(root / "data"),
                        "CHAOSENGINE_MAVEN_TOOLS_MCP_JAR": str(jar),
                    },
                    clear=True,
                ), mock.patch.object(module.shutil, "which", return_value=str(linked_java)):
                    self.assertEqual(
                        (real_java.resolve(), jar.resolve()),
                        module.discover_maven_tools_runtime(),
                    )
            finally:
                globals_["java_major"] = prior

    def test_legacy_docker_maven_server_is_removed_during_host_rendering(self):
        module = load(HOSTS, "chaos_engine_hosts_legacy_maven")
        legacy_json = json.dumps(
            {"mcpServers": {"maven-tools-mcp": module.LEGACY_MAVEN_TOOLS_SERVER}}
        ).encode()
        legacy_toml = (
            '[mcp_servers.maven-tools-mcp]\ncommand = "docker"\n'
            'args = ["run", "-i", "--rm", "arvindand/maven-tools-mcp:3.2.0"]\n'
            "required = false\n"
        ).encode()

        rendered_json = json.loads(module.json_content(legacy_json))
        rendered_toml = module.codex_content(legacy_toml).decode()

        self.assertNotIn("maven-tools-mcp", rendered_json["mcpServers"])
        self.assertNotIn("docker", rendered_toml.casefold())

        rendered_crlf = module.codex_content(legacy_toml.replace(b"\n", b"\r\n")).decode()
        self.assertNotIn("docker", rendered_crlf.casefold())

        managed = module.codex_content(None)
        for legacy in (legacy_toml, legacy_toml.replace(b"\n", b"\r\n")):
            rerendered = module.codex_content(managed + b"\n" + legacy).decode()
            self.assertIn("# CHAOSENGINE:START", rerendered)
            self.assertNotIn("docker", rerendered.casefold())

    def test_existing_unrelated_config_is_preserved_and_owned_collision_fails_closed(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            project.joinpath(".mcp.json").write_text(
                json.dumps({"mcpServers": {"other": {"command": "other"}}}),
                encoding="utf-8",
            )
            module.install(project)
            merged = json.loads(project.joinpath(".mcp.json").read_text(encoding="utf-8"))
            self.assertEqual("other", merged["mcpServers"]["other"]["command"])

            before = project.joinpath(".mcp.json").read_bytes()
            merged["mcpServers"]["chaosengine-memory"] = {"command": "user-owned"}
            project.joinpath(".mcp.json").write_text(json.dumps(merged), encoding="utf-8")
            collision = project.joinpath(".mcp.json").read_bytes()
            with self.assertRaisesRegex(ValueError, "drift"):
                module.install(project)
            self.assertEqual(collision, project.joinpath(".mcp.json").read_bytes())
            self.assertNotEqual(before, collision)

    def test_late_host_collision_leaves_the_project_unchanged(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            project.joinpath(".codex").mkdir()
            project.joinpath(".codex/config.toml").write_text(
                '[mcp_servers."chaosengine-memory"]\ncommand = "mine"\n',
                encoding="utf-8",
            )
            before = {
                path.relative_to(project).as_posix(): path.read_bytes()
                for path in project.rglob("*")
                if path.is_file()
            }

            with self.assertRaisesRegex(ValueError, "collision"):
                module.install(project)

            after = {
                path.relative_to(project).as_posix(): path.read_bytes()
                for path in project.rglob("*")
                if path.is_file()
            }
            self.assertEqual(before, after)

    def test_failed_install_never_deletes_a_concurrent_adapter_replacement(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            first = project / ".agents/skills/chaos-engine/SKILL.md"
            second = project / ".claude/skills/chaos-engine/SKILL.md"
            original = module.atomic_write

            def interleave(root, path, content, expected):
                if path == second:
                    raise RuntimeError("later write failed")
                original(root, path, content, expected)
                if path == first:
                    path.write_text("concurrent user content\n", encoding="utf-8")

            with mock.patch.object(module, "atomic_write", side_effect=interleave):
                with self.assertRaises(Exception):
                    module.install(project)

            self.assertEqual("concurrent user content\n", first.read_text(encoding="utf-8"))

    def test_atomic_write_does_not_overwrite_a_last_moment_replacement(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            module.host_anchor(project, create=True)
            target = project / "AGENTS.md"
            target.write_bytes(b"before\n")
            original = os.link

            def interleave(source, destination, *args, **kwargs):
                if Path(destination) == target:
                    target.write_bytes(b"concurrent\n")
                return original(source, destination, *args, **kwargs)

            with mock.patch.object(os, "link", interleave):
                with self.assertRaises(Exception):
                    module.atomic_write(project, target, b"owned\n", b"before\n")

            self.assertEqual(b"concurrent\n", target.read_bytes())

    def test_prepare_uninstall_does_not_delete_a_last_moment_replacement(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            module.install(project)
            target = project / ".agents/skills/chaos-engine/SKILL.md"
            original = Path.replace

            def interleave(path, destination, *args, **kwargs):
                if path == target:
                    target.write_bytes(b"concurrent\n")
                return original(path, destination, *args, **kwargs)

            with mock.patch.object(Path, "replace", interleave):
                with self.assertRaises(Exception):
                    module.prepare_uninstall(project)

            self.assertEqual(b"concurrent\n", target.read_bytes())

    def test_unsupported_hard_links_fail_before_user_files_move(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            target = project / "AGENTS.md"
            target.write_bytes(b"user\n")

            with mock.patch.object(os, "link", side_effect=OSError(errno.ENOTSUP, "unsupported")):
                with self.assertRaisesRegex(ValueError, "hard links"):
                    module.install(project)

            self.assertEqual(b"user\n", target.read_bytes())

    def test_write_and_remove_scratch_states_are_recovered_on_retry(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            module.host_anchor(project, create=True)
            target = project / "AGENTS.md"
            target.write_bytes(b"before\n")
            old = target.with_name(f".{target.name}.chaos-engine-old")
            write_payload = (
                b"AGENTS.md\0"
                + module.sha256_bytes(b"before\n").encode()
                + b"\0"
                + module.sha256_bytes(b"after\n").encode()
            )
            write_claim = module.ensure_claim(
                project,
                target.with_name(f".{target.name}.chaos-engine-write-claim"),
                "host-write",
                write_payload,
            )
            self.assertEqual(b"", write_claim.read_bytes())
            self.assertFalse(
                target.with_name(f".{target.name}.chaos-engine-write-claim").exists()
            )
            target.replace(old)

            module.atomic_write(project, target, b"after\n", b"before\n")
            self.assertEqual(b"after\n", target.read_bytes())
            self.assertFalse(old.exists())

            removed = target.with_name(f".{target.name}.chaos-engine-removed")
            remove_payload = (
                b"AGENTS.md\0" + module.sha256_bytes(b"after\n").encode()
            )
            remove_claim = module.ensure_claim(
                project,
                target.with_name(f".{target.name}.chaos-engine-remove-claim"),
                "host-remove",
                remove_payload,
            )
            self.assertEqual(b"", remove_claim.read_bytes())
            self.assertFalse(
                target.with_name(f".{target.name}.chaos-engine-remove-claim").exists()
            )
            target.replace(removed)
            module.atomic_remove(project, target, b"after\n")
            self.assertFalse(target.exists())
            self.assertFalse(removed.exists())

    def test_completed_write_recovery_clears_claim_for_the_next_generation(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            module.host_anchor(project, create=True)
            target = project / "AGENTS.md"
            target.write_bytes(b"new\n")
            old = target.with_name(f".{target.name}.chaos-engine-old")
            old.write_bytes(b"old\n")
            payload = (
                b"AGENTS.md\0"
                + module.sha256_bytes(b"old\n").encode()
                + b"\0"
                + module.sha256_bytes(b"new\n").encode()
            )
            claim = module.ensure_claim(
                project,
                target.with_name(f".{target.name}.chaos-engine-write-claim"),
                "host-write",
                payload,
            )

            module.atomic_write(project, target, b"new\n", b"old\n")
            self.assertFalse(old.exists())
            self.assertFalse(claim.exists())

            module.atomic_write(project, target, b"later\n", b"new\n")
            self.assertEqual(b"later\n", target.read_bytes())

    def test_foreign_prefix_publication_scratch_is_never_claimed_or_deleted(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            module.host_anchor(project, create=True)
            target = project / "AGENTS.md"
            target.write_bytes(b"before\n")
            scratch = target.with_name(f".{target.name}.chaos-engine-new")
            scratch.write_bytes(b"a")

            with self.assertRaisesRegex(ValueError, "publication scratch collision"):
                module.atomic_write(project, target, b"after\n", b"before\n")

            self.assertEqual(b"before\n", target.read_bytes())
            self.assertEqual(b"a", scratch.read_bytes())
            self.assertFalse(
                target.with_name(f".{target.name}.chaos-engine-write-claim").exists()
            )

    def test_partial_fixed_transaction_claim_never_bricks_install_retry(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            project.joinpath(".chaos-engine/manifest.json").write_text(
                json.dumps({"hostToken": "b" * 64}), encoding="utf-8"
            )
            partial = project / f".{module.RECEIPT_NAME}.chaos-engine-write-claim"
            partial.write_bytes(b"partial")

            with self.assertRaisesRegex(ValueError, "claim collision"):
                module.install(project)
            self.assertEqual(b"partial", partial.read_bytes())

            partial.unlink()
            module.install(project)
            self.assertEqual("healthy", module.verify(project)["status"])

    def test_preexisting_legacy_anchor_is_preserved_and_rejected(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            anchor = project / module.ANCHOR_NAME
            anchor.write_bytes(b"u" * 32)

            with self.assertRaisesRegex(ValueError, "anchor collision"):
                module.install(project)

            self.assertEqual(b"u" * 32, anchor.read_bytes())
            self.assertFalse(project.joinpath(module.RECEIPT_NAME).exists())

    def test_preexisting_patterned_anchor_is_preserved_and_rejected(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            anchor = project / f"{module.ACTIVE_ANCHOR_PREFIX}{'a' * 64}"
            anchor.write_bytes(b"")

            with self.assertRaisesRegex(ValueError, "anchor collision"):
                module.install(project)

            self.assertTrue(anchor.exists())
            self.assertFalse(project.joinpath(module.RECEIPT_NAME).exists())

    def test_anchor_creation_is_complete_and_recoverable_from_its_name(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            project.joinpath(".chaos-engine/manifest.json").write_text(
                json.dumps({"hostToken": "b" * 64}), encoding="utf-8"
            )
            anchor = module.host_anchor_path(project, create=True)

            self.assertEqual(b"", anchor.read_bytes())
            self.assertEqual(32, len(module.host_anchor(project)))
            module.install(project)
            self.assertEqual("healthy", module.verify(project)["status"])

    def test_uninstall_retries_after_receipt_was_deleted_before_anchor_cleanup(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            module.install(project)
            module.prepare_uninstall(project)
            original = Path.unlink

            def interrupt(path, *args, **kwargs):
                if path.name.startswith(module.REMOVING_ANCHOR_PREFIX):
                    raise OSError("anchor cleanup failed")
                return original(path, *args, **kwargs)

            with mock.patch.object(Path, "unlink", interrupt):
                with self.assertRaisesRegex(OSError, "anchor cleanup failed"):
                    module.finalize_uninstall(project)

            self.assertFalse(project.joinpath(module.RECEIPT_NAME).exists())
            module.finalize_uninstall(project)
            self.assertFalse(module.host_anchor_paths(project))

    def test_receipt_cannot_claim_an_unrelated_empty_directory(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            module.install(project)
            victim = project / "valuable-empty"
            victim.mkdir()
            receipt_path = project / module.RECEIPT_NAME
            receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
            receipt["createdDirectories"].append("valuable-empty")
            receipt.pop("integritySha256")
            receipt_path.write_bytes(module.receipt_bytes(receipt))

            with self.assertRaisesRegex(ValueError, "integrity drift"):
                module.prepare_uninstall(project)

            self.assertTrue(victim.is_dir())

    def test_installing_receipt_retry_finishes_every_directory_marker(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            original = module.prepare_created_directories

            def interrupt(root, receipt):
                first = receipt["createdDirectories"][0]
                partial = dict(receipt)
                partial["createdDirectories"] = [first]
                original(root, partial)
                raise SystemExit("crash")

            with mock.patch.object(module, "prepare_created_directories", side_effect=interrupt):
                with self.assertRaises(SystemExit):
                    module.install(project)

            module.install(project)

            self.assertEqual("healthy", module.verify(project)["status"])

    def test_concurrent_empty_directory_is_never_claimed(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            original = Path.mkdir
            raced = project / ".agents"

            def interleave(path, *args, **kwargs):
                if path == raced and not path.exists():
                    original(path)
                return original(path, *args, **kwargs)

            with mock.patch.object(Path, "mkdir", interleave):
                with self.assertRaisesRegex(ValueError, "directory claim collision"):
                    module.install(project)

            self.assertTrue(raced.is_dir())

    def test_directory_removal_retries_after_marker_was_deleted(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            module.install(project)
            receipt = module.prepare_uninstall(project)
            owned = project / receipt["createdDirectories"][-1]
            marker = owned / module.DIRECTORY_MARKER
            module.write_directory_claim(
                project, receipt, receipt["createdDirectories"][-1]
            )
            marker.unlink()

            module.finalize_uninstall(project)

            self.assertFalse(owned.exists())

    def test_linked_host_config_is_rejected_without_touching_external_content(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            external = root / "external.json"
            external.write_text('{"outside": true}\n', encoding="utf-8")
            try:
                project.joinpath(".mcp.json").symlink_to(external)
            except OSError as error:
                self.skipTest(f"symlink creation unavailable: {error}")

            with self.assertRaisesRegex(ValueError, "link or reparse"):
                module.install(project)

            self.assertEqual('{"outside": true}\n', external.read_text(encoding="utf-8"))

    def test_uninstall_restores_exact_before_images(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            project.joinpath("AGENTS.md").write_bytes(b"user instructions\r\n")
            project.joinpath(".mcp.json").write_text(
                json.dumps({"mcpServers": {"other": {"command": "other"}}}) + "\n",
                encoding="utf-8",
            )
            before_agents = project.joinpath("AGENTS.md").read_bytes()
            before_mcp = project.joinpath(".mcp.json").read_bytes()
            before_directories = {
                path.relative_to(project).as_posix()
                for path in project.rglob("*")
                if path.is_dir()
            }

            module.install(project)
            module.uninstall(project)

            self.assertEqual(before_agents, project.joinpath("AGENTS.md").read_bytes())
            self.assertEqual(before_mcp, project.joinpath(".mcp.json").read_bytes())
            self.assertFalse(project.joinpath(module.RECEIPT_NAME).exists())
            self.assertFalse(project.joinpath(".agents/skills/chaos-engine/SKILL.md").exists())
            self.assertEqual(
                before_directories,
                {
                    path.relative_to(project).as_posix()
                    for path in project.rglob("*")
                    if path.is_dir()
                },
            )

    def test_tool_launcher_rejects_legacy_flat_runtime_without_active_pointer(self):
        module = load(TOOL, "chaos_engine_tool")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "legacy project"
            core = project / ".chaos-engine"
            runtime = project / ".chaos-engine-runtime"
            core.mkdir(parents=True)
            runtime.joinpath("bin").mkdir(parents=True)
            command = runtime / "bin" / ("graphify.exe" if os.name == "nt" else "graphify")
            command.write_text("tool\n", encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "dependency controller"):
                module.resolve_command(core, "graphify")

    def test_tool_launcher_suppresses_runtime_bytecode_caches(self):
        module = load(TOOL, "chaos_engine_tool_environment")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            core = project / ".chaos-engine"
            runtime = project / ".chaos-engine-runtime/bin"
            core.mkdir()
            runtime.mkdir(parents=True)
            command = runtime / ("mempalace.exe" if os.name == "nt" else "mempalace")
            command.write_text("tool\n", encoding="utf-8")
            with mock.patch.object(module.sys, "argv", ["tool.py", "mempalace", "status"]):
                with mock.patch.object(module, "resolve_command", return_value=command):
                    with mock.patch.object(module.subprocess, "call", return_value=0) as call:
                        self.assertEqual(0, module.main())

            self.assertEqual("1", call.call_args.kwargs["env"]["PYTHONDONTWRITEBYTECODE"])

    def test_host_tests_are_reached_by_pull_request_gate(self):
        budget = json.loads(
            (ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
        )
        self.assertIn(
            "tests/scripts/test_chaos_engine_hosts.py",
            budget["harness_reachability"]["element_globs"],
        )
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertIn("python scripts/ci/harness_pr_gate.py", workflow)
        gate = (ROOT / "scripts/ci/harness_pr_gate.py").read_text(encoding="utf-8")
        self.assertIn("tests.scripts.test_chaos_engine_hosts", gate)


class HostReceiptImageTest(unittest.TestCase):
    def test_decode_images_keeps_receipt_keys_when_live_inventory_shrinks(self) -> None:
        hosts = load(HOSTS, "chaos_engine_hosts_receipt")
        recorded = {path: None for path in hosts.managed_paths()}
        payload = hosts.encode_images(recorded)
        shrunk = hosts.LEGACY_MANAGED_PATHS
        self.assertLess(len(shrunk), len(recorded))
        with mock.patch.object(hosts, "managed_paths", return_value=shrunk):
            try:
                decoded = hosts.decode_images(payload, nullable=True)
            except ValueError as error:
                self.fail(f"receipt-owned keys were rejected after inventory shrink: {error}")
        self.assertEqual(decoded, recorded)

    def test_decode_images_rejects_an_escaped_receipt_path(self) -> None:
        hosts = load(HOSTS, "chaos_engine_hosts_receipt_escape")
        payload = hosts.encode_images({path: None for path in hosts.LEGACY_MANAGED_PATHS})
        payload["../secret"] = None
        with self.assertRaisesRegex(ValueError, "unsafe receipt path"):
            hosts.decode_images(payload, nullable=True)

class CompanionPinTest(unittest.TestCase):
    def test_checked_in_blobs_match_pin_digests(self) -> None:
        pins = (
            ROOT / "chaos-engine/vendor/caveman/PIN.json",
            ROOT / "chaos-engine/vendor/ponytail/PIN.json",
        )
        for pin_path in pins:
            with self.subTest(pin=pin_path.as_posix()):
                pin = json.loads(pin_path.read_text(encoding="utf-8"))
                files = pin["files"]
                self.assertTrue(files)
                root = pin_path.parent
                listed = set(files)
                on_disk = {
                    path.relative_to(root).as_posix()
                    for path in root.rglob("*")
                    if path.is_file() and path.name not in {"PIN.json", "INVENTORY.md"}
                }
                self.assertEqual(listed, on_disk)
                for relative, expected in files.items():
                    digest = hashlib.sha256((root / relative).read_bytes()).hexdigest()
                    self.assertEqual(digest, expected, relative)

    def test_entrypoint_does_not_restate_vendor_skill_bodies(self) -> None:
        entrypoint = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(
            encoding="utf-8"
        )
        caveman = (
            ROOT / "chaos-engine/vendor/caveman/skills/caveman/SKILL.md"
        ).read_text(encoding="utf-8")
        ponytail = (
            ROOT / "chaos-engine/vendor/ponytail/skills/ponytail/SKILL.md"
        ).read_text(encoding="utf-8")
        self.assertIn("Respond terse like smart caveman", caveman)
        self.assertIn("You are a lazy senior developer", ponytail)
        self.assertNotIn("Respond terse like smart caveman", entrypoint)
        self.assertNotIn("You are a lazy senior developer", entrypoint)
        self.assertNotIn("@caveman-ai/cli", entrypoint)


if __name__ == "__main__":
    unittest.main()
