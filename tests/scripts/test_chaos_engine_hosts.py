"""Portable ChaosEngine host adapter tests (#4795)."""

from __future__ import annotations

import importlib.util
import errno
import hashlib
import json
import os
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
                    value = {}
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
                {"codex:chaos-engine", "codex:caveman", "claude:chaos-engine", "claude:caveman"},
                set(receipt["createdPlugins"]),
            )
            self.assertEqual("detected", receipt["cavemanProxy"]["providers"])
            self.assertTrue(all(item["status"] == "healthy" for item in status.values()))
            self.assertTrue(all(cwd == project for _, cwd in calls))

            module.uninstall(project, runner=runner, which=lambda name: name)
            self.assertFalse(any(state.values()))
            self.assertFalse(activation_root.exists())

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
                    {"chaosengine-memory", "chaosengine-mempalace"}, set(servers)
                )
                for server in servers.values():
                    self.assertEqual("py" if os.name == "nt" else "python3", server["command"])
                    if os.name == "nt":
                        self.assertEqual("-3", server["args"][0])
                    self.assertIn(".chaos-engine/tool.py", server["args"])
                mempalace = servers["chaosengine-mempalace"]
                self.assertEqual(
                    ["--backend", "sqlite_exact"],
                    mempalace["args"][-2:],
                )
            self.assertIn('[mcp_servers."chaosengine-memory"]', codex)
            self.assertIn('".chaos-engine/tool.py", "memory-mcp"]', codex)
            self.assertIn(".chaos-engine-state/mempalace", str(claude))
            self.assertIn('"--backend", "sqlite_exact"', codex)

    def test_complete_host_harness_installs_inventory_roles_hooks_and_plugin(self):
        module = load(HOSTS, "chaos_engine_complete_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")

            receipt = module.install(project)

            required = {
                ".agents/skills/README.md",
                ".agents/plugins/marketplace.json",
                ".claude-plugin/marketplace.json",
                "plugins/chaos-engine/.codex-plugin/plugin.json",
                "plugins/chaos-engine/.claude-plugin/plugin.json",
                "plugins/chaos-engine/hooks/hooks.json",
                "plugins/chaos-engine/hooks/guard.py",
                "plugins/chaos-engine/hooks/reflection.py",
                "plugins/chaos-engine/skills/chaos-engine/SKILL.md",
                "plugins/caveman/.codex-plugin/plugin.json",
                "plugins/caveman/.claude-plugin/plugin.json",
                "plugins/caveman/skills/caveman/SKILL.md",
                "plugins/caveman/LICENSE",
                "plugins/caveman/UPSTREAM.md",
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
            memory_config = json.loads(project.joinpath(".memory/config.json").read_text())
            self.assertEqual(5, memory_config["version"])
            self.assertEqual({"version", "project", "memory"}, set(memory_config))
            self.assertEqual("consumer", memory_config["project"]["name"])
            self.assertTrue(module.retrieval_configs_healthy(project))
            self.assertIn("wing: consumer", project.joinpath("mempalace.yaml").read_text())
            ignores = project.joinpath(".gitignore").read_text()
            self.assertIn(".chaos-engine-runtime/", ignores)
            self.assertIn(".chaos-engine.lock", ignores)
            self.assertIn(".chaos-engine-runtime.lock", ignores)
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

            lifecycle = json.loads(project.joinpath(".codex/hooks.json").read_text())["hooks"]
            self.assertEqual({}, lifecycle)
            plugin_lifecycle = json.loads(
                project.joinpath("plugins/chaos-engine/hooks/hooks.json").read_text()
            )["hooks"]
            self.assertEqual({}, plugin_lifecycle)
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
            self.assertEqual(0, first.returncode, first.stderr)
            self.assertEqual(0, second.returncode, second.stderr)
            self.assertIn("Reflection required", second.stdout)

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
                ["unrelated", "chaos-engine", "caveman"],
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
            self.assertEqual(original, merged)
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

    def test_unrelated_claude_marketplace_fails_closed_without_mutation(self):
        module = load(HOSTS, "chaos_engine_claude_marketplace_collision")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            path = project / ".claude-plugin/marketplace.json"
            path.parent.mkdir(parents=True)
            original = {"name": "user-marketplace", "plugins": []}
            path.write_text(json.dumps(original), encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "Claude marketplace collision"):
                module.install(project)
            self.assertEqual(original, json.loads(path.read_text()))
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
            self.assertIn("wing: actual-project", project.joinpath("mempalace.yaml").read_text())

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
            stdout=json.dumps({"jsonrpc": "2.0", "id": 1, "result": {}}) + "\n",
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
                            "result": {
                                "content": [
                                    {
                                        "type": "text",
                                        "text": json.dumps(
                                            {
                                                "total_drawers": 0,
                                                "backend": "sqlite_exact",
                                            }
                                        ),
                                    }
                                ]
                            },
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
                "mempalace_status",
                json.loads(mempalace_requests[2])["params"]["name"],
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

    def test_launcher_rendering_is_explicit_for_windows_and_posix(self):
        module = load(HOSTS, "chaos_engine_hosts")

        windows = module.owned_servers("nt")
        posix = module.owned_servers("posix")

        for server in windows.values():
            self.assertEqual("py", server["command"])
            self.assertEqual("-3", server["args"][0])
        for server in posix.values():
            self.assertEqual("python3", server["command"])
            self.assertNotEqual("-3", server["args"][0])

    def test_native_maven_tools_runtime_uses_resolved_host_paths(self):
        module = load(HOSTS, "chaos_engine_hosts_native_maven")
        java = Path(r"C:\runtime\jdk-25\bin\java.exe")
        jar = Path(r"C:\runtime\maven-tools-mcp\3.2.0\server.jar")

        servers = module.owned_servers("nt", maven_runtime=(java, jar))
        maven = servers["maven-tools-mcp"]

        self.assertEqual(str(java), maven["command"])
        self.assertEqual(
            ["-jar", str(jar), "--spring.profiles.active=docker,no-context7"],
            maven["args"],
        )
        self.assertNotEqual("docker", Path(str(maven["command"])).name.casefold())

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
        self.assertIn("--spring.profiles.active=docker,no-context7", codex)

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
                        "LOCALAPPDATA": str(root / "data"),
                        "XDG_DATA_HOME": str(root / "other-data"),
                        "CHAOSENGINE_JAVA": str(java),
                        "CHAOSENGINE_MAVEN_TOOLS_MCP_JAR": "",
                    },
                    clear=False,
                ), mock.patch.object(module.os, "name", "nt"):
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

    def test_tool_launcher_resolves_runtime_after_project_move(self):
        module = load(TOOL, "chaos_engine_tool")
        with tempfile.TemporaryDirectory() as temporary:
            first = Path(temporary) / "first"
            second = Path(temporary) / "moved project"
            core = first / ".chaos-engine"
            runtime = first / ".chaos-engine-runtime"
            core.mkdir(parents=True)
            runtime.joinpath("bin").mkdir(parents=True)
            command = runtime / "bin" / ("graphify.exe" if os.name == "nt" else "graphify")
            command.write_text("tool\n", encoding="utf-8")
            first.replace(second)

            resolved = module.resolve_command(second / ".chaos-engine", "graphify")

            self.assertEqual(second / ".chaos-engine-runtime/bin" / command.name, resolved)

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
        self.assertIn("tests.scripts.test_chaos_engine_hosts", workflow)


if __name__ == "__main__":
    unittest.main()
