"""Portable ChaosEngine host adapter tests (#4795)."""

from __future__ import annotations

import importlib.util
import errno
import hashlib
import json
import os
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


class ChaosEngineHostsTest(unittest.TestCase):
    def test_detected_client_plugins_are_registered_installed_and_verified(self):
        module = load(HOSTS, "chaos_engine_plugin_activation")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            manifest = project / "plugins/chaos-engine/.codex-plugin/plugin.json"
            manifest.parent.mkdir(parents=True)
            manifest.write_text(
                json.dumps({"name": "chaos-engine", "version": "1.0.7"}),
                encoding="utf-8",
            )
            for relative in ("hooks/guard.py", "skills/chaos-engine/SKILL.md"):
                path = project / "plugins/chaos-engine" / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(relative, encoding="utf-8")
            activation_root, marketplace_name, plugin_id, version = module.activation_contract(project)
            state = {
                "codex_marketplace": False,
                "codex_plugin": False,
                "claude_marketplace": False,
                "claude_plugin": False,
            }
            calls = []

            def runner(command, **options):
                calls.append((command, options.get("cwd")))
                client = Path(command[0]).stem
                joined = " ".join(command[1:])
                key = f"{client}_marketplace"
                plugin_key = f"{client}_plugin"
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
                    if client == "codex":
                        record = {"pluginId": plugin_id, "version": version, "installed": True, "enabled": True, "source": {"path": str(activation_root / "plugins/chaos-engine")}}
                    else:
                        record = {"id": plugin_id, "version": version, "enabled": True, "projectPath": str(project), "installPath": str(activation_root / "plugins/chaos-engine")}
                    value = {"installed": [record] if state[plugin_key] else [], "available": []}
                elif "plugin add" in joined or "plugin install" in joined:
                    state[plugin_key] = True
                    value = {}
                elif "plugin remove" in joined or "plugin uninstall" in joined:
                    state[plugin_key] = False
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

            self.assertEqual({"codex", "claude"}, set(receipt["createdPlugins"]))
            self.assertTrue(all(item["status"] == "healthy" for item in status.values()))
            self.assertTrue(all(cwd == project for _, cwd in calls))

            module.deactivate_created_plugins(project, receipt, runner=runner, which=lambda name: name)
            self.assertFalse(any(state.values()))

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
            self.assertIn('[mcp_servers."chaosengine-memory"]', codex)
            self.assertIn('".chaos-engine/tool.py", "memory-mcp"]', codex)
            self.assertIn(".chaos-engine-state/mempalace", str(claude))

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
                "plugins/chaos-engine/skills/chaos-engine/SKILL.md",
                ".codex/hooks.json",
                ".claude/settings.json",
                ".memory/config.json",
                "mempalace.yaml",
                ".gitignore",
            }
            for role in ("orchestrator", "implementer", "reviewer", "tester", "mechanical-helper"):
                required.add(f".claude/agents/chaos-engine-{role}.md")
                required.add(f".codex/agents/chaos-engine-{role}.toml")
            self.assertTrue(required <= set(receipt["after"]))
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
            self.assertEqual("consumer", memory_config["project"]["name"])
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
            self.assertIn("!.claude/**", ignores)
            self.assertIn("!.codex/**", ignores)
            self.assertIn(".claude/settings.local.json", ignores)

            hook_events = set(
                json.loads(project.joinpath(".codex/hooks.json").read_text())["hooks"]
            )
            self.assertEqual(
                {
                    "SessionStart",
                    "UserPromptSubmit",
                    "PreToolUse",
                    "PostToolUse",
                    "Stop",
                    "SubagentStop",
                },
                hook_events,
            )

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
            self.assertEqual(["unrelated", "chaos-engine"], [item["name"] for item in merged["plugins"]])
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
            module.uninstall(project)
            self.assertEqual(original, json.loads(hook_path.read_text()))

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

    def test_invalid_retrieval_configs_fail_before_mutation(self):
        module = load(HOSTS, "chaos_engine_invalid_retrieval")
        for relative, content, message in (
            (".memory/config.json", "{}", "Memory configuration"),
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
