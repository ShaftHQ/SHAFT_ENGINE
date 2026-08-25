"""Standalone ChaosEngine dependency provisioning tests (#4794)."""

from __future__ import annotations

import importlib.util
import json
import os
import shutil
import tempfile
import unittest
import unittest.mock as mock
from datetime import datetime, timedelta, timezone
from pathlib import Path
from types import SimpleNamespace


ROOT = Path(__file__).resolve().parents[2]
CONTROLLER = ROOT / "chaos-engine/dependencies.py"
SPECIFICATION = ROOT / "chaos-engine/dependencies.json"
TOOL = ROOT / "chaos-engine/tool.py"


def load_controller():
    spec = importlib.util.spec_from_file_location("chaos_engine_dependencies", CONTROLLER)
    if spec is None or spec.loader is None:
        raise RuntimeError("dependency controller test module could not be loaded")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def load_tool():
    spec = importlib.util.spec_from_file_location("chaos_engine_tool", TOOL)
    if spec is None or spec.loader is None:
        raise RuntimeError("tool launcher test module could not be loaded")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class ChaosEngineDependenciesTest(unittest.TestCase):
    def symlink_or_skip(self, target: Path | str, link: Path) -> None:
        try:
            link.symlink_to(target)
        except OSError as error:
            if os.name == "nt" and getattr(error, "winerror", None) == 1314:
                self.skipTest("Windows symlink privilege is unavailable")
            raise

    @staticmethod
    def write_legacy_receipt_with_cache(module, runtime: Path) -> None:
        receipt_path = runtime / module.RECEIPT_NAME
        receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
        ownership = receipt["ownership"]
        cache_directory = "bin/__pycache__"
        cache_file = f"{cache_directory}/graphify.cpython-314.pyc"
        ownership["directories"] = sorted(
            set(ownership["directories"]) | {cache_directory}
        )
        ownership["files"][cache_file] = module.sha256(runtime / cache_file)
        digest = module.hashlib.sha256()
        for relative, file_digest in sorted(ownership["files"].items()):
            digest.update(relative.encode())
            digest.update(b"\0")
            digest.update(bytes.fromhex(file_digest))
        ownership["sha256"] = digest.hexdigest()
        integrity_receipt = {
            key: value
            for key, value in receipt.items()
            if key != "receiptIntegritySha256"
        }
        encoded = json.dumps(
            integrity_receipt, sort_keys=True, separators=(",", ":")
        ).encode()
        receipt["receiptIntegritySha256"] = module.hashlib.sha256(encoded).hexdigest()
        receipt_path.write_text(
            json.dumps(receipt, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )

    def test_runtime_lock_closes_descriptor_when_stream_creation_fails(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            with mock.patch.object(module.os, "fdopen", side_effect=OSError("stream failed")):
                with mock.patch.object(module.os, "close", wraps=module.os.close) as close:
                    with self.assertRaisesRegex(OSError, "stream failed"):
                        with module.runtime_lock(runtime):
                            self.fail("lock unexpectedly acquired")
            close.assert_called_once()

    def test_tool_launcher_blocks_unhealthy_mempalace_state_before_native_launch(self):
        module = load_tool()
        self.assertTrue(hasattr(module, "guard_mempalace_mcp"))
        controller = """from pathlib import Path
def mempalace_runtime_status(project: Path):
    status = (project / '.chaos-engine-state/mempalace/status.txt').read_text().strip()
    return {'status': status, 'detail': 'fixture state'}
"""
        arguments = [
            "tool.py",
            "mempalace-mcp",
            "--palace",
            ".chaos-engine-state/mempalace",
            "--backend",
            "sqlite_exact",
        ]
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            core = project / ".chaos-engine"
            palace = project / ".chaos-engine-state/mempalace"
            core.mkdir()
            palace.mkdir(parents=True)
            core.joinpath("hosts.py").write_text(controller, encoding="utf-8")
            command = project / ".chaos-engine-runtime/bin/mempalace-mcp"

            for status in ("migration-required", "recovery-required"):
                palace.joinpath("status.txt").write_text(status, encoding="utf-8")
                with self.subTest(status=status):
                    with mock.patch.object(module, "__file__", str(core / "tool.py")):
                        with mock.patch.object(module.sys, "argv", arguments):
                            with mock.patch.object(module, "resolve_command", return_value=command):
                                with mock.patch.object(module.sys, "dont_write_bytecode", False):
                                    with mock.patch.object(module.subprocess, "call") as call:
                                        self.assertEqual(1, module.main())
                    call.assert_not_called()
                    self.assertFalse(core.joinpath("__pycache__").exists())

            palace.joinpath("status.txt").write_text("healthy", encoding="utf-8")
            with mock.patch.object(module, "__file__", str(core / "tool.py")):
                with mock.patch.object(module.sys, "argv", arguments):
                    with mock.patch.object(module, "resolve_command", return_value=command):
                        with mock.patch.object(module.sys, "dont_write_bytecode", False):
                            with mock.patch.object(module.subprocess, "call", return_value=0) as call:
                                self.assertEqual(0, module.main())
            call.assert_called_once()
            self.assertFalse(core.joinpath("__pycache__").exists())

            for invalid in (
                [*arguments, "--backend=chroma"],
                [*arguments, "--palace=external"],
                [*arguments, "--read-only"],
                [*arguments[:-1], "chroma"],
            ):
                with self.subTest(arguments=invalid):
                    with mock.patch.object(module, "__file__", str(core / "tool.py")):
                        with mock.patch.object(module.sys, "argv", invalid):
                            with mock.patch.object(module.subprocess, "call") as call:
                                self.assertEqual(1, module.main())
                    call.assert_not_called()

    @staticmethod
    def fake_runner(root: Path):
        def runner(command, environment):
            del environment
            executable = Path(command[0])
            if not executable.exists() and executable.is_relative_to(root.parent):
                executable.parent.mkdir(parents=True, exist_ok=True)
                executable.write_text("tool\n", encoding="utf-8")
            return SimpleNamespace(stdout="tool 1.0\n", stderr="")

        return runner

    def test_plan_uses_user_account_commands_for_every_required_tool(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            plan = module.account_tool_plan(
                project,
                specification,
                actions={
                    "mempalace": "installed",
                    "graphify": "upgraded",
                    "memory": "installed",
                    "context7": "reused",
                },
                executables={"uv": "/user/bin/uv", "npm": "/user/bin/npm"},
            )

        self.assertEqual(3, specification["schemaVersion"])
        self.assertEqual(
            [["/user/bin/uv", "tool", "install", "mempalace"]],
            plan["mempalace"],
        )
        self.assertEqual(
            [["/user/bin/uv", "tool", "upgrade", "graphifyy"]],
            plan["graphify"],
        )
        self.assertEqual(
            [["/user/bin/npm", "install", "-g", "@aictx/memory@latest"]],
            plan["memory"],
        )
        self.assertEqual([], plan["context7"])

    def test_stable_versions_reject_prerelease_and_yanked_candidates(self):
        module = load_controller()
        candidates = [
            {"version": "3.8.0rc1", "yanked": False},
            {"version": "3.7.9", "yanked": True},
            {"version": "3.7.8", "yanked": False},
            {"version": "3.7.10", "yanked": False},
        ]
        self.assertEqual(
            "3.7.10",
            module.latest_compatible_stable(candidates, minimum="3.7.0"),
        )
        with self.assertRaisesRegex(ValueError, "compatible stable"):
            module.latest_compatible_stable(
                [{"version": "4.0.0-beta.1", "yanked": False}], minimum="3.7.0"
            )

    def test_dependency_action_uses_health_version_and_lookup_state(self):
        module = load_controller()
        cases = (
            (None, "1.0.0", False, True, "installed"),
            ("1.0.0", "1.0.0", True, True, "reused"),
            ("1.0.0", "1.1.0", True, True, "upgraded"),
            ("1.1.0", "1.1.0", False, True, "repaired"),
            ("1.0.0", None, True, False, "blocked"),
            (None, None, False, False, "blocked"),
        )
        for installed, latest, healthy, verified, expected in cases:
            with self.subTest(expected=expected):
                self.assertEqual(
                    expected,
                    module.dependency_action(
                        installed_version=installed,
                        resolved_version=latest,
                        healthy=healthy,
                        latest_version_verified=verified,
                    ),
                )

    def test_account_discovery_requires_every_sibling_and_sanitizes_receipt(self):
        module = load_controller()
        paths = {
            "node": "/opt/node/bin/node",
            "npm": "/opt/node/bin/npm",
            "java": "/opt/java/bin/java",
        }
        discovered = module.discover_executables(
            ["node", "npm", "npx", "java"], which=paths.get
        )
        self.assertEqual("missing", discovered["npx"]["status"])
        receipt = module.sanitize_receipt(
            {
                "provider": "path",
                "executable": "/home/person/.local/bin/node",
                "token": "secret-value",
                "probe": {"authorization": "Bearer secret", "status": "healthy"},
            },
            home=Path("/home/person"),
        )
        self.assertNotIn("token", receipt)
        self.assertNotIn("authorization", receipt["probe"])
        self.assertEqual("<home>/.local/bin/node", receipt["executable"])

    def test_account_discovery_rejects_project_local_generation_executables(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            executable = Path(temporary) / ".chaos-engine-runtime-generations/owned/bin/node"
            executable.parent.mkdir(parents=True)
            executable.write_text("node\n", encoding="utf-8")
            executable.chmod(0o755)
            discovered = module.discover_executables(
                ["node"], which=lambda _name: str(executable)
            )
        self.assertEqual("invalid", discovered["node"]["status"])

    def test_prerequisite_plans_scope_elevation_to_package_manager_commands(self):
        module = load_controller()
        linux = module.prerequisite_command_plan(
            "linux", "apt", {"uv": "installed", "node": "installed", "java": "installed"}
        )
        self.assertEqual(
            ["sh", "-c", "curl -LsSf https://astral.sh/uv/install.sh | sh"],
            linux["uv"][0],
        )
        self.assertNotIn("sudo", linux["uv"][0])
        for command in [*linux["node"], *linux["java"]]:
            if "sudo" in command:
                self.assertEqual("-n", command[command.index("sudo") + 1])
                self.assertIn(command[command.index("sudo") + 2], {"apt-get", "dnf"})

    def test_uv_upgrade_uses_self_update_and_platform_plans_are_provider_native(self):
        module = load_controller()
        upgraded = module.prerequisite_command_plan(
            "linux", "apt", {"uv": "upgraded", "python": "installed", "node": "reused", "java": "reused"},
            python_version="3.14.7",
        )
        self.assertEqual([["uv", "self", "update"]], upgraded["uv"])
        self.assertEqual(
            [["uv", "python", "install", "3.14.7", "--no-progress"]],
            upgraded["python"],
        )
        macos = module.prerequisite_command_plan(
            "macos", "brew", {"uv": "reused", "node": "installed", "java": "installed"},
            node_major=24,
        )
        self.assertEqual([["brew", "install", "node@24"]], macos["node"])
        windows = module.prerequisite_command_plan(
            "windows", "winget", {"uv": "reused", "node": "installed", "java": "installed"}
        )
        self.assertTrue(
            all(command[0] == "winget" for command in windows["node"] + windows["java"])
        )

    def test_stable_channel_parsers_cover_node_pypi_npm_and_github(self):
        module = load_controller()
        payloads = {
            "node": [{"version": "v24.1.0", "lts": "Krypton"}, {"version": "v25.0.0", "lts": False}],
            "python": [
                {"name": "Python 3.14.7", "is_published": True, "pre_release": False},
                {"name": "Python 3.15.0rc1", "is_published": True, "pre_release": True},
                {"name": "Python install manager 26.3", "is_published": True, "pre_release": False},
            ],
            "mempalace": {"releases": {"3.8.0": [{"yanked": False}], "3.9.0rc1": [{"yanked": False}]}},
            "memory": {"version": "0.2.2"},
            "java": {"versions": [
                {"major": 26, "semver": "26.0.1+8"},
                {"major": 25, "semver": "25.0.4+7.0.LTS"},
                {"major": 25, "semver": "25.0.3+9.0.LTS"},
            ]},
            "uv": {"tag_name": "0.12.5", "prerelease": False, "draft": False},
        }

        class Response:
            def __init__(self, payload):
                self.payload = json.dumps(payload).encode()
            def __enter__(self):
                return self
            def __exit__(self, *_args):
                return None
            def read(self, _size):
                return self.payload

        for name, expected in (("node", "24.1.0"), ("python", "3.14.7"), ("mempalace", "3.8.0"), ("memory", "0.2.2"), ("java", "25.0.4+7.0.LTS"), ("uv", "0.12.5")):
            with self.subTest(name=name):
                contract = {
                    "minimumVersion": "25.0.0" if name == "java" else "0.1.0" if name not in {"node", "python"} else ("22.0.0" if name == "node" else "3.14.0"),
                    "stableChannel": f"https://example.invalid/{name}",
                }
                self.assertEqual(
                    expected,
                    module.resolve_stable_version(
                        name, contract, opener=lambda *_args, **_kwargs: Response(payloads[name])
                    ),
                )

    def test_account_receipt_dispatches_absolute_commands_and_redacts_home(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            executable = project / "user tools/memory-mcp"
            executable.parent.mkdir()
            executable.write_text("tool\n", encoding="utf-8")
            executable.chmod(0o755)
            receipt = module.write_account_receipt(
                project,
                {"memory": {"action": "installed", "probe": "passed"}},
                {"memory-mcp": str(executable)},
                now=datetime(2026, 8, 25, tzinfo=timezone.utc),
            )
            self.assertEqual(2, receipt["schemaVersion"])
            self.assertEqual(
                [str(executable.resolve()), "--stdio"],
                module.active_dispatch(project, "memory-mcp", ["--stdio"]),
            )
            with self.assertRaisesRegex(ValueError, "dispatch is missing"):
                module.active_dispatch(project, "mempalace", [])

    def test_account_receipt_v1_migrates_deterministically_in_memory(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            path = project / module.ACCOUNT_RECEIPT_NAME
            path.write_text(json.dumps({
                "schemaVersion": 1,
                "components": {"node": {"status": "healthy"}},
                "commands": {"node": "/usr/bin/node"},
            }), encoding="utf-8")

            migrated = module.read_account_receipt(project)

            self.assertEqual(2, migrated["schemaVersion"])
            self.assertEqual("user", migrated["scope"])
            self.assertEqual("migrated-v1", migrated["migration"])
            self.assertEqual(1, json.loads(path.read_text())["schemaVersion"])

    def test_project_setup_plan_initializes_only_absent_or_stale_state(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            commands = {
                "mempalace": "/tools/mempalace",
                "graphify": "/tools/graphify",
                "memory": "/tools/memory",
            }
            fresh = module.project_setup_plan(project, commands)
            self.assertEqual(["/tools/mempalace", "init", "."], fresh[0])
            self.assertIn(["/tools/mempalace", "mine", "."], fresh)
            self.assertIn(
                ["/tools/graphify", "install", "--platform", "agents", "--project"],
                fresh,
            )
            self.assertIn(
                ["/tools/graphify", "extract", ".", "--code-only"], fresh
            )
            self.assertIn(["/tools/memory", "init", "--no-view"], fresh)

            project.joinpath("mempalace.yaml").write_text("wing: test\n", encoding="utf-8")
            state = project / ".chaos-engine-state/mempalace"
            state.mkdir(parents=True)
            state.joinpath(".mined").write_text("current\n", encoding="utf-8")
            graph = project / "graphify-out/graph.json"
            graph.parent.mkdir()
            graph.write_text("{}\n", encoding="utf-8")
            skill = project / ".agents/skills/graphify/SKILL.md"
            skill.parent.mkdir(parents=True)
            skill.write_text("# graphify\n", encoding="utf-8")
            memory = project / ".memory/config.json"
            memory.parent.mkdir()
            memory.write_text("{}\n", encoding="utf-8")
            self.assertEqual([], module.project_setup_plan(project, commands))

    def test_account_install_blocks_missing_tool_when_stable_lookup_is_offline(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        local = {
            name: {"status": "missing", "healthy": False, "version": None}
            for name in ("uv", "node", "java", "mempalace", "graphify", "memory", "context7")
        }
        with mock.patch.object(module, "discover_account_commands", return_value=(local, {})):
            with self.assertRaisesRegex(RuntimeError, "blocked"):
                module.install_account_dependencies(
                    Path("."),
                    specification,
                    opener=lambda *_args, **_kwargs: (_ for _ in ()).throw(OSError("offline")),
                    allow_root=True,
                )

    def test_freshness_is_read_only_and_stale_after_24_hours(self):
        module = load_controller()
        now = datetime(2026, 8, 12, tzinfo=timezone.utc)
        fresh = {"checkedAt": (now - timedelta(hours=23)).isoformat()}
        stale = {"checkedAt": (now - timedelta(hours=25)).isoformat()}
        self.assertEqual("fresh", module.freshness(fresh, now))
        self.assertEqual("stale", module.freshness(stale, now))

    def test_repair_runs_install_and_entrypoint_probes_then_writes_a_typed_receipt(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        calls = []

        def runner(command, environment):
            calls.append((command, environment.copy()))
            executable = Path(command[0])
            if not executable.exists() and executable.is_relative_to(Path(temporary)):
                executable.parent.mkdir(parents=True, exist_ok=True)
                executable.write_text("tool\n", encoding="utf-8")
            return SimpleNamespace(stdout="tool 1.0\n", stderr="")

        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            now = datetime(2026, 8, 12, tzinfo=timezone.utc)
            receipt = module.repair(runtime, specification, runner=runner, now=now)
            persisted = json.loads((runtime / "receipt.json").read_text(encoding="utf-8"))

        self.assertEqual(1, receipt["schemaVersion"])
        self.assertEqual(module.RUNTIME_CONTRACT_VERSION, receipt["runtimeContractVersion"])
        self.assertNotIn("links", receipt["ownership"])
        self.assertEqual(receipt, persisted)
        self.assertRegex(receipt["capabilityPolicySha256"], r"^[0-9a-f]{64}$")
        joined = " ".join(part.replace("\\", "/") for command, _ in calls for part in command)
        self.assertIn("mempalace", joined)
        self.assertIn("mempalace-mcp", joined)
        self.assertIn("graphify", joined)
        self.assertIn("dist/cli/main.js", joined)
        self.assertIn("dist/mcp/server.js", joined)
        self.assertNotIn("node_modules/.bin/memory", joined)
        self.assertTrue(all("UV_TOOL_DIR" in environment for _, environment in calls))

    def test_linkless_receipt_preserves_legacy_controller_ownership_shape(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            receipt = module.repair(
                runtime, specification, runner=self.fake_runner(runtime)
            )
            legacy_ownership = module.ownership_record(runtime)
            legacy_ownership.pop("links")
            metadata = {
                key: value
                for key, value in receipt.items()
                if key not in {"ownership", "receiptIntegritySha256"}
            }
            legacy_ownership["metadataSha256"] = module.hashlib.sha256(
                json.dumps(metadata, sort_keys=True, separators=(",", ":")).encode()
            ).hexdigest()

            self.assertNotIn("links", receipt["ownership"])
            self.assertEqual(legacy_ownership, receipt["ownership"])

    def test_internal_tool_link_is_canonicalized_recorded_and_relocatable(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))

        with tempfile.TemporaryDirectory() as temporary:
            first = Path(temporary) / "first/.chaos-engine-runtime"
            second = Path(temporary) / "second/.chaos-engine-runtime"

            def runner(command, environment):
                executable = Path(command[0])
                if command[1:3] == ["tool", "install"] or "install" in command[1:3]:
                    package = command[-1].split("==", 1)[0]
                    if package in {"graphifyy", "mempalace"}:
                        target_name = "graphify" if package == "graphifyy" else "mempalace"
                        target = first / "uv-tools" / package / "bin" / target_name
                        target.parent.mkdir(parents=True, exist_ok=True)
                        target.write_text("tool\n", encoding="utf-8")
                        link = first / "bin" / target_name
                        link.parent.mkdir(parents=True, exist_ok=True)
                        if not link.exists() and not link.is_symlink():
                            self.symlink_or_skip(target, link)
                if not executable.exists() and executable.is_relative_to(first):
                    executable.parent.mkdir(parents=True, exist_ok=True)
                    executable.write_text("tool\n", encoding="utf-8")
                return SimpleNamespace(stdout="tool 1.0\n", stderr="")

            receipt = module.repair(first, specification, runner=runner)
            links = {item["path"]: item["target"] for item in receipt["ownership"]["links"]}
            self.assertIn("bin/graphify", links)
            self.assertFalse(Path(links["bin/graphify"]).is_absolute())
            second.parent.mkdir()
            first.replace(second)

            module.verify_receipt(second, module.read_receipt(second), specification)
            self.assertEqual("tool\n", (second / "bin/graphify").read_text(encoding="utf-8"))
            graphify = second / "bin/graphify"
            graphify.unlink()
            self.symlink_or_skip(
                Path("../uv-tools/mempalace/bin/mempalace"), graphify
            )
            with self.assertRaisesRegex(ValueError, "ownership drift"):
                module.verify_receipt(
                    second, module.read_receipt(second), specification
                )

    def test_tool_venv_and_npm_link_shapes_are_portable_owned_links(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            targets = {
                "uv-tools/graphify/bin/graphify": "graphify\n",
                "uv-python/python3": "python\n",
                "npm/node_modules/@aictx/memory/bin/memory.js": "memory\n",
            }
            for relative, content in targets.items():
                target = runtime / relative
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_text(content, encoding="utf-8")
            links = {
                "bin/graphify": runtime / "uv-tools/graphify/bin/graphify",
                "bootstrap/bin/python3": runtime / "uv-python/python3",
                "bootstrap/bin/python": Path("python3"),
                "npm/node_modules/.bin/memory": Path("../@aictx/memory/bin/memory.js"),
            }
            for relative, target in links.items():
                link = runtime / relative
                link.parent.mkdir(parents=True, exist_ok=True)
                self.symlink_or_skip(target, link)

            module.canonicalize_runtime_links(runtime)
            ownership = module.ownership_record(runtime)
            recorded = {item["path"]: item["target"] for item in ownership["links"]}

            self.assertEqual(set(links), set(recorded))
            self.assertTrue(all(not Path(target).is_absolute() for target in recorded.values()))

    def test_runtime_links_reject_external_dangling_and_cycles(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            outside = root / "outside"
            outside.write_text("mine\n", encoding="utf-8")
            cases = {
                "external": outside,
                "dangling": Path("missing"),
                "cycle": Path("cycle"),
            }
            for name, target in cases.items():
                runtime = root / name
                runtime.mkdir()
                link = runtime / "cycle"
                self.symlink_or_skip(target, link)
                with self.subTest(name=name):
                    with self.assertRaisesRegex(ValueError, "link"):
                        module.canonicalize_runtime_links(runtime)

    def test_missing_managed_tools_rebuild_but_foreign_content_fails_closed(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        for missing in (("graphify",), ("mempalace",), ("graphify", "mempalace")):
            with self.subTest(missing=missing), tempfile.TemporaryDirectory() as temporary:
                runtime = Path(temporary) / ".chaos-engine-runtime"
                runner = self.fake_runner(runtime)
                first = module.repair(runtime, specification, runner=runner)
                for name in missing:
                    entrypoint = Path(module.executable(runtime / "bin", name))
                    entrypoint.unlink()
                repaired = module.repair(runtime, specification, runner=runner)
                self.assertNotEqual(first["checkedAt"], repaired["checkedAt"])
                for name in missing:
                    self.assertTrue(Path(module.executable(runtime / "bin", name)).is_file())

                unknown = runtime / "foreign.txt"
                unknown.write_text("mine\n", encoding="utf-8")
                with self.assertRaisesRegex(ValueError, "foreign|drift"):
                    module.repair(runtime, specification, runner=runner)
                self.assertEqual("mine\n", unknown.read_text(encoding="utf-8"))

    def test_missing_owned_target_behind_unchanged_shim_rebuilds(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"

            def runner(command, environment):
                del environment
                executable = Path(command[0])
                if command[1:3] == ["tool", "install"] and command[-1].startswith(
                    "graphifyy=="
                ):
                    target = runtime / "uv-tools/graphifyy/bin/graphify"
                    target.parent.mkdir(parents=True, exist_ok=True)
                    target.write_text("tool\n", encoding="utf-8")
                    link = runtime / "bin/graphify"
                    link.parent.mkdir(parents=True, exist_ok=True)
                    if not link.exists() and not link.is_symlink():
                        self.symlink_or_skip(target, link)
                if not executable.exists() and executable.is_relative_to(runtime.parent):
                    executable.parent.mkdir(parents=True, exist_ok=True)
                    executable.write_text("tool\n", encoding="utf-8")
                return SimpleNamespace(stdout="tool 1.0\n", stderr="")

            module.repair(runtime, specification, runner=runner)
            shim = runtime / "bin/graphify"
            target = runtime / "uv-tools/graphifyy/bin/graphify"
            target.unlink()
            self.assertTrue(shim.is_symlink())

            module.repair(runtime, specification, runner=runner)

            self.assertTrue(target.is_file())
            self.assertEqual("tool\n", shim.read_text(encoding="utf-8"))

    def test_specification_change_rebuilds_without_force(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            first = module.repair(runtime, specification, runner=self.fake_runner(runtime))
            changed = json.loads(json.dumps(specification))
            changed["tools"]["graphify"]["package"] = "graphifyy==next"
            upgraded = module.repair(runtime, changed, runner=self.fake_runner(runtime))

            self.assertNotEqual(first["specificationSha256"], upgraded["specificationSha256"])
            self.assertEqual(module.specification_digest(changed), upgraded["specificationSha256"])

    def test_runtime_contract_version_preserves_specification_digest_semantics(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        current = module.specification_digest(specification)
        current_version = getattr(module, "RUNTIME_CONTRACT_VERSION", 0)

        with mock.patch.object(
            module, "RUNTIME_CONTRACT_VERSION", current_version + 1, create=True
        ):
            changed = module.specification_digest(specification)

        self.assertEqual(current, changed)

    def test_previous_runtime_contract_receipt_rebuilds_on_normal_repair(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            receipt_path = runtime / module.RECEIPT_NAME
            receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
            receipt.pop("runtimeContractVersion")
            metadata = {
                key: value
                for key, value in receipt.items()
                if key not in {"ownership", "receiptIntegritySha256"}
            }
            receipt["ownership"]["metadataSha256"] = module.hashlib.sha256(
                json.dumps(metadata, sort_keys=True, separators=(",", ":")).encode()
            ).hexdigest()
            integrity = {
                key: value
                for key, value in receipt.items()
                if key != "receiptIntegritySha256"
            }
            receipt["receiptIntegritySha256"] = module.hashlib.sha256(
                json.dumps(integrity, sort_keys=True, separators=(",", ":")).encode()
            ).hexdigest()
            receipt_path.write_text(json.dumps(receipt), encoding="utf-8")
            calls = []
            runner = self.fake_runner(runtime)

            def counting_runner(command, environment):
                calls.append(command)
                return runner(command, environment)

            upgraded = module.repair(runtime, specification, runner=counting_runner)

            self.assertTrue(calls)
            self.assertEqual(
                module.specification_digest(specification),
                upgraded["specificationSha256"],
            )

    def test_malformed_authenticated_link_records_fail_closed(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            receipt_path = runtime / module.RECEIPT_NAME
            receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
            receipt["ownership"]["links"] = ["invalid"]
            integrity = {
                key: value
                for key, value in receipt.items()
                if key != "receiptIntegritySha256"
            }
            receipt["receiptIntegritySha256"] = module.hashlib.sha256(
                json.dumps(integrity, sort_keys=True, separators=(",", ":")).encode()
            ).hexdigest()
            receipt_path.write_text(json.dumps(receipt), encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "ownership record"):
                module.repair(runtime, specification, runner=self.fake_runner(runtime))

            removing = runtime.with_name(f"{runtime.name}.removing")
            runtime.replace(removing)
            with self.assertRaisesRegex(ValueError, "ownership record"):
                module.remove(
                    runtime,
                    specification,
                    removal_path=removing,
                    already_locked=True,
                )

    def test_remove_never_follows_a_retargeted_link(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            runtime = root / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            outside = root / "outside.txt"
            outside.write_text("mine\n", encoding="utf-8")
            entrypoint = Path(module.executable(runtime / "bin", "graphify"))
            entrypoint.unlink()
            self.symlink_or_skip(outside, entrypoint)

            with self.assertRaisesRegex(ValueError, "link|ownership drift"):
                module.remove(runtime, specification)

            self.assertEqual("mine\n", outside.read_text(encoding="utf-8"))

    def test_legacy_schema_v1_receipt_without_capability_digest_remains_readable(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            receipt = module.repair(runtime, specification, runner=self.fake_runner(runtime))
            receipt.pop("capabilityPolicySha256")
            metadata_receipt = {
                key: value for key, value in receipt.items()
                if key not in {"ownership", "receiptIntegritySha256"}
            }
            receipt["ownership"]["metadataSha256"] = module.hashlib.sha256(
                json.dumps(metadata_receipt, sort_keys=True, separators=(",", ":")).encode()
            ).hexdigest()
            integrity_receipt = {
                key: value for key, value in receipt.items()
                if key != "receiptIntegritySha256"
            }
            receipt["receiptIntegritySha256"] = module.hashlib.sha256(
                json.dumps(integrity_receipt, sort_keys=True, separators=(",", ":")).encode()
            ).hexdigest()
            (runtime / module.RECEIPT_NAME).write_text(
                json.dumps(receipt, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )

            loaded = module.read_receipt(runtime)
            module.verify_receipt(runtime, loaded, specification)

            self.assertNotIn("capabilityPolicySha256", loaded)
            upgraded = module.repair(
                runtime, specification, runner=self.fake_runner(runtime)
            )
            self.assertRegex(upgraded["capabilityPolicySha256"], r"^[0-9a-f]{64}$")

    def test_failed_repair_preserves_last_known_good_runtime(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))

        def failing_runner(command, environment):
            del command, environment
            raise OSError("offline")

        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            # An unknown addition is drift, so use an owned entrypoint as the marker.
            marker = Path(module.probe_plan(runtime)["graphify"][0][0])
            before = marker.read_text(encoding="utf-8")
            with self.assertRaisesRegex(RuntimeError, "uv install"):
                module.repair(runtime, specification, runner=failing_runner, force=True)
            self.assertEqual(before, marker.read_text(encoding="utf-8"))

    def test_failed_missing_only_repair_restores_state_and_retry_succeeds(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            runner = self.fake_runner(runtime)
            module.repair(runtime, specification, runner=runner)
            graphify = Path(module.executable(runtime / "bin", "graphify"))
            graphify.unlink()

            def fail(command, environment):
                del command, environment
                raise OSError("offline")

            with self.assertRaisesRegex(RuntimeError, "uv install"):
                module.repair(runtime, specification, runner=fail)
            self.assertTrue(runtime.is_dir())
            self.assertFalse(graphify.exists())

            module.repair(runtime, specification, runner=runner)
            self.assertTrue(graphify.is_file())

    def test_repair_rejects_an_unknown_runtime_without_claiming_it(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            runtime.mkdir()
            marker = runtime / "user-owned.txt"
            marker.write_text("mine\n", encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "receipt"):
                module.repair(runtime, specification, runner=self.fake_runner(runtime))

            self.assertEqual("mine\n", marker.read_text(encoding="utf-8"))
            self.assertFalse(runtime.with_name(f"{runtime.name}.backup").exists())

    def test_same_specification_repair_is_an_idempotent_health_check(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            calls = []

            def runner(command, environment):
                calls.append(command)
                return SimpleNamespace(stdout="ok\n", stderr="")

            module.repair(runtime, specification, runner=runner)

        self.assertEqual(6, len(calls))
        self.assertTrue(all("install" not in command for command in calls))

    def test_repair_restores_owned_runtime_when_replacement_creation_fails(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            before = (runtime / "receipt.json").read_bytes()
            original_mkdir = Path.mkdir

            def fail_replacement(path, *args, **kwargs):
                if path == runtime and not path.exists():
                    raise OSError("disk full")
                return original_mkdir(path, *args, **kwargs)

            with mock.patch.object(module.Path, "mkdir", fail_replacement):
                with self.assertRaisesRegex(OSError, "disk full"):
                    module.repair(
                        runtime,
                        specification,
                        runner=self.fake_runner(runtime),
                        force=True,
                    )

            self.assertEqual(before, (runtime / "receipt.json").read_bytes())
            self.assertFalse(runtime.with_name(f"{runtime.name}.backup").exists())

    def test_doctor_rejects_a_fabricated_or_drifted_receipt(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            runtime.mkdir()
            (runtime / "receipt.json").write_text(
                json.dumps({"schemaVersion": 1, "checkedAt": "2026-08-12T00:00:00+00:00"}),
                encoding="utf-8",
            )
            with self.assertRaisesRegex(ValueError, "schema"):
                module.doctor(runtime, runner=self.fake_runner(runtime), specification=specification)

    def test_remove_requires_exact_owned_runtime(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            module.remove(runtime, specification)
            self.assertFalse(runtime.exists())

    def test_status_ignores_direct_generated_python_cache_files(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            cache = runtime / "bin/__pycache__"
            cache.mkdir()
            (cache / "graphify.cpython-314.pyc").write_bytes(b"generated")

            try:
                result = module.status(runtime, specification)
            except ValueError as error:
                self.fail(f"generated cache rejected by status: {error}")

        self.assertEqual("healthy", result["status"])

    def test_new_receipt_excludes_direct_generated_python_cache_files(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            runner = self.fake_runner(runtime)

            def cache_creating_runner(command, environment):
                result = runner(command, environment)
                cache = runtime / "bin/__pycache__"
                cache.mkdir(parents=True, exist_ok=True)
                (cache / "graphify.cpython-314.pyc").write_bytes(b"generated")
                return result

            receipt = module.repair(runtime, specification, runner=cache_creating_runner)

        self.assertNotIn("bin/__pycache__", receipt["ownership"]["directories"])
        self.assertNotIn(
            "bin/__pycache__/graphify.cpython-314.pyc",
            receipt["ownership"]["files"],
        )

    def test_legacy_receipt_cache_entries_are_normalized_during_verification(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            cache = runtime / "bin/__pycache__"
            cache.mkdir()
            bytecode = cache / "graphify.cpython-314.pyc"
            bytecode.write_bytes(b"legacy")
            self.write_legacy_receipt_with_cache(module, runtime)
            bytecode.write_bytes(b"regenerated")

            try:
                result = module.status(runtime, specification)
            except ValueError as error:
                self.fail(f"legacy generated cache rejected by status: {error}")

        self.assertEqual("healthy", result["status"])

    def test_unknown_file_inside_python_cache_directory_still_fails_closed(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            cache = runtime / "bin/__pycache__"
            cache.mkdir()
            (cache / "graphify.cpython-314.pyc").write_bytes(b"generated")
            unknown = cache / "keep.txt"
            unknown.write_text("mine\n", encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "ownership drift"):
                module.status(runtime, specification)
            self.assertEqual("mine\n", unknown.read_text(encoding="utf-8"))

    def test_changed_owned_file_still_fails_closed(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            entrypoint = Path(module.probe_plan(runtime)["graphify"][0][0])
            entrypoint.write_text("changed\n", encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "ownership drift"):
                module.status(runtime, specification)

    def test_remove_deletes_direct_generated_python_cache_files(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            cache = runtime / "bin/__pycache__"
            cache.mkdir()
            (cache / "graphify.cpython-314.pyc").write_bytes(b"generated")

            try:
                module.remove(runtime, specification)
            except ValueError as error:
                self.fail(f"generated cache rejected by remove: {error}")

            self.assertFalse(runtime.exists())

    def test_remove_rejects_unknown_file_inside_python_cache_directory(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            cache = runtime / "bin/__pycache__"
            cache.mkdir()
            (cache / "graphify.cpython-314.pyc").write_bytes(b"generated")
            unknown = cache / "keep.txt"
            unknown.write_text("mine\n", encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "ownership drift"):
                module.remove(runtime, specification)

            self.assertEqual("mine\n", unknown.read_text(encoding="utf-8"))

    def test_upgrade_accepts_a_new_specification_and_receipts_uv(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            changed = json.loads(json.dumps(specification))
            changed["tools"]["memory"]["package"] = "@aictx/memory@next"
            receipt = module.repair(
                runtime, changed, runner=self.fake_runner(runtime), force=True
            )

            self.assertEqual(
                module.specification_digest(changed), receipt["specificationSha256"]
            )
            self.assertEqual(
                {"uv", "mempalace", "graphify", "memory"}, set(receipt["tools"])
            )

    def test_receipt_survives_relocation_and_doctor_binds_selected_specification(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            first = Path(temporary) / "first/.chaos-engine-runtime"
            second = Path(temporary) / "second/.chaos-engine-runtime"
            first.parent.mkdir()
            module.repair(first, specification, runner=self.fake_runner(first))
            second.parent.mkdir()
            first.replace(second)

            result = module.doctor(
                second, runner=self.fake_runner(second), specification=specification
            )

        self.assertEqual("healthy", result["status"])

    def test_receipt_metadata_tampering_and_future_timestamps_are_rejected(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            receipt_path = runtime / "receipt.json"
            receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
            receipt["checkedAt"] = "2099-01-01T00:00:00+00:00"
            receipt_path.write_text(json.dumps(receipt), encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "integrity drift"):
                module.status(runtime, specification)

    def test_post_publish_backup_is_recovered_on_retry(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            backup = runtime.with_name(f"{runtime.name}.backup")
            shutil.copytree(runtime, backup)

            receipt = module.repair(runtime, specification, runner=self.fake_runner(runtime))

            self.assertFalse(backup.exists())
            self.assertEqual(module.specification_digest(specification), receipt["specificationSha256"])

    def test_unknown_empty_directory_is_ownership_drift(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            (runtime / "user-empty").mkdir()

            with self.assertRaisesRegex(ValueError, "ownership drift"):
                module.status(runtime, specification)

    def test_mid_build_crash_restores_the_verified_backup(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            before = (runtime / "receipt.json").read_bytes()
            backup = runtime.with_name(f"{runtime.name}.backup")
            building = runtime.with_name(f"{runtime.name}.building")
            runtime.replace(backup)
            runtime.mkdir()
            (runtime / "partial.txt").write_text("partial", encoding="utf-8")
            building.write_text(module.BUILD_MARKER_MAGIC, encoding="utf-8")

            module.repair(runtime, specification, runner=self.fake_runner(runtime))

            self.assertEqual(before, (runtime / "receipt.json").read_bytes())
            self.assertFalse(backup.exists())
            self.assertFalse(building.exists())

    def test_initial_build_crash_removes_only_marker_authorized_partial_runtime(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            building = runtime.with_name(f"{runtime.name}.building")
            runtime.mkdir()
            (runtime / "partial.txt").write_text("partial", encoding="utf-8")
            building.write_text(module.BUILD_MARKER_MAGIC, encoding="utf-8")

            module.repair(runtime, specification, runner=self.fake_runner(runtime))

            self.assertTrue((runtime / "receipt.json").exists())
            self.assertFalse(building.exists())

    def test_cli_doctor_rejects_specification_drift(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            runtime = root / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            changed = json.loads(json.dumps(specification))
            changed["tools"]["memory"]["package"] = "@aictx/memory@next"
            selected = root / "dependencies.json"
            selected.write_text(json.dumps(changed), encoding="utf-8")
            with mock.patch.object(module, "run_command", self.fake_runner(runtime)):
                with mock.patch("sys.argv", ["dependencies.py", "doctor", "--runtime", str(runtime), "--specification", str(selected)]):
                    self.assertEqual(1, module.main())

    def test_cli_doctor_respects_the_runtime_lock(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            with module.runtime_lock(runtime):
                with mock.patch(
                    "sys.argv",
                    [
                        "dependencies.py",
                        "doctor",
                        "--runtime",
                        str(runtime),
                        "--specification",
                        str(SPECIFICATION),
                    ],
                ):
                    self.assertEqual(1, module.main())

    def test_owned_marker_scratch_is_recovered_on_retry(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            building = runtime.with_name(f"{runtime.name}.building")
            owned = building.with_name(
                f"{building.name}{module.BUILD_MARKER_OWNED_SUFFIX}"
            )
            building.write_text(module.BUILD_MARKER_MAGIC, encoding="utf-8")
            building.replace(owned)

            module.repair(runtime, specification, runner=self.fake_runner(runtime))

            self.assertTrue(runtime.exists())
            self.assertFalse(building.exists())
            self.assertFalse(owned.exists())

    def test_unowned_marker_scratch_collision_is_untouched(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            building = runtime.with_name(f"{runtime.name}.building")
            owned = building.with_name(
                f"{building.name}{module.BUILD_MARKER_OWNED_SUFFIX}"
            )
            owned.write_text("mine\n", encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "scratch collision"):
                module.repair(runtime, specification, runner=self.fake_runner(runtime))

            self.assertEqual("mine\n", owned.read_text(encoding="utf-8"))
            self.assertFalse(building.exists())

    def test_interrupted_remove_is_retryable_from_the_owned_removal_tree(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            original_unlink = Path.unlink
            failed = False

            def fail_once(path, *args, **kwargs):
                nonlocal failed
                if path.name != "receipt.json" and not failed:
                    failed = True
                    raise OSError("busy")
                return original_unlink(path, *args, **kwargs)

            with mock.patch.object(module.Path, "unlink", fail_once):
                with self.assertRaisesRegex(OSError, "busy"):
                    module.remove(runtime, specification)
            module.remove(runtime, specification)

            self.assertFalse(runtime.exists())
            self.assertFalse(runtime.with_name(f"{runtime.name}.removing").exists())

    def test_dependency_tests_are_reached_by_pull_request_gate(self):
        budget = json.loads(
            (ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
        )
        self.assertIn(
            "tests/scripts/test_chaos_engine_dependencies.py",
            budget["harness_reachability"]["element_globs"],
        )
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertIn("python scripts/ci/harness_pr_gate.py", workflow)
        gate = (ROOT / "scripts/ci/harness_pr_gate.py").read_text(encoding="utf-8")
        self.assertIn("tests.scripts.test_chaos_engine_dependencies", gate)


if __name__ == "__main__":
    unittest.main()
