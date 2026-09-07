"""Installer self-heal coverage for #5630 and Windows provision diagnostics #5629."""

from __future__ import annotations

import importlib.util
import json
import sys
import tempfile
import unittest
import unittest.mock as mock
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
HOSTS = ROOT / "chaos-engine" / "hosts.py"
INSTALL = ROOT / "chaos-engine" / "install.py"
DEPENDENCIES = ROOT / "chaos-engine" / "dependencies.py"
TOOL = ROOT / "chaos-engine" / "tool.py"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise AssertionError(f"failed to load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class InstallerSelfHeal5630Test(unittest.TestCase):
    def test_codex_orphan_context7_heals_and_keeps_foreign_mcp(self):
        module = load(HOSTS, "chaos_engine_hosts_5630_codex")
        before = (
            '[mcp_servers.context7]\nurl = "https://mcp.context7.com/mcp"\n\n'
            '[mcp_servers.my-custom]\ncommand = "keep-me"\n'
            'args = ["--ok"]\n'
        ).encode()
        rendered = module.codex_content(before).decode()
        self.assertIn("# CHAOSENGINE:START", rendered)
        self.assertIn('[mcp_servers.context7]\nurl = "https://mcp.context7.com/mcp"', rendered)
        self.assertIn("[mcp_servers.my-custom]", rendered)
        self.assertIn('command = "keep-me"', rendered)
        # Exactly one managed context7 stanza inside markers.
        self.assertEqual(1, rendered.count("# CHAOSENGINE:START"))
        self.assertIn("my-custom", rendered)

    def test_codex_drifted_managed_block_is_replaced_preserving_foreign(self):
        module = load(HOSTS, "chaos_engine_hosts_5630_codex_drift")
        drifted = (
            "# CHAOSENGINE:START\n"
            '[mcp_servers."chaosengine-memory"]\ncommand = "stale"\n'
            '[mcp_servers.user-wrapped]\ncommand = "preserve"\n'
            "# CHAOSENGINE:END\n"
            '[mcp_servers.outside]\ncommand = "also-keep"\n'
        )
        healed = module.codex_content(drifted.encode()).decode()
        self.assertIn("# CHAOSENGINE:START", healed)
        self.assertIn('".chaos-engine/tool.py", "memory-mcp"', healed)
        self.assertIn("[mcp_servers.user-wrapped]", healed)
        self.assertIn('command = "preserve"', healed)
        self.assertIn("[mcp_servers.outside]", healed)
        self.assertNotIn('command = "stale"', healed)

    def test_orphan_active_anchor_missing_core_quarantines(self):
        install = load(INSTALL, "chaos_engine_install_5630_orphan")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            token = "a" * 64
            (project / f".chaos-engine-hosts.active-{token}").write_bytes(b"")
            self.assertTrue(install.orphan_host_anchors_without_core(project))
            self.assertTrue(install.wiped_runtime_recovery_needed(project))
            moved = install.quarantine_orphaned_host_receipt(project)
            self.assertFalse((project / f".chaos-engine-hosts.active-{token}").exists())
            quarantined = list(
                (project / ".chaos-engine-state").glob(
                    "orphaned-.chaos-engine-hosts.active-*"
                )
            )
            self.assertTrue(quarantined)
            # No receipt case may return None, but anchors must move.
            self.assertTrue(quarantined or moved is not None)

    def test_healthy_deps_receipt_matching_token_not_quarantined(self):
        install = load(INSTALL, "chaos_engine_install_5630_live")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            core = project / ".chaos-engine"
            core.mkdir()
            # Minimal fake core that try_verify_install cannot validate → stale path
            # uses account_dependency_receipt_missing gate. Present deps receipt blocks.
            (project / ".chaos-engine-dependencies.json").write_text(
                json.dumps({"schemaVersion": 2, "components": {}, "commands": {}}),
                encoding="utf-8",
            )
            (project / f".chaos-engine-hosts.active-{'b' * 64}").write_bytes(b"")
            self.assertFalse(install.account_dependency_receipt_missing(project))
            self.assertFalse(install.stale_host_state_after_wiped_runtime(project))
            # Core exists so orphan-without-core is false.
            self.assertFalse(install.orphan_host_anchors_without_core(project))
            self.assertFalse(install.wiped_runtime_recovery_needed(project))
            self.assertIsNone(install.quarantine_orphaned_host_receipt(project))
            self.assertTrue(
                (project / f".chaos-engine-hosts.active-{'b' * 64}").exists()
            )

    def test_mcp_runtime_desync_is_compatible_legacy_not_recovery(self):
        hosts = load(HOSTS, "chaos_engine_hosts_5630_desync")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            palace = project / ".chaos-engine-state" / "mempalace"
            palace.mkdir(parents=True)
            # Minimal healthy sqlite_exact fixture via status mock.
            tool = project / ".chaos-engine" / "tool.py"
            tool.parent.mkdir(parents=True)
            tool.write_text("# tool\n", encoding="utf-8")

            desync = mock.Mock(
                returncode=1,
                stdout="",
                stderr=(
                    "primary checkout HEAD (aaa) != origin/main (bbb) "
                    "(not synchronized with origin/main). "
                    "fix-next: git fetch origin main && git merge --ff-only origin/main\n"
                ),
            )
            healthy = mock.Mock(
                returncode=0,
                stdout="\n".join(
                    (
                        json.dumps({"jsonrpc": "2.0", "id": 1, "result": {}}),
                        json.dumps(
                            {"jsonrpc": "2.0", "id": 2, "result": {"tools": []}}
                        ),
                    )
                )
                + "\n",
            )

            with mock.patch.object(
                hosts, "mempalace_directory_status", return_value={"status": "healthy"}
            ), mock.patch.object(
                hosts, "resolved_central_palace", return_value=palace
            ), mock.patch.object(
                hosts, "repository_map_resolver_present", return_value=False
            ), mock.patch.object(
                hosts.subprocess, "run", side_effect=[desync, healthy]
            ):
                status = hosts.mcp_runtime_status(project, managed_python=Path(sys.executable))

            self.assertEqual("compatible-legacy", status["status"])
            self.assertEqual("memory-origin-main-desync", status["detail"])
            self.assertIn("git fetch origin main", status["fixNext"])

    def test_mcp_runtime_real_memory_crash_still_recovery_required(self):
        hosts = load(HOSTS, "chaos_engine_hosts_5630_crash")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            palace = project / ".chaos-engine-state" / "mempalace"
            palace.mkdir(parents=True)
            tool = project / ".chaos-engine" / "tool.py"
            tool.parent.mkdir(parents=True)
            tool.write_text("# tool\n", encoding="utf-8")
            crashed = mock.Mock(returncode=17, stdout="", stderr="segfault boom")
            with mock.patch.object(
                hosts, "mempalace_directory_status", return_value={"status": "healthy"}
            ), mock.patch.object(
                hosts, "resolved_central_palace", return_value=palace
            ), mock.patch.object(
                hosts, "repository_map_resolver_present", return_value=False
            ), mock.patch.object(hosts.subprocess, "run", return_value=crashed):
                status = hosts.mcp_runtime_status(project, managed_python=Path(sys.executable))
            self.assertEqual("recovery-required", status["status"])
            self.assertEqual("memory-mcp-exit", status["detail"])

    def test_tool_memory_still_hard_fails_on_desync(self):
        tool = load(TOOL, "chaos_engine_tool_5630")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "tools/repository-map").mkdir(parents=True)
            (project / "tools/repository-map/resolve_mempalace.py").write_text(
                "def find_shared_mempalace(p): return p\n", encoding="utf-8"
            )
            with mock.patch.object(
                tool, "shared_project_root", return_value=project
            ), mock.patch.object(
                tool, "origin_main_revisions", return_value=("aaa", "bbb")
            ):
                with self.assertRaisesRegex(ValueError, "not synchronized with origin/main"):
                    tool.enforce_tool_origin_main_policy(project, "memory-mcp")

    def test_jre_without_javac_provisions_managed_jdk(self):
        hosts = load(HOSTS, "chaos_engine_hosts_5630_jdk")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            jre_java = root / "jre" / "bin" / "java"
            jre_java.parent.mkdir(parents=True)
            jre_java.write_bytes(b"java")
            self.assertFalse(hosts.java_compiler_present(jre_java))

            jdk_root = root / "temurin" / "25.0.4+7" / "linux-x64"
            jdk_java = jdk_root / "bin" / "java"
            jdk_javac = jdk_root / "bin" / "javac"
            jdk_java.parent.mkdir(parents=True)
            jdk_java.write_bytes(b"jdk")
            jdk_javac.write_bytes(b"javac")
            (jdk_root / hosts.TEMURIN_RECEIPT).write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "runtime": "temurin",
                        "version": "25.0.4+7",
                        "hostPlatform": "linux-x64",
                        "artifactArchitecture": "x64",
                        "emulated": False,
                        "java": "bin/java",
                        "javaSha256": __import__("hashlib")
                        .sha256(b"jdk")
                        .hexdigest(),
                    },
                    sort_keys=True,
                )
                + "\n",
                encoding="utf-8",
            )
            with mock.patch.object(hosts, "managed_temurin_root", return_value=jdk_root), mock.patch.object(
                hosts, "java_major", return_value=25
            ):
                ensured = hosts.ensure_managed_temurin_jdk(None)
            self.assertEqual(jdk_java.resolve(), ensured)

    def test_windows_missing_launcher_emits_named_diagnostic(self):
        deps = load(DEPENDENCIES, "chaos_engine_deps_5629")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)

            def boom(*_args, **_kwargs):
                raise FileNotFoundError(2, "The system cannot find the file specified", "pwsh")

            with mock.patch.object(deps, "resolve_account_launcher", side_effect=FileNotFoundError("pwsh")):
                with self.assertRaisesRegex(RuntimeError, r"dependency launcher not found: pwsh"):
                    deps._run_account_command(["pwsh", "-c", "1"], project, runner=boom)

    def test_resolve_account_launcher_prefers_powershell_when_pwsh_missing(self):
        deps = load(DEPENDENCIES, "chaos_engine_deps_5629_which")
        which = lambda name, path=None: (
            "C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"
            if name in {"powershell", "powershell.exe"}
            else None
        )
        with mock.patch.object(deps.os, "name", "nt"):
            resolved = deps.resolve_account_launcher("pwsh", which=which)
        self.assertTrue(resolved.lower().endswith("powershell.exe"))

    def test_mempalace_operator_bak_quarantined(self):
        hosts = load(HOSTS, "chaos_engine_hosts_5630_bak")
        with tempfile.TemporaryDirectory() as temporary:
            palace = Path(temporary)
            bak = palace / "sqlite_exact.sqlite3.bak"
            bak.write_bytes(b"operator-backup")
            # No exact db → initialization after quarantine of bak alone still init.
            moved = hosts.quarantine_mempalace_operator_backups(palace, [bak])
            self.assertTrue(moved)
            self.assertFalse(bak.exists())
            self.assertTrue((palace / ".chaos-engine-quarantine" / "sqlite_exact.sqlite3.bak").exists())


if __name__ == "__main__":
    unittest.main()
