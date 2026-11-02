"""CE 2026.10.31.11: L94 installer self-provision + 3-item failure UX (L74)."""

from __future__ import annotations

import inspect
import io
import os
import tempfile
import unittest.mock as mock
from pathlib import Path
from types import SimpleNamespace
from unittest import TestCase, main

import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))
from tests.scripts.test_chaos_engine_installer import MODULE  # noqa: E402


CE = Path(__file__).resolve().parents[2] / "chaos-engine"



def _ce_key(value: str):
    import importlib.util as _u
    from pathlib import Path as _P

    spec = _u.spec_from_file_location("ce_version", _P(__file__).resolve().parents[2] / "scripts/ci/ce_version.py")
    mod = _u.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod.version_key(value)

class VersionFloorTest(TestCase):
    def test_version_at_least_2026_10_31_11(self) -> None:
        version = (CE / "VERSION").read_text(encoding="utf-8").strip()
        floor = _ce_key("2026.10.31.11")
        self.assertGreaterEqual(_ce_key(version), floor)


class L94InstallerSelfProvisionTest(TestCase):
    def test_mutant_build_self_provisions_cli_and_env(self) -> None:
        """L74: removing resolve/apply from the build path must turn RED."""
        source = inspect.getsource(MODULE._build_and_publish_maven_tools)
        self.assertIn("resolve_maven_tools_build_cli", source)
        self.assertIn("apply_jdk_environment", source)
        self.assertLess(
            source.index("apply_jdk_environment"),
            source.index("clean"),
        )
        self.assertLess(
            source.index("resolve_maven_tools_build_cli"),
            source.index("clean"),
        )

    def test_missing_wrapper_falls_back_to_managed_maven(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source"
            source.mkdir()
            managed = root / "tools" / "bin" / ("mvn.cmd" if os.name == "nt" else "mvn")
            managed.parent.mkdir(parents=True)
            managed.write_text("mvn\n", encoding="utf-8")
            hosts = SimpleNamespace(
                ensure_managed_maven=mock.Mock(return_value=managed),
            )
            resolved = MODULE.resolve_maven_tools_build_cli(
                source, hosts, {"dependencies": {}}, opener=None
            )
            self.assertEqual(managed.resolve(), resolved.resolve())
            hosts.ensure_managed_maven.assert_called_once()

    def test_missing_wrapper_and_maven_is_unfixable(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            source = Path(temporary) / "source"
            source.mkdir()
            hosts = SimpleNamespace(ensure_managed_maven=mock.Mock(return_value=None))
            with self.assertRaisesRegex(ValueError, "mvn path unresolved"):
                MODULE.resolve_maven_tools_build_cli(source, hosts, {}, opener=None)

    def test_apply_jdk_environment_sets_session_env(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary) / "jdk"
            (home / "bin").mkdir(parents=True)
            previous_home = os.environ.get("JAVA_HOME")
            previous_path = os.environ.get("PATH", "")
            try:
                MODULE.apply_jdk_environment(str(home), persist_user=False)
                self.assertEqual(str(home), os.environ["JAVA_HOME"])
                self.assertEqual(str(home / "bin"), os.environ["PATH"].split(os.pathsep)[0])
            finally:
                if previous_home is None:
                    os.environ.pop("JAVA_HOME", None)
                else:
                    os.environ["JAVA_HOME"] = previous_home
                os.environ["PATH"] = previous_path

    def test_build_uses_managed_maven_when_wrapper_missing(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            cache = root / "cache"
            cache.mkdir()
            java = root / "jdk" / "bin" / ("java.exe" if os.name == "nt" else "java")
            java.parent.mkdir(parents=True)
            java.write_text("java\n", encoding="utf-8")
            (java.parent / ("javac.exe" if os.name == "nt" else "javac")).write_text(
                "javac\n", encoding="utf-8"
            )
            managed = root / "maven" / "bin" / ("mvn.cmd" if os.name == "nt" else "mvn")
            managed.parent.mkdir(parents=True)
            managed.write_text("mvn\n", encoding="utf-8")
            calls: list[list[str]] = []

            def runner(command, **kwargs):
                calls.append(list(command))
                if "clone" in command:
                    source = Path(command[-1])
                    source.mkdir(parents=True)
                    # Intentionally no mvnw — must fall back to managed Maven.
                    (source / "pom.xml").write_text(
                        "<project><properties><jdk.version>25</jdk.version></properties></project>\n",
                        encoding="utf-8",
                    )
                    return mock.Mock(stdout="", stderr="", returncode=0)
                if "rev-parse" in command:
                    return mock.Mock(stdout=("a" * 40) + "\n", stderr="", returncode=0)
                if command[-4:] == ["clean", "package", "-Pci", "-DskipTests"]:
                    source = Path(kwargs["cwd"])
                    target = source / "target"
                    target.mkdir(parents=True)
                    jar = target / "maven-tools-mcp-3.2.4.jar"
                    jar.write_bytes(b"jar")
                    return mock.Mock(stdout="", stderr="", returncode=0)
                raise AssertionError(f"unexpected command {command}")

            hosts = SimpleNamespace(
                maven_tools_cache_root=lambda: cache,
                java_major=lambda path: 25,
                java_compiler_present=lambda path: True,
                ensure_managed_maven=mock.Mock(return_value=managed),
                ensure_managed_temurin_jdk=mock.Mock(return_value=None),
                MAVEN_TOOLS_MCP_RECEIPT="install-receipt.json",
                publish_maven_tools_cache=mock.Mock(),
            )
            MODULE._build_and_publish_maven_tools(
                hosts,
                runner,
                java,
                "3.2.4",
                "v3.2.4",
                confirmer=None,
                reporter=None,
                specification={"dependencies": {"java": {"minimumVersion": "25.0.0"}}},
            )
            package_calls = [
                cmd for cmd in calls if cmd[-4:] == ["clean", "package", "-Pci", "-DskipTests"]
            ]
            self.assertEqual(1, len(package_calls))
            self.assertEqual(str(managed), package_calls[0][0])
            hosts.ensure_managed_maven.assert_called()
            hosts.publish_maven_tools_cache.assert_called_once()


class L94FailureUxThreeItemsTest(TestCase):
    def test_mutant_emit_install_failure_is_three_items(self) -> None:
        """L74: verbose stderr diagnostics must not return on the user path."""
        from importlib.util import module_from_spec, spec_from_file_location

        bootstrap_path = CE / "bootstrap.py"
        spec = spec_from_file_location("ce_bootstrap_l94", bootstrap_path)
        assert spec is not None and spec.loader is not None
        boot = module_from_spec(spec)
        spec.loader.exec_module(boot)
        source = inspect.getsource(boot.emit_install_failure)
        self.assertIn("Installation failed", source)
        self.assertIn("Launch your agent with this prompt", source)
        self.assertNotIn("Next fix:", source)
        self.assertNotIn("Filesystem:", source)
        self.assertNotIn('print(f"Status:', source)
        self.assertNotIn('print(f"Doctor:', source)
        self.assertNotIn('print(f"Help:', source)

    def test_emit_install_failure_prints_only_three_user_items(self) -> None:
        from importlib.util import module_from_spec, spec_from_file_location

        bootstrap_path = CE / "bootstrap.py"
        spec = spec_from_file_location("ce_bootstrap_l94_runtime", bootstrap_path)
        assert spec is not None and spec.loader is not None
        boot = module_from_spec(spec)
        spec.loader.exec_module(boot)
        buf = io.StringIO()
        with mock.patch.object(boot.sys, "stderr", buf):
            with mock.patch.object(
                boot,
                "publish_installer_issue",
                return_value="https://github.com/ShaftHQ/SHAFT_ENGINE/issues/new?title=x",
            ):
                with mock.patch.object(boot, "installer_issue_fields", return_value={
                    "error_code": "CE-INSTALL-FAILED",
                    "cause": "managed Maven auto-provision failed",
                    "doctor_command": "python3 .chaos-engine/install.py doctor --project .",
                    "failed_phase": "Install Maven Tools",
                    "unhealthy": "maven-tools-mcp",
                    "doctor_details": "not reported",
                }):
                    with mock.patch.object(boot, "write_heal_handoff"):
                        with mock.patch.object(boot, "core_install_py", return_value=False):
                            with mock.patch.object(boot, "present_state_artifacts", return_value=[]):
                                with mock.patch.object(
                                    boot,
                                    "heal_handoff_prompt",
                                    return_value="fix the install and open upstream issue",
                                ):
                                    boot.emit_install_failure(
                                        "CE-INSTALL-FAILED",
                                        ValueError("mvn path unresolved"),
                                        "ShaftHQ/SHAFT_ENGINE",
                                    )
        err = buf.getvalue()
        self.assertIn("Installation failed", err)
        self.assertIn("Open issue:", err)
        self.assertIn("issues/new?", err)
        self.assertIn("Launch your agent with this prompt:", err)
        self.assertIn("fix the install and open upstream issue", err)
        self.assertNotIn("Next fix:", err)
        self.assertNotIn("Filesystem:", err)
        self.assertNotIn("Doctor:", err)
        self.assertNotIn("Status:", err)
        self.assertNotIn("Help:", err)

    def test_l94_reference_exists(self) -> None:
        path = CE / "references/details/ce-rules-2026-10-31-11/installer-self-provision.md"
        body = path.read_text(encoding="utf-8")
        self.assertIn("L94", body)
        self.assertIn("three user-facing", body.casefold())
        self.assertIn("auto-provision", body.casefold())


if __name__ == "__main__":
    main()
