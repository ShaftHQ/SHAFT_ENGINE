"""Maven Tools JDK preflight before mvn package (Windows JAVA_HOME fail-closed)."""

from __future__ import annotations

import inspect
import os
import tempfile
import unittest.mock as mock
from pathlib import Path
from types import SimpleNamespace
from unittest import TestCase, main

from tests.scripts.test_chaos_engine_installer import MODULE


class MavenToolsJavaPreflightTest(TestCase):
    def test_mutant_build_calls_preflight_before_package(self) -> None:
        """Removing preflight_maven_tools_jdk from the build path must turn RED."""
        source = inspect.getsource(MODULE._build_and_publish_maven_tools)
        self.assertIn("preflight_maven_tools_jdk", source)
        self.assertLess(
            source.index("preflight_maven_tools_jdk"),
            source.index("clean"),
        )

    def test_mutant_wrappers_declare_jdk_preflight(self) -> None:
        root = Path(__file__).resolve().parents[2] / "chaos-engine"
        ps1 = (root / "install.ps1").read_text(encoding="utf-8")
        sh = (root / "install.sh").read_text(encoding="utf-8")
        self.assertIn("Assert-ChaosEngineMavenToolsJdkPreflight", ps1)
        self.assertIn("assert_maven_tools_jdk_preflight", sh)
        # PowerShell StrictMode: never assign read-only $HOME / $home.
        fn_start = ps1.index("function Assert-ChaosEngineMavenToolsJdkPreflight")
        fn_end = ps1.index("\nfunction ", fn_start + 1)
        body = ps1[fn_start:fn_end]
        self.assertNotRegex(body, r"(?m)^\s*\$home\s*=")
        self.assertNotRegex(body, r"(?m)^\s*\$HOME\s*=")
        self.assertIn("$jdkHome", body)
        # Lean core: wrappers must not introduce Maven/Java/pom.xml ecosystem terms.
        import re
        ecosystem = re.compile(r"\bMaven\b|\bJava\b|pom\.xml")
        self.assertEqual([], ecosystem.findall(ps1))
        self.assertEqual([], ecosystem.findall(sh))

    def test_preflight_rejects_home_without_javac(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            java = root / "jdk" / "bin" / ("java.exe" if os.name == "nt" else "java")
            java.parent.mkdir(parents=True)
            java.write_text("java\n", encoding="utf-8")
            hosts = SimpleNamespace(
                java_major=lambda path: 25,
                java_compiler_present=lambda path: False,
            )
            with self.assertRaisesRegex(ValueError, "javac"):
                MODULE.preflight_maven_tools_jdk(hosts, java, required_major=25)

    def test_preflight_rejects_old_major_with_fix_next(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            java = root / "jdk" / "bin" / ("java.exe" if os.name == "nt" else "java")
            java.parent.mkdir(parents=True)
            java.write_text("java\n", encoding="utf-8")
            (java.parent / ("javac.exe" if os.name == "nt" else "javac")).write_text(
                "javac\n", encoding="utf-8"
            )
            hosts = SimpleNamespace(
                java_major=lambda path: 17,
                java_compiler_present=lambda path: True,
            )
            with self.assertRaises(ValueError) as caught:
                MODULE.preflight_maven_tools_jdk(hosts, java, required_major=25)
            message = str(caught.exception)
            self.assertIn("fix-next:", message)
            if os.name == "nt":
                self.assertIn("SetEnvironmentVariable", message)
            else:
                self.assertIn("export JAVA_HOME", message)

    def test_windows_fix_next_path_uses_literal_jdk_not_stale_env(self) -> None:
        import types

        fake_os = types.SimpleNamespace(name="nt")
        target = MODULE.maven_tools_java_home_fix_next
        with mock.patch.dict(target.__globals__, {"os": fake_os}):
            win = target(
                required_major=25,
                suggested=[r"C:\Program Files\Eclipse Adoptium\jdk-25"],
            )
        self.assertIn("$env:JAVA_HOME=$jdk", win)
        path_section = win.split("GetEnvironmentVariable('Path','User')", 1)[1].split(
            "then open", 1
        )[0]
        self.assertNotIn("$env:JAVA_HOME", path_section)
        self.assertIn("+$jdk+", path_section)
        self.assertIn("SetEnvironmentVariable", win)

    def test_missing_wrapper_fix_next_excludes_java_home(self) -> None:
        hosts = SimpleNamespace(
            java_major=lambda path: 25,
            java_compiler_present=lambda path: True,
        )
        with tempfile.TemporaryDirectory() as temporary:
            java = Path(temporary) / "jdk" / "bin" / ("java.exe" if os.name == "nt" else "java")
            java.parent.mkdir(parents=True)
            java.write_text("java\n", encoding="utf-8")
            (java.parent / ("javac.exe" if os.name == "nt" else "javac")).write_text(
                "javac\n", encoding="utf-8"
            )
            missing = Path(temporary) / "missing-mvnw"
            with self.assertRaises(ValueError) as caught:
                MODULE.preflight_maven_tools_jdk(
                    hosts, java, required_major=25, wrapper=missing
                )
            message = str(caught.exception)
            self.assertIn("wrapper is missing", message)
            self.assertIn("fix-next:", message)
            self.assertNotIn("JAVA_HOME", message)
            self.assertNotIn("SetEnvironmentVariable", message)

    def test_preflight_accepts_usable_jdk_home(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            home = root / "jdk"
            java = home / "bin" / ("java.exe" if os.name == "nt" else "java")
            java.parent.mkdir(parents=True)
            java.write_text("java\n", encoding="utf-8")
            (java.parent / ("javac.exe" if os.name == "nt" else "javac")).write_text(
                "javac\n", encoding="utf-8"
            )
            hosts = SimpleNamespace(
                java_major=lambda path: 25,
                java_compiler_present=lambda path: True,
            )
            resolved = MODULE.preflight_maven_tools_jdk(
                hosts, java, required_major=25
            )
            self.assertEqual(str(home.resolve()), resolved)

    def test_build_skips_package_when_preflight_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            cache = root / "cache"
            cache.mkdir()
            java = root / "jdk" / "bin" / ("java.exe" if os.name == "nt" else "java")
            java.parent.mkdir(parents=True)
            java.write_text("java\n", encoding="utf-8")
            calls: list[list[str]] = []

            def runner(command, **_kwargs):
                calls.append(list(command))
                if "clone" in command:
                    source = Path(command[-1])
                    source.mkdir(parents=True)
                    wrapper = source / ("mvnw.cmd" if os.name == "nt" else "mvnw")
                    wrapper.write_text("#!/bin/sh\n", encoding="utf-8")
                    (source / "pom.xml").write_text(
                        "<project><properties><jdk.version>25</jdk.version></properties></project>\n",
                        encoding="utf-8",
                    )
                    return mock.Mock(stdout="", stderr="", returncode=0)
                if "rev-parse" in command:
                    return mock.Mock(stdout=("a" * 40) + "\n", stderr="", returncode=0)
                raise AssertionError(f"unexpected command {command}")

            hosts = SimpleNamespace(
                maven_tools_cache_root=lambda: cache,
                java_major=lambda path: 11,
                java_compiler_present=lambda path: True,
                MAVEN_TOOLS_MCP_RECEIPT="install-receipt.json",
                publish_maven_tools_cache=mock.Mock(
                    side_effect=AssertionError("must not publish")
                ),
            )
            with self.assertRaisesRegex(ValueError, "fix-next:"):
                MODULE._build_and_publish_maven_tools(
                    hosts,
                    runner,
                    java,
                    "3.2.4",
                    "v3.2.4",
                    confirmer=None,
                    reporter=None,
                    specification={
                        "dependencies": {"java": {"minimumVersion": "25.0.0"}}
                    },
                )
            self.assertFalse(
                any(cmd[-4:] == ["clean", "package", "-Pci", "-DskipTests"] for cmd in calls)
            )

    def test_fix_next_hint_matches_java_home_cause(self) -> None:
        from importlib.util import module_from_spec, spec_from_file_location

        bootstrap_path = Path(__file__).resolve().parents[2] / "chaos-engine/bootstrap.py"
        spec = spec_from_file_location("ce_bootstrap_preflight", bootstrap_path)
        assert spec is not None and spec.loader is not None
        boot = module_from_spec(spec)
        spec.loader.exec_module(boot)
        hint = boot.next_fix_hint(
            "CE-INSTALL-FAILED",
            ValueError("JAVA_HOME is unset or unusable"),
            "py -3 .chaos-engine/install.py",
        )
        self.assertIn("JAVA_HOME", hint)
        self.assertIn("SetEnvironmentVariable", hint)

    def test_heal_handoff_writes_cause_and_fix_next(self) -> None:
        from importlib.util import module_from_spec, spec_from_file_location

        bootstrap_path = Path(__file__).resolve().parents[2] / "chaos-engine/bootstrap.py"
        spec = spec_from_file_location("ce_bootstrap_handoff", bootstrap_path)
        assert spec is not None and spec.loader is not None
        boot = module_from_spec(spec)
        spec.loader.exec_module(boot)
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine").mkdir()
            (project / ".chaos-engine" / "install.py").write_text("#\n", encoding="utf-8")
            boot.write_heal_handoff(
                project,
                {
                    "error_code": "CE-INSTALL-FAILED",
                    "failed_phase": "Install Maven Tools",
                    "unhealthy": "not reported",
                    "doctor_details": "not reported",
                    "doctor_command": "py -3 .chaos-engine/install.py doctor --project .",
                    "cause": "JAVA_HOME is unset or unusable",
                    "fix_next": "set JAVA_HOME then reopen PowerShell",
                },
                "https://example.invalid/issue",
            )
            handoff = (project / ".chaos-engine-state/heal-handoff.md").read_text(
                encoding="utf-8"
            )
            self.assertIn("Cause: JAVA_HOME is unset or unusable", handoff)
            self.assertIn("Fix-next: set JAVA_HOME then reopen PowerShell", handoff)

    def test_pom_java_release_major_reads_jdk_version(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            pom = Path(temporary) / "pom.xml"
            pom.write_text(
                "<project><properties><jdk.version>25</jdk.version></properties></project>\n",
                encoding="utf-8",
            )
            self.assertEqual(25, MODULE._pom_java_release_major(pom))


if __name__ == "__main__":
    main()
