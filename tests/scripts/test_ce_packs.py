"""Epic #6342 CE-10/CE-11: java and project packs live outside the agnostic core."""

from __future__ import annotations

import ast
import importlib.util
import json
import sys
import tempfile
import unittest
from pathlib import Path, PurePosixPath

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
SHAFT_PACK = ROOT / "shaft-skills/ce-pack"
JAVA_PACK = CE / "packs/java"


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


class AgnosticCoreZeroTests(unittest.TestCase):
    def test_core_leak_count_is_zero_and_allowlist_is_empty(self):
        lint = _load("ce_lean_lint_packs", ROOT / "scripts/ci/ce_lean_lint.py")
        self.assertEqual({}, lint.leak_counts())
        allow = json.loads((ROOT / "scripts/ci/ce_lean_allowlist.json").read_text(encoding="utf-8"))
        self.assertEqual({}, allow["leaks"])

    def test_only_the_portable_profile_lives_in_the_core(self):
        profiles = sorted(path.name for path in (CE / "profiles").iterdir() if path.is_dir())
        self.assertEqual(["portable"], profiles)
        self.assertNotIn("shaft", (CE / "distributions.json").read_text(encoding="utf-8").casefold())


class ProjectPackTests(unittest.TestCase):
    def setUp(self):
        self.install = _load("ce_install_packs", CE / "install.py")

    def test_shaft_pack_lives_in_shaft_skills_and_declares_its_distribution(self):
        profile = json.loads((SHAFT_PACK / "profile.json").read_text(encoding="utf-8"))
        self.assertEqual("shaft", profile["name"])
        self.assertEqual("repository", profile["distribution"]["id"])
        self.assertTrue((SHAFT_PACK / "entrypoint.md").is_file())
        self.assertTrue((SHAFT_PACK / "references/playbooks/java-tests.md").is_file())

    def test_matching_project_selects_the_pack_and_installs_it_under_packs(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "pom.xml").write_text(
                "<project><artifactId>demo</artifactId><dependencies><dependency>"
                "<artifactId>shaft-engine</artifactId></dependency></dependencies></project>",
                encoding="utf-8",
            )
            self.assertEqual("repository", self.install.detect_distribution(project, CE))
        files = self.install.source_files(CE, "repository")
        relatives = {self.install.payload_relative(CE, path).as_posix() for path in files}
        self.assertIn("packs/shaft/entrypoint.md", relatives)
        self.assertIn("packs/java/pack.json", relatives)
        self.assertFalse(any(item.startswith("profiles/shaft") for item in relatives))

    def test_unmatched_project_stays_portable_without_the_project_pack(self):
        with tempfile.TemporaryDirectory() as temporary:
            self.assertEqual("portable", self.install.detect_distribution(Path(temporary), CE))
        relatives = {
            self.install.payload_relative(CE, path).as_posix()
            for path in self.install.source_files(CE, "portable")
        }
        self.assertFalse(any(item.startswith("packs/shaft/") for item in relatives))

    def test_hard_cut_message_names_the_old_layout_and_the_new_one(self):
        with tempfile.TemporaryDirectory() as temporary:
            target = Path(temporary)
            (target / "profiles/shaft").mkdir(parents=True)
            (target / "profiles/shaft/entrypoint.md").write_text("old\n", encoding="utf-8")
            (target / "profiles/portable").mkdir(parents=True)
            self.assertEqual(["shaft"], self.install.legacy_profile_layout(target))
            message = self.install.hard_cut_message(["shaft"])
        self.assertIn("hard cut", message.casefold())
        self.assertIn("profiles/shaft", message)
        self.assertIn("packs/shaft", message)
        self.assertIn("rerun the installer", message)
        self.assertIn("replaced", self.install.hard_cut_message(["shaft"], replaced=True))

    def test_doctor_and_install_surface_the_hard_cut_notice(self):
        source = (ROOT / "chaos-engine/install.py").read_text(encoding="utf-8")
        bootstrap = (ROOT / "chaos-engine/bootstrap.py").read_text(encoding="utf-8")
        self.assertIn('print(f"WARNING: {migration[\'hardCut\']}")', source)
        self.assertIn("print(notice, file=sys.stderr)", source)
        self.assertIn("installer.hard_cut_message(legacy_packs", bootstrap)


class JavaPackTests(unittest.TestCase):
    def test_java_runtime_code_lives_in_the_pack_not_the_core_controllers(self):
        for name in ("hosts.py", "install.py"):
            source = (CE / name).read_text(encoding="utf-8")
            for definition in (
                "def ensure_managed_maven(",
                "def ensure_managed_temurin_jdk(",
                "def probe_maven_tools_runtime(",
                "def ensure_maven_tools(",
                "def repair_maven_tools(",
                "def maven_coordinate_ids(",
            ):
                self.assertNotIn(definition, source, f"{name}: {definition}")
        manifest = json.loads((JAVA_PACK / "pack.json").read_text(encoding="utf-8"))
        self.assertEqual("java", manifest["name"])
        self.assertIn("maven-tools-mcp", manifest["components"])

    def test_controllers_still_expose_the_bound_pack_api(self):
        hosts = _load("ce_hosts_packs", CE / "hosts.py")
        install = _load("ce_install_packs_api", CE / "install.py")
        for name in ("ensure_managed_maven", "probe_maven_tools_runtime", "maven_tools_cache_root"):
            self.assertTrue(callable(getattr(hosts, name)), name)
            self.assertEqual(hosts.__dict__, getattr(hosts, name).__globals__)
        for name in ("ensure_maven_tools", "repair_maven_tools", "maven_coordinate_ids"):
            self.assertTrue(callable(getattr(install, name)), name)
        self.assertIn("maven-tools-mcp", install.maven_tools_repair_fix_next())


class PackBindingContractTests(unittest.TestCase):
    def test_every_pack_function_is_exported_so_binding_resolves_it(self):
        for module in ("maven_tools.py", "installer.py"):
            path = ROOT / "chaos-engine/packs/java" / module
            tree = ast.parse(path.read_text(encoding="utf-8"))
            exported = next(
                ast.literal_eval(node.value)
                for node in tree.body
                if isinstance(node, ast.Assign) and getattr(node.targets[0], "id", "") == "__all__"
            )
            defined = [
                node.name
                for node in tree.body
                if isinstance(node, ast.FunctionDef) and node.name != "_core_helper"
            ]
            with self.subTest(module=module):
                self.assertEqual([], [name for name in defined if name not in exported])

    def test_core_placeholders_are_callable_until_bound(self):
        spec = importlib.util.spec_from_file_location(
            "ce_pack_placeholder_probe", ROOT / "chaos-engine/packs/java/maven_tools.py"
        )
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        self.assertTrue(callable(module.is_link_or_reparse))
        with self.assertRaises(RuntimeError):
            module.is_link_or_reparse(ROOT)


class BootstrapPackDownloadTests(unittest.TestCase):
    def test_bootstrap_downloads_ce_pack_directories_next_to_the_core(self):
        bootstrap = _load("ce_bootstrap_packs", CE / "bootstrap.py")
        self.assertTrue(bootstrap.is_pack_path(PurePosixPath("shaft-skills/ce-pack/profile.json")))
        self.assertFalse(bootstrap.is_pack_path(PurePosixPath("shaft-skills/shaft-api-testing/SKILL.md")))
        self.assertFalse(bootstrap.is_pack_path(PurePosixPath("ce-pack/profile.json")))


if __name__ == "__main__":
    unittest.main()
