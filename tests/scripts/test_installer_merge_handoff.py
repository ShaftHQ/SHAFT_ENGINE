"""Installer-program merge, profile, upgrade, and handoff proofs."""

from __future__ import annotations

import hashlib
import importlib.util
import io
import json
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
HOSTS = ROOT / "chaos-engine" / "hosts.py"
INSTALL = ROOT / "chaos-engine" / "install.py"
BOOTSTRAP = ROOT / "chaos-engine" / "bootstrap.py"
SOURCE = ROOT / "chaos-engine"
TEST_COMMIT = "1" * 40
NEXT_COMMIT = "2" * 40


def load_module(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def load_hosts():
    return load_module(HOSTS, "ce_merge_handoff_hosts")


def sha256_bytes(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def seed_core(project: Path, install, commit: str = TEST_COMMIT, distribution: str = "portable"):
    install.install(project, SOURCE, commit, distribution=distribution)


def bind_hosts(hosts, project: Path, commit: str = TEST_COMMIT):
    return hosts.install(project, core_commit=commit)


class InstructionMergeHandoffTest(unittest.TestCase):
    def setUp(self) -> None:
        self.hosts = load_hosts()
        self.hosts.consume_merge_handoffs()

    def test_absent_markers_append_the_owned_block(self) -> None:
        merged = self.hosts.merge_instruction(
            b"foreign notes\n",
            self.hosts.INSTRUCTION,
            relative="AGENTS.md",
        )
        text = merged.decode("utf-8")
        self.assertIn("foreign notes\n", text)
        self.assertIn(self.hosts.START, text)
        self.assertEqual(self.hosts.consume_merge_handoffs(), [])

    def test_current_span_is_replaced_without_handoff(self) -> None:
        before = ("keep\n" + self.hosts.INSTRUCTION + "tail\n").encode()
        merged = self.hosts.merge_instruction(
            before, self.hosts.INSTRUCTION, relative="AGENTS.md"
        )
        text = merged.decode("utf-8")
        self.assertIn("keep\n", text)
        self.assertIn("tail\n", text)
        self.assertEqual(text.count(self.hosts.START), 1)
        self.assertEqual(self.hosts.consume_merge_handoffs(), [])

    def test_edited_span_is_left_unchanged_and_handed_off(self) -> None:
        original = (
            "foreign\n"
            f"{self.hosts.START}\noperator edited this span\n{self.hosts.END}\n"
        ).encode()
        merged = self.hosts.merge_instruction(
            original, self.hosts.INSTRUCTION, relative="AGENTS.md"
        )
        self.assertEqual(merged, original)
        notes = self.hosts.consume_merge_handoffs()
        self.assertEqual(len(notes), 1)
        self.assertEqual(notes[0]["path"], "AGENTS.md")
        self.assertIn("interior", notes[0]["reason"])

    def test_split_markers_are_left_unchanged_and_handed_off(self) -> None:
        original = (
            f"{self.hosts.START}\none\n{self.hosts.END}\n"
            f"{self.hosts.START}\ntwo\n{self.hosts.END}\n"
        ).encode()
        merged = self.hosts.merge_instruction(
            original, self.hosts.INSTRUCTION, relative="AGENTS.md"
        )
        self.assertEqual(merged, original)
        notes = self.hosts.consume_merge_handoffs()
        self.assertEqual(len(notes), 1)
        self.assertIn("marker count", notes[0]["reason"])

    def test_invalid_utf8_instruction_is_byte_preserved(self) -> None:
        original = b"\xff\xfe foreign"
        merged = self.hosts.merge_instruction(
            original, self.hosts.INSTRUCTION, relative="AGENTS.md"
        )
        self.assertEqual(merged, original)
        notes = self.hosts.consume_merge_handoffs()
        self.assertEqual(len(notes), 1)
        self.assertIn("UTF-8", notes[0]["reason"])

    def test_handoff_markdown_names_the_path_and_prompt(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw)
            self.hosts.merge_instruction(
                f"{self.hosts.START}\nedited\n{self.hosts.END}\n".encode(),
                self.hosts.INSTRUCTION,
                relative="CLAUDE.md",
            )
            written = self.hosts.write_merge_handoff(
                project, "python3 .chaos-engine/install.py doctor --project ."
            )
            self.assertIsNotNone(written)
            body = written.read_text(encoding="utf-8")
            self.assertIn("## CLAUDE.md", body)
            self.assertIn("Desired owned block:", body)
            prompt = self.hosts.merge_handoff_prompt(
                "python3 .chaos-engine/install.py doctor --project ."
            )
            self.assertTrue(prompt.startswith("Merge ChaosEngine host configuration"))
            self.assertIn(".chaos-engine-state/merge-handoff.md", prompt)
            self.assertNotIn("\n", prompt)


class NamedRecordMergeHandoffTest(unittest.TestCase):
    def setUp(self) -> None:
        self.hosts = load_hosts()
        self.hosts.consume_merge_handoffs()

    def test_unknown_same_name_mcp_is_byte_preserved(self) -> None:
        original = json.dumps(
            {
                "mcpServers": {
                    "chaosengine-memory": {
                        "command": "foreign-memory",
                        "args": ["--owned-by-operator"],
                    },
                    "keep-me": {"command": "keep", "args": []},
                }
            },
            indent=2,
            sort_keys=True,
        ).encode() + b"\n"
        merged = self.hosts.json_content(original)
        self.assertEqual(merged, original)
        notes = self.hosts.consume_merge_handoffs()
        self.assertEqual(len(notes), 1)
        self.assertEqual(notes[0]["path"], ".mcp.json")
        self.assertIn("unknown ownership", notes[0]["reason"])

    def test_unparsable_mcp_json_is_byte_preserved(self) -> None:
        original = b"{not-json"
        merged = self.hosts.json_content(original)
        self.assertEqual(merged, original)
        notes = self.hosts.consume_merge_handoffs()
        self.assertEqual(len(notes), 1)
        self.assertIn("parse", notes[0]["reason"])

    def test_codex_marker_collision_is_byte_preserved(self) -> None:
        original = (
            b"# CHAOSENGINE:START\n[mcp_servers.x]\n"
            b"# CHAOSENGINE:START\n# CHAOSENGINE:END\n"
        )
        merged = self.hosts.codex_content(original)
        self.assertEqual(merged, original)
        notes = self.hosts.consume_merge_handoffs()
        self.assertEqual(len(notes), 1)
        self.assertEqual(notes[0]["path"], ".codex/config.toml")

    def test_gitattributes_marker_collision_is_byte_preserved(self) -> None:
        original = b"# CHAOSENGINE-EOL:START\nchanged\n"
        merged = self.hosts.gitattributes_content(original)
        self.assertEqual(merged, original)
        notes = self.hosts.consume_merge_handoffs()
        self.assertTrue(notes)
        self.assertIn("marker count", notes[0]["reason"])


class InstallProfileSelectionTest(unittest.TestCase):
    def setUp(self) -> None:
        self.install = load_module(INSTALL, "ce_profile_install")
        self.hosts = load_hosts()
        self.hosts.consume_merge_handoffs()

    def test_empty_and_non_java_stay_portable_java_shaft_selects_repository(self) -> None:
        source = ROOT / "chaos-engine"
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            empty = root / "empty"
            empty.mkdir()
            non_java = root / "node"
            non_java.mkdir()
            (non_java / "package.json").write_text("{}", encoding="utf-8")
            java = root / "shaft"
            java.mkdir()
            (java / "pom.xml").write_text(
                "<project><artifactId>shaft-engine</artifactId></project>\n",
                encoding="utf-8",
            )
            self.assertEqual(self.install.detect_distribution(empty, source), "portable")
            self.assertEqual(self.install.detect_distribution(non_java, source), "portable")
            self.assertEqual(self.install.detect_distribution(java, source), "repository")

    def _required_names(self, project: Path) -> dict[str, str]:
        doctor = self.install.status_with_dependencies(project)
        return self.install.required_component_statuses(doctor)

    def test_empty_first_install_is_portable_without_handoff_or_required_maven_tools(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw) / "empty"
            project.mkdir()
            seed_core(project, self.install)
            bind_hosts(self.hosts, project)
            manifest = json.loads(
                (project / ".chaos-engine/manifest.json").read_text(encoding="utf-8")
            )
            self.assertEqual("portable", manifest["distribution"]["id"])
            self.assertFalse((project / ".chaos-engine-state/merge-handoff.md").exists())
            required = self._required_names(project)
            self.assertNotIn("maven-tools-mcp", required)
            status = self.install.status_with_dependencies(project)
            maven = status["components"]["maven-tools-mcp"]
            self.assertEqual("optional", maven["taskImpact"])
            if maven.get("status") != "healthy":
                self.assertNotIn("maven-tools-mcp", required)

    def test_java_first_install_keeps_pom_bytes_and_requires_maven_tools(self) -> None:
        pom = "<project><artifactId>shaft-engine</artifactId></project>\n"
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw) / "shaft"
            project.mkdir()
            pom_path = project / "pom.xml"
            pom_path.write_text(pom, encoding="utf-8")
            digest = sha256_bytes(pom_path.read_bytes())
            distribution = self.install.detect_distribution(project, SOURCE)
            self.assertEqual("repository", distribution)
            seed_core(project, self.install, distribution=distribution)
            bind_hosts(self.hosts, project)
            self.assertEqual(digest, sha256_bytes(pom_path.read_bytes()))
            manifest = json.loads(
                (project / ".chaos-engine/manifest.json").read_text(encoding="utf-8")
            )
            self.assertEqual("repository", manifest["distribution"]["id"])
            status = self.install.status_with_dependencies(project)
            maven = status["components"]["maven-tools-mcp"]
            self.assertEqual("required", maven["taskImpact"])
            self.assertFalse((project / ".chaos-engine-state/merge-handoff.md").exists())

    def test_non_java_first_install_stays_portable_without_required_maven_tools(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw) / "node"
            project.mkdir()
            package = project / "package.json"
            package.write_text('{"name":"demo"}\n', encoding="utf-8")
            digest = sha256_bytes(package.read_bytes())
            seed_core(project, self.install)
            bind_hosts(self.hosts, project)
            self.assertEqual(digest, sha256_bytes(package.read_bytes()))
            manifest = json.loads(
                (project / ".chaos-engine/manifest.json").read_text(encoding="utf-8")
            )
            self.assertEqual("portable", manifest["distribution"]["id"])
            required = self._required_names(project)
            self.assertNotIn("maven-tools-mcp", required)
            self.assertFalse((project / ".chaos-engine-state/merge-handoff.md").exists())


class InstallUpgradeHandoffTest(unittest.TestCase):
    def setUp(self) -> None:
        self.install = load_module(INSTALL, "ce_upgrade_install")
        self.hosts = load_hosts()
        self.hosts.consume_merge_handoffs()

    def test_empty_upgrade_keeps_core_commit_palace_data_and_skips_handoff(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw) / "empty"
            project.mkdir()
            seed_core(project, self.install, TEST_COMMIT)
            bind_hosts(self.hosts, project, TEST_COMMIT)
            events = project / ".memory/events.jsonl"
            events.write_bytes(b'{"keep":true}\n')
            palace = project / ".chaos-engine-state/mempalace"
            palace.mkdir(parents=True, exist_ok=True)
            marker = palace / "operator-data.txt"
            marker.write_text("keep-me\n", encoding="utf-8")
            seed_core(project, self.install, NEXT_COMMIT)
            bind_hosts(self.hosts, project, NEXT_COMMIT)
            manifest = json.loads(
                (project / ".chaos-engine/manifest.json").read_text(encoding="utf-8")
            )
            self.assertEqual(NEXT_COMMIT, manifest["source"]["commit"])
            self.assertEqual("portable", manifest["distribution"]["id"])
            self.assertEqual(b'{"keep":true}\n', events.read_bytes())
            self.assertEqual("keep-me\n", marker.read_text(encoding="utf-8"))
            self.assertFalse((project / ".chaos-engine-state/merge-handoff.md").exists())

    def test_java_upgrade_keeps_repository_and_pom_digest(self) -> None:
        pom = "<project><artifactId>shaft-engine</artifactId></project>\n"
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw) / "shaft"
            project.mkdir()
            pom_path = project / "pom.xml"
            pom_path.write_text(pom, encoding="utf-8")
            foreign = {
                "mcpServers": {
                    "operator-server": {"command": "keep", "args": ["x"]},
                }
            }
            (project / ".mcp.json").write_text(
                json.dumps(foreign, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )
            digest = sha256_bytes(pom_path.read_bytes())
            seed_core(project, self.install, TEST_COMMIT, distribution="repository")
            bind_hosts(self.hosts, project, TEST_COMMIT)
            seed_core(project, self.install, NEXT_COMMIT, distribution="repository")
            bind_hosts(self.hosts, project, NEXT_COMMIT)
            self.assertEqual(digest, sha256_bytes(pom_path.read_bytes()))
            manifest = json.loads(
                (project / ".chaos-engine/manifest.json").read_text(encoding="utf-8")
            )
            self.assertEqual("repository", manifest["distribution"]["id"])
            servers = json.loads((project / ".mcp.json").read_text(encoding="utf-8"))[
                "mcpServers"
            ]
            self.assertEqual({"command": "keep", "args": ["x"]}, servers["operator-server"])

    def test_non_java_upgrade_stays_portable_and_keeps_foreign_bytes(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw) / "node"
            project.mkdir()
            package = project / "package.json"
            package.write_text('{"name":"demo"}\n', encoding="utf-8")
            (project / "AGENTS.md").write_text("operator prose\n", encoding="utf-8")
            seed_core(project, self.install, TEST_COMMIT)
            bind_hosts(self.hosts, project, TEST_COMMIT)
            seed_core(project, self.install, NEXT_COMMIT)
            bind_hosts(self.hosts, project, NEXT_COMMIT)
            manifest = json.loads(
                (project / ".chaos-engine/manifest.json").read_text(encoding="utf-8")
            )
            self.assertEqual("portable", manifest["distribution"]["id"])
            self.assertEqual('{"name":"demo"}\n', package.read_text(encoding="utf-8"))
            agents = (project / "AGENTS.md").read_text(encoding="utf-8")
            self.assertIn("operator prose\n", agents)
            self.assertIn(self.hosts.START, agents)
            self.assertFalse((project / ".chaos-engine-state/merge-handoff.md").exists())


class InstallConflictHandoffTest(unittest.TestCase):
    def setUp(self) -> None:
        self.install = load_module(INSTALL, "ce_conflict_install")
        self.hosts = load_hosts()
        self.hosts.consume_merge_handoffs()

    def test_partial_success_merges_agents_and_hands_off_colliding_mcp(self) -> None:
        colliding = json.dumps(
            {
                "mcpServers": {
                    "chaosengine-memory": {
                        "command": "foreign-memory",
                        "args": ["--owned-by-operator"],
                    }
                }
            },
            indent=2,
            sort_keys=True,
        ).encode() + b"\n"
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw) / "mixed"
            project.mkdir()
            (project / "AGENTS.md").write_text("keep this prose\n", encoding="utf-8")
            (project / ".mcp.json").write_bytes(colliding)
            seed_core(project, self.install)
            bind_hosts(self.hosts, project)
            agents = (project / "AGENTS.md").read_text(encoding="utf-8")
            self.assertIn("keep this prose\n", agents)
            self.assertIn(self.hosts.START, agents)
            self.assertEqual(colliding, (project / ".mcp.json").read_bytes())
            handoff = project / ".chaos-engine-state/merge-handoff.md"
            self.assertTrue(handoff.is_file())
            body = handoff.read_text(encoding="utf-8")
            self.assertIn(".mcp.json", body)
            self.assertNotIn("## AGENTS.md", body)
            bind_hosts(self.hosts, project)
            self.assertEqual(colliding, (project / ".mcp.json").read_bytes())
            self.assertTrue(handoff.is_file())
            self.assertIn(".mcp.json", handoff.read_text(encoding="utf-8"))

    def test_claude_plugin_collision_byte_preserves_and_hands_off(self) -> None:
        settings = json.dumps(
            {
                "enabledPlugins": {"chaos-engine@chaos-engine-project": False},
                "extraKnownMarketplaces": {},
            },
            indent=2,
            sort_keys=True,
        ).encode() + b"\n"
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw) / "claude-plugin"
            project.mkdir()
            claude = project / ".claude"
            claude.mkdir()
            (claude / "settings.json").write_bytes(settings)
            seed_core(project, self.install)
            bind_hosts(self.hosts, project)
            self.assertEqual(settings, (claude / "settings.json").read_bytes())
            handoff = project / ".chaos-engine-state/merge-handoff.md"
            self.assertTrue(handoff.is_file())
            self.assertIn(".claude/settings.json", handoff.read_text(encoding="utf-8"))

    def test_edited_gitignore_interior_is_left_unchanged_and_handed_off(self) -> None:
        original = (
            f"{self.hosts.GITIGNORE_START}\n"
            "secret.env\n"
            f"{self.hosts.GITIGNORE_END}\n"
        ).encode()
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw) / "ignore"
            project.mkdir()
            (project / ".gitignore").write_bytes(original)
            seed_core(project, self.install)
            bind_hosts(self.hosts, project)
            self.assertEqual(original, (project / ".gitignore").read_bytes())
            handoff = project / ".chaos-engine-state/merge-handoff.md"
            self.assertTrue(handoff.is_file())
            self.assertIn(".gitignore", handoff.read_text(encoding="utf-8"))
            self.assertIn("secret.env", (project / ".gitignore").read_text(encoding="utf-8"))

    def test_symlink_instruction_file_fails_closed_without_handoff(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw) / "linked"
            project.mkdir()
            target = Path(raw) / "outside.md"
            target.write_text("outside\n", encoding="utf-8")
            (project / "AGENTS.md").symlink_to(target)
            seed_core(project, self.install)
            with self.assertRaisesRegex(ValueError, "link or reparse"):
                bind_hosts(self.hosts, project)
            self.assertFalse((project / ".chaos-engine-state/merge-handoff.md").exists())
            self.assertEqual("outside\n", target.read_text(encoding="utf-8"))

    def test_doctor_fix_next_names_handoff_not_reinstall(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw)
            (project / ".chaos-engine-state").mkdir()
            (project / ".chaos-engine-state/merge-handoff.md").write_text(
                "# Merge handoff\n", encoding="utf-8"
            )
            components = {
                "hooks": {"status": "recovery-required", "taskImpact": "required"},
                "mcps": {"status": "recovery-required", "taskImpact": "required"},
            }
            self.install.apply_merge_handoff_fix_next(project, components)
            for name in ("hooks", "mcps"):
                fix = components[name]["fixNext"]
                self.assertIn(".chaos-engine-state/merge-handoff.md", fix)
                self.assertNotIn("reinstall", fix.casefold())

    def test_success_panel_prints_one_backtick_prompt_without_ce_install_failed(self) -> None:
        bootstrap = load_module(BOOTSTRAP, "ce_merge_handoff_bootstrap")
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw)
            (project / ".chaos-engine-state").mkdir()
            (project / ".chaos-engine-state/merge-handoff.md").write_text(
                "# Merge handoff\n", encoding="utf-8"
            )
            stream = io.StringIO()
            reporter = bootstrap.InstallReporter(stream=stream)
            reporter.success(
                project,
                {"status": "healthy", "commit": TEST_COMMIT, "components": {}},
                {},
                repository="owner/repo",
            )
            output = stream.getvalue()
            self.assertIn("Installation Successful!", output)
            self.assertIn("Merge handoff", output)
            self.assertNotIn("CE-INSTALL-FAILED", output)
            self.assertEqual(1, output.count("`Merge ChaosEngine host configuration"))


if __name__ == "__main__":
    unittest.main()
