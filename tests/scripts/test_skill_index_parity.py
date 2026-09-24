"""#6177: one generated harness/skill index, reachable on every host.

Fails on: a SKILL.md not in the index, an index entry without a file, a
declared host mechanism that does not expose the skill, a skill within
512 B of its cap, a description outside 60-220 characters or a near
duplicate, a catalog path the guard blocks, and generated artifacts that
drifted from the index.
"""

from __future__ import annotations

import difflib
import json
import re
import tempfile
import unittest
from pathlib import Path

from tests.scripts.ce_installed_fixture import (
    ROOT,
    SOURCE,
    build_installed_project,
    load_module,
    parity_hosts,
)

INDEX = SOURCE / "harness-index.json"
BUDGET = ROOT / "scripts/ci/agent_guidance_budget.json"
MECHANISM = re.compile(r"^(plugin|adapter|catalog|gap:GAP-[A-Z0-9-]+)$")
NEAR_CAP_BYTES = 512


def index() -> dict:
    return json.loads(INDEX.read_text(encoding="utf-8"))


def frontmatter_description(text: str) -> str:
    match = re.search(r"(?ms)^---\n.*?^description:\s*(.+?)\n(?:[a-z_-]+:|---)", text)
    return " ".join(match.group(1).strip().strip("\"'").split()) if match else ""


class SkillIndexParityTest(unittest.TestCase):
    def test_index_schema_and_hosts(self):
        payload = index()
        self.assertEqual(1, payload["schemaVersion"])
        self.assertEqual(parity_hosts(), payload["hosts"])
        self.assertIn(".chaos-engine/", payload["harnessRoots"])
        for entry in payload["entries"]:
            with self.subTest(entry=entry.get("name")):
                self.assertIn(entry["kind"], {"portable", "vendor", "role", "route"})
                self.assertTrue((SOURCE / entry["path"]).is_file(), entry["path"])
                self.assertEqual(set(parity_hosts()), set(entry["hosts"]))
                for mechanism in entry["hosts"].values():
                    self.assertRegex(mechanism, MECHANISM)

    def test_every_skill_file_is_indexed(self):
        indexed = {entry["path"] for entry in index()["entries"]}
        on_disk = {
            path.relative_to(SOURCE).as_posix()
            for pattern in ("skills/*/SKILL.md", "vendor/*/skills/*/SKILL.md")
            for path in SOURCE.glob(pattern)
        }
        self.assertEqual(set(), on_disk - indexed)

    def test_descriptions_are_bounded_say_when_and_are_distinct(self):
        entries = [entry for entry in index()["entries"] if entry["kind"] != "role"]
        for entry in entries:
            with self.subTest(entry=entry["name"]):
                self.assertGreaterEqual(len(entry["description"]), 60)
                self.assertLessEqual(len(entry["description"]), 220)
                self.assertRegex(entry["description"].casefold(), r"\buse\b|\bwhen\b")
        for first, second in [(a, b) for i, a in enumerate(entries) for b in entries[i + 1:]]:
            ratio = difflib.SequenceMatcher(None, first["description"], second["description"]).ratio()
            with self.subTest(pair=(first["name"], second["name"])):
                self.assertLess(ratio, 0.75)

    def test_portable_skill_descriptions_match_their_index_entry(self):
        for entry in index()["entries"]:
            if entry["kind"] not in {"portable", "route"}:
                continue
            with self.subTest(entry=entry["name"]):
                text = (SOURCE / entry["path"]).read_text(encoding="utf-8")
                self.assertEqual(entry["description"], frontmatter_description(text))

    def test_no_skill_within_512_bytes_of_its_cap(self):
        cap = json.loads(BUDGET.read_text(encoding="utf-8"))["skill_budgets"]["chaos-engine/skills"]["max_skill_md_bytes"]
        for path in sorted(SOURCE.glob("skills/*/SKILL.md")):
            with self.subTest(skill=path.parent.name):
                self.assertLess(len(path.read_bytes()), cap - NEAR_CAP_BYTES)

    def test_one_local_runtime_skill(self):
        runtime = [
            entry for entry in index()["entries"]
            if entry.get("family") == "local-runtime" and entry["kind"] == "portable"
        ]
        self.assertEqual(["local-runtimes"], [entry["name"] for entry in runtime])

    def test_gap_mechanisms_are_documented(self):
        matrix = (SOURCE / "references/host-parity-matrix.md").read_text(encoding="utf-8")
        for entry in index()["entries"]:
            for host, mechanism in entry["hosts"].items():
                if mechanism.startswith("gap:"):
                    with self.subTest(entry=entry["name"], host=host):
                        self.assertIn(mechanism[4:], matrix)

    def test_declared_mechanisms_expose_the_skill(self):
        hosts_module = load_module("ce_index_hosts", SOURCE / "hosts.py")
        documents = hosts_module.marketplace_documents("chaos-engine-project", "1.0.0")
        codex = {item["name"]: item for item in documents[".agents/plugins/marketplace.json"]["plugins"]}
        claude = {item["name"] for item in documents[".claude-plugin/marketplace.json"]["plugins"]}
        catalog = (SOURCE / "references/catalog.md").read_text(encoding="utf-8")
        for entry in index()["entries"]:
            for host, mechanism in entry["hosts"].items():
                with self.subTest(entry=entry["name"], host=host):
                    if mechanism == "plugin" and entry["kind"] == "vendor":
                        self.assertIn(entry["name"], codex)
                        self.assertIn(entry["name"], claude)
                    if mechanism == "catalog":
                        self.assertIn(entry["name"], catalog)
        self.assertEqual("AVAILABLE", codex["icm-architect"]["policy"]["installation"])

    def test_vendor_plugin_adapters_are_short(self):
        hosts_module = load_module("ce_index_hosts_plugins", SOURCE / "hosts.py")
        images = hosts_module.companion_plugin_images()
        for name in ("caveman", "ponytail", "icm-architect"):
            body = images[f"plugins/{name}/skills/{name}/SKILL.md"].decode("utf-8")
            with self.subTest(plugin=name):
                description = frontmatter_description(body)
                self.assertTrue(60 <= len(description) <= 220, description)
                self.assertIn(f".chaos-engine/vendor/{name}/skills/{name}/SKILL.md", body)

    def test_generated_artifacts_match_the_index(self):
        module = load_module("ce_harness_index", SOURCE / "harness_index.py")
        self.assertEqual([], module.check(ROOT))

    def test_catalog_paths_are_allowed_by_the_guard(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = build_installed_project(Path(temporary) / "adopter")
            gate = load_module("ce_index_gate", project / ".chaos-engine/hooks/retrieve_justification.py")
            for entry in index()["entries"]:
                with self.subTest(entry=entry["name"]):
                    self.assertIsNone(
                        gate.file_read_block_reason(
                            project=project, event_name="PreToolUse", tool_name="Read",
                            tool_input={"file_path": f".chaos-engine/{entry['path']}"}, commands=(),
                        )
                    )

    def test_validate_skills_lints_real_skill_roots(self):
        validator = load_module("ce_validate_skills", ROOT / "scripts/ci/validate_skills.py")
        self.assertIn("chaos-engine/skills", validator.SKILLS_ROOTS)
        self.assertIn("chaos-engine/vendor/*/skills", validator.SKILLS_ROOTS)
        self.assertGreaterEqual(len(validator.skill_dirs(ROOT)), 12)


if __name__ == "__main__":
    unittest.main()
