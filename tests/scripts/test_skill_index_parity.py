"""#6177: one generated harness/skill index, reachable on every host.

Fails on: a SKILL.md not in the index, an index entry without a file, a
declared host mechanism that does not expose the skill, a skill within
512 B of its cap, a description outside 60-220 characters or a near
duplicate, a catalog path the guard blocks, and generated artifacts that
drifted from the index.
"""

from __future__ import annotations

import contextlib
import difflib
import io
import json
import re
import tempfile
import unittest
from pathlib import Path

import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402 - self-locating under python3 -I (#7197)
from scripts.ci.validate_agent_guidance import skill_listing_chars  # noqa: E402
from tests.scripts.ce_installed_fixture import (  # noqa: E402
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
    if not match:
        return ""
    value = re.sub(r"^[>|][-+]?\s*", "", match.group(1).strip())
    return " ".join(value.strip("\"'").split())


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
        # #6198: Ponytail keeps its body; its pinned hooks read the plugin copy.
        hosts_module = load_module("ce_index_hosts_plugins", SOURCE / "hosts.py")
        images = hosts_module.companion_plugin_images()
        for name in ("caveman", "icm-architect"):
            body = images[f"plugins/{name}/skills/{name}/SKILL.md"].decode("utf-8")
            with self.subTest(plugin=name):
                description = frontmatter_description(body)
                self.assertTrue(60 <= len(description) <= 220, description)
                self.assertIn(f".chaos-engine/vendor/{name}/skills/{name}/SKILL.md", body)

    def test_generated_artifacts_match_the_index(self):
        module = load_module("ce_harness_index", SOURCE / "harness_index.py")
        self.assertEqual([], module.check(ROOT))
        self.assertEqual(0, module.main(["--check"]))

    def test_router_keeps_route_locators_without_full_descriptions(self):
        """The always-loaded router must not repeat catalog descriptions."""
        module = load_module("ce_harness_index_routes", SOURCE / "harness_index.py")
        payload = module.build_index()
        table = module.render_route_table(payload)
        skill = (SOURCE / "skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        descriptions = [
            entry["description"]
            for entry in payload["entries"]
        ]
        self.assertGreater(len(descriptions), 10)
        self.assertLess(len(table), sum(len(item) for item in descriptions))
        self.assertIn(table, skill)
        self.assertIn("| Heal |", table)
        self.assertIn("heal-route.md", table)
        self.assertIn("git-cleanup/SKILL.md", table)
        ui = [line for line in table.splitlines() if line.startswith("| UI delivery |")]
        self.assertEqual(1, len(ui))
        self.assertIn("user-visible UI", ui[0])
        for description in descriptions:
            self.assertNotIn(description, table)
            self.assertNotIn(description, skill)

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

    def test_untangle_lesson_rejects_test_deletion(self):
        text = (SOURCE / "references/untangle-lesson.md").read_text(encoding="utf-8")
        for phrase in (
            "Do not remove tests",
            "Hidden links",
            "Needless links",
            "Surplus",
            "Do not untangle a whole repository",
            "never weaken, delete, or rewrite a test to reach green",
            "remove tests that would not catch a real bug",
        ):
            self.assertIn(phrase, text)


_GENERATED_SNAPSHOTS = (
    SOURCE / "harness-index.json",
    SOURCE / "references/catalog.md",
    SOURCE / "skills/chaos-engine/SKILL.md",
    ROOT / ".agents/skills/README.md",
    ROOT / "scripts/ci/agent_guidance_budget.json",
)


def _snapshot_generated() -> dict[Path, bytes]:
    return {path: path.read_bytes() for path in _GENERATED_SNAPSHOTS}


def _restore_generated(snapshot: dict[Path, bytes]) -> None:
    for path, payload in snapshot.items():
        if path.read_bytes() != payload:
            path.write_bytes(payload)


def _write_skill(root: Path, relative: str, name: str, description: str) -> None:
    path = root / relative
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(f"---\nname: {name}\ndescription: {description}\n---\n\n# {name}\n", encoding="utf-8")


def _write_listing_budget(root: Path, ceiling: int, hosts: dict[str, list[str]]) -> Path:
    path = root / "scripts/ci/agent_guidance_budget.json"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps({"max_skill_listing_chars": ceiling, "host_skill_metadata_globs": hosts}),
        encoding="utf-8",
    )
    return path


class HarnessIndexLocalCheckTest(unittest.TestCase):
    def test_foreign_root_write_is_refused_before_any_write(self):
        module = load_module("ce_harness_index_foreign", SOURCE / "harness_index.py")
        snapshot = _snapshot_generated()
        try:
            with tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                script = root / "chaos-engine" / "harness_index.py"
                script.parent.mkdir(parents=True)
                script.write_text("# other tree\n", encoding="utf-8")
                buffer = io.StringIO()
                with contextlib.redirect_stderr(buffer):
                    code = module.main(["--root", str(root), "--write"])
                self.assertNotEqual(code, 0)
                refusal = buffer.getvalue()
                self.assertIn("refusing to write this skill tuple into", refusal)
                self.assertIn(str(script.resolve()), refusal)
                self.assertIn("run that tree's script", refusal)
                self.assertEqual([], module.write(root))
                created = [path.relative_to(root).as_posix() for path in root.rglob("*") if path.is_file()]
                self.assertEqual(["chaos-engine/harness_index.py"], created)
        finally:
            _restore_generated(snapshot)

    def test_same_file_symlink_root_may_write(self):
        module = load_module("ce_harness_index_same", SOURCE / "harness_index.py")
        snapshot = _snapshot_generated()
        try:
            with tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                overlay = root / "chaos-engine"
                overlay.mkdir()
                (overlay / "harness_index.py").symlink_to(Path(module.__file__).resolve())
                self.assertIsNone(module.foreign_write_block(root.resolve()))
                buffer = io.StringIO()
                with contextlib.redirect_stderr(buffer):
                    code = module.main(["--root", str(root), "--write"])
                self.assertEqual(0, code)
                self.assertNotIn("refusing to write this skill tuple into", buffer.getvalue())
                self.assertTrue((overlay / "harness-index.json").is_file())
        finally:
            _restore_generated(snapshot)

    def test_root_without_harness_index_keeps_write_behavior(self):
        module = load_module("ce_harness_index_absent", SOURCE / "harness_index.py")
        snapshot = _snapshot_generated()
        try:
            with tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.assertIsNone(module.foreign_write_block(root.resolve()))
                buffer = io.StringIO()
                with contextlib.redirect_stderr(buffer):
                    code = module.main(["--root", str(root), "--write"])
                self.assertEqual(0, code)
                self.assertNotIn("refusing to write this skill tuple into", buffer.getvalue())
        finally:
            _restore_generated(snapshot)

    def test_listing_under_the_configured_cap_passes(self):
        module = load_module("ce_harness_index_listing_ok", SOURCE / "harness_index.py")
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            hosts = {"codex": ["pack/codex/*/SKILL.md"]}
            budget_path = _write_listing_budget(root, 1000, hosts)
            _write_skill(root, "pack/codex/alpha/SKILL.md", "alpha", "short")
            self.assertEqual([], module.listing_overflows(root))
            budget = json.loads(budget_path.read_text(encoding="utf-8"))
            count = skill_listing_chars(root, budget["host_skill_metadata_globs"]["codex"])
            self.assertLessEqual(count, budget["max_skill_listing_chars"])

    def test_check_fails_when_a_host_listing_exceeds_the_configured_cap(self):
        module = load_module("ce_harness_index_listing", SOURCE / "harness_index.py")
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            hosts = {
                "codex": ["pack/codex/*/SKILL.md"],
                "claude": ["pack/claude/*/SKILL.md"],
            }
            budget_path = _write_listing_budget(root, 5, hosts)
            _write_skill(root, "pack/codex/alpha/SKILL.md", "alpha", "use when the codex pack is selected")
            _write_skill(root, "pack/claude/beta/SKILL.md", "beta", "use when the claude pack is selected")
            buffer = io.StringIO()
            with contextlib.redirect_stderr(buffer):
                code = module.main(["--root", str(root), "--check"])
            self.assertNotEqual(code, 0)
            parsed: dict[str, tuple[int, int]] = {}
            for line in buffer.getvalue().splitlines():
                match = re.fullmatch(r"listing overflow: host=(\S+) count=(\d+) ceiling=(\d+)", line)
                if match is None:
                    continue
                parsed[match.group(1)] = (int(match.group(2)), int(match.group(3)))
            budget = json.loads(budget_path.read_text(encoding="utf-8"))
            ceiling = budget["max_skill_listing_chars"]
            patterns = budget["host_skill_metadata_globs"]
            self.assertEqual(set(patterns), set(parsed))
            for host, host_patterns in patterns.items():
                count, found_ceiling = parsed[host]
                self.assertEqual(ceiling, found_ceiling)
                self.assertEqual(skill_listing_chars(root, host_patterns), count)
                self.assertGreater(count, ceiling)


if __name__ == "__main__":
    unittest.main()
