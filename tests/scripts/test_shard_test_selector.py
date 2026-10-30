"""shaft-engine unit-test shards cover exactly the PR Gate selector (#6188)."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

import yaml

from scripts.ci.shard_test_selector import entries, shard_entries
from scripts.ci.unit_test_discovery import iter_unit_test_sources
from scripts.ci.validate_quality_configuration import _pr_gate_unit_selectors

ROOT = Path(__file__).resolve().parents[2]
PR_GATE = ROOT / ".github/workflows/pr-gate.yml"


def workflow_selector() -> str:
    step = next(
        step
        for step in yaml.safe_load(PR_GATE.read_text(encoding="utf-8"))["jobs"]["unit-tests"]["steps"]
        if step.get("name") == "Run shaft-engine unit tests"
    )
    match = re.search(r"'(-Dtest=[^']*)'", step["run"])
    if match is None:
        raise AssertionError("shaft-engine unit-test step lost its -Dtest selector")
    return match.group(1)


UNIT_TEST_ROOT = ROOT / "shaft-engine/src/test/java"


def unselected_unit_test_classes(selector: str, test_root: Path = UNIT_TEST_ROOT) -> list[str]:
    """Return *UnitTest classes no PR Gate shard runs (#7310).

    Classes under ``testPackage/unitTests`` ride the shard-1 glob; any other
    ``*UnitTest`` class runs only when the selector names it. Discovery keeps a
    case-sensitive ``UnitTest.java`` suffix so Windows ``rglob`` cannot treat
    ``JunitTest.java`` as a unit-test class (#7627).
    """
    named = {entry.strip() for entry in selector.removeprefix("-Dtest=").split(",")}
    missing = []
    for path in iter_unit_test_sources(test_root):
        relative = path.relative_to(test_root).as_posix()
        if relative.startswith("testPackage/unitTests/") or path.stem in named:
            continue
        missing.append(relative)
    return missing


class ShardSelectorTest(unittest.TestCase):
    def test_every_unit_test_class_is_in_a_shard_selector(self) -> None:
        missing = unselected_unit_test_classes(workflow_selector())
        self.assertEqual(
            missing,
            [],
            "These *UnitTest classes run in no PR Gate shard, so codecov never counts them. "
            "Add each class name to the -Dtest selector of 'Run shaft-engine unit tests' in "
            ".github/workflows/pr-gate.yml and a matching !%regex[.*Name.*] exclusion to "
            "e2eTests.yml and e2eLocalTests.yml.",
        )

    def test_unselected_unit_test_detection(self) -> None:
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for relative in (
                "testPackage/unitTests/GlobbedUnitTest.java",
                "com/x/NamedUnitTest.java",
                "com/x/ForgottenUnitTest.java",
                "com/x/BrowserE2eTest.java",
                "junitTestPackage/JunitTest.java",
            ):
                (root / relative).parent.mkdir(parents=True, exist_ok=True)
                (root / relative).write_text("class X {}", encoding="utf-8")
            self.assertEqual(
                unselected_unit_test_classes("-Dtest=testPackage/unitTests/*, NamedUnitTest", root),
                ["com/x/ForgottenUnitTest.java"],
            )
            # Case-sensitive suffix: JunitTest.java must never require shard registration (#7627).
            self.assertNotIn(
                "junitTestPackage/JunitTest.java",
                unselected_unit_test_classes("-Dtest=testPackage/unitTests/*", root),
            )

    def test_union_of_shards_is_exactly_the_selector(self) -> None:
        selector = workflow_selector()
        original = entries(selector)
        positives = [entry for entry in original if not entry.startswith("!")]
        exclusions = [entry for entry in original if entry.startswith("!")]
        shards = [shard_entries(selector, index, 3) for index in (1, 2, 3)]
        chosen = [entry for shard in shards for entry in shard if not entry.startswith("!")]
        self.assertCountEqual(positives, chosen, "a class was dropped, added, or duplicated")
        for shard in shards:
            with self.subTest(shard=shard[:2]):
                self.assertTrue([entry for entry in shard if not entry.startswith("!")])
                self.assertEqual(exclusions, [entry for entry in shard if entry.startswith("!")])
        self.assertIn("!RestValidationsBuilderUnitTest", exclusions)

    def test_validator_still_reads_the_full_selector(self) -> None:
        selectors = _pr_gate_unit_selectors(PR_GATE.read_text(encoding="utf-8"))
        self.assertIn("testPackage/unitTests/*", selectors)
        self.assertIn("LazyLoadingFixtureLiveTest", selectors)

    def test_round_robin_for_more_shards_is_deterministic_and_complete(self) -> None:
        selector = "-Dtest=pkg/*, A, B, C, D, E, !X"
        shards = [shard_entries(selector, index, 3) for index in (1, 2, 3)]
        self.assertEqual([["pkg/*", "!X"], ["A", "C", "E", "!X"], ["B", "D", "!X"]], shards)
        self.assertEqual(["pkg/*", "A", "B", "!X"], shard_entries("pkg/*, A, B, !X", 1, 1))
        with self.assertRaises(ValueError):
            shard_entries(selector, 3, 2)

    def test_matrix_runs_two_shards_with_per_shard_verification_and_coverage(self) -> None:
        job = yaml.safe_load(PR_GATE.read_text(encoding="utf-8"))["jobs"]["unit-tests"]
        include = job["strategy"]["matrix"]["include"]
        self.assertEqual(
            [
                ("shaft-engine", 1, "shaft-engine-shard-1"),
                ("shaft-engine", 2, "shaft-engine-shard-2"),
                ("shaft-engine", 3, "shaft-engine-shard-3"),
            ],
            [(item["module"], item["shard"], item["leg"]) for item in include],
        )
        self.assertNotIn("shaft-engine", job["strategy"]["matrix"]["module"])
        self.assertIn("matrix.leg || matrix.module", job["name"])
        steps = {step.get("name"): step for step in job["steps"]}
        self.assertIn("--total 3", steps["Run shaft-engine unit tests"]["run"])
        self.assertIn("failed or total == 0", steps["Verify shaft-engine unit test results"]["run"])
        upload = steps["Upload module coverage to Codecov"]
        self.assertEqual("always()", upload["if"])
        self.assertIn("matrix.leg || matrix.module", upload["with"]["source-id"])

    def test_capture_and_template_jobs_use_the_shared_maven_cache(self) -> None:
        jobs = yaml.safe_load(PR_GATE.read_text(encoding="utf-8"))["jobs"]
        for name in ("capture-e2e", "template-coupling"):
            with self.subTest(job=name):
                uses = [step.get("uses", "") for step in jobs[name]["steps"]]
                self.assertIn("./.github/actions/cache-maven-repo", uses)


if __name__ == "__main__":
    unittest.main()
