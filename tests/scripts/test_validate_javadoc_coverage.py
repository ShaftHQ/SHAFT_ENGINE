"""Tests for the shrink-only Javadoc coverage ratchet (issue #6375)."""

import re
import sys
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "scripts" / "ci"))

import validate_javadoc_coverage as coverage  # noqa: E402

PROPERTIES = REPO_ROOT / "shaft-engine/src/main/java/com/shaft/properties/internal"

SAMPLE = """package demo;

/** Demo. */
public class Demo {
    /** Documented. */
    public void documented() {}

    public void missing() {}

    @Override
    public String toString() { return ""; }

    // public void commented() {}

    /*
    public void blockCommented() {}
    */

    private void hidden() {}

    public static class Nested {}
}
"""

INTERFACE = """package demo;

/** Demo. */
public interface Props {
    private static void helper(String key) {
        if (key.isEmpty()) {
            setProperty("x", key);
        }
    }

    @Key("a")
    int undocumented();

    /** Documented. */
    @Key("b")
    int documented();

    default Setter set() {
        return new Setter();
    }
}
"""


class UndocumentedMembersTest(unittest.TestCase):
    """Detection of public members without Javadoc."""

    def test_class_members(self):
        """Only the public, non-override, uncommented member without Javadoc is reported."""
        self.assertEqual(coverage.undocumented_members(SAMPLE), [8])

    def test_interface_members(self):
        """Interface getters and default methods count; private helpers and statements do not."""
        self.assertEqual(coverage.undocumented_members(INTERFACE), [12, 18])


class CompareTest(unittest.TestCase):
    """The baseline may only shrink."""

    def test_regression_and_new_file_fail(self):
        """A higher count, or any count for a file missing from the baseline, is a regression."""
        regressions, _ = coverage.compare({"a.java": 3, "b.java": 1}, {"a.java": 2})
        self.assertEqual(len(regressions), 2)

    def test_improvement_is_stale(self):
        """A lower count must shrink the baseline."""
        regressions, stale = coverage.compare({"a.java": 1}, {"a.java": 2, "gone.java": 4})
        self.assertEqual(regressions, [])
        self.assertEqual(len(stale), 2)


class RepositoryGateTest(unittest.TestCase):
    """The checked-in baseline matches the repository."""

    def test_baseline_matches_repository(self):
        """The ratchet passes on the current tree."""
        config = coverage.load_config()
        regressions, stale = coverage.compare(coverage.scan(REPO_ROOT, config), config["baseline"])
        self.assertEqual(regressions, [])
        self.assertEqual(stale, [])

    def test_every_exclusion_states_a_reason(self):
        """Every exclusion and override explains why."""
        config = coverage.load_config()
        for rule in config["exclusions"] + config.get("included_overrides", []):
            self.assertTrue(rule.get("reason", "").strip(), rule)

    def test_every_property_key_has_a_description(self):
        """Every @Key property getter carries a Javadoc description."""
        missing = []
        for source in sorted(PROPERTIES.glob("*.java")):
            lines = source.read_text(encoding="utf-8").split("\n")
            for index, line in enumerate(lines):
                if not re.match(r"\s*@Key\(", line):
                    continue
                j = index - 1
                while j >= 0 and lines[j].strip().startswith("@"):
                    j -= 1
                if j < 0 or not lines[j].strip().endswith("*/"):
                    missing.append(f"{source.name}:{index + 1}")
        self.assertEqual(missing, [])


if __name__ == "__main__":
    unittest.main()
