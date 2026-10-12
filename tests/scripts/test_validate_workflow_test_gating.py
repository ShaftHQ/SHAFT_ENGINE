"""Tests for scripts/ci/validate_workflow_test_gating.py (issue #6870)."""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from scripts.ci import validate_workflow_test_gating as gating

ENGINE_POM = "<project><testFailureIgnore>true</testFailureIgnore></project>"


class WorkflowTestGatingTest(unittest.TestCase):
    def _root(self, workflow: str) -> Path:
        root = Path(tempfile.mkdtemp())
        (root / "shaft-engine").mkdir()
        (root / "shaft-engine/pom.xml").write_text(ENGINE_POM, encoding="utf-8")
        (root / "shaft-mcp").mkdir()
        (root / "shaft-mcp/pom.xml").write_text("<project/>", encoding="utf-8")
        (root / ".github/workflows").mkdir(parents=True)
        (root / ".github/workflows/w.yml").write_text(workflow, encoding="utf-8")
        return root

    def test_engine_tests_without_assertion_are_flagged(self) -> None:
        root = self._root("jobs:\n  e2e:\n    steps:\n      - name: Run tests\n        run: mvn -pl shaft-engine test\n")
        findings = gating.validate(root)
        self.assertEqual(1, len(findings))
        self.assertIn("Run tests", findings[0])

    def test_continue_on_error_without_assertion_is_flagged(self) -> None:
        root = self._root("jobs:\n  e2e:\n    steps:\n      - name: Run\n        continue-on-error: true\n"
                          "        run: mvn -pl shaft-mcp test\n")
        self.assertEqual(1, len(gating.validate(root)))

    def test_later_report_assertion_and_gating_modules_pass(self) -> None:
        root = self._root("jobs:\n  e2e:\n    steps:\n      - name: Run\n        continue-on-error: true\n"
                          "        run: mvn -pl shaft-engine test\n"
                          "      - uses: ./.github/actions/post-test-report\n"
                          "  mcp:\n    steps:\n      - run: mvn -pl shaft-mcp test\n"
                          "      - run: mvn -pl shaft-engine install -DskipTests\n")
        self.assertEqual([], gating.validate(root))

    def test_repository_workflows_are_gated(self) -> None:
        self.assertEqual([], gating.validate())


if __name__ == "__main__":
    unittest.main()
