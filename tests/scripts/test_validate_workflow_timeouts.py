import tempfile
import unittest
from pathlib import Path

import yaml

from scripts.ci.validate_workflow_timeouts import validate_repository


class ValidateWorkflowTimeoutsTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)

    def tearDown(self):
        self.temporary_directory.cleanup()

    def write(self, relative_path, content):
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def codes(self):
        return {error["code"] for error in validate_repository(self.root)}

    def test_job_missing_timeout_minutes_is_flagged(self):
        self.write(
            ".github/workflows/example.yml",
            """
name: Example
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - run: echo hi
""",
        )
        errors = validate_repository(self.root)
        self.assertEqual(len(errors), 1)
        self.assertEqual(errors[0]["code"], "workflow-timeout-missing")
        self.assertEqual(errors[0]["path"], ".github/workflows/example.yml")
        self.assertIn("build", errors[0]["message"])

    def test_job_with_timeout_minutes_passes(self):
        self.write(
            ".github/workflows/example.yml",
            """
name: Example
jobs:
  build:
    runs-on: ubuntu-latest
    timeout-minutes: 30
    steps:
      - run: echo hi
""",
        )
        self.assertEqual(validate_repository(self.root), [])

    def test_reusable_workflow_call_job_is_exempt(self):
        self.write(
            ".github/workflows/example.yml",
            """
name: Example
jobs:
  call-shared:
    uses: ./.github/workflows/shared.yml
""",
        )
        self.assertEqual(validate_repository(self.root), [])

    def test_multiple_jobs_each_flagged_by_name(self):
        self.write(
            ".github/workflows/example.yml",
            """
name: Example
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - run: echo hi
  notify:
    runs-on: ubuntu-latest
    timeout-minutes: 10
    steps:
      - run: echo notify
  deploy:
    runs-on: ubuntu-latest
    steps:
      - run: echo deploy
""",
        )
        errors = validate_repository(self.root)
        flagged_jobs = {error["message"].split("'")[1] for error in errors}
        self.assertEqual(flagged_jobs, {"build", "deploy"})

    def test_no_workflows_directory_returns_no_errors(self):
        self.assertEqual(validate_repository(self.root), [])

    def test_current_repository_workflows_all_declare_timeout_minutes(self):
        repository_root = Path(__file__).resolve().parents[2]
        self.assertEqual(validate_repository(repository_root), [])

    def test_agent_guidance_job_allows_expanded_cross_host_history_suite(self):
        repository_root = Path(__file__).resolve().parents[2]
        workflow = yaml.safe_load(
            (repository_root / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        )
        timeout = workflow["jobs"]["agent-guidance"].get("timeout-minutes", 0)
        self.assertGreaterEqual(timeout, 15)


if __name__ == "__main__":
    unittest.main()


class TimeoutGuardIsReRunByItsOwnEditsTest(unittest.TestCase):
    """#4529: the guard's own files must re-trigger the job that runs them.

    The issue reported that three of the four workflow files the timeout
    assertion covers matched no path filter, so lowering a timeout would land
    green. That half is no longer true: `workflows` includes
    `.github/workflows/**`, and the Workflow Timeout Guard job runs on it, so
    any workflow edit does re-run the assertion. Verified by reading the
    filter rather than assumed from the issue.

    What is still open is worse. The guard's *own* implementation and test --
    `scripts/ci/validate_workflow_timeouts.py` and this module -- are in no
    filter at all. So a timeout cannot be lowered unnoticed, but the check
    that prevents it can be gutted and land green. A guard that cannot see
    edits to itself is the unbound-check shape one level up.

    `tests/scripts/test_intellij_verify_retry.py`, which the same job runs, is
    in the filter with a comment explaining why it belongs there. Its
    neighbours being absent is an oversight rather than a decision, which is
    the argument for pinning it here rather than only fixing it.
    """

    WORKFLOW = Path(__file__).resolve().parents[2] / ".github/workflows/pr-gate.yml"

    def test_the_guard_reruns_when_its_own_implementation_or_test_changes(self):
        content = self.WORKFLOW.read_text(encoding="utf-8")
        missing = [
            path
            for path in (
                "scripts/ci/validate_workflow_timeouts.py",
                "tests/scripts/test_validate_workflow_timeouts.py",
            )
            if f"- '{path}'" not in content
        ]
        self.assertEqual(
            missing,
            [],
            "a check whose own edits do not re-run it can be weakened green",
        )


class CliGatePackagingContractTest(unittest.TestCase):
    """#4606: keep the CLI binary smoke test without packaging the same JAR twice."""

    WORKFLOW = Path(__file__).resolve().parents[2] / ".github/workflows/pr-gate.yml"

    def test_cli_smoke_reuses_the_jar_created_by_reactor_install(self):
        content = self.WORKFLOW.read_text(encoding="utf-8")
        self.assertIn(
            "mvn --batch-mode -pl shaft-cli -am -DskipTests install -q",
            content,
        )
        self.assertIn("- name: Run packaged shaft-cli binary", content)
        self.assertNotIn(
            "mvn --batch-mode -pl shaft-cli package -DskipTests",
            content,
        )
