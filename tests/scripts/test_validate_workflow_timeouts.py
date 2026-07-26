import tempfile
import unittest
from pathlib import Path

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


if __name__ == "__main__":
    unittest.main()
