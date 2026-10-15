"""Tests for scripts/ci/codecov_status_gate.py (#6990 enforced coverage)."""
from __future__ import annotations

import importlib.util
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("codecov_status_gate", ROOT / "scripts/ci/codecov_status_gate.py")
gate = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(gate)


def run(name, conclusion, status="completed"):
    return {"name": name, "status": status, "conclusion": conclusion}


class CodecovStatusGateTest(unittest.TestCase):
    def test_red_patch_check_run_fails(self) -> None:
        states = gate.latest_states([run("codecov/patch", "failure"), run("codecov/project", "success")], [])
        self.assertEqual(("fail", ["codecov/patch"]), gate.verdict(states))

    def test_legacy_commit_status_is_read(self) -> None:
        states = gate.latest_states([], [{"context": "codecov/project", "state": "failure"},
                                         {"context": "codecov/project", "state": "success"}])
        self.assertEqual("fail", gate.verdict(states)[0])

    def test_absent_or_running_is_pending(self) -> None:
        self.assertEqual("pending", gate.verdict({})[0])
        self.assertEqual("pending", gate.verdict(gate.latest_states([run("codecov/patch", None, "in_progress")], []))[0])

    def test_settled_failure_blocks_and_absent_never_hangs(self) -> None:
        red = {"codecov/patch": "failure"}
        with mock.patch.object(gate, "fetch", return_value=red), mock.patch.object(gate.time, "sleep"):
            self.assertEqual(1, gate.main(["--repo", "o/r", "--sha", "x", "--settle", "0"]))
        with mock.patch.object(gate, "fetch", return_value={}), mock.patch.object(gate.time, "sleep"):
            self.assertEqual(0, gate.main(["--repo", "o/r", "--sha", "x", "--timeout", "0"]))

    def test_summary_runs_gate_on_pull_request_only(self) -> None:
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertIn("scripts/ci/codecov_status_gate.py", workflow)
        self.assertIn("if: github.event_name == 'pull_request'", workflow)
        config = (ROOT / "codecov.yml").read_text(encoding="utf-8")
        self.assertIn("wait_for_ci: false", config)


if __name__ == "__main__":
    unittest.main()
