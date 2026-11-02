"""Machine-local consent for anonymized upstream learning tickets."""

from __future__ import annotations

import importlib.util
import os
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _load():
    spec = importlib.util.spec_from_file_location(
        "ce_upstream_tickets", ROOT / "chaos-engine/learning_session.py"
    )
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


class UpstreamTicketConsentTest(unittest.TestCase):
    def setUp(self) -> None:
        self.mod = _load()
        self.tmp = tempfile.TemporaryDirectory()
        self.path = Path(self.tmp.name) / "consent.json"
        self.old = os.environ.get("CHAOS_ENGINE_CONSENT_PATH")
        self.ci = os.environ.pop("CI", None)
        self.actions = os.environ.pop("GITHUB_ACTIONS", None)
        self.upstream = os.environ.pop("CHAOS_ENGINE_UPSTREAM_REPO", None)
        os.environ["CHAOS_ENGINE_CONSENT_PATH"] = str(self.path)

    def tearDown(self) -> None:
        if self.old is None:
            os.environ.pop("CHAOS_ENGINE_CONSENT_PATH", None)
        else:
            os.environ["CHAOS_ENGINE_CONSENT_PATH"] = self.old
        if self.ci is not None:
            os.environ["CI"] = self.ci
        if self.actions is not None:
            os.environ["GITHUB_ACTIONS"] = self.actions
        if self.upstream is None:
            os.environ.pop("CHAOS_ENGINE_UPSTREAM_REPO", None)
        else:
            os.environ["CHAOS_ENGINE_UPSTREAM_REPO"] = self.upstream
        self.tmp.cleanup()

    def test_redact_strips_path_email_and_token(self) -> None:
        text = self.mod._redact("see /home/ada/proj ada@ex.com ghp_abc123 keep")
        self.assertNotIn("/home", text)
        self.assertNotIn("@", text)
        self.assertNotIn("ghp_", text)
        self.assertIn("keep", text)

    def test_project_lesson_stays_on_the_project(self) -> None:
        plan = self.mod._upstream_plan(["The button label is unclear"], "allow")
        self.assertEqual("none", plan["action"])
        self.assertEqual("project", plan["project"][0]["track"])
        self.assertEqual([], plan["bodies"])

    def test_unknown_consent_asks_once_then_stays_pending(self) -> None:
        plan = self.mod._upstream_plan(["the chaos-engine hook dropped context"], "unknown")
        self.assertEqual("ask-once", plan["action"])
        self.assertIn("allow or deny", plan["question"])
        self.mod._apply_upstream({"upstreamTickets": "ask-once"})
        self.assertEqual("pending", self.mod._read_consent(self.path))
        again = self.mod._upstream_plan(
            ["the chaos-engine hook dropped context"], self.mod._read_consent(self.path)
        )
        self.assertEqual("already-asked", again["action"])

    def test_deny_skips_filing(self) -> None:
        plan = self.mod._upstream_plan(["mempalace refresh looped"], "deny")
        self.assertEqual("skipped-denied", plan["action"])

    def test_allow_files_a_redacted_body_once(self) -> None:
        calls: list[tuple[str, str]] = []

        def fake(repo: str, body: str) -> str:
            calls.append((repo, body))
            return "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/1"

        self.mod._create_issue = fake
        os.environ["CHAOS_ENGINE_UPSTREAM_REPO"] = "example/chaos-engine"
        plan = self.mod._upstream_plan(["chaos-engine kept /home/ada/secret ghp_zzz"], "allow")
        receipt = {"upstreamTickets": "file", "upstreamBodies": plan["bodies"]}
        self.mod._apply_upstream(receipt)
        self.mod._apply_upstream(receipt)
        self.assertEqual(1, len(calls))
        self.assertNotIn("/home", calls[0][1])
        self.assertNotIn("ghp_", calls[0][1])
        self.assertEqual(
            ["https://github.com/ShaftHQ/SHAFT_ENGINE/issues/1"], receipt["upstreamIssueUrls"]
        )


if __name__ == "__main__":
    unittest.main()
