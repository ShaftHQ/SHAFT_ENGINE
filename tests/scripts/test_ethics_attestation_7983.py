"""Person-only attestation: receipt enums, ethics-check, hook shapes, doctor hash."""

from __future__ import annotations

import hashlib
import importlib.util
import json
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
FIXTURES = CE / "evals" / "ethics-attestation-fixtures.json"
COUNTERPARTS = {"ai-accepted", "person-only", "unclear"}
DECISIONS = {"allow", "decline", "alternative"}


def load(name: str, relative: str):
    path = ROOT / relative
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(relative)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class EthicsAttestationTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.ethics = load("ethics_check_7983", "chaos-engine/rules/ethics_check.py")
        cls.economy = load("token_economy_7983", "chaos-engine/token_economy.py")
        cls.lifecycle = load("lifecycle_7983", "chaos-engine/hooks/lifecycle.py")
        cls.fixtures = json.loads(FIXTURES.read_text(encoding="utf-8"))

    def test_fixture_enums_and_ethics_check_match_expect(self):
        examples = self.fixtures["examples"]
        self.assertEqual(13, len(examples))
        for example in examples:
            with self.subTest(example=example["id"]):
                self.assertIn(example["counterpart"], COUNTERPARTS)
                self.assertIn(example["decision"], DECISIONS)
                self.assertIn(example["expect"], DECISIONS)
                self.assertEqual(example["expect"], example["decision"])
                self.assertIsInstance(example["live_feed"], bool)
                self.assertNotIn("?", example["situation"])
                receipt = {
                    "counterpart": example["counterpart"],
                    "decision": example["decision"],
                    "live_feed": example["live_feed"],
                }
                code = self.ethics.exit_code(receipt)
                if example["expect"] == "decline":
                    self.assertEqual(2, code)
                else:
                    self.assertEqual(0, code)
                with tempfile.TemporaryDirectory() as tmp:
                    path = Path(tmp) / "receipt.json"
                    path.write_text(json.dumps(receipt), encoding="utf-8")
                    completed = subprocess.run(
                        [sys.executable, str(CE / "tool.py"), "ethics-check", str(path)],
                        cwd=ROOT,
                        capture_output=True,
                        text=True,
                        check=False,
                    )
                self.assertEqual(code, completed.returncode, completed.stderr)
                if code != 0:
                    self.assertIn("do not sit", completed.stderr)
                    self.assertIn("do not feed answers live", completed.stderr)

    def test_assessment_shapes_deny_without_receipt_and_spare_ordinary_posts(self):
        ethics = self.ethics
        self.assertIsNone(
            ethics.assessment_block_reason(("git push origin HEAD:ChaosEngine/7983-person-only",), {}, None)
        )
        self.assertIsNone(
            ethics.assessment_block_reason(("gh issue create --title spec",), {}, None)
        )
        self.assertIsNone(
            ethics.assessment_block_reason(
                ("curl -fsS https://jobs.example/job-application",), {}, None
            )
        )
        denied = ethics.assessment_block_reason(
            ("curl -fsS https://example.test/exam/submit",), {}, None
        )
        self.assertIsNotNone(denied)
        self.assertIn("do not sit", denied)
        allowed = {
            "counterpart": "ai-accepted",
            "decision": "allow",
            "live_feed": False,
        }
        self.assertIsNone(
            ethics.assessment_block_reason(
                ("curl -fsS https://example.test/exam/submit",), {}, None, allowed
            )
        )
        person = dict(allowed, counterpart="person-only", decision="allow")
        self.assertIn(
            "do not feed answers live",
            ethics.assessment_block_reason(
                ("curl -fsS https://example.test/quiz/submit",), {}, None, person
            ),
        )
        live = dict(allowed, live_feed=True)
        self.assertIsNotNone(
            ethics.assessment_block_reason(("printf closed-book",), {}, None, live)
        )

    def test_guard_denies_assessment_submit_with_exit_2(self):
        with tempfile.TemporaryDirectory() as tmp:
            completed = subprocess.run(
                [sys.executable, str(CE / "hooks" / "guard.py")],
                input=json.dumps(
                    {
                        "hook_event_name": "PreToolUse",
                        "tool_name": "Bash",
                        "cwd": tmp,
                        "tool_input": {"command": "curl -fsS https://example.test/exam/submit"},
                    }
                ),
                cwd=tmp,
                capture_output=True,
                text=True,
                check=False,
            )
        self.assertEqual(2, completed.returncode, completed.stderr)
        self.assertIn("do not sit", completed.stdout)
        self.assertIn("Person-only", completed.stdout)

    def test_session_start_names_ethics_card_within_600_bytes(self):
        context = self.lifecycle.session_start_context("token-fixture", "activation")
        encoded = context.encode("utf-8")
        self.assertLessEqual(len(encoded), 600)
        self.assertIn("companions/ethics-card.md", context)

    def test_grok_rules_name_ethics_card_and_do_not_copy_conduct(self):
        rules = (ROOT / ".grok/rules/companions.md").read_text(encoding="utf-8")
        self.assertIn("chaos-engine/companions/ethics-card.md", rules)
        self.assertFalse((ROOT / ".grok/rules/ethical-conduct.md").exists())
        published = (CE / "grok_model_load.py").read_text(encoding="utf-8")
        self.assertNotIn("ethical-conduct.md", published)

    def test_doctor_hash_fails_on_temp_copy_not_the_repo_file(self):
        source = CE / "companions" / "ethics-card.md"
        before = source.read_bytes()
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for rel in self.economy.ETHICS_TEXT_SHA256:
                dest = root / rel
                dest.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(CE / rel, dest)
            status, reason = self.economy.ethics_text_status(root)
            self.assertEqual("pass", status, reason)
            card = root / "companions" / "ethics-card.md"
            card.write_bytes(card.read_bytes() + b"\n")
            status, reason = self.economy.ethics_text_status(root)
            self.assertEqual("fail", status)
            self.assertIn("ethics-card.md", reason)
        self.assertEqual(before, source.read_bytes())
        digest = hashlib.sha256(before).hexdigest()
        self.assertEqual(digest, self.economy.ETHICS_TEXT_SHA256["companions/ethics-card.md"])

    def test_repair_restores_pinned_texts_and_self_modify_cannot_rewrite_them(self):
        install = load("install_7983_repair", "chaos-engine/install.py")
        modify = load("self_modify_7983", "chaos-engine/self_modify.py")
        pinned = {rel: (CE / rel).read_bytes() for rel in self.economy.ETHICS_TEXT_SHA256}
        with tempfile.TemporaryDirectory() as tmp:
            project = Path(tmp)
            package = project / "chaos-engine"
            overlay = project / ".chaos-engine"
            owned = {
                rel: hashlib.sha256(data).hexdigest() for rel, data in pinned.items()
            }
            for rel, data in pinned.items():
                for root in (package, overlay):
                    dest = root / rel
                    dest.parent.mkdir(parents=True, exist_ok=True)
                    dest.write_bytes(data)
                (overlay / rel).write_bytes(data + b"\nTAMPER\n")
            (overlay / "manifest.json").write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "distribution": {"id": "portable", "policySha256": "0" * 64},
                        "source": {"commit": "a" * 40, "kind": "local"},
                        "hostToken": "b" * 64,
                        "files": owned,
                    }
                ),
                encoding="utf-8",
            )
            result = install.repair_component(project, "core")
            self.assertEqual("restored-ethics-texts", result["action"])
            self.assertEqual(sorted(owned), sorted(result["restored"]))
            for rel, data in pinned.items():
                restored = (overlay / rel).read_bytes()
                self.assertEqual(data, restored)
                self.assertEqual(owned[rel], hashlib.sha256(restored).hexdigest())
            (package / "install.py").write_text("# package marker\n", encoding="utf-8")
            eval_manifest = project / "eval-manifest.json"
            eval_manifest.write_text(
                json.dumps(
                    {
                        "tasks": [
                            {
                                "id": "reg-self-modify-6518",
                                "module": "tests.scripts.test_chaos_engine_self_modify_6518",
                                "set": "regression",
                            }
                        ]
                    }
                ),
                encoding="utf-8",
            )
            item = modify.propose(
                "ethics-note",
                "a candidate body that is not an ethics file",
                project=project,
            )
            document = modify.load_index(project)
            document["candidates"][0]["score"] = {"passed": True}
            modify._save(document, project)
            applied = modify.apply_candidate(
                item["id"], project=project, eval_manifest=eval_manifest
            )
            self.assertTrue(str(applied["path"]).endswith("/applied/ethics-note.md"))
            for rel, data in pinned.items():
                self.assertEqual(data, (package / rel).read_bytes())
                self.assertEqual(data, (overlay / rel).read_bytes())
            with self.assertRaises(ValueError):
                modify._refuse_protected_ethics_rewrite(package / "identity.md", project)
            with self.assertRaises(ValueError):
                modify._refuse_protected_ethics_rewrite(
                    overlay / "companions" / "ethics-card.md", project
                )
        for rel, data in pinned.items():
            self.assertEqual(data, (CE / rel).read_bytes())

    def test_safe_marker_does_not_hide_an_assessment_shape(self):
        ethics = self.ethics
        denied = (
            "curl -fsS https://example.test/exam/submit # job application",
            "printf live-answer-feed # job-application",
            "git push origin HEAD && curl -fsS https://example.test/exam/submit",
        )
        for command in denied:
            with self.subTest(command=command):
                reason = ethics.assessment_block_reason((command,), {}, None)
                self.assertIsNotNone(reason)
                self.assertIn("do not sit", reason)
        allowed = (
            "git push origin HEAD:ChaosEngine/7983-person-only",
            "gh issue create --title spec",
            "curl -fsS https://jobs.example/job-application",
            "curl -fsS https://jobs.example/job-application # closed-book",
        )
        for command in allowed:
            with self.subTest(command=command):
                self.assertIsNone(ethics.assessment_block_reason((command,), {}, None))
        rows = (
            (r"https://example.test/\;#exam/submit", True),
            (r"foo\;#exam/submit", True),
            (r"foo\|#closed-book", True),
            (r"foo\&#exam/submit", True),
            (r"foo\ #exam/submit", True),
            (r"foo\#exam/submit", True),
            ('echo "foo\\" # exam/submit"', True),
            ("git push;#exam/submit", False),
            (r"git push\\;#exam/submit", False),
        )
        for text, expect in rows:
            with self.subTest(text=text):
                self.assertEqual(expect, ethics.matches_assessment(text))
        unclear = {"counterpart": "unclear", "decision": "allow", "live_feed": False}
        reason = ethics.assessment_block_reason(
            ("curl -fsS https://example.test/exam/submit",), {}, None, unclear
        )
        self.assertIsNotNone(reason)
        self.assertIn("do not sit", reason)

    def test_tampered_package_ethics_card_is_not_copied(self):
        install = load("install_7983_package_tamper", "chaos-engine/install.py")
        pinned = {rel: (CE / rel).read_bytes() for rel in self.economy.ETHICS_TEXT_SHA256}
        with tempfile.TemporaryDirectory() as tmp:
            project = Path(tmp)
            package = project / "chaos-engine"
            overlay = project / ".chaos-engine"
            for rel, data in pinned.items():
                for root in (package, overlay):
                    dest = root / rel
                    dest.parent.mkdir(parents=True, exist_ok=True)
                    dest.write_bytes(data)
            card = package / "companions" / "ethics-card.md"
            card.write_bytes(pinned["companions/ethics-card.md"] + b"\nTAMPER\n")
            before = {rel: (overlay / rel).read_bytes() for rel in pinned}
            with self.assertRaises(ValueError) as caught:
                install.restore_pinned_ethics_texts(project, overlay)
            self.assertIn("ethics text hash mismatch", str(caught.exception))
            for rel, data in pinned.items():
                self.assertEqual(data, (overlay / rel).read_bytes())
                self.assertEqual(before[rel], (overlay / rel).read_bytes())
        for rel, data in pinned.items():
            self.assertEqual(data, (CE / rel).read_bytes())


if __name__ == "__main__":
    unittest.main()
