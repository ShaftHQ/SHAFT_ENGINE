"""#5863/#5864 OmniRoute proof-of-dispatch and agent/user-machine rails."""

from __future__ import annotations

import importlib.util
import json
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RUNNER_PATH = ROOT / "chaos-engine/skills/omniroute/scripts/runner.py"
SKILL_PATH = ROOT / "chaos-engine/skills/omniroute/SKILL.md"
GUIDE_PATH = ROOT / "chaos-engine/guides/omniroute.md"
PROOF_REF = ROOT / "chaos-engine/skills/omniroute/references/proof-of-dispatch.md"
DOCS_NOTES = ROOT / "chaos-engine/skills/omniroute/references/docs-study-notes.md"
LESSONS = ROOT / "chaos-engine/skills/omniroute/references/living-lessons.md"
WORKFLOWS = ROOT / "chaos-engine/references/execution-workflows.md"
ROUTER = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"


def load_runner():
    spec = importlib.util.spec_from_file_location("omniroute_runner_proof", RUNNER_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


class OmniRouteProofOfDispatchTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.runner = load_runner()

    def test_not_required_is_pass(self):
        result = self.runner.evaluate_required_dispatch_proof(omniroute_required=False)
        self.assertTrue(result["proved"])
        self.assertEqual("NOT_REQUIRED", result["state"])
        self.assertIsNone(result["blocker"])

    def test_required_with_only_probes_blocks(self):
        result = self.runner.evaluate_required_dispatch_proof(
            omniroute_required=True,
            claimed_steps=["probe", "candidates", "health"],
            call_logs=[
                {
                    "model": "connection-test",
                    "provider": "groq",
                    "method": "POST",
                    "tokens": 0,
                    "status": 200,
                },
                {
                    "model": "CredentialHealth",
                    "provider": "antigravity",
                    "method": "POST",
                    "tokens": 0,
                    "status": 200,
                },
            ],
        )
        self.assertFalse(result["proved"])
        self.assertEqual("BLOCKED", result["state"])
        self.assertTrue(result["probeOnlyEvidence"])
        self.assertIn("omniroute run", result["blocker"])
        self.assertIn("probes alone", result["blocker"])

    def test_required_with_run_receipt_proves(self):
        result = self.runner.evaluate_required_dispatch_proof(
            omniroute_required=True,
            run_receipt={"outcome": "success", "status": "completed", "exitCode": 0},
            claimed_steps=["probe", "candidates"],
        )
        self.assertTrue(result["proved"])
        self.assertEqual("PROVED", result["state"])
        self.assertEqual("run_receipt", result["proofKind"])

    def test_required_with_coding_call_log_proves(self):
        result = self.runner.evaluate_required_dispatch_proof(
            omniroute_required=True,
            call_logs=[
                {
                    "model": "connection-test",
                    "provider": "groq",
                    "tokens": 0,
                    "status": 200,
                    "method": "POST",
                },
                {
                    "model": "antigravity/claude-sonnet-4-6",
                    "provider": "antigravity",
                    "tokens": 128,
                    "status": 200,
                    "method": "POST",
                },
            ],
        )
        self.assertTrue(result["proved"])
        self.assertEqual("coding_call_log", result["proofKind"])

    def test_probe_helpers_classify_connection_test(self):
        self.assertTrue(
            self.runner.call_log_is_probe({"model": "connection-test", "tokens": 0})
        )
        self.assertFalse(
            self.runner.call_log_is_coding_completion(
                {"model": "connection-test", "tokens": 0, "status": 200, "method": "POST"}
            )
        )
        self.assertTrue(
            self.runner.call_log_is_coding_completion(
                {
                    "model": "moonshot/kimi-k2.7-code",
                    "tokens": 12,
                    "status": 200,
                    "method": "POST",
                }
            )
        )

    def test_cli_proof_exit_codes(self):
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            receipt = tmp_path / "receipt.json"
            receipt.write_text(
                json.dumps({"outcome": "success", "status": "completed"}),
                encoding="utf-8",
            )
            probes = tmp_path / "probes.json"
            probes.write_text(
                json.dumps(
                    [{"model": "connection-test", "tokens": 0, "status": 200, "method": "POST"}]
                ),
                encoding="utf-8",
            )
            ok = subprocess.run(
                [
                    "python3",
                    str(RUNNER_PATH),
                    "proof",
                    "--required",
                    "--receipt",
                    str(receipt),
                ],
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(0, ok.returncode, ok.stderr)
            payload = json.loads(ok.stdout)
            self.assertTrue(payload["proved"])

            blocked = subprocess.run(
                [
                    "python3",
                    str(RUNNER_PATH),
                    "proof",
                    "--required",
                    "--call-logs",
                    str(probes),
                    "--steps",
                    "probe,candidates",
                ],
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(2, blocked.returncode, blocked.stdout)
            blocked_payload = json.loads(blocked.stdout)
            self.assertEqual("BLOCKED", blocked_payload["state"])

    def test_skill_guide_and_notes_cover_rails(self):
        skill = SKILL_PATH.read_text(encoding="utf-8")
        guide = GUIDE_PATH.read_text(encoding="utf-8")
        proof = PROOF_REF.read_text(encoding="utf-8")
        notes = DOCS_NOTES.read_text(encoding="utf-8")
        lessons = LESSONS.read_text(encoding="utf-8")
        workflows = WORKFLOWS.read_text(encoding="utf-8")

        self.assertIn("Agent machine vs user machine", skill)
        self.assertIn("Proof of dispatch when required", skill)
        self.assertIn("proof --required", skill)
        self.assertIn("Local Execution", skill)
        self.assertIn("machineId", skill)

        self.assertIn("Agent machine vs user machine", guide)
        self.assertIn("Proof of dispatch", guide)
        self.assertIn("connection-test", guide)

        self.assertIn("Catalog, candidates, health", proof)
        self.assertIn("Coding completion call_log", proof)
        self.assertIn("127.0.0.1:20128/docs", notes)
        self.assertIn("Box ≠ ROG loopback", lessons)
        self.assertIn("auto/coding", lessons)
        self.assertIn("Dismiss OmniRoute when thrashing", lessons)
        self.assertIn("requires** OmniRoute", workflows)
        self.assertIn("proof-of-dispatch", workflows)

    def test_router_skill_stays_under_size_budget(self):
        self.assertLessEqual(ROUTER.stat().st_size, 20000)


if __name__ == "__main__":
    unittest.main()
