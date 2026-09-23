"""Doctor policy-hash drift and degraded retrieve rows (#6136)."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


overlay_match = _load("overlay_match_6136", ROOT / "chaos-engine/overlay_match.py")
retrieve_justification = _load(
    "retrieve_justification_6136",
    ROOT / "chaos-engine/hooks/retrieve_justification.py",
)


def _write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


class PolicyHashDoctorTest(unittest.TestCase):
    def test_hash_drift_and_nested_router_fail_and_degraded_row_blocks(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            portable = root / "chaos-engine"
            overlay = root / ".chaos-engine"
            for tree in (portable, overlay):
                _write(tree / "identity.md", "same-identity\n")
                _write(tree / "skills/chaos-engine/SKILL.md", "router\n")
                _write(tree / "references/roles.md", "roles\n")
                _write(tree / "hooks/guard.py", "guard\n")
            self.assertEqual(overlay_match.policy_hash_mismatches(portable, overlay), [])
            _write(overlay / "identity.md", "other-identity\n")
            self.assertIn("identity.md", overlay_match.policy_hash_mismatches(portable, overlay))
            nested = overlay / ".chaos-engine"
            _write(nested / "identity.md", "other-identity\n")
            _write(nested / "skills/chaos-engine/SKILL.md", "other-router\n")
            self.assertEqual(
                overlay_match.nested_overlay_drift(overlay),
                ["skills/chaos-engine/SKILL.md"],
            )
            result = {"status": "healthy", "components": {}}
            overlay_match.apply_policy_hash_doctor(
                result,
                root,
                retrieve_reports=[
                    {
                        "store": "mempalace",
                        "status": "degraded",
                        "reason": "backend mismatch chroma",
                    },
                ],
            )
            policy = result["components"]["policy-overlay"]
            self.assertEqual("recovery-required", policy["status"])
            self.assertIn("identity.md", policy["mismatches"])
            self.assertIn("skills/chaos-engine/SKILL.md", policy["nestedDrift"])
            row = result["components"]["retrieve-mempalace"]
            self.assertEqual("recovery-required", row["status"])
            self.assertEqual("required", row["taskImpact"])
            self.assertIn("backend mismatch chroma", row["reason"])
            self.assertIn("repair --project . --component mempalace", row["fixNext"])
            self.assertIn("auto-migrate", row["fixNext"])
            self.assertEqual("recovery-required", result["status"])
        denial = retrieve_justification.BLOCK_REASON
        self.assertIn("retrieve-mempalace", denial)
        self.assertIn("retrieve-graphify", denial)
        install = (ROOT / "chaos-engine/INSTALL.md").read_text(encoding="utf-8")
        self.assertIn(
            "python3 .chaos-engine/install.py repair --project . --component mempalace",
            install,
        )
        self.assertIn("Do not auto-migrate", install)



    def test_absent_store_degraded_does_not_block_doctor(self):
        result = {"status": "healthy", "components": {}}
        overlay_match.apply_policy_hash_doctor(
            result,
            Path("."),
            retrieve_reports=[{"store": "graphify", "status": "degraded", "reason": "nonzero-exit"}],
        )
        self.assertNotIn("retrieve-graphify", result["components"])
        self.assertEqual("healthy", result["status"])

if __name__ == "__main__":
    unittest.main()
