"""CE 2026.10.31.6 consolidated retro gates (H-* + L60–L65 + H-IDENTITY/H-SCOPE/H-PR-SCOPE)."""
from __future__ import annotations

import hashlib
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DESIGN_QC = ROOT / "chaos-engine/addons/design-skills/scripts/design_qc.py"
CRAFT_QC = ROOT / "chaos-engine/addons/design-skills/scripts/craft_qc.py"

def run_design(*argv: str) -> tuple[int, dict]:
    proc = subprocess.run(  # nosec B603 — fixed argv: local design_qc + test paths
        [sys.executable, str(DESIGN_QC), *argv],
        check=False,
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    payload = {}
    for line in (proc.stdout or "").splitlines():
        line = line.strip()
        if line.startswith("{") and line.endswith("}"):
            try:
                payload = json.loads(line)
            except json.JSONDecodeError:
                # Non-JSON stdout lines from CLI helpers are ignored on purpose.
                continue
    return proc.returncode, payload

def _ce_key(value: str):
    import importlib.util as _u
    from pathlib import Path as _P

    spec = _u.spec_from_file_location("ce_version", _P(__file__).resolve().parents[2] / "scripts/ci/ce_version.py")
    mod = _u.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod.version_key(value)

class WorkerOwnershipTest(unittest.TestCase):
    def test_record_and_allows(self) -> None:
        sys.path.insert(0, str(ROOT))
        from scripts.ci import worker_ownership as wo

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            created = root / "wt-demo"
            created.mkdir()
            manifest = wo.record(
                "demo", created, kind="worktree", checkpoints_dir=root,
            )
            self.assertTrue(wo.allows_cleanup(manifest, created))
            other = root / "foreign"
            other.mkdir()
            self.assertFalse(wo.allows_cleanup(manifest, other))

class SemanticBaselineTest(unittest.TestCase):
    def test_refresh_updates_hash_keeps_reason(self) -> None:
        sys.path.insert(0, str(ROOT))
        from scripts.ci.skillspector.semantic_baseline import refresh_entry, semantic_id

        baseline = {"version": 2, "fingerprints": []}
        first = refresh_entry(
            baseline,
            rule_id="LP4",
            file="SKILL.md",
            new_hash="sha256:aaa",
            reason="keep-me",
        )
        self.assertEqual("keep-me", first["reason"])
        sid = semantic_id("LP4", "SKILL.md")
        self.assertEqual(sid, first["semantic_id"])
        second = refresh_entry(
            baseline,
            rule_id="LP4",
            file="SKILL.md",
            new_hash="sha256:bbb",
        )
        self.assertEqual("sha256:bbb", second["hash"])
        self.assertEqual("keep-me", second["reason"])
        self.assertEqual(1, len(baseline["fingerprints"]))

class InfraPreflightTest(unittest.TestCase):
    def test_swap_missing(self) -> None:
        sys.path.insert(0, str(ROOT))
        from scripts.ci import box_disk_guard as bdg

        reasons = bdg.blocked_infra_reasons(
            ROOT, free=100 * 1024**3, used_percent=10.0, swap_kib=0,
        )
        self.assertIn("swap-missing", reasons)
        reasons_ok = bdg.blocked_infra_reasons(
            ROOT, free=100 * 1024**3, used_percent=10.0, swap_kib=1024,
            apt_mirror_ok=False,
        )
        self.assertIn("apt-mirror-unreachable", reasons_ok)
        self.assertNotIn("swap-missing", reasons_ok)

class PrScopeCheckTest(unittest.TestCase):
    def test_out_of_scope_and_missing(self) -> None:
        sys.path.insert(0, str(ROOT))
        from scripts.ci.pr_scope_check import evaluate

        body = "<!-- chaos-engine:scope chaos-engine/,tests/scripts/ -->\n"
        result = evaluate(body, ["chaos-engine/VERSION", "src/main/java/X.java"])
        self.assertFalse(result["ok"])
        self.assertTrue(any("out-of-scope" in h for h in result["hits"]))
        result_ok = evaluate(body, ["chaos-engine/VERSION", "tests/scripts/t.py"])
        self.assertTrue(result_ok["ok"], result_ok)
        missing = evaluate("no scope here", ["chaos-engine/VERSION"])
        self.assertFalse(missing["ok"])
        self.assertIn("missing-scope", missing["hits"])

    def test_scope_section_bullets(self) -> None:
        sys.path.insert(0, str(ROOT))
        from scripts.ci.pr_scope_check import parse_scope_prefixes

        body = "## Scope\n- chaos-engine/\n- scripts/ci/\n\n## Plan\n- do stuff\n"
        prefixes = parse_scope_prefixes(body)
        self.assertEqual(["chaos-engine/", "scripts/ci/"], prefixes)

class PrScopeWiredTest(unittest.TestCase):
    def test_overlay_and_harness_call_pr_scope(self) -> None:
        opp = (ROOT / "scripts/ci/overlay_pre_push.py").read_text(encoding="utf-8")
        self.assertIn("def pr_scope_failures", opp)
        self.assertIn("pr_scope_failures(root, changed)", opp)
        harness = (ROOT / "scripts/ci/harness_pr_gate.py").read_text(encoding="utf-8")
        self.assertIn("def pr_scope_gate_failures", harness)
        self.assertIn("pr_scope_gate_failures(", harness)
        sys.path.insert(0, str(ROOT))
        from scripts.ci.pr_scope_check import pr_scope_failures, is_agentic_pr

        body = "<!-- chaos-engine:agentic -->\n<!-- chaos-engine:scope chaos-engine/ -->\n"
        self.assertTrue(is_agentic_pr({"body": body, "labels": []}))
        hits = pr_scope_failures(body, ["src/X.java"], require_agentic=True, pr={"body": body})
        self.assertTrue(hits)
        ok = pr_scope_failures(body, ["chaos-engine/VERSION"], require_agentic=True, pr={"body": body})
        self.assertEqual([], ok)

if __name__ == "__main__":
    unittest.main()
