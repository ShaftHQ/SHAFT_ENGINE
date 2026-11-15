"""CE 2026.10.31.7: WoW + memory dump + L65-fu/L66–L77 (L74 proven-red)."""
from __future__ import annotations

import hashlib
import importlib.util
import json
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"

def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    assert spec and spec.loader
    sys.modules[name] = mod  # dataclasses need the module registered
    spec.loader.exec_module(mod)
    return mod

def _ce_key(value: str):
    import importlib.util as _u
    from pathlib import Path as _P

    spec = _u.spec_from_file_location("ce_version", _P(__file__).resolve().parents[2] / "scripts/ci/ce_version.py")
    mod = _u.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod.version_key(value)

class FileClaimLockTest(unittest.TestCase):
    def setUp(self) -> None:
        self.mod = _load(
            "ce_file_claim_lock", ROOT / "scripts/ci/file_claim_lock.py"
        )

    def test_overlap_refuses(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "claims.json"
            self.mod.claim("a", "w1", ["chaos-engine/foo.py"], path=path, kind="ce-rules")
            with self.assertRaises(PermissionError):
                self.mod.claim(
                    "b", "w2", ["chaos-engine/foo.py"], path=path, kind="other"
                )

    def test_same_kind_refuses(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "claims.json"
            self.mod.claim("a", "w1", ["a/one.py"], path=path, kind="ce-rules")
            with self.assertRaises(PermissionError):
                self.mod.claim(
                    "b", "w2", ["b/two.py"], path=path, kind="ce-rules"
                )

    def test_release_allows_reclaim(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "claims.json"
            self.mod.claim("a", "w1", ["x/y.py"], path=path, kind="k")
            self.mod.release("a", path=path)
            self.mod.claim("b", "w2", ["x/y.py"], path=path, kind="k")

class ProvenRedMixin:
    """L74: every new gate proves FAIL on a known-bad fixture first."""

    def _prove(self, gate_path: Path, bad: dict, hit_sub: str) -> None:
        proven = _load("ce_gate_proven_red", CE / "rules/gate_proven_red.py")
        gate_mod = _load("ce_gate_under_test", gate_path)
        proven.prove_red(gate_mod.evaluate, bad, expected_hit_substr=hit_sub)

class AdversarialFileClaimRelAbsTest(unittest.TestCase):
    def test_rel_abs_same_file_overlaps(self) -> None:
        mod = _load("ce_file_claim_lock", ROOT / "scripts/ci/file_claim_lock.py")
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            target = root / "chaos-engine" / "foo.py"
            target.parent.mkdir(parents=True)
            target.write_text("x", encoding="utf-8")
            claims = root / "claims.json"
            mod.claim(
                "a", "w1", ["chaos-engine/foo.py"],
                path=claims, kind="k1", project_root=root,
            )
            with self.assertRaises(PermissionError):
                mod.claim(
                    "b", "w2", [str(target.resolve())],
                    path=claims, kind="k2", project_root=root,
                )

if __name__ == "__main__":
    unittest.main()
