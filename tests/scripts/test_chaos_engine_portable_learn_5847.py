"""#5847: portable /learn UX — map-reduce-verify file contract, no host TUI."""

from __future__ import annotations

import contextlib
import importlib.util
import json
import os
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

_CLEAR_ENV = (
    "CLAUDE_CONFIG",
    "CODEX_HOME",
    "GROK_HOME",
    "GEMINI_HOME",
    "COPILOT_HOME",
    "XDG_CONFIG_HOME",
)


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    if spec.loader is None:
        raise ImportError(f"unable to load {path}")
    spec.loader.exec_module(module)
    return module


@contextlib.contextmanager
def _cleared_host_homes():
    saved = {key: os.environ.pop(key) for key in _CLEAR_ENV if key in os.environ}
    try:
        yield
    finally:
        os.environ.update(saved)


def _write_human_session(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(
            {
                "type": "user",
                "message": {"role": "user", "content": content},
            }
        )
        + "\n"
        + json.dumps(
            {
                "type": "assistant",
                "message": {"role": "assistant", "content": "ack"},
            }
        )
        + "\n",
        encoding="utf-8",
    )


class PortableLearn5847Tests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.learn = load(ROOT / "chaos-engine/learn_traces.py", "ce_learn_traces_5847")

    def test_learn_offline_writes_report_and_git_tracked_actions(self):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            out = home / "run"
            claude = home / ".claude" / "projects" / "-demo"
            _write_human_session(
                claude / "one.jsonl",
                "please keep portable learn contract for overlay policy api_key=SECRET999",
            )
            _write_human_session(
                claude / "two.jsonl",
                "please keep portable learn contract for overlay policy",
            )
            with _cleared_host_homes():
                result = self.learn.learn(out, home=home, offline=True)
            self.assertEqual("complete", result["status"])
            self.assertTrue((out / "report.md").is_file())
            self.assertTrue((out / "actions.json").is_file())
            self.assertTrue((out / "layout.json").is_file())
            self.assertTrue((out / "map" / "batch-000.json").is_file())
            self.assertTrue((out / "reduce" / "synthesis.json").is_file())
            self.assertTrue((out / "verify" / "verdict.json").is_file())
            self.assertTrue((out / "prompts" / "map.md").is_file())
            manifest = json.loads((out / "manifest.json").read_text(encoding="utf-8"))
            kept = int(manifest["sessions_kept"])
            self.assertGreaterEqual(kept, 2)
            report = (out / "report.md").read_text(encoding="utf-8")
            self.assertIn(f"Coverage: {kept} sessions kept in manifest", report)
            actions = json.loads((out / "actions.json").read_text(encoding="utf-8"))
            self.assertEqual(kept, actions["sessions_kept"])
            self.assertIn(f"Coverage: {kept} sessions kept", actions["coverageLine"])
            self.assertTrue(actions["policy"]["forbidHomeSkills"])
            blob = json.dumps(actions) + report
            self.assertNotIn("SECRET999", blob)
            targets = "".join(str(item.get("target") or "") for item in actions["actions"])
            self.assertNotIn("~/.grok/skills", targets)
            for item in actions["actions"]:
                target = str(item["target"])
                self.assertTrue(self.learn.is_git_tracked_overlay_target(target), msg=target)
                self.assertFalse(self.learn.is_forbidden_target(target))
                self.assertTrue(item.get("evidenceSessions"))

    def test_finalize_rejects_home_skill_targets(self):
        with tempfile.TemporaryDirectory() as temporary:
            run = Path(temporary) / "run"
            run.mkdir()
            (run / "manifest.json").write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "sessions_kept": 1,
                        "sessions": [{"id": "s1", "path": "sessions/s1.json"}],
                        "dropped": {},
                    }
                )
                + "\n",
                encoding="utf-8",
            )
            (run / "sessions").mkdir()
            (run / "sessions" / "s1.json").write_text(
                json.dumps({"id": "s1", "messages": [{"role": "user", "content": "x"}]})
                + "\n",
                encoding="utf-8",
            )
            verify = run / "verify"
            verify.mkdir()
            (verify / "verdict.json").write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "kept": [
                            {
                                "id": "bad",
                                "kind": "overlay-edit",
                                "target": "~/.grok/skills/learn/SKILL.md",
                                "summary": "nope",
                                "evidenceSessions": ["s1"],
                            },
                            {
                                "id": "good",
                                "kind": "overlay-edit",
                                "target": "chaos-engine/references/learn-traces.md",
                                "summary": "ok",
                                "evidenceSessions": ["s1"],
                            },
                        ],
                        "dropped": [],
                        "sessionsCited": ["s1"],
                    }
                )
                + "\n",
                encoding="utf-8",
            )
            actions = self.learn.finalize(run)
            targets = [item["target"] for item in actions["actions"]]
            self.assertEqual(["chaos-engine/references/learn-traces.md"], targets)
            self.assertEqual(1, len(actions["rejected"]))
            self.assertIn("non_git_tracked_or_home", actions["rejected"][0]["rejectReason"])

    def test_host_agents_mode_awaits_without_grok_bundled_skill(self):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            out = home / "run"
            _write_human_session(
                home / ".claude" / "projects" / "-x" / "s.jsonl",
                "hello portable learn",
            )
            with _cleared_host_homes():
                result = self.learn.learn(
                    out, home=home, collect_first=True, offline=False
                )
            self.assertEqual("awaiting_host_map_reduce_verify", result["status"])
            self.assertTrue((out / "prompts" / "map.md").is_file())
            self.assertFalse((out / "report.md").is_file())
            prompts = (out / "prompts" / "map.md").read_text(encoding="utf-8")
            self.assertNotIn("learn-traces.rhai", prompts)
            self.assertNotIn("~/.grok/bundled/skills/learn", prompts)

    def test_collect_contract_still_works(self):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            out = home / "run"
            with _cleared_host_homes():
                manifest = self.learn.collect(home, out)
            self.assertEqual(1, manifest["schemaVersion"])
            self.assertTrue((out / "manifest.json").is_file())

    def test_docs_forbid_rhai_and_document_portable_learn(self):
        traces = (ROOT / "chaos-engine/references/learn-traces.md").read_text(
            encoding="utf-8"
        )
        harness = (ROOT / "chaos-engine/references/harness-learn.md").read_text(
            encoding="utf-8"
        )
        catalog = (ROOT / "chaos-engine/references/zero-llm-catalog.md").read_text(
            encoding="utf-8"
        )
        self.assertIn("learn_traces.py learn", traces)
        self.assertIn("actions.json", traces)
        self.assertIn("Thin host adapters", traces)
        self.assertIn("Do not vendor a Grok TUI learn-traces workflow", traces)
        self.assertNotIn("learn-traces.rhai", traces)
        self.assertIn("learn_traces.py learn", harness)
        self.assertIn("learn --out", catalog)
        overlay = ROOT / "chaos-engine"
        hits = list(overlay.rglob("*learn-traces.rhai"))
        self.assertEqual([], hits)
        for path in overlay.rglob("*"):
            if not path.is_file():
                continue
            if path.suffix.casefold() in {".rhai", ".lua"} and "learn" in path.name.casefold():
                self.fail(f"unexpected host TUI workflow artifact: {path}")


if __name__ == "__main__":
    unittest.main()
