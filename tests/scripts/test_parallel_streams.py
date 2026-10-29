"""Parallel-streams protocol: ownership conflict, disk gate, post-merge cleanup."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
REFS = ROOT / "chaos-engine/references"
DOC = REFS / "parallel-streams.md"

import sys as _sys; from pathlib import Path as _Path; _sys.path.insert(0, str(_Path(__file__).resolve().parents[2]))  # noqa: E402,E702 - self-locating under python3 -I (#7197)
from scripts.ci.parallel_stream_conflict import (  # noqa: E402
    conflicting_peer_stream_ids,
    refuse_dispatch_on_ownership_overlap,
    writable_globs_overlap,
)


class ParallelStreamsProtocolTest(unittest.TestCase):
    def test_doc_states_ownership_disk_review_cleanup_and_no_force_push(self) -> None:
        text = DOC.read_text(encoding="utf-8")
        for needle in (
            "Stream-ownership manifest",
            "owned path globs",
            "Conflict detection",
            "peer owned path globs",
            "ownership manifest",
            "Brief template",
            "riskiest assumption",
            "live behaviour",
            "no force-push",
            "Never force-push",
            "15 GiB",
            "10 GiB",
            "box_disk_guard.py",
            "Cache caps",
            "Own scratch TTL",
            "Definition of Done",
            "filter=blob:none",
            "git worktree add",
            "scratch_pending_cleanup",
            "WIP limit 1",
            "delete-check",
            "Post-merge own cleanup",
            "MERGED",
            "--cleanup-task-worktree",
            "--cleanup-task-local-branch",
            "--cleanup-task-remote-head",
            "--prune-after-task-cleanup",
            "never-touch",
            "parallel_stream_conflict.py",
            "refuse_dispatch",
        ):
            self.assertIn(needle, text)

    def test_core_doc_has_no_ecosystem_or_person_names(self) -> None:
        text = DOC.read_text(encoding="utf-8").lower()
        for term in (
            "shaft",
            "grok",
            "selenium",
            "maven",
            "mohab",
            "automatest",
            "carl",
            "coach",
        ):
            self.assertNotIn(term, text)

    def test_linked_from_entry_flow_pdca_delegation_cleanup(self) -> None:
        for name in (
            "bot-entry.md",
            "iterative-flow.md",
            "multi-agent-pdca.md",
            "delegation.md",
            "cleanup-scopes.md",
        ):
            body = (REFS / name).read_text(encoding="utf-8")
            self.assertIn("parallel-streams.md", body, msg=name)

    def test_pdca_requires_live_behaviour_evidence(self) -> None:
        text = (REFS / "multi-agent-pdca.md").read_text(encoding="utf-8")
        self.assertIn("live behaviour", text)
        self.assertIn("offline fixtures", text)

    def test_bot_entry_disk_gate_and_per_stream_wip(self) -> None:
        text = (REFS / "bot-entry.md").read_text(encoding="utf-8")
        self.assertIn("15 GiB", text)
        self.assertIn("10 GiB", text)
        self.assertIn("box_disk_guard.py", text)
        self.assertIn("Post-merge own cleanup", text)
        self.assertIn("WIP limit 1\nper stream", text)
        self.assertIn("(parallel-streams.md)", text)

    def test_harness_map_lists_parallel_streams(self) -> None:
        hmap = (REFS / "harness-map.md").read_text(encoding="utf-8")
        self.assertIn("references/parallel-streams.md", hmap)

    def test_cleanup_scopes_lists_scratch_after_ship(self) -> None:
        text = (REFS / "cleanup-scopes.md").read_text(encoding="utf-8")
        self.assertIn("scratch_pending_cleanup", text)
        self.assertIn("verified-pushed", text)
        self.assertIn("--cleanup-task-local-branch", text)
        self.assertIn("post-merge own cleanup", text)

    def test_delegation_and_iterative_flow_require_post_merge_own_cleanup(self) -> None:
        delegation = (REFS / "delegation.md").read_text(encoding="utf-8")
        iterative = (REFS / "iterative-flow.md").read_text(encoding="utf-8")
        self.assertIn("post-merge own cleanup", delegation)
        self.assertIn("post-merge own cleanup", iterative)
        self.assertIn("owned globs in the manifest", iterative)
        self.assertIn("Post-merge own cleanup ran", iterative)

    def test_skills_map_lists_parallel_streams(self) -> None:
        readme = (ROOT / ".agents/skills/README.md").read_text(encoding="utf-8")
        self.assertIn(
            "../../chaos-engine/references/parallel-streams.md",
            readme,
        )


class ParallelStreamConflictBehaviourTest(unittest.TestCase):
    def test_overlap_refused_disjoint_allowed(self) -> None:
        peers = [
            {
                "stream_id": "A",
                "status": "active",
                "owned_path_globs": ["chaos-engine/references/**"],
            },
            {
                "stream_id": "B",
                "status": "done",
                "owned_path_globs": ["scripts/ci/**"],
            },
        ]
        overlap = ["chaos-engine/references/parallel-streams.md"]
        disjoint = ["scripts/ci/worktree_hygiene.py"]

        self.assertTrue(writable_globs_overlap(overlap, peers[0]["owned_path_globs"]))
        self.assertTrue(refuse_dispatch_on_ownership_overlap(overlap, peers))
        self.assertEqual(["A"], conflicting_peer_stream_ids(overlap, peers))

        self.assertFalse(refuse_dispatch_on_ownership_overlap(disjoint, peers))
        self.assertEqual([], conflicting_peer_stream_ids(disjoint, peers))

    def test_prefix_and_equal_globs_conflict_nested_siblings_do_not(self) -> None:
        self.assertTrue(
            writable_globs_overlap(["docs/**"], ["docs/guide.md"])
        )
        self.assertTrue(
            writable_globs_overlap(["chaos-engine/**"], ["chaos-engine/references/**"])
        )
        self.assertFalse(
            writable_globs_overlap(
                ["chaos-engine/references/**"],
                ["chaos-engine/skills/**"],
            )
        )

    def test_exclude_self_and_inactive_peers(self) -> None:
        peers = [
            {
                "stream_id": "self",
                "status": "active",
                "owned_path_globs": ["src/**"],
            },
            {
                "stream_id": "old",
                "status": "merged",
                "owned_path_globs": ["src/**"],
            },
        ]
        self.assertEqual(
            [],
            conflicting_peer_stream_ids(
                ["src/a.py"], peers, exclude_stream_id="self"
            ),
        )


if __name__ == "__main__":
    unittest.main()
