"""Parallel-streams protocol: ownership conflict, disk gate, post-merge cleanup."""

from __future__ import annotations

import unittest

import sys as _sys
from pathlib import Path as _Path

_sys.path.insert(0, str(_Path(__file__).resolve().parents[2]))  # noqa: E402 - self-locating under python3 -I (#7197)

from scripts.ci.parallel_stream_conflict import (  # noqa: E402
    active_peers_of_kind,
    conflicting_peer_stream_ids,
    conflicting_same_kind_stream_ids,
    kind_wip_limit,
    refuse_dispatch_on_ownership_overlap,
    refuse_dispatch_on_same_kind_wip,
    writable_globs_overlap,
)


# Int streak counters (not secrets). Name refs avoid Bandit B105 on dict keys
# whose names contain the substring "pass" (first_pass_signoff_streak).
_STREAK_0 = 0
_STREAK_2 = 2


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



class DesignKindWipConflictTest(unittest.TestCase):
    """Same-kind batch = conflict; different kinds and WIP=2 after streak are OK."""

    def test_same_kind_in_flight_refused(self) -> None:
        """FN guard: two in-flight video streams must refuse a third / second."""
        peers = [
            {
                "stream_id": "v1",
                "status": "active",
                "kind": "video",
                "owned_path_globs": ["content/videos/a/**"],
            },
        ]
        proposed = {
            "stream_id": "v2",
            "status": "active",
            "kind": "video",
            "owned_path_globs": ["content/videos/b/**"],
            "first_pass_signoff_streak": _STREAK_0,
        }
        self.assertTrue(refuse_dispatch_on_same_kind_wip(proposed, peers))
        self.assertEqual(["v1"], conflicting_same_kind_stream_ids(proposed, peers))
        # Path globs are disjoint — ownership alone would allow; kind must refuse.
        self.assertFalse(
            refuse_dispatch_on_ownership_overlap(
                proposed["owned_path_globs"], peers
            )
        )

    def test_different_kinds_allowed(self) -> None:
        """FP guard: video + podcast may run together when paths disjoint."""
        peers = [
            {
                "stream_id": "v1",
                "status": "active",
                "kind": "video",
                "owned_path_globs": ["content/videos/**"],
            },
        ]
        proposed = {
            "stream_id": "p1",
            "kind": "podcast",
            "owned_path_globs": ["content/podcast/**"],
        }
        self.assertFalse(refuse_dispatch_on_same_kind_wip(proposed, peers))
        self.assertEqual([], conflicting_same_kind_stream_ids(proposed, peers))
        self.assertEqual(1, kind_wip_limit("podcast", first_pass_signoff_streak=0))
        self.assertEqual(1, kind_wip_limit("video", first_pass_signoff_streak=1))

    def test_wip2_after_two_first_pass_signs(self) -> None:
        """FP guard: streak>=2 allows a second same-kind in flight; reject resets."""
        peers = [
            {
                "stream_id": "img1",
                "status": "active",
                "kind": "image",
                "owned_path_globs": ["content/images/a/**"],
                "first_pass_signoff_streak": _STREAK_2,
            },
        ]
        proposed = {
            "stream_id": "img2",
            "kind": "image",
            "owned_path_globs": ["content/images/b/**"],
            "first_pass_signoff_streak": _STREAK_2,
        }
        self.assertEqual(2, kind_wip_limit("image", first_pass_signoff_streak=2))
        self.assertFalse(refuse_dispatch_on_same_kind_wip(proposed, peers))
        self.assertEqual([], conflicting_same_kind_stream_ids(proposed, peers))
        # Third in flight still refused at WIP=2.
        peers2 = peers + [
            {
                "stream_id": "img2",
                "status": "active",
                "kind": "image",
                "owned_path_globs": ["content/images/b/**"],
            }
        ]
        proposed3 = {
            "stream_id": "img3",
            "kind": "image",
            "first_pass_signoff_streak": _STREAK_2,
            "owned_path_globs": ["content/images/c/**"],
        }
        self.assertTrue(refuse_dispatch_on_same_kind_wip(proposed3, peers2))
        self.assertEqual(
            ["img1", "img2"],
            conflicting_same_kind_stream_ids(proposed3, peers2),
        )
        # Reject → streak 0 → back to WIP=1 (one peer already blocks).
        proposed_reset = {
            "stream_id": "img4",
            "kind": "image",
            "first_pass_signoff_streak": _STREAK_0,
            "owned_path_globs": ["content/images/d/**"],
        }
        self.assertTrue(refuse_dispatch_on_same_kind_wip(proposed_reset, peers))
        self.assertEqual(["v1"], active_peers_of_kind(
            [
                {"stream_id": "v1", "status": "active", "kind": "video"},
                {"stream_id": "p1", "status": "active", "kind": "podcast"},
            ],
            "video",
        ))



if __name__ == "__main__":
    unittest.main()
