"""Executable disk gate, worktree cap, cache prune allowlist, scratch TTL FPs."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
import unittest.mock
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

import sys as _sys
from pathlib import Path as _Path

_sys.path.insert(0, str(_Path(__file__).resolve().parents[2]))  # noqa: E402

from scripts.ci.box_disk_guard import (  # noqa: E402
    DISK_USED_PCT_BLOCKED_INFRA,
    FREE_BLOCK_BYTES,
    FREE_WARN_BYTES,
    GiB,
    MAX_LINKED_WORKTREES,
    OWN_SCRATCH_MARKER,
    SHARED_VENV_ROOT,
    blocked_infra_reasons,
    cache_inventory,
    caches_over_cap,
    classify_cleanup_path,
    disk_level,
    disk_used_percent,
    disk_warn,
    doctor_dirt_close_blockers,
    item_close_blockers,
    log_deleted_youtube_ids,
    new_worker_blockers,
    own_scratch_ttl_candidates,
    post_merge_dod_required,
    prune_cache,
    refuse_new_worker,
    scratch_ttl_blockers,
    stale_tip_on_origin_candidates,
    verify_live_item_state_before_status,
    youtube_replace_cleanup_allowed,
)
from scripts.ci.parallel_stream_conflict import refuse_dispatch  # noqa: E402


class DiskGateTest(unittest.TestCase):
    def test_tiers_ok_warn_block(self) -> None:
        self.assertEqual("ok", disk_level(".", free=20 * GiB))
        self.assertEqual("warn", disk_level(".", free=12 * GiB))
        self.assertEqual("block", disk_level(".", free=5 * GiB))
        self.assertFalse(disk_warn(".", free=20 * GiB))
        self.assertTrue(disk_warn(".", free=12 * GiB))
        self.assertTrue(disk_warn(".", free=FREE_BLOCK_BYTES - 1))

    def test_block_threshold_is_10_warn_is_15(self) -> None:
        self.assertEqual(10 * GiB, FREE_BLOCK_BYTES)
        self.assertEqual(15 * GiB, FREE_WARN_BYTES)

    def test_refuse_new_worker_on_low_disk(self) -> None:
        self.assertTrue(refuse_new_worker(".", free=5 * GiB))
        self.assertIn(
            "disk-free-below-10giB",
            new_worker_blockers(".", free=5 * GiB),
        )
        self.assertFalse(refuse_new_worker(".", free=20 * GiB, linked_count=0))

    def test_refuse_new_worker_on_linked_cap(self) -> None:
        blockers = new_worker_blockers(
            ".",
            repo_root=".",
            free=50 * GiB,
            linked_count=MAX_LINKED_WORKTREES,
        )
        self.assertIn("linked-worktree-cap", blockers)
        self.assertTrue(
            refuse_new_worker(
                ".",
                repo_root=".",
                free=50 * GiB,
                linked_count=MAX_LINKED_WORKTREES,
            )
        )
        self.assertFalse(
            refuse_new_worker(
                ".",
                repo_root=".",
                free=50 * GiB,
                linked_count=MAX_LINKED_WORKTREES - 1,
            )
        )


class CachePruneTest(unittest.TestCase):
    def test_inventory_marks_over_cap(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            home = Path(tmp)
            fat = home / ".cache" / "pip"
            fat.mkdir(parents=True)
            # 3 GiB marker file size via mock of _dir_size_bytes path walk is heavy;
            # write a small file and patch size.
            (fat / "wheel").write_bytes(b"x")
            with unittest.mock.patch(
                "scripts.ci.box_disk_guard._dir_size_bytes",
                side_effect=lambda path: 3 * GiB if "pip" in str(path) else 0,
            ):
                rows = cache_inventory(home=home)
                over = caches_over_cap(home=home)
            pip_rows = [r for r in rows if r["name"] == "pip"]
            self.assertEqual(1, len(pip_rows))
            self.assertTrue(pip_rows[0]["over_cap"])
            self.assertTrue(any(r["name"] == "pip" for r in over))

    def test_prune_uses_allowlisted_argv_only(self) -> None:
        calls: list[list[str]] = []

        def fake_run(argv, **_kwargs):  # type: ignore[no-untyped-def]
            calls.append(list(argv))
            return subprocess.CompletedProcess(argv, 0, "", "")

        with tempfile.TemporaryDirectory() as tmp:
            home = Path(tmp)
            (home / ".cache" / "pip").mkdir(parents=True)
            with unittest.mock.patch(
                "scripts.ci.box_disk_guard.never_touch_blockers",
                return_value=[],
            ):
                result = prune_cache("pip", home=home, runner=fake_run)
        self.assertEqual("pruned", result["status"])
        self.assertEqual([["pip", "cache", "purge"]], calls)

    def test_prune_unavailable_for_gradle_no_rm(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            home = Path(tmp)
            (home / ".gradle").mkdir()
            with unittest.mock.patch(
                "scripts.ci.box_disk_guard.never_touch_blockers",
                return_value=[],
            ):
                result = prune_cache("gradle", home=home)
        self.assertEqual("prune-unavailable", result["status"])
        self.assertIn("prune-unavailable", result["blockers"])

    def test_prune_refuses_never_touch(self) -> None:
        with unittest.mock.patch(
            "scripts.ci.box_disk_guard.never_touch_blockers",
            return_value=["never-touch-path"],
        ):
            result = prune_cache("pip")
        self.assertEqual("refused", result["status"])
        self.assertEqual(["never-touch-path"], result["blockers"])

    def test_prune_unknown_cache(self) -> None:
        result = prune_cache("not-a-real-cache")
        self.assertEqual("unknown-cache", result["status"])


class ScratchTtlFpTest(unittest.TestCase):
    def test_lists_only_own_aged_scratch(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            own_old = root / "scratch-old"
            own_new = root / "scratch-new"
            foreign = root / "foreign"
            for path in (own_old, own_new, foreign):
                path.mkdir()
            (own_old / OWN_SCRATCH_MARKER).write_text("stream-a\n", encoding="utf-8")
            (own_new / OWN_SCRATCH_MARKER).write_text("stream-a\n", encoding="utf-8")
            # foreign: no marker
            now = 1_700_000_000.0
            import os

            os.utime(own_old, (now - 48 * 3600, now - 48 * 3600))
            os.utime(own_new, (now - 1 * 3600, now - 1 * 3600))
            with unittest.mock.patch(
                "scripts.ci.box_disk_guard.never_touch_blockers",
                return_value=[],
            ):
                found = own_scratch_ttl_candidates(
                    [root], ttl_hours=24, now_epoch=now
                )
            paths = {row["path"] for row in found}
            self.assertIn(str(own_old.resolve()), paths)
            self.assertNotIn(str(own_new.resolve()), paths)
            self.assertNotIn(str(foreign.resolve()), paths)

    def test_fp_no_marker_within_ttl_protected(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            target = Path(tmp) / "x"
            target.mkdir()
            now = 1_700_000_000.0
            with unittest.mock.patch(
                "scripts.ci.box_disk_guard.never_touch_blockers",
                return_value=[],
            ):
                blockers = scratch_ttl_blockers(
                    target, ttl_hours=24, now_epoch=now
                )
            self.assertIn("not-own-scratch", blockers)

            (target / OWN_SCRATCH_MARKER).write_text("own\n", encoding="utf-8")
            import os

            os.utime(target, (now - 60, now - 60))
            with unittest.mock.patch(
                "scripts.ci.box_disk_guard.never_touch_blockers",
                return_value=[],
            ):
                blockers = scratch_ttl_blockers(
                    target, ttl_hours=24, now_epoch=now
                )
            self.assertIn("within-ttl", blockers)

            with unittest.mock.patch(
                "scripts.ci.box_disk_guard.never_touch_blockers",
                return_value=["never-touch-path"],
            ):
                blockers = scratch_ttl_blockers(
                    target, ttl_hours=24, now_epoch=now
                )
            self.assertIn("never-touch-path", blockers)


class DispatchIntegrationTest(unittest.TestCase):
    def test_refuse_dispatch_combines_ownership_and_disk(self) -> None:
        peers = [
            {
                "stream_id": "A",
                "status": "active",
                "owned_path_globs": ["chaos-engine/**"],
            }
        ]
        blockers = refuse_dispatch(
            ["scripts/ci/**"],
            peers,
            workspace=".",
            free=5 * GiB,
            linked_count=0,
        )
        self.assertIn("disk-free-below-10giB", blockers)
        self.assertFalse(any(b.startswith("ownership-overlap") for b in blockers))

        overlap = refuse_dispatch(
            ["chaos-engine/references/**"],
            peers,
            workspace=".",
            free=50 * GiB,
            linked_count=0,
        )
        self.assertTrue(any(b.startswith("ownership-overlap:A") for b in overlap))


class DodContractTest(unittest.TestCase):
    def test_post_merge_dod_names_hygiene_and_disk_guard(self) -> None:
        contract = post_merge_dod_required()
        self.assertEqual("post-merge own cleanup", contract["cleanup"])
        self.assertIn("worktree_hygiene.py", contract["hygiene"])
        self.assertIn("box_disk_guard.py", contract["disk_guard"])



class CleanupAtCloseGateTest(unittest.TestCase):
    def test_blocked_infra_at_80_percent(self) -> None:
        self.assertEqual(80, DISK_USED_PCT_BLOCKED_INFRA)
        reasons = blocked_infra_reasons(".", free=50 * GiB, used_percent=81.0)
        self.assertIn("disk-used-pct-ge-80", reasons)
        ok = blocked_infra_reasons(".", free=50 * GiB, used_percent=50.0)
        self.assertEqual([], ok)
        warn = blocked_infra_reasons(".", free=12 * GiB, used_percent=50.0)
        self.assertIn("disk-free-below-15giB", warn)

    def test_disk_used_percent_math(self) -> None:
        self.assertAlmostEqual(80.0, disk_used_percent(".", used=80, total=100))

    def test_classify_intermediate_vs_final(self) -> None:
        self.assertEqual("intermediate", classify_cleanup_path("cut.mp4"))
        self.assertEqual("intermediate", classify_cleanup_path("scratch.wav"))
        self.assertEqual("intermediate", classify_cleanup_path(".venv-task"))
        self.assertEqual("intermediate", classify_cleanup_path("work"))
        self.assertEqual("final", classify_cleanup_path("final-signed.mp4"))
        self.assertEqual("final", classify_cleanup_path("chapter-release.png"))

    def test_stale_tip_on_origin_own_only(self) -> None:
        now = 1_700_000_000.0
        entries = [
            {"path": "/tmp/wt-own-old", "own": True, "tip_on_origin": True, "mtime": now - 48 * 3600},
            {"path": "/tmp/wt-own-new", "own": True, "tip_on_origin": True, "mtime": now - 1 * 3600},
            {"path": "/tmp/wt-foreign", "own": False, "tip_on_origin": True, "mtime": now - 48 * 3600},
            {"path": "/tmp/wt-unpushed", "own": True, "tip_on_origin": False, "mtime": now - 48 * 3600},
        ]
        with unittest.mock.patch(
            "scripts.ci.box_disk_guard.never_touch_blockers",
            return_value=[],
        ):
            found = stale_tip_on_origin_candidates(entries, now_epoch=now)
        paths = {row["path"] for row in found}
        self.assertEqual({"/tmp/wt-own-old"}, paths)

    def test_shared_venv_root_outside_trees(self) -> None:
        self.assertIn("chaos-engine/venvs", SHARED_VENV_ROOT)

    def test_youtube_replace_requires_live_verify(self) -> None:
        self.assertEqual(
            [],
            youtube_replace_cleanup_allowed(
                replacement_watch_ok=True,
                replacement_schedule_ok=True,
                outdated_video_id="abc123",
            ),
        )
        blockers = youtube_replace_cleanup_allowed(
            replacement_watch_ok=False,
            replacement_schedule_ok=True,
            outdated_video_id="abc123",
        )
        self.assertIn("replacement-watch-not-verified", blockers)

    def test_log_deleted_youtube_ids_append_only(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            log = Path(tmp) / "deleted-yt.log"
            first = log_deleted_youtube_ids(
                ["vidA"], log_path=log, now_iso="2026-10-30T10:00:00Z"
            )
            second = log_deleted_youtube_ids(
                ["vidB"], log_path=log, now_iso="2026-10-30T11:00:00Z"
            )
            self.assertEqual(1, first["logged"])
            self.assertEqual(1, second["logged"])
            body = log.read_text(encoding="utf-8")
            self.assertIn("vidA", body)
            self.assertIn("vidB", body)
            self.assertEqual(2, len(body.splitlines()))

    def test_dod_mentions_finals_and_80pct(self) -> None:
        contract = post_merge_dod_required()
        self.assertIn("finals hold", contract["order"])
        self.assertIn("80", contract["blocked_infra"])


    def test_live_status_refuses_stale_merge_queue(self) -> None:
        blockers = verify_live_item_state_before_status(
            proposed_status="merge queue", live_state="MERGED"
        )
        self.assertTrue(any("stale-status" in b for b in blockers))
        self.assertEqual(
            [],
            verify_live_item_state_before_status(
                proposed_status="merged", live_state="MERGED"
            ),
        )

    def test_item_close_requires_clean_and_accounted(self) -> None:
        self.assertEqual(
            [],
            item_close_blockers(
                git_status_clean=True,
                created_files=["/tmp/a"],
                created_files_accounted=True,
                session_logs_cleared=True,
            ),
        )
        blockers = item_close_blockers(
            git_status_clean=False,
            created_files=["/tmp/a"],
            created_files_accounted=False,
            session_logs_cleared=False,
        )
        self.assertIn("git-status-not-clean", blockers)
        self.assertIn("created-files-unaccounted", blockers)
        self.assertIn("session-logs-remain", blockers)

    def test_doctor_dirt_blocks_close(self) -> None:
        self.assertEqual([], doctor_dirt_close_blockers(porcelain=""))
        blockers = doctor_dirt_close_blockers(
            porcelain=" M chaos-engine/tool.py",
            doctor_touched_paths=["chaos-engine/tool.py"],
        )
        self.assertIn("git-status-dirty", blockers)
        self.assertIn("doctor-repair-dirt-unresolved", blockers)



if __name__ == "__main__":
    unittest.main()
