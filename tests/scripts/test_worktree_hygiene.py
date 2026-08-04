"""Abandoned, superseded, and corrupt worktrees must be reportable conditions."""
# Issue #4437: seven stale worktrees accumulated in one checkout. Three held
# uncommitted changes, and nothing distinguished them from live work -- two
# carried documentation that had already landed on `origin/main` through
# another path, and one held 652 entirely NUL-filled files. A routine cleanup
# would have destroyed all of it, and re-doing the "recoverable" work would
# have regressed `main`.
#
# Every fixture below is a real repository with real `git worktree add` links.

from __future__ import annotations

import os
import subprocess  # nosec B404 - tests drive the local git binary on fixtures.
import tempfile
import time
import unittest
from pathlib import Path
from unittest.mock import patch

from scripts.ci.worktree_hygiene import (
    collect_worktree_report,
    format_advisories,
    open_pull_requests_via_gh,
)


def git(cwd: Path, *arguments: str) -> subprocess.CompletedProcess:
    return subprocess.run(  # nosec B603 B607 - fixed git commands on a temp fixture.
        ["git", "-c", "core.longpaths=true", *arguments],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
    )


class WorktreeHygieneTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.base = Path(self.temporary_directory.name)
        self.main = self.base / "checkout"
        self.main.mkdir()
        git(self.main, "init", "-q", "-b", "main", ".")
        git(self.main, "config", "user.email", "harness@example.invalid")
        git(self.main, "config", "user.name", "Harness")
        self.write(self.main, "README.md", "# Project\n")
        git(self.main, "add", "-A")
        git(self.main, "commit", "-qm", "initial")
        self.publish_main()

    def tearDown(self):
        self.temporary_directory.cleanup()

    def publish_main(self) -> None:
        """Point refs/remotes/origin/main at the current main, without a remote."""
        head = git(self.main, "rev-parse", "main").stdout.strip()
        git(self.main, "update-ref", "refs/remotes/origin/main", head)

    def write(self, root: Path, relative_path: str, content: str) -> Path:
        path = root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")
        return path

    def add_worktree(self, name: str, branch: str) -> Path:
        path = self.base / name
        git(self.main, "worktree", "add", "-q", "-b", branch, str(path), "main")
        git(path, "config", "user.email", "harness@example.invalid")
        git(path, "config", "user.name", "Harness")
        return path

    def push_and_remove_worktree(
        self, name: str, branch: str, *, commit_epoch: int | None = None
    ) -> str:
        """Simulate a session that pushed `branch` to origin, then cleaned up

        locally -- exactly the shape issue #4450 describes: a real commit only
        `refs/remotes/origin/<branch>` still references, no worktree, no local
        branch, and (by default) no pull request.
        """
        worktree = self.add_worktree(name, branch)
        self.write(worktree, "notes.md", "session work\n")
        git(worktree, "add", "-A")
        env = None
        if commit_epoch is not None:
            stamp = f"{commit_epoch} +0000"
            env = {**os.environ, "GIT_AUTHOR_DATE": stamp, "GIT_COMMITTER_DATE": stamp}
        subprocess.run(  # nosec B603 B607 - fixed git command on a temp fixture.
            ["git", "-c", "core.longpaths=true", "commit", "-qm", "session work"],
            cwd=worktree,
            capture_output=True,
            text=True,
            env=env,
            check=False,
        )
        head = git(worktree, "rev-parse", branch).stdout.strip()
        git(self.main, "update-ref", f"refs/remotes/origin/{branch}", head)
        git(self.main, "worktree", "remove", "--force", str(worktree))
        git(self.main, "branch", "-D", branch)
        return head

    def entry(self, report: list[dict], name: str) -> dict:
        matches = [item for item in report if Path(item["path"]).name == name]
        self.assertEqual(len(matches), 1, f"{name} not found in {report}")
        return matches[0]

    # --- nothing to report --------------------------------------------------

    def test_lone_clean_checkout_reports_nothing(self):
        report = collect_worktree_report(self.main)
        self.assertEqual(len(report), 1)
        self.assertTrue(report[0]["is_main"])
        self.assertTrue(report[0]["is_current"])
        self.assertEqual(report[0]["state"], "clean")
        self.assertEqual(format_advisories(report), [])

    def test_directory_outside_any_repository_reports_nothing(self):
        with tempfile.TemporaryDirectory() as outside:
            self.assertEqual(collect_worktree_report(Path(outside)), [])

    # --- abandoned: uncommitted work nobody is looking at --------------------

    def test_other_worktree_holding_uncommitted_work_is_abandoned(self):
        worktree = self.add_worktree("stale", "ChaosEngine/stale")
        self.write(worktree, "docs/guide.md", "Thirteen files of real work.\n")
        self.write(worktree, "README.md", "# Project\n\nEdited.\n")

        entry = self.entry(
            collect_worktree_report(self.main, open_pull_requests=lambda _branch: 0),
            "stale",
        )
        self.assertEqual(entry["state"], "abandoned")
        self.assertEqual(entry["uncommitted_files"], 2)
        self.assertEqual(entry["unique_commits"], 0)
        self.assertEqual(entry["branch"], "ChaosEngine/stale")
        self.assertFalse(entry["is_current"])

    def test_abandoned_advisory_names_the_path_and_the_pending_work(self):
        worktree = self.add_worktree("stale", "ChaosEngine/stale")
        self.write(worktree, "docs/guide.md", "work\n")

        advisories = format_advisories(
            collect_worktree_report(self.main, open_pull_requests=lambda _branch: 0)
        )
        self.assertEqual(len(advisories), 1)
        advisory = advisories[0]
        self.assertIn("abandoned", advisory)
        self.assertIn("stale", advisory)
        self.assertIn("1 uncommitted", advisory)

    def test_worktree_with_its_own_commits_is_not_abandoned(self):
        worktree = self.add_worktree("live", "ChaosEngine/live")
        self.write(worktree, "feature.txt", "new\n")
        git(worktree, "add", "-A")
        git(worktree, "commit", "-qm", "feature")
        self.write(worktree, "feature.txt", "new and edited\n")

        entry = self.entry(collect_worktree_report(self.main), "live")
        self.assertEqual(entry["unique_commits"], 1)
        self.assertEqual(entry["ahead"], 1)
        self.assertEqual(entry["state"], "uncommitted")

    # --- superseded: already upstream through another path -------------------

    def test_a_freshly_created_clean_worktree_is_not_superseded(self):
        # A worktree another session created seconds ago from origin/main is
        # clean and carries no unique patch -- identical on paper to one whose
        # work has landed. Calling it superseded tells the reader to delete a
        # live session's checkout, which the mandatory entrypoint forbids.
        self.add_worktree("fresh", "ChaosEngine/fresh")

        entry = self.entry(collect_worktree_report(self.main), "fresh")
        self.assertEqual(entry["ahead"], 0, "it has never committed anything")
        self.assertEqual(entry["state"], "clean")
        self.assertEqual(format_advisories(collect_worktree_report(self.main)), [])

    def test_a_worktree_holding_the_upstream_branch_is_never_superseded(self):
        # Advising "delete the branch" for a worktree parked on main would
        # propose deleting main itself.
        git(self.main, "checkout", "-q", "-b", "ChaosEngine/elsewhere")
        self.write(self.main, "feature.txt", "work\n")
        git(self.main, "add", "-A")
        git(self.main, "commit", "-qm", "feature")

        path = self.base / "mainline"
        git(self.main, "worktree", "add", "-q", str(path), "main")

        entry = self.entry(collect_worktree_report(self.main), "mainline")
        self.assertEqual(entry["branch"], "main")
        self.assertNotEqual(entry["state"], "superseded")

    def test_a_locked_worktree_is_left_alone(self):
        # `git worktree list --porcelain` marks a worktree a live session holds.
        worktree = self.add_worktree("held", "ChaosEngine/held")
        self.write(worktree, "docs/guide.md", "in progress\n")
        git(self.main, "worktree", "lock", str(worktree), "--reason", "agent session")

        entry = self.entry(collect_worktree_report(self.main), "held")
        self.assertTrue(entry["locked"])
        self.assertEqual(entry["state"], "uncommitted")
        advisory = next(item for item in format_advisories(collect_worktree_report(self.main))
                        if "held" in item)
        self.assertNotIn("abandoned", advisory)

    def test_a_prunable_worktree_entry_is_skipped(self):
        import shutil

        worktree = self.add_worktree("gone", "ChaosEngine/gone")
        shutil.rmtree(worktree)

        report = collect_worktree_report(self.main)
        self.assertEqual([Path(item["path"]).name for item in report], ["checkout"])

    def test_patch_identical_commits_count_as_already_upstream(self):
        # The 2026-08-04 case: the content had already landed on origin/main
        # through a different commit. Ahead/behind alone calls this live work;
        # only patch identity (`git cherry`) sees that it is already upstream.
        worktree = self.add_worktree("duplicate", "ChaosEngine/duplicate")
        self.write(worktree, "catalog.md", "The properties catalog.\n")
        git(worktree, "add", "-A")
        git(worktree, "commit", "-qm", "add catalog")
        carried = git(worktree, "rev-parse", "HEAD").stdout.strip()

        # Main moved on independently, so replaying the patch there produces a
        # different commit hash carrying identical content -- the shape that
        # made the abandoned catalog draft look like unlanded work.
        self.write(self.main, "CHANGELOG.md", "Unrelated progress.\n")
        git(self.main, "add", "-A")
        git(self.main, "commit", "-qm", "unrelated")
        picked = git(self.main, "cherry-pick", carried)
        self.assertEqual(picked.returncode, 0, picked.stderr)
        self.assertNotEqual(
            git(self.main, "rev-parse", "main").stdout.strip(),
            carried,
            "the replayed commit must be a different commit, not a fast-forward",
        )
        self.publish_main()

        entry = self.entry(collect_worktree_report(self.main), "duplicate")
        self.assertEqual(entry["ahead"], 1, "the branch really does carry a commit")
        self.assertEqual(entry["unique_commits"], 0, "but its patch is already upstream")
        self.assertEqual(entry["state"], "superseded")

        advisory = next(item for item in format_advisories(collect_worktree_report(self.main))
                        if "duplicate" in item)
        self.assertIn("superseded", advisory)
        self.assertIn("already", advisory)

    # --- corrupt: NUL-filled content masquerading as ordinary edits ----------

    def test_nul_filled_files_make_a_worktree_corrupt(self):
        worktree = self.add_worktree("zeroed", "ChaosEngine/zeroed")
        (worktree / "README.md").write_bytes(b"\x00" * 200)

        entry = self.entry(collect_worktree_report(self.main), "zeroed")
        self.assertEqual(entry["corrupt_files"], 1)
        self.assertEqual(entry["state"], "corrupt")

        advisory = next(item for item in format_advisories(collect_worktree_report(self.main))
                        if "zeroed" in item)
        self.assertIn("corrupt", advisory)
        self.assertIn("NUL", advisory)
        self.assertIn("git restore", advisory)

    def test_corruption_outranks_abandonment(self):
        worktree = self.add_worktree("zeroed", "ChaosEngine/zeroed")
        (worktree / "README.md").write_bytes(b"\x00" * 200)
        self.write(worktree, "notes.md", "real work\n")

        report = collect_worktree_report(self.main)
        entry = self.entry(report, "zeroed")
        self.assertEqual(entry["state"], "corrupt")
        advisory = next(item for item in format_advisories(report) if "zeroed" in item)
        self.assertIn("-- <confirmed-corrupt-path>", advisory)
        self.assertNotIn("-- .", advisory)

    def test_corrupt_path_advice_is_shell_neutral(self):
        worktree = self.add_worktree("quoted", "ChaosEngine/quoted")
        self.write(worktree, "O'Brien.md", "healthy\n")
        git(worktree, "add", "-A")
        git(worktree, "commit", "-qm", "apostrophe path")
        (worktree / "O'Brien.md").write_bytes(b"\x00" * 200)

        advisory = next(
            item
            for item in format_advisories(collect_worktree_report(self.main))
            if "quoted" in item
        )

        self.assertIn("O'Brien.md", advisory)
        self.assertIn("<confirmed-corrupt-path>", advisory)
        self.assertNotIn('"\'"\'', advisory)

    def test_truncated_corruption_scan_is_disclosed(self):
        worktree = self.add_worktree("large", "ChaosEngine/large")
        self.write(worktree, "notes.md", "real work\n")

        with patch(
            "scripts.ci.worktree_hygiene.scan_for_nul_corruption",
            return_value=([], 2001, True),
        ):
            report = collect_worktree_report(self.main)

        entry = self.entry(report, "large")
        self.assertTrue(entry["scan_truncated"])
        advisory = next(item for item in format_advisories(report) if "large" in item)
        self.assertIn("first 2000", advisory)

    # --- ending a turn with uncommitted changes -----------------------------

    def test_current_worktree_with_uncommitted_changes_is_reported(self):
        self.write(self.main, "README.md", "# Project\n\nUnsaved.\n")

        report = collect_worktree_report(self.main)
        entry = self.entry(report, "checkout")
        self.assertTrue(entry["is_current"])
        self.assertEqual(entry["state"], "uncommitted")

        advisories = format_advisories(report)
        self.assertEqual(len(advisories), 1)
        self.assertIn("uncommitted", advisories[0])
        self.assertIn("commit", advisories[0])

    def test_report_is_taken_from_the_worktree_it_runs_in(self):
        worktree = self.add_worktree("other", "ChaosEngine/other")
        report = collect_worktree_report(worktree)
        self.assertTrue(self.entry(report, "other")["is_current"])
        self.assertFalse(self.entry(report, "checkout")["is_current"])

    def test_running_from_a_subdirectory_still_identifies_the_current_worktree(self):
        # Comparing the given directory against worktree roots would call the
        # live worktree "not current" and then, if it were dirty, abandoned.
        worktree = self.add_worktree("other", "ChaosEngine/other")
        self.write(worktree, "docs/guide.md", "work in progress\n")

        report = collect_worktree_report(worktree / "docs")
        entry = self.entry(report, "other")
        self.assertTrue(entry["is_current"])
        self.assertEqual(entry["state"], "uncommitted")

    # --- open pull requests refine, but never gate, the classification ------

    def test_open_pull_request_keeps_a_dirty_worktree_out_of_abandoned(self):
        worktree = self.add_worktree("reviewed", "ChaosEngine/reviewed")
        self.write(worktree, "docs/guide.md", "work\n")

        def pull_requests(branch: str) -> int:
            return 1 if branch == "ChaosEngine/reviewed" else 0

        entry = self.entry(
            collect_worktree_report(self.main, open_pull_requests=pull_requests),
            "reviewed",
        )
        self.assertEqual(entry["open_pull_requests"], 1)
        self.assertEqual(entry["state"], "uncommitted")

    def test_pull_request_lookup_is_optional_and_unknown_by_default(self):
        worktree = self.add_worktree("stale", "ChaosEngine/stale")
        self.write(worktree, "docs/guide.md", "work\n")

        entry = self.entry(collect_worktree_report(self.main), "stale")
        self.assertIsNone(entry["open_pull_requests"])
        self.assertEqual(entry["state"], "uncommitted")

    def test_a_failing_pull_request_lookup_does_not_break_the_report(self):
        worktree = self.add_worktree("stale", "ChaosEngine/stale")
        self.write(worktree, "docs/guide.md", "work\n")

        def pull_requests(branch: str) -> int:
            raise RuntimeError("gh is unavailable")

        entry = self.entry(
            collect_worktree_report(self.main, open_pull_requests=pull_requests),
            "stale",
        )
        self.assertIsNone(entry["open_pull_requests"])
        self.assertEqual(entry["state"], "uncommitted")

    def test_advisory_says_when_no_pull_request_lookup_happened(self):
        worktree = self.add_worktree("stale", "ChaosEngine/stale")
        self.write(worktree, "docs/guide.md", "work\n")

        advisory = next(item for item in format_advisories(collect_worktree_report(self.main))
                        if "stale" in item)
        self.assertIn("not checked", advisory)

    # --- detached HEAD holding work no branch references --------------------

    def test_detached_head_carrying_unreferenced_commits_is_abandoned(self):
        worktree = self.add_worktree("orphan", "ChaosEngine/orphan")
        self.write(worktree, "rescue.md", "the only copy of this work\n")
        git(worktree, "add", "-A")
        git(worktree, "commit", "-qm", "work")
        carried = git(worktree, "rev-parse", "HEAD").stdout.strip()
        git(worktree, "checkout", "-q", "--detach", carried)
        git(self.main, "branch", "-D", "ChaosEngine/orphan")

        entry = self.entry(collect_worktree_report(self.main), "orphan")
        self.assertIsNone(entry["branch"])
        self.assertEqual(entry["unique_commits"], 1)
        self.assertEqual(entry["state"], "abandoned")
        advisory = next(item for item in format_advisories(collect_worktree_report(self.main))
                        if "orphan" in item)
        self.assertIn("detached HEAD", advisory)

    def test_detached_head_already_upstream_is_not_abandoned(self):
        worktree = self.add_worktree("detached", "ChaosEngine/detached")
        head = git(worktree, "rev-parse", "HEAD").stdout.strip()
        git(worktree, "checkout", "-q", "--detach", head)

        entry = self.entry(collect_worktree_report(self.main), "detached")
        self.assertEqual(entry["unique_commits"], 0)
        self.assertNotEqual(entry["state"], "abandoned")

    # --- missing upstream must not invent a verdict -------------------------

    def test_without_an_upstream_ref_nothing_is_called_superseded(self):
        git(self.main, "update-ref", "-d", "refs/remotes/origin/main")
        self.add_worktree("landed", "ChaosEngine/landed")

        entry = self.entry(collect_worktree_report(self.main), "landed")
        self.assertIsNone(entry["unique_commits"])
        self.assertNotEqual(entry["state"], "superseded")

    # --- orphaned: pushed to origin, no worktree, no PR, no activity --------
    # Issue #4450: `ChaosEngine/user-evaluation-20260802` was pushed, its
    # worktree cleaned up, and no PR was ever opened. Its one commit -- a
    # durable memory object -- was invisible to this report (local worktrees
    # only) and to `--check-pull-requests` (open pull requests only).

    def test_orphaned_remote_branch_with_no_worktree_is_reported(self):
        old_epoch = int(time.time()) - (10 * 86400)
        self.push_and_remove_worktree(
            "gone", "ChaosEngine/gone-4450", commit_epoch=old_epoch
        )

        report = collect_worktree_report(self.main, open_pull_requests=lambda _b: 0)
        advisories = format_advisories(report)

        self.assertTrue(
            any("ChaosEngine/gone-4450" in advisory for advisory in advisories),
            f"expected an advisory naming the orphaned branch, got: {advisories}",
        )
        entry = next(item for item in report if item["path"] == "origin/ChaosEngine/gone-4450")
        self.assertEqual(entry["state"], "orphaned")
        self.assertTrue(entry["is_remote_only"])

    def test_a_branch_pushed_minutes_ago_is_not_yet_orphaned(self):
        # A session mid-task, or one about to open its pull request in the
        # same turn, must not be flagged before it has had a chance to.
        recent_epoch = int(time.time()) - 3600
        self.push_and_remove_worktree(
            "brand-new", "ChaosEngine/brand-new", commit_epoch=recent_epoch
        )

        report = collect_worktree_report(self.main, open_pull_requests=lambda _b: 0)
        self.assertEqual(
            [item for item in report if item.get("is_remote_only")],
            [],
        )
        self.assertEqual(format_advisories(report), [])

    def test_orphaned_branch_with_an_open_pull_request_is_not_flagged(self):
        # It is already visible through the normal review flow -- the actual
        # gap this guard closes is a branch with no worktree AND no PR.
        old_epoch = int(time.time()) - (10 * 86400)
        self.push_and_remove_worktree(
            "reviewed", "ChaosEngine/reviewed-elsewhere", commit_epoch=old_epoch
        )

        report = collect_worktree_report(
            self.main, open_pull_requests=lambda branch: 1
        )
        self.assertEqual(
            [item for item in report if item.get("is_remote_only")],
            [],
        )
        self.assertEqual(format_advisories(report), [])

    def test_orphaned_branch_advisory_caveats_an_unchecked_pull_request(self):
        # `--check-pull-requests` is opt-in and hits the network; the default
        # local-only run must still surface the branch, honestly qualified.
        old_epoch = int(time.time()) - (10 * 86400)
        self.push_and_remove_worktree(
            "unchecked", "ChaosEngine/unchecked-pr", commit_epoch=old_epoch
        )

        report = collect_worktree_report(self.main)  # no open_pull_requests callback
        entry = next(
            item for item in report if item["path"] == "origin/ChaosEngine/unchecked-pr"
        )
        self.assertEqual(entry["state"], "orphaned")
        self.assertIsNone(entry["open_pull_requests"])

        advisory = next(
            item for item in format_advisories(report) if "unchecked-pr" in item
        )
        self.assertIn("not checked", advisory)
        self.assertIn("--check-pull-requests", advisory)

    def test_a_failing_pull_request_lookup_still_reports_the_orphaned_branch(self):
        # gh missing or unauthenticated must not hide the branch, and must not
        # be mistaken for a confirmed "no open pull request" either.
        old_epoch = int(time.time()) - (10 * 86400)
        self.push_and_remove_worktree(
            "flaky-gh", "ChaosEngine/flaky-gh-lookup", commit_epoch=old_epoch
        )

        def pull_requests(branch: str) -> int:
            raise RuntimeError("gh is unavailable")

        report = collect_worktree_report(self.main, open_pull_requests=pull_requests)
        entry = next(
            item for item in report if item["path"] == "origin/ChaosEngine/flaky-gh-lookup"
        )
        self.assertEqual(entry["state"], "orphaned")
        self.assertIsNone(entry["open_pull_requests"])

        advisory = next(
            item for item in format_advisories(report) if "flaky-gh-lookup" in item
        )
        self.assertIn("not checked", advisory)

    def test_orphaned_branch_advisory_does_not_claim_ahead_count_as_proof(self):
        # The issue's own correctness point, in bold: this repo squash-merges,
        # so a landed branch still shows commits ahead of main. The advisory
        # must send the reader to a content diff, never assert "unmerged".
        old_epoch = int(time.time()) - (10 * 86400)
        self.push_and_remove_worktree(
            "maybe-landed", "ChaosEngine/maybe-landed", commit_epoch=old_epoch
        )

        advisory = next(
            item
            for item in format_advisories(
                collect_worktree_report(self.main, open_pull_requests=lambda _b: 0)
            )
            if "maybe-landed" in item
        )
        self.assertIn("squash-merges", advisory)
        self.assertIn("not evidence", advisory)
        self.assertIn("Diff its tip against main", advisory)

    def test_a_branch_that_still_has_a_local_worktree_is_not_reported_as_orphaned(self):
        # The remote-only scan must not double-report a branch the ordinary
        # worktree walk already covers.
        old_epoch = int(time.time()) - (10 * 86400)
        worktree = self.add_worktree("still-here", "ChaosEngine/still-here")
        self.write(worktree, "notes.md", "session work\n")
        git(worktree, "add", "-A")
        env = {**os.environ, "GIT_AUTHOR_DATE": f"{old_epoch} +0000",
               "GIT_COMMITTER_DATE": f"{old_epoch} +0000"}
        subprocess.run(  # nosec B603 B607 - fixed git command on a temp fixture.
            ["git", "-c", "core.longpaths=true", "commit", "-qm", "session work"],
            cwd=worktree, capture_output=True, text=True, env=env, check=False,
        )
        head = git(worktree, "rev-parse", "ChaosEngine/still-here").stdout.strip()
        git(self.main, "update-ref", "refs/remotes/origin/ChaosEngine/still-here", head)

        report = collect_worktree_report(self.main, open_pull_requests=lambda _b: 0)
        self.assertEqual(
            [item for item in report if item.get("is_remote_only")],
            [],
        )

    def test_origin_main_is_never_reported_as_orphaned(self):
        old_epoch = int(time.time()) - (10 * 86400)
        env = {**os.environ, "GIT_AUTHOR_DATE": f"{old_epoch} +0000",
               "GIT_COMMITTER_DATE": f"{old_epoch} +0000"}
        subprocess.run(  # nosec B603 B607 - fixed git command on a temp fixture.
            ["git", "-c", "core.longpaths=true", "commit", "--allow-empty", "-qm", "old"],
            cwd=self.main, capture_output=True, text=True, env=env, check=False,
        )
        self.publish_main()

        report = collect_worktree_report(self.main, open_pull_requests=lambda _b: 0)
        self.assertEqual(
            [item for item in report if item.get("is_remote_only")],
            [],
        )


class OpenPullRequestLookupTest(unittest.TestCase):
    """The one function that leaves the machine."""

    def test_missing_github_cli_raises_rather_than_reporting_zero(self):
        # Reporting 0 would be indistinguishable from "no open pull request",
        # which is one of the inputs that decides the abandoned verdict.
        with patch("scripts.ci.worktree_hygiene.shutil.which", return_value=None):
            with self.assertRaises(RuntimeError):
                open_pull_requests_via_gh("ChaosEngine/example")

    def test_a_failed_lookup_raises_rather_than_reporting_zero(self):
        completed = subprocess.CompletedProcess([], 1, "", "not authenticated")
        with patch("scripts.ci.worktree_hygiene.shutil.which", return_value="gh"), patch(
            "scripts.ci.worktree_hygiene.subprocess.run", return_value=completed
        ):
            with self.assertRaises(RuntimeError):
                open_pull_requests_via_gh("ChaosEngine/example")

    def test_open_pull_requests_are_counted(self):
        completed = subprocess.CompletedProcess([], 0, '[{"number": 1}, {"number": 2}]', "")
        with patch("scripts.ci.worktree_hygiene.shutil.which", return_value="gh"), patch(
            "scripts.ci.worktree_hygiene.subprocess.run", return_value=completed
        ):
            self.assertEqual(open_pull_requests_via_gh("ChaosEngine/example"), 2)


if __name__ == "__main__":
    unittest.main()
