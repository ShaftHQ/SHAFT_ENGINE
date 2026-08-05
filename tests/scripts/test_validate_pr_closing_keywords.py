import json
import os

# subprocess is used only to invoke this repo's own CLI script below with a
# fixed, list-args argv (never shell=True, no untrusted command construction).
import subprocess  # nosec B404
import sys
import tempfile
import unittest

from scripts.ci.validate_pr_closing_keywords import (
    find_credited_symbols_not_in_diff,
    find_negated_autocloses,
    find_negated_autocloses_in_commits,
    parse_commits_json,
)

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
CLI_SCRIPT = os.path.join(REPO_ROOT, "scripts", "ci", "validate_pr_closing_keywords.py")

# Real body of PR #4009 (verified via `gh pr view 4009 --json body`). GitHub's
# closing-keyword scanner matched "fix #3930" inside "Does not fix #3930's..."
# and auto-closed issue #3930 the instant this PR merged, 2 seconds after a
# human had manually reopened it with "Leaving open." (issue #4142).
PR_4009_BODY = """## Summary

Reverts the `-Xmx1024m` addition to root `pom.xml`'s `surefireArgLine` property (and its explanatory comment) from 3022ebb4ae (#3942), back to exactly its pre-3022ebb4ae value. This is a pure revert -- no per-OS profiles, no workflow-level overrides, no replacement number.

## Why

Before 3022ebb4ae, no `-Xmx` was set anywhere (root pom.xml, shaft-engine/pom.xml, or the workflow files), so every GitHub-hosted runner used HotSpot's ergonomic default heap: `MaxRAMPercentage=25%` of physical RAM (verified locally with `java -XX:+PrintFlagsFinal -version`, which reports `MaxRAMPercentage = 25.000000 {product} {default}` on this machine).

Related, but explicitly NOT closed by this PR (no closing keywords used, on purpose):
- #3985 -- has a second live cause (Playwright-scope leak in Grid jobs), tracked separately as #4006. Not fixed here.
- #3987 -- has an entangled Safari finding tracked as #1548. Not fixed here.

Both should be closed manually only after a green nightly run confirms this revert plus their other tracked causes are resolved.

## What this PR does NOT do

- Does not touch `.github/workflows/e2eTests.yml` or `e2eLocalTests.yml`.
- Does not add per-OS Surefire heap profiles or a `-DsurefireArgLine` override anywhere.
- Does not touch `shaft-intellij` or `shaft-mcp`.
- Does not fix #3930's original macOS OOM -- that issue is being reopened separately with an explanation.

## Test plan

CI is the real verification for this change.
"""


class FindNegatedAutoclosesTest(unittest.TestCase):
    def test_pr_4009_real_body_is_flagged(self):
        """RED fixture: this real body wrongly auto-closed #3930 (issue #4142)."""
        errors = find_negated_autocloses(PR_4009_BODY)
        codes = {error["code"] for error in errors}
        self.assertIn("autoclose-negated-reference", codes)
        messages = " ".join(error["message"] for error in errors)
        self.assertIn("#3930", messages)

    def test_genuine_closes_reference_is_accepted(self):
        """The common case: intentional 'Closes #N' must never be flagged."""
        self.assertEqual(find_negated_autocloses("Closes #4127\n\n## Summary"), [])

    def test_bare_issue_reference_is_accepted(self):
        """A bare '#N' with no closing keyword next to it is always safe."""
        self.assertEqual(
            find_negated_autocloses("Related to #4053, #4048, #4065. See #3930 for context."),
            [],
        )

    def test_ordinary_prose_mentioning_issue_is_accepted(self):
        self.assertEqual(
            find_negated_autocloses(
                "This regression was first reported in #3930 and reproduced again this week."
            ),
            [],
        )

    def test_doesnt_close_is_flagged(self):
        errors = find_negated_autocloses("This doesn't close #10 by itself.")
        self.assertEqual(len(errors), 1)
        self.assertIn("#10", errors[0]["message"])

    def test_cannot_resolve_is_flagged(self):
        errors = find_negated_autocloses("We cannot resolve #20 until the upstream release ships.")
        self.assertEqual(len(errors), 1)

    def test_wont_fix_is_flagged(self):
        errors = find_negated_autocloses("This won't fix #30, it only mitigates the symptom.")
        self.assertEqual(len(errors), 1)

    def test_does_not_resolve_yet_is_flagged(self):
        errors = find_negated_autocloses("This does not resolve #40 yet -- follow-up tracked separately.")
        self.assertEqual(len(errors), 1)

    def test_markdown_bold_negation_is_flagged(self):
        """Real phrasing from merged PR #4076: 'Does **not** close #4046'."""
        errors = find_negated_autocloses(
            "Fixes the deadlock behind #4046. Does **not** close #4046: the nightly's "
            "E2E path still fails on a second, separate issue."
        )
        codes_and_refs = [(error["code"], "#4046" in error["message"]) for error in errors]
        self.assertIn(("autoclose-negated-reference", True), codes_and_refs)
        # "Fixes the deadlock behind #4046" has words between the keyword and the
        # reference, so it is not itself a GitHub-recognized closing pair.
        self.assertEqual(len(errors), 1)

    def test_unrelated_earlier_negation_does_not_block_a_real_close(self):
        """A 'not' several words back in the same sentence must not gate a real close."""
        self.assertEqual(
            find_negated_autocloses(
                "This does not directly affect the OOM, but Closes #10 anyway as a followup."
            ),
            [],
        )

    def test_hard_wrapped_body_negation_is_flagged(self):
        """Coordinator-reproduced gap: an editor/CLI that hard-wraps a PR body at ~72-80
        columns can split "Does not" onto its own line from "fix #N", and a bare '\\n'
        clause boundary would silently swallow the negation. Confirmed independently
        against the shipped guard: the real PR #4141 commit text (see
        FindNegatedAutoclosesInCommitsTest) scores 0 matches unwrapped, 1 flattened."""
        errors = find_negated_autocloses("This PR does not\nfix #3930 here.")
        self.assertEqual(len(errors), 1)
        self.assertIn("#3930", errors[0]["message"])

    def test_full_issue_url_negation_is_flagged(self):
        errors = find_negated_autocloses(
            "This does not fix https://github.com/ShaftHQ/SHAFT_ENGINE/issues/3930 fully."
        )
        self.assertEqual(len(errors), 1)
        self.assertIn("#3930", errors[0]["message"])


class FindNegatedAutoclosesInCommitsTest(unittest.TestCase):
    # Exact raw commit text (byte-for-byte, via `gh api
    # repos/ShaftHQ/SHAFT_ENGINE/commits/82b7d875331ecb28a16256907949477f86f8add2`) of the
    # real branch commit behind the #3930 incident (issue #4146): a force-push at
    # 15:47:32Z added this as a second commit to PR #4141's already-auto-merge-armed
    # branch, and the merge one minute later (15:48:09Z) squash-merged it byte-for-byte
    # into commit c0f82a611bbf5a93d81c4f7cdc3c56ac55069d8f -- arming auto-merge does not
    # freeze a PR against later pushes. Git hard-wraps prose at ~72 columns, so this real
    # text splits "Does not" from "fix #3930" across a single newline; the shipped `main`
    # implementation (before this PR) scores 0 matches against it, confirmed empirically.
    PR_4141_COMMIT_BODY = (
        "Record two post-PR gotchas: negation-blind auto-close, PR prose isn't merge-state\n"
        "\n"
        "GitHub's issue-closing keyword scan matched \"fix #3930\" inside \"Does not\n"
        "fix #3930\" in PR 4009's body, auto-closing the issue on merge despite the\n"
        "PR explicitly disclaiming it and despite a human having manually reopened\n"
        "it 7 minutes earlier with \"Leaving open.\" Verified via the issue's own\n"
        "timeline and PR 4009's closingIssuesReferences. Cross-references 4142,\n"
        "which tracks the fix.\n"
        "\n"
        "Also records that PR/issue prose describing work as landed is not a\n"
        "merge-state source of truth -- gh pr view state/mergedAt is.\n"
    )

    def test_real_pr_4141_commit_body_is_flagged(self):
        """RED fixture: real hard-wrapped commit text that squash-merged and closed #3930 (issue #4146)."""
        errors = find_negated_autocloses_in_commits(
            [("82b7d875331ecb28a16256907949477f86f8add2", self.PR_4141_COMMIT_BODY)]
        )
        self.assertEqual(len(errors), 1)
        self.assertIn("#3930", errors[0]["message"])
        self.assertIn("82b7d875331e", errors[0]["message"])

    # Exact raw commit text (via `gh api
    # repos/ShaftHQ/SHAFT_ENGINE/commits/41cf8fb0ea5fd5c971b7123327870b37c87379dd`) found
    # while measuring dewrap false positives against this repo's own merged-PR history:
    # an independent, unrelated occurrence of the same shape, months after #3930 and by
    # a different author, quoting PR #4068's disclaimer while explaining the #3930
    # incident -- proof this is a recurring pattern in this repo, not a one-off. Its own
    # subject line ("Fix #4101: ...") is a legitimate, unrelated close and must stay
    # unflagged; only the quoted "does not\nclose #4046" is hazardous.
    PR_4102_COMMIT_BODY = (
        "Fix #4101: add concrete example that the disclaimer itself is the trap\n"
        "\n"
        "Orchestrator follow-up: the removed branch-naming claim obscured a\n"
        "sharper point about the real mechanism. PR #4068's body said \"does not\n"
        "close #4046\" while trying to explicitly NOT close it, and that phrase\n"
        "is exactly what GitHub matched -- close #N ignores any preceding\n"
        "negation. The existing \"later disclaimer doesn't neutralize an earlier\n"
        "match\" line describes keyword-then-disclaimer ordering; it doesn't\n"
        "cover the case where the disclaimer sentence IS the match. Add one\n"
        "concrete example so a reader writing \"does not close #N\" to be safe\n"
        "doesn't get bitten the same way twice.\n"
    )

    def test_real_pr_4102_commit_body_flags_only_the_quoted_hazard(self):
        """A second, independent real occurrence of the same shape (found via the FP sweep, not #3930)."""
        errors = find_negated_autocloses_in_commits(
            [("41cf8fb0ea5fd5c971b7123327870b37c87379dd", self.PR_4102_COMMIT_BODY)]
        )
        self.assertEqual(len(errors), 1)
        self.assertIn("#4046", errors[0]["message"])
        self.assertNotIn("#4101", errors[0]["message"])

    def test_multi_commit_branch_only_flags_the_offending_commit(self):
        """A multi-commit branch must identify which commit is hazardous, not just that one is."""
        commits = [
            ("aaaaaaaaaaaa", "Add a helper method\n\nNo issues referenced here."),
            ("bbbbbbbbbbbb", "This doesn't close #10 by itself.\n"),
            ("cccccccccccc", "Refactor tests for clarity\n"),
        ]
        errors = find_negated_autocloses_in_commits(commits)
        self.assertEqual(len(errors), 1)
        self.assertIn("bbbbbbbbbbbb", errors[0]["path"])
        self.assertIn("#10", errors[0]["message"])

    def test_commit_with_genuine_close_is_accepted(self):
        self.assertEqual(
            find_negated_autocloses_in_commits([("dddddddddddd", "Closes #20\n")]),
            [],
        )

    def test_no_commits_is_accepted(self):
        self.assertEqual(find_negated_autocloses_in_commits([]), [])


class ParseCommitsJsonTest(unittest.TestCase):
    def test_empty_string_yields_no_commits(self):
        self.assertEqual(parse_commits_json(""), [])

    def test_parses_sha_and_message_pairs(self):
        raw = json.dumps(
            [{"sha": "abc123", "message": "Fix bug\n"}, {"sha": "def456", "message": "Add test\n"}]
        )
        self.assertEqual(
            parse_commits_json(raw),
            [("abc123", "Fix bug\n"), ("def456", "Add test\n")],
        )


class GhApiCommitsShapeRoundTripTest(unittest.TestCase):
    """Issue #4237: pins the mapping `.github/workflows/pr-gate.yml`'s
    `pr-body-autoclose-guard` job performs on the raw `gh api
    repos/{owner}/{repo}/pulls/{number}/commits` response (each element nests the message
    under `commit.message`) before handing it to this script. The CI step's jq filter
    (`.[] | {sha: .sha, message: .commit.message}`, slurped across pages with a separate
    `jq -s -c '.'`) is mirrored here in Python so a mismatch between that shape and what
    `parse_commits_json` expects fails a fast local test instead of only in CI."""

    @staticmethod
    def _pr_gate_jq_equivalent(raw_commits_api_response):
        """Mirror `.[] | {sha: .sha, message: .commit.message}` piped through `jq -s -c '.'`."""
        return json.dumps(
            [
                {"sha": entry["sha"], "message": entry["commit"]["message"]}
                for entry in raw_commits_api_response
            ]
        )

    def test_real_gh_api_shape_round_trips_and_flags_the_hazardous_commit(self):
        raw_commits_api_response = [
            {"sha": "aaaaaaaaaaaa", "commit": {"message": "Add a helper method\n"}},
            {"sha": "bbbbbbbbbbbb", "commit": {"message": "This doesn't close #10 by itself.\n"}},
            {"sha": "cccccccccccc", "commit": {"message": "Closes #20\n"}},
        ]
        commits_json = self._pr_gate_jq_equivalent(raw_commits_api_response)
        commits = parse_commits_json(commits_json)
        errors = find_negated_autocloses_in_commits(commits)
        self.assertEqual(len(errors), 1)
        self.assertIn("bbbbbbbbbbbb", errors[0]["path"])
        self.assertIn("#10", errors[0]["message"])


class CreditedSymbolsNotInDiffTest(unittest.TestCase):
    """#4567 section 4.3: a commit must not credit a symbol its own diff never touches.

    Two false credits in `254a830710` consumed a whole round-two review finding and
    forced PR #4554's body to carry a `## Corrections` section retracting them.
    """

    # Verbatim excerpt of `254a830710` (`git show -s --format=%B`). Every bullet
    # names the symbol it changed; `raw_decode` and `HOOK_BUDGET_SECONDS` were
    # both landed by EARLIER commits on the same branch, so this commit's own
    # diff contains neither.
    COMMIT_254A_MESSAGE = """Apply the adversarial review's confirmed findings

Fourteen findings from the independent review round, each with the check
that would have caught it:

- `_ledger_path` refuses a relative base and falls back to the system
  temp directory, so a hook launched from an unexpected cwd cannot
  scatter ledgers.
- `ledger_events` decodes every value on a line with `raw_decode` and
  flattens legacy arrays.
- `HOOK_BUDGET_SECONDS` is a real shared budget rather than a comment
  claiming one.
"""

    # Verbatim excerpt of `a95c0c7172`. `EXEMPTION_MARKERS` is discussed in a
    # narrative paragraph -- "this check caught me" -- not credited in the change
    # list. Scanning the whole message flags it; scanning only the change list
    # does not, and the change list is where a commit enumerates what it did.
    COMMIT_A95C_MESSAGE = """Give iron law 6 a countable trigger (#4545 defect 1)

The qualification check then caught the author. The first wording explained the
floor with "a step is a judgement call", and `EXEMPTION_MARKERS` reads
"judgement" as a hedge -- correctly, since a law whose own text calls part of
itself a judgement call reads as softer than it is.
"""

    def test_credits_absent_from_the_commits_own_diff_are_reported(self):
        """RED fixture: the exact pair that cost a round-two finding."""
        diffs = {"254a830710": "--- a/scripts/agents/guard.py\n+    def _ledger_path(self):\n"}
        findings = find_credited_symbols_not_in_diff(
            [("254a830710", self.COMMIT_254A_MESSAGE)], diffs.get
        )
        credited = " ".join(finding["message"] for finding in findings)
        self.assertIn("raw_decode", credited)
        self.assertIn("HOOK_BUDGET_SECONDS", credited)
        self.assertEqual({finding["code"] for finding in findings}, {"credit-not-in-diff"})

    def test_a_symbol_the_diff_does_touch_is_not_reported(self):
        """Every symbol this excerpt credits appears in the diff: all four are true credits."""
        diff = (
            "+def _ledger_path(base):\n"
            "+def ledger_events(path):\n"
            "+    return raw_decode(base)\n"
            "+HOOK_BUDGET_SECONDS = 5\n"
        )
        findings = find_credited_symbols_not_in_diff(
            [("254a830710", self.COMMIT_254A_MESSAGE)], lambda sha: diff
        )
        self.assertEqual(findings, [])

    def test_a_narrative_mention_outside_the_change_list_is_not_a_credit(self):
        """The measured false positive: `EXEMPTION_MARKERS` in running prose, not a bullet."""
        findings = find_credited_symbols_not_in_diff(
            [("a95c0c7172", self.COMMIT_A95C_MESSAGE)], lambda sha: ""
        )
        self.assertEqual(findings, [])

    def test_prose_words_in_backticks_are_never_treated_as_symbols(self):
        """A token test, not a meaning test: `review` and `main` are English, not identifiers."""
        message = "Fix the gate\n\n- `review` now runs on `main` before the `gh` call.\n"
        findings = find_credited_symbols_not_in_diff([("abc123", message)], lambda sha: "")
        self.assertEqual(findings, [])

    def test_an_unreadable_diff_is_reported_rather_than_passing_silently(self):
        """A shallow CI checkout cannot resolve a PR commit; that must be loud, not vacuous."""
        findings = find_credited_symbols_not_in_diff(
            [("254a830710", self.COMMIT_254A_MESSAGE)], lambda sha: None
        )
        self.assertEqual([finding["code"] for finding in findings], ["credit-scan-unavailable"])


class MainCLIIntegrationTest(unittest.TestCase):
    """Exercises the actual CLI entry point CI invokes: PR_BODY + PR_COMMITS_JSON env vars."""

    def _run_cli(self, *, body="", commits_json="", cwd=REPO_ROOT):
        env = {**os.environ, "PR_BODY": body, "PR_COMMITS_JSON": commits_json}
        return subprocess.run(  # nosec B603
            [sys.executable, CLI_SCRIPT],
            capture_output=True,
            text=True,
            env=env,
            cwd=cwd,
        )

    def test_cli_fails_and_identifies_commit_for_commit_only_hazard(self):
        commits = json.dumps([{"sha": "1234567890ab", "message": "This doesn't close #77 yet."}])
        result = self._run_cli(body="Closes #4127", commits_json=commits)
        self.assertEqual(result.returncode, 1)
        self.assertIn("#77", result.stderr)
        self.assertIn("1234567890ab", result.stderr)

    def test_cli_passes_when_body_and_commits_are_clean(self):
        commits = json.dumps([{"sha": "1234567890ab", "message": "Add a helper.\n"}])
        result = self._run_cli(body="Closes #4127", commits_json=commits)
        self.assertEqual(result.returncode, 0)

    def test_cli_reports_a_false_credit_without_failing_the_gate(self):
        """#4567 item 5 is advisory: a commit message cannot be reworded after push.  # noqa: D213

        The fixture has a real commit whose diff omits `raw_decode`. The scan must
        say so and still exit 0 -- failing here would demand an amend the branch
        protection forbids.
        """
        message = (
            "Apply the adversarial review's confirmed findings\n\n"
            "- `ledger_events` decodes every value on a line with `raw_decode`.\n"
        )
        with tempfile.TemporaryDirectory() as temporary:
            for command in (
                ["git", "init"],
                ["git", "config", "user.email", "test@example.com"],
                ["git", "config", "user.name", "Test"],
            ):
                subprocess.run(command, cwd=temporary, check=True, capture_output=True)  # nosec B603 B607
            with open(os.path.join(temporary, "example.py"), "w", encoding="utf-8") as source:
                source.write("value = 1\n")
            subprocess.run(["git", "add", "example.py"], cwd=temporary, check=True)  # nosec B603 B607
            subprocess.run(["git", "commit", "-m", "fixture"], cwd=temporary, check=True)  # nosec B603 B607
            sha = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=temporary, text=True).strip()  # nosec B603 B607
            commits = json.dumps([{"sha": sha, "message": message}])
            result = self._run_cli(body="Closes #4127", commits_json=commits, cwd=temporary)
        self.assertEqual(result.returncode, 0)
        self.assertIn("raw_decode", result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main()
