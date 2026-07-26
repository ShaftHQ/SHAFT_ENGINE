import unittest

from scripts.ci.validate_pr_closing_keywords import find_negated_autocloses

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

    def test_full_issue_url_negation_is_flagged(self):
        errors = find_negated_autocloses(
            "This does not fix https://github.com/ShaftHQ/SHAFT_ENGINE/issues/3930 fully."
        )
        self.assertEqual(len(errors), 1)
        self.assertIn("#3930", errors[0]["message"])


if __name__ == "__main__":
    unittest.main()
