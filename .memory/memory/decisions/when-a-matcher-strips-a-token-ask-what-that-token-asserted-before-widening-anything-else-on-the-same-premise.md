One commit on #4554 (`64d46cfdac`) made two changes to R21's recorder `_updates_a_tracked_issue` in `scripts/agents/guard.py`, and they cancelled each other.

Half one added `_skip_gh_global_flags` so `gh -R owner/repo issue comment ...` is recognised again -- a real false positive, #4548.
Half two dropped `gh issue create` from the commands that count, on the stated premise that "`pr create` is bound to the current branch the way an issue is not".

`-R other/repo` is exactly how that branch binding is removed. Stripping the flag and then relying on the property the flag negates left `gh -R ShaftHQ/shafthq.github.io pr create ...` and `gh -R someone/unrelated issue comment ...` both returning True, where `origin/main` returned False for both. `AGENTS.md` sends companion docs changes to their own PR in `../shafthq.github.io`, so opening that one cleared R21 for the SHAFT_ENGINE session that had posted nothing.

The fix that was rejected, and why it matters more than the one taken: the reviewer suggested a one-line "any skipped `-R` disqualifies". That reverts #4548 for the same-repo case it was written for, and it is incoherent as a rule -- it punishes the explicit correct form while `cd ../other-repo && gh pr create` keeps counting, because no token in that one names a repository either.

The rule to carry forward: a flag a matcher skips is not noise, it is an assertion. Use its value as evidence rather than discarding its presence. Here the value is compared against `git remote get-url origin` from the hook working directory, on the repository name after the slash (a fork's `origin` is `someone/SHAFT_ENGINE`, so comparing owners would resurrect #4548 under a new name), failing open when git will not answer -- the direction R15 and R20 already take. The residual `cd` half is stated as a known limit in the docstring and filed as #4566, not papered over.

Evidence: commits `64d46cfdac` (the defect) and `0584b7ef5e` (the fix), `tests/scripts/test_guard_lifecycle.py::RunStateStopGateTest::test_writing_to_another_repository_does_not_count`.