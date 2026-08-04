On 2026-08-04 two agents worked overlapping scope from tracker #4434 in sibling worktrees. PR #4441 landed first, declared 'Supersedes #4439', closed issue #4437, and #4439 was closed unmerged.

But #4441 absorbed only #4439's FIRST commit. The second commit -- 22 Codacy fixes (3 Bandit B105 rated High, 2 mccabe complexity, 18 pydocstyle D213) plus three .memory records -- was never taken. Those findings therefore landed on main, and #4441 itself merged with the Codacy check in ACTION_REQUIRED. Recovered as issue #4442 / PR #4446.

So when your pull request is superseded, do NOT assume your work is on main. Diff before discarding:

    git fetch origin
    git log --oneline origin/main..<your-branch>
    git diff origin/main..<your-branch> -- <files you touched>

and grep main for the specific defects you fixed (`git show origin/main:<path>`) rather than trusting the superseding PR's summary. Cherry-picking the unabsorbed commit onto a fresh branch off origin/main is usually clean, but expect conflicts where the other agent extended the same functions -- resolve by keeping THEIR behaviour inside YOUR structural fix, then run the full affected suite to prove nothing regressed.

Prevention: two agents must not be dispatched on overlapping scope from one tracker. Where it happens anyway, the second-to-land agent owns the reconciliation.