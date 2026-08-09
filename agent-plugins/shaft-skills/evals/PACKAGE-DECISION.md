# Package split/profile decision

Decision: keep the current single `shaft-skills` package unchanged, with an
`insufficient-evidence` verdict until both authenticated clients complete the corpus.

The post-compression evidence contains no measured reason to split, but it is not yet
complete enough to claim that retention is proven:

- the deterministic corpus covers all 30 skills and all 37 reviewed directed confusion
  boundaries at a required 100% case and positive-skill threshold;
- both pinned native clients validate, discover, install, enable, and clean up the package
  without a context-budget warning;
- the available Codex live routing proof selected the expected requirements specialist;
- unavailable authenticated full-corpus runs are recorded as external blockers, not as
  passes and not as routing failures.

The scheduled full-corpus artifact is the decision trigger. A retain verdict requires both
clients to complete all cases at 100% case and positive-skill coverage with no context-budget
warning. Any wrong specialist selection,
missing positive coverage, or post-compression context-budget warning changes the generated
recommendation to `investigate-split-or-profile`; an unavailable or partial client remains
`insufficient-evidence`. Until complete evidence or measured degradation exists, profiles
would add discovery and release complexity without evidence that they improve routing.
