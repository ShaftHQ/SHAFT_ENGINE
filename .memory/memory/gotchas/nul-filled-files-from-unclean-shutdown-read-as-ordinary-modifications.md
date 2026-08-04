After an unclean shutdown (this machine has BSOD history) NTFS can record a file's allocation without ever flushing its data blocks, leaving files of a PLAUSIBLE SIZE filled entirely with NUL bytes. Found 2026-08-04: 652 of 653 changed files in one worktree were wholly zeroed -- a 676-byte .gitignore had become 726 bytes of NUL -- and one 45 MB .ipa was 84% NUL.

Why it is invisible: `git status` shows these as plain ' M' entries, identical to real edits. Committing them would have zeroed 653 source files across 8 modules, and the PR diff would have looked large but unremarkable.

How to detect it cheaply, verified live:
- `git diff --shortstat HEAD` reports the files changed with `0 insertions(+), 0 deletions(-)`. That aggregate is the tell.
- `git diff --numstat HEAD` reports each zeroed file as `-\t-\t<path>`: git's binary sniff looks for a NUL in the first 8000 bytes, so an all-NUL file is always classified binary.
- Confirm a specific file by sampling its bytes; a real binary (jar, png, class) is never near-uniformly zero across head, middle, and tail.

Do not sample only a prefix: a legitimately zero-padded file opens with a long NUL run and still carries real content later.

This is now enforced -- see fact.worktree-hygiene-report-and-guard-rule-r10.