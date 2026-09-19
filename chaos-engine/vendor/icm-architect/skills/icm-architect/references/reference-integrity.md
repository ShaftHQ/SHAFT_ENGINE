# Reference integrity — the move-safety gate

Restructure's walk test proves the *result* is navigable. It does not prove the *move* was safe. Before proposing a move, enumerate what points at the file. Apparent disuse is not proof.

## What to search

1. **In-vault** — other files in this workspace that name the path.
2. **Sibling-path** — relative `../` references. These break when you regroup folders even if nothing "outside" is involved.
3. **Symlink** — a moved target orphans the link; a moved link disappears.
4. **External** — other repos, deploy scripts, cron, issue trackers, agent configs that hardcode a path in. Nothing in the tree names these, so the walk test cannot see them. Ask the owner. Record what they name on the migration map.

A file with a live referrer is **held**, or moved only if every referrer is updated in the same change. It is not Dead until this comes back clean.

## Destination collision

Before any copy or rename, check whether the destination already exists **case-folded**. On Windows and macOS the filesystem is case-insensitive, so the rename the method itself uses — `CLAUDE.md` → `CONTEXT.md` — silently overwrites an existing `context.md`. A migration map built from a file inventory lists the two files separately. A parity check run *after* the copy cannot catch it: by then the destination is the source.

## Copy, verify, then remove

1. Copy to the new home.
2. Verify parity — file count and content hash (byte-for-byte for text). Zip-based office formats embed metadata; compare unzipped content, not the archive's outer hash.
3. Remove the original only after parity passes. Leave a pointer if anything referenced it.

## Durability

Confirm the workspace root is tracked or backed up before reorganizing inside it. Reorganizing files that live only in a temp dir or a gitignored path is rearranging deck chairs.
