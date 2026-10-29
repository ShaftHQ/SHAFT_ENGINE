# Box cleanup inventory — Chaos (ChaosEngine / SHAFT)

- Generated: 2026-10-29 08:54 UTC+03:00 (Africa/Cairo)
- Agent: Chaos · The Engineer (executor inventory)
- Mode: READ-ONLY (no rm, no git branch -D, no delete-check requests issued)
- retrieve: used (graphify; store stale vs main 9d22ebb — refresh advisory only)
- Doctor: pass (backup-remote: warn consumer mode)
- Disk: 128G total, 108G used, **21G free** (85%)
- Keep active: `/workspace/SHAFT_ENGINE-main`

## Summary

- **SAFE total (Chaos-owned, verified pushed or regenerable): 11.1G** (11869976575 bytes)
- SAFE* shared /tmp (confirm owners): 1.2G
- KEEP (unique/unverified/active): 4.6G
- No deletes performed. Coach runs delete-check with Mohab code.

## Table: path | size | class | proof

| path | size | class | proof |
|---|---:|:---:|---|
| `/workspace/backups/git-chaos-engine-pre-ff-20261028-182941` | 970.4M | SAFE | store-snapshot tree matches indexed_revision 023051190 (ancestor origin/main #7466); mempalace sqlite regenerable; hallways.json local ephemeral ~2.9MiB |
| `/workspace/backups/git-chaos-engine-pre-reinstall-20261028-172141` | 978.2M | SAFE | indexed_revision 56b17d3db ancestor origin/main (#7249); mempalace regenerable; hallways.json local ~2.6MiB |
| `/workspace/backups/ce-overlay-pre-1e18abe-20261028-135926.chaos-engine` | 3.2M | SAFE | overlay copy; 1e18abe971 on origin/main (#7459) |
| `/workspace/backups/ce-overlay-pre-66dc730-20261028-145613.chaos-engine` | 3.2M | SAFE | overlay copy; 66dc730a6 on origin/main (#7464) |
| `/workspace/backups/ce-overlay-pre-shaftaddon-20261028-140711.chaos-engine` | 3.1M | SAFE | overlay copy regenerable from installer/git |
| `/workspace/backups/ce-overlay-pre-uninstall-20261028-140836.chaos-engine` | 3.2M | SAFE | overlay copy regenerable from installer/git |
| `/workspace/backups/chaos-engine-pre-ff-20261028-182941` | 3.2M | SAFE | overlay snapshot; paired with ff backup; regenerable |
| `/workspace/backups/chaos-engine-pre-reinstall-20261028-172141` | 3.2M | SAFE | overlay snapshot; paired with reinstall backup; regenerable |
| `/workspace/backups/ce-overlay-pre-1e18abe-20261028-135926.graphify-out` | 134.4M | SAFE | graphify derived cache; regenerable via stores refresh |
| `/workspace/backups/ce-state-pre-66dc730-20261028-145613.chaos-engine-state` | 198.4M | KEEP | local-only harness state; bulk=orphaned-hosts-receipt JSON; not on remote; live .chaos-engine-state also 199M |
| `/workspace/backups/ce-state-pre-uninstall-20261028-140836.chaos-engine-state` | 195.2M | KEEP | local-only; near-duplicate of sibling ce-state backup |
| `/workspace/backups/ce-overlay-pre-1e18abe-20261028-135926..chaos-engine-state` | 188.1M | KEEP | local-only state (double-dot name); orphaned-hosts receipts; not on remote |
| `/workspace/review-7512-351ed82` | 238.1M | SAFE | detached 351ed82fd; on remote; PR #7512 MERGED |
| `/workspace/review-7513-2e482ab` | 199.5M | SAFE | detached 2e482ab7a; on remote; PR #7513 MERGED @9d22ebb |
| `/workspace/review-7514-b8f7687` | 200.0M | SAFE | detached b8f768763; on remote; PR #7514 CLOSED superseded |
| `/workspace/wt-7478-review` | 241.4M | SAFE | df1addc7e on remote; PR #7478 MERGED; dirty=1 untracked src/ only |
| `/workspace/wt-coach-pr7512-r1` | 238.3M | SAFE | tip not at origin/coach branch but review notes only; PR merged; untracked _coach_*.txt ephemeral |
| `/workspace/wt-coach-pr7513-r1` | 201.9M | SAFE | PR #7513 merged; untracked coach notes ephemeral |
| `/workspace/wt-coach-pr7514-r1` | 200.1M | SAFE | PR #7514 closed; tip diverged from deleted intent |
| `/workspace/wt-g5-7434` | 206.6M | SAFE | fc9ccc41c on origin (pre-merge head); #7513 MERGED |
| `/workspace/wt-mobile-retro` | 208.0M | SAFE | 45be0ab43 on remote; #7516 MERGED |
| `/workspace/wt-h5s2e1h4` | 259.5M | SAFE | b8f768763 on remote; superseded by #7516; REMOTE branch delete still waits Mohab (separate) |
| `/workspace/wt-pr7492-review` | 199.1M | SAFE | 7b1ce5754; #7492 CLOSED superseded by #7513 |
| `/workspace/wt-g5-7425` | 199.6M | SAFE | 536f34980; absorbed into #7513 cluster / on related history |
| `/workspace/wt-ce/ce-0230511` | 197.7M | SAFE | 023051190 ancestor origin/main; dirty=22 identical deleted-file drift pattern |
| `/workspace/wt-ce/ce-66dc730` | 197.7M | SAFE | 66dc730a6 on origin/main; same dirty drift |
| `/workspace/wt-ce/ce-lockin-src` | 198.0M | SAFE | a1aae5746 on origin/main (#7507) |
| `/workspace/ce-stage-071c7d5` | 790.6M | SAFE | ce-src 071c7d577 ancestor origin/main; brain subclone tip on remote |
| `/workspace/ce-stage-1e18abe` | 355.1M | SAFE | ce-src 1e18abe971 on origin/main |
| `/workspace/ce-stage-bc85d73` | 197.9M | SAFE | staging src tree; no unique unpushed git tip required for drop after confirm |
| `/workspace/pr7464-review` | 349.6M | SAFE | review of #7464 MERGED |
| `/workspace/wt-u1-ce` | 197.7M | SAFE | 6294bff1f on remote ce/shorts-u1-g; #7504 era merged |
| `/workspace/wt-u2-ce` | 197.7M | SAFE | e25cf62a1 on remote |
| `/workspace/wt-u4-ce` | 199.5M | SAFE | b82584c97 on remote |
| `/workspace/wt-u567-ce` | 199.7M | SAFE | 62b5a181f on remote |
| `/workspace/wt-u9-ce` | 199.6M | SAFE | 2dc647d90 also on origin/backup/cleanup-20261029-u9 |
| `/workspace/wt-u10-ce` | 199.8M | SAFE | cca1c1141 also on origin/backup/cleanup-20261029-u10 |
| `/workspace/wt-u11-ce` | 199.8M | SAFE | dbee8c696 on remote; dirty REPORT.md ephemeral |
| `/workspace/wt-u12-ce` | 204.3M | SAFE | 11b9bf735 on remote; #7504 MERGED |
| `/workspace/wt-u13-ce` | 199.7M | SAFE | 5e8c9f66c == origin/ce/shorts-u13-g; #7509 MERGED |
| `/workspace/wt-decks1028` | 197.7M | SAFE | b48b4dfeb on remote |
| `/workspace/wt-dm-retro-ce` | 199.9M | SAFE | d3f54eec3 == origin/ce/digital-avatar-v1-retro |
| `/workspace/wt-drift` | 197.7M | SAFE | 079eaa8dd on remote; #7464 lineage |
| `/workspace/wt-g1` | 201.5M | SAFE | 27d2998aa on remote |
| `/workspace/wt-g2` | 204.3M | SAFE | 4595959ee on origin + backup/cleanup-20261029-g2 |
| `/workspace/wt-mpfix` | 197.7M | SAFE | 092ca8fbb on remote |
| `/workspace/wt-noscorecard` | 197.7M | SAFE | 9d7354323 on remote; OpenSSF work abandoned per standing rule |
| `/workspace/wt-7489-review` | 198.8M | SAFE | 7490ff6cb on origin/backup/cleanup-20261029-pr7489; review WT only — keep wt-ce-install |
| `/workspace/SHAFT_ENGINE-main` | 3.2G | KEEP | ACTIVE primary checkout (excluded by brief) |
| `/workspace/SHAFT_ENGINE-main/.git/chaos-engine/store-snapshot` | 197.9M | KEEP | live CE store-snapshot worktree; linked from backup .git files |
| `/workspace/wt-scrub-main` | 206.9M | KEEP | ACTIVE open PR #7522 @fd85dc262 == origin tip |
| `/workspace/wt-scrub-client` | 208.9M | KEEP | ACTIVE related scrub branch @ce591a90b on origin; may fold into #7522 |
| `/workspace/wt-ce-install` | 200.7M | KEEP | ACTIVE open PR #7489 @cb68375e9 == origin tip; dirty ce-followups-logs/ |
| `/workspace/brain-b2` | 5.8M | SAFE | 90854f0 on origin/ce/coach-lessons-20261024 |
| `/workspace/brain-src-18cc420` | 4.2M | SAFE | 18cc420c ancestor origin/main |
| `/workspace/wt-u1-brain` | 5.7M | SAFE | b0adc606 on origin/skills/shorts-u1-a + PR#31 |
| `/workspace/wt-u2-brain` | 5.7M | SAFE | 27a446c16 ancestor origin/main (merged) |
| `/workspace/wt-u10-brain` | 4.3M | SAFE | 55c39aa0 on origin/brain/shorts-u10-g16 + PR#33 |
| `/workspace/wt-u11-brain` | 4.3M | SAFE | 51d2ee1e on origin/brain/shorts-u11-g17g18 + PR#35 |
| `/workspace/wt-u12-brain` | 4.3M | SAFE | 39904013 on origin/brain/shorts-u12-g + PR#37 |
| `/workspace/wt-u13-brain` | 4.3M | SAFE | e2f77592 on origin/brain/shorts-u13-g + PR#40 |
| `/workspace/wt-brain-decks1028` | 5.7M | SAFE | 12df65c3 on origin/brain/deck-lessons-1028-b + PR#30 |
| `/workspace/wt-brain-v132` | 4.2M | SAFE | 49c55235 on origin/ce/v13-2-deck-standard |
| `/workspace/wt-dm-retro-brain` | 4.3M | KEEP | af2a6691 on origin PR#39 head; dirty=1 → KEEP until clean |
| `/home/box/.m2` | 613.0M | SAFE | Maven regenerable cache |
| `/home/box/.cache/pip` | 155.5M | SAFE | pip regenerable |
| `/home/box/.cache/uv` | 44.1M | SAFE | uv regenerable |
| `/home/box/.npm` | 32.9M | SAFE | npm regenerable |
| `/home/box/.gradle` | 6.1M | SAFE | gradle regenerable |
| `/home/box/.cache/chaos-engine` | 32.5M | SAFE | CE user cache regenerable |
| `/home/box/.cache/gh` | 13.6M | SAFE | gh API cache regenerable |
| `/tmp (aggregate, shared)` | 1.2G | SAFE* | mostly regenerable tmp.*; NOT Chaos-exclusive; exclude sand-box-store-pack (perm denied); confirm no other-bot jobs before drop |

## Other bots / protected (note only — not Chaos drop list)

| path | size | class | note |
|---|---:|:---:|---|
| `/workspace/digital-mohab-v2-work` | 18G | OTHER | digital-mohab / designer stack |
| `/workspace/yt` | 8.3G | PROTECTED/OTHER | yt — excluded |
| `/workspace/backups/digital-mohab-v1` | 7.4G | OTHER | digital-mohab backup |
| `/workspace/digital-mohab-models` | 4.9G | OTHER | models |
| `/workspace/automatest-coach-workspace` | 4.9G | OTHER | Coach |
| `/workspace/designer` | 2.5G | OTHER | Designer |
| `/workspace/wt-shorts-*` | ~4G+ | OTHER | shorts pilot units — Designer/Coach |
| `/home/box/chrome-profile` | 823M | PROTECTED | Chrome profiles — never-touch |
| `/workspace/backups/rog-*` | ~1M | PROTECTED | ROG* — inventory only; Mohab delete-guard |
| `/workspace/automatest-coach-brain` | 83M | OTHER | Coach brain primary; dirty=7 |
| `/home/box/.automatest-coach-brain` | 9.4M | OTHER | Coach |
| `/home/box/.cache/huggingface` | 427M | OTHER | likely digital-mohab |
| `/home/box/.cache/torch` | 107M | OTHER | likely digital-mohab |
| `/home/box/.cache/chroma` | 167M | OTHER | likely digital-mohab |

## Classification rules applied

- **SAFE**: tip SHA on `origin/*` (or ancestor of origin/main), OR pure regenerable cache (m2/pip/uv/npm/gradle/graphify/mempalace index).
- **KEEP**: active open PR worktrees, primary checkout, local-only CE state backups, dirty unique uncommitted work, protected paths.
- Backup `store-snapshot/.git` files point at live worktree gitdir — `git status` inside backups is misleading; file trees compared to indexed revisions instead.
- Dirty=22 pattern across many WTs = identical deleted tracked coverage-journey files (checkout drift), not unique work.
- Remote branch `ChaosEngine/h4-7493-hrsd-j11-harness` still contains client-name history — **branch delete waits Mohab**; worktree itself is SAFE to drop.

## Tool upgrade candidates

| tool | current | latest known | action |
|---|---|---|---|
| gh | 2.103.0 (2026-10-21) | 2.103.3 | minor bump |
| git | 2.47.3 (apt) | 2.47.3 candidate | none |
| java | 21.0.12.1 LTS (tools/jdk21); also 25.0.5+8 / 25.0.4.1 installed | Temurin 25.0.5+8 present | PATH: `java` not on default PATH until env.sh; wire CE tools |
| maven | 3.9.11 (tools); 3.9.12 (CE tools); wrapper dists include 3.10.0 | 3.10.0 / CE 3.9.12 | prefer CE-managed 3.9.12 or project wrapper |
| node | v24.22.1 | v24.22.2 (LTS Krypton); newest line v26.13.0 LTS Lithium | patch bump 24.22.1→24.22.2 |
| npm | 11.21.1 | (ships with node) | with node |
| python | 3.13.5 | 3.13.5 (box) | none |
| uv | 0.13.6 | 0.13.6 | none |
| grok CLI | not on PATH (`pip index` shows unrelated PyPI `grok` 6.2) | install/auth via box grok CLI skill if needed | verify dedicated xAI grok CLI binary |
| mempalace | 3.11.0 (uv tool) | 3.11.0 PyPI | none |
| graphify (graphifyy) | 0.9.97 (uv tool) | 0.9.97 PyPI | none |

## Recommended delete batches (for Coach + Mohab code — not executed)

1. **Backups SAFE ~2.2G**: git-chaos-engine-pre-*, ce-overlay-pre-*.chaos-engine, chaos-engine-pre-*, graphify-out
2. **Stale SHAFT worktrees SAFE ~8G+**: review-*, wt-coach-pr*, wt-g5-*, wt-mobile-retro, wt-h5s2e1h4, wt-u*-ce, wt-ce/ce-*, ce-stage-*, pr7464-review, merged cluster WTs
3. **Brain clone SAFE ~70M**: wt-*-brain (except keep dirty dm-retro until cleaned)
4. **Caches SAFE ~0.9G Chaos-ish** (+ optional /tmp 1.4G after owner scan)
5. **KEEP until reviewed**: all ce-state-pre-* / double-dot state (~610M); active scrub + #7489 WTs; main

## Remote backup of this report

- Local path: `/workspace/checkpoints/box-cleanup-chaos-20261029.md`
- Push target: `origin/backup/box-cleanup-chaos-20261029` (orphan inventory commit) — see push status below.

