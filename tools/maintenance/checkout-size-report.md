# SHAFT_ENGINE checkout size report

_Measured 2026-11-02 08:53 EET (Africa/Cairo) on commit `f2e7c9857`._

Issue: [#7547](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7547).
This report is measurement-only. **No history rewrite** was performed.

## Working tree and object database

| Metric | Value |
| --- | --- |
| Shared object DB (`git rev-parse --git-common-dir`) | `/workspace/SHAFT_ENGINE-main/.git` |
| Shared `.git` on disk | 1.3G |
| This worktree (excluding `.git`) | 86M |
| `git count-objects` `count` | 1311 |
| `git count-objects` `size` | 8.64 MiB |
| `git count-objects` `size-pack` | 394.34 MiB |
| `git count-objects` `prune-packable` | 45 |
| `git count-objects` `garbage` | 1 |
| `git count-objects` `size-garbage` | 4.00 KiB |

## Largest blobs still reachable in history


| Size (MiB) | Object | Path (at some revision) |
| ---: | --- | --- |
| 42.81 | `5e47692ab7e4` | `shaft-engine/src/test/resources/testDataFiles/apps/BStackSampleApp.ipa` |
| 29.38 | `68e5bc80c99c` | `shaft-engine/src/test/resources/testDataFiles/apps/Android-NativeDemoApp-0.4.0.apk` |
| 26.71 | `6912587eef43` | `shaft-engine/src/test/resources/testDataFiles/apps/flutter-demo.apk` |
| 24.61 | `f68837b50716` | `graphify-out/graph.json` |
| 24.54 | `eeb406c39eef` | `graphify-out/graph.json` |
| 24.49 | `1443aa94f6d9` | `graphify-out/graph.json` |
| 24.45 | `2077b09ed940` | `graphify-out/graph.json` |
| 24.42 | `20665e63a2cd` | `graphify-out/graph.json` |
| 24.34 | `32f0f52fe25a` | `graphify-out/graph.json` |
| 24.32 | `7cb16eace83b` | `graphify-out/graph.json` |
| 24.32 | `449b096071d8` | `graphify-out/graph.json` |
| 24.31 | `7f960d7ae7e9` | `graphify-out/graph.json` |
| 24.17 | `7608da5de26f` | `graphify-out/graph.json` |
| 24.11 | `8ca6e09b1aec` | `graphify-out/graph.json` |
| 24.02 | `587c0589a8e5` | `graphify-out/graph.json` |
| 23.94 | `8e6ca99a3efb` | `graphify-out/graph.json` |
| 23.78 | `d3ebbc8a3c9d` | `graphify-out/graph.json` |
| 23.77 | `6bdbbc765d24` | `graphify-out/graph.json` |
| 23.73 | `fff67a316f76` | `graphify-out/graph.json` |
| 23.65 | `f2a8c5d99e59` | `graphify-out/graph.json` |

## Notes

- Sizes come from `git count-objects -vH` and `git rev-list --objects --all` piped through `git cat-file --batch-check`.
- This checkout is a linked worktree; pack size is measured on the shared object database, not the per-worktree `.git` private dir.
- Paths are the name recorded with the blob at some reachable commit; they may no longer exist on `HEAD`.
- Reducing checkout size further would require a history rewrite (filter-repo / BFGs). That remains out of scope until the owner explicitly approves a rewrite plan that preserves contributor attribution.
