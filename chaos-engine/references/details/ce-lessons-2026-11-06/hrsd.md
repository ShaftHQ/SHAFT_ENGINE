# Lessons — HRSD — 2026-11-06

Retrospective of client mobile automation work since about 2026-10-31 (and still-relevant patterns from earlier). Abstracted: no secrets, tokens, credentials, hostnames, or private personal data. Say "a client mobile project" in any public reuse.

## Lessons

### 1. Keep three documentation sources of truth
- **What happened:** Delivery notes, gap catalogues, and a parallel `docs/defects/` tree drifted from the journey map and issue tracker. A later cleanup PR had to delete the extra files. A teammate PR then re-added `docs/defects/`.
- **Root cause:** Agents treat every new finding as a new markdown file instead of updating the named sources of truth.
- **Enforceable rule:** A journey-based mobile automation repo keeps three SoTs: a plan (progress and coverage/hours), a journey map (full journey/scenario/step detail, updated only after implementation), and an issue tracker split into locator/missing-accessibility vs product inconsistencies. Setup belongs in README. Do not recreate retired folders such as `docs/defects/`; fold content into the issue tracker.
- **Acceptance criteria:** A docs PR that adds a fourth tracker file, or re-adds a retired defects folder, fails review. Skills and CI sync scripts reference only the SoT paths.
- **Suggested destination:** `chaosengine` (skill `docs-hygiene`, already filed as #7726)

### 2. Status cells are only Done, In progress, or Not started
- **What happened:** Plan and journey tables used `Blocked` and `Done (with comments)` as statuses. The human corrected this twice: remaining bugs belong in the last comments column; the status itself stays `Done`.
- **Root cause:** Status and commentary were mixed in one cell, and "blocked" felt like a status rather than a comment.
- **Enforceable rule:** Active vocabulary is exactly `Done`, `In progress`, `Not started`. Bugs, SHAFT upstream links, out-of-scope reasons, and owner notes go in Comments. Legends, mermaid classDefs, sync color maps, and guard scripts must not treat retired words as live statuses.
- **Acceptance criteria:** A guard or review checklist fails any plan/journey/scenario table cell that is not one of the three statuses.
- **Suggested destination:** `chaosengine` (skill `docs-hygiene`, #7726)

### 3. Done hours equal planned hours; totals are live formulas
- **What happened:** Completed journeys were marked Done with 0 completed hours. A Summary tab pointed at the wrong row, so the overall completion percentage disagreed with the plan.
- **Root cause:** Status was updated without the hours model, and totals were treated as narrative numbers.
- **Enforceable rule:** For every Done row, Completed hours = Planned/Effort hours. Bottom totals use SUM formulas (or regenerated sync) over the same totals row the Summary percentage reads. If a workbook includes a framework row outside the coding budget, document that so percentages cannot silently disagree.
- **Acceptance criteria:** A Done row with Completed != Planned fails review. Summary % equals completed/planned from the totals row.
- **Suggested destination:** `chaosengine` (skill `docs-hygiene`, #7726)

### 4. Journey-map statuses must agree across table, diagram, and detail
- **What happened:** A teammate PR marked a scenario Done in the summary table and mermaid diagram while the detail section still said In progress. The PR description's roll-up counts were also stale versus the tip.
- **Root cause:** Several representations of the same status were edited independently.
- **Enforceable rule:** Table, mermaid, detail prose, and PR description roll-up must match at the same HEAD. After any tracker edit, regenerate or re-read all four.
- **Acceptance criteria:** Adversarial review fails a PR where those four disagree.
- **Suggested destination:** `chaosengine` (skill `docs-hygiene`, #7726) and `graphify` (journey-map consistency)

### 5. Do not merge a partial journey as the full journey
- **What happened:** A PR covering only the first scenario of a multi-scenario journey was presented for merge into main. Remaining scenarios were unimplemented.
- **Root cause:** Scenario-level Done was treated as journey-level Done.
- **Enforceable rule:** Merge a journey as complete only when every in-scope scenario (both languages, both platforms as scoped) is implemented or explicitly parked in the issue tracker with a comment. A first-scenario PR may merge only if trackers still say the journey is In progress and the description does not claim full coverage.
- **Acceptance criteria:** Review maps every journey-map scenario to tests before approve/merge. Missing scenarios are listed on the PR and block a "full journey" claim.
- **Suggested destination:** `chaosengine` (review gate) and `brain`

### 6. Independent adversarial review is SHA-bound
- **What happened:** Self-review missed broken skill paths, lost content, credential-bearing examples, status/hours drift, and coverage math. A separate adversarial pass caught them. Later polish commits landed after a PASS and needed another ack before merge.
- **Root cause:** The author cannot see mechanical damage they just made, and a PASS on an older SHA does not cover new commits.
- **Enforceable rule:** Before merge, run an independent adversarial review that tries to reject the PR. A PASS is bound to a SHA; any new commit invalidates it (#7709). Review against the owner's latest vocabulary, not the previous round's.
- **Acceptance criteria:** No merge after an invalidated PASS. Re-review notes cite the new SHA.
- **Suggested destination:** `chaosengine` (#7709 and skill `docs-hygiene`, #7726)

### 7. Delete-guards may require an upstream branch first
- **What happened:** Path deletes were rejected until the branch existed on the remote, which forced extra pushes that looked like a "push once" violation.
- **Root cause:** The repo's delete/secret guard is designed to refuse deletes on a branch that has no upstream.
- **Enforceable rule:** If a delete-guard requires an upstream, push a start commit first, then delete in a follow-up. Document this as an allowed exception to "single push," never `--no-verify`, and never edit allowlists to bypass the guard.
- **Acceptance criteria:** Agents can explain the two-push sequence and do not disable hooks.
- **Suggested destination:** `chaosengine` (skill `docs-hygiene`, #7726)

### 8. Secrets stay in gitignored properties; never print values
- **What happened:** A BrowserStack YAML with a plaintext key had to be deleted and gitignored. Allure single-file HTML can also embed keys. The suite actually loaded a classpath properties file; a repo-root copy was assumed missing when the classpath file was already present. Example docs once showed real credential curls instead of `sed` from gitignored keys.
- **Root cause:** Multiple secret locations, leaky report artifacts, and agents checking only one path.
- **Enforceable rule:** Credentials live only in a gitignored properties file (root and/or `src/main/resources/properties/`). Document key names only (`browserStack.userName`, `browserStack.accessKey`, `browserStack.appUrl`, optional `browserStack.local`, plus any API client keys the project uses). Never print values. Secret-scan before every commit. Never `--no-verify`. Keep Allure HTML and cloud YAML out of git.
- **Acceptance criteria:** Secret-scan fails the commit if values appear. README examples use placeholders or `sed` from gitignored keys.
- **Suggested destination:** `chaosengine` (skill `shaft-browserstack-native`, #7723)

### 9. Client tests must never modify production
- **What happened:** Flows that could submit, pay, or tap a force-update prompt had to be stopped before the mutating action. Cloud devices sometimes showed a backend force-update gate that looked like a locator failure.
- **Root cause:** Happy-path scripts follow the UI to the final button; cloud backends can also demand an update.
- **Enforceable rule:** No final submit/pay taps, no state-changing API calls, never tap "Update Now". Assert visuals for image elements, non-empty text for data, and API schema+data for services. Stop-before-submit is a pass, not a gap. Triage blank cloud launches against device logs/backend gates before rewriting locators.
- **Acceptance criteria:** Review finds no mutating terminal actions. Force-update / blank-launch triage is documented.
- **Suggested destination:** `chaosengine` (skill `shaft-browserstack-native`, #7723) and `brain`

### 10. Mobile taps go through the project tap helper, at the element centre
- **What happened:** Coordinate taps and raw driver clicks drifted from the shared helper and were harder to observe or recover.
- **Root cause:** Shortcuts around the page-object tap wrapper.
- **Enforceable rule:** All taps go through the shared page-object tap helper (element centre). No screen-position taps unless a filed upstream engine issue documents why the helper cannot work.
- **Acceptance criteria:** Review greps for raw coordinate clicks in new tests and fails them without an linked exception.
- **Suggested destination:** `brain` and `chaosengine`

### 11. One test at a time, retry 0, fresh JVM after page-object commits
- **What happened:** Parallel or retried runs hid flakes. A Surefire JVM started before a fix commit kept running old classes, so a "fixed" test still failed.
- **Root cause:** Mixed bytecode and retry-as-green.
- **Enforceable rule:** Run one test at a time with retry 0 during implementation. After any page-object or helper commit, stop in-flight Surefire processes and start a new JVM. Journal the SHA the retest used.
- **Acceptance criteria:** A progress journal records method, SHA, engine/CLI/CE versions, and PASS/FAIL. Retests after commits cite a new PID/SHA.
- **Suggested destination:** `chaosengine` (skill `shaft-mobile-retest-hygiene`, #7727)

### 12. Evidence hygiene for client issues
- **What happened:** Generated HTML reports and videos were at risk of being committed; the human asked that they stay out of git and be attached to the PR, while the issue tracker and screenshots stay in the repo. Large binaries also cannot go raw through some chat tools (zip first).
- **Root cause:** Reports are convenient locally and look like documentation.
- **Enforceable rule:** Never commit generated HTML reports or videos. Attach them to the PR. Keep `ISSUES.md` (or equivalent) and screenshots in the repo. Zip large binaries before chat upload when the channel blocks the raw type.
- **Acceptance criteria:** Secret-scan / git review fails HTML/video report paths. Issue tickets have viewable evidence.
- **Suggested destination:** `chaosengine` and `brain`

### 13. iOS SwiftUI lists need a drag fallback until the engine release ships
- **What happened:** `swipeElementIntoView` did not move a SwiftUI vertical list. A workaround (tallest `XCUIElementTypeScrollView` + `mobile: dragFromToForDuration`, row identity via `name` not `content-desc`) unblocked tests. The engine fix merged as PR #7681 after Maven `10.4.20261102` was published.
- **Root cause:** The iOS swipe path was not targeting the actual scrollable list; Android's earlier fix did not cover iOS.
- **Enforceable rule:** Keep the consumer drag fallback until a Maven release newer than `10.4.20261102` includes #7681. Then bump the pom and delete the fallback. File engine gaps upstream; do not silently live on workarounds.
- **Acceptance criteria:** Pom version is checked against Maven Central before removing the fallback. A release-notes or commit check confirms #7681 is included.
- **Suggested destination:** `chaosengine` (skill `shaft-mobile-retest-hygiene`, #7727) — engine work already in #7680 / #7681

### 14. Map/confirm taps that stay green while the UI does not change are app defects after triage
- **What happened:** Select-address / confirm controls logged click success, screenshots stayed put, and the next control never appeared. `hittable` was absent from page source. Retapping could cancel in-flight map navigation.
- **Root cause:** After a good locator and a settle wait, the control itself does not activate. SHAFT only recovered when `hittable` was explicitly `false` (#6908), not when it was blank (#7706).
- **Enforceable rule:** Triage order: rule out locator → before/after page source and screenshots → one tap then wait → if still no effect, file an app defect and park map-dependent siblings. Do not burn the rest of the suite. Leave engine observability to #7706.
- **Acceptance criteria:** Agents stop rewriting SHAFT taps after dual evidence and file the product issue instead.
- **Suggested destination:** `chaosengine` (skill `shaft-mobile-retest-hygiene`, #7727; engine #7706)

### 15. Unlabeled pickers are classified by destination, not guessed labels
- **What happened:** Option rows exposed no human-readable name/label/value. Guessing from Continue-button text sent tests into the wrong wizard.
- **Root cause:** Accessibility data was missing; heuristics filled the gap incorrectly.
- **Enforceable rule:** When rows have no labels, tap candidates and probe the destination screen to classify them. If every row leads to the same unexpected destination, file an app defect (missing option set) rather than churning locators.
- **Acceptance criteria:** New picker helpers document destination-probe, not invented label strings.
- **Suggested destination:** `chaosengine` (skill `shaft-mobile-retest-hygiene`, #7727)

### 16. Local mobile environment is a split toolchain
- **What happened:** Maven/`--release 25` needed Temurin 25; Android Gradle builds needed Temurin 21 with a full javac. Wrong Maven JDK produced class-file version 69 errors. Headless emulator host RSS grew far past guest RAM and OOM-killed the desktop. A stale snapshot hung the emulator until cold-boot.
- **Root cause:** One `JAVA_HOME` was assumed enough, and the emulator was started unbounded.
- **Enforceable rule:** JDK 25 for Maven/SHAFT tests, JDK 21 for Android app Gradle. Prefer SHAFT-managed Appium (known-good pin: Appium 3.8.0, uiautomator2 8.5.2) with `--relaxed-security`. Set `ANDROID_HOME` and `ANDROID_SDK_ROOT` together. Start the emulator in a memory-capped scope (systemd user example: `MemoryMax=8G` or `10G`, `MemorySwapMax=0`). Cold-boot when dead/offline/snapshot-mismatched, not between every method. Doctor → fix only reported gaps → doctor → one smoke test at retry 0.
- **Acceptance criteria:** A second machine can follow the skill and pass doctor plus one smoke method.
- **Suggested destination:** `chaosengine` (skill `shaft-mobile-env`, #7722)

### 17. Cloud artifacts must match the target ABI and platform
- **What happened:** An arm64-only APK would not install on an x86_64 emulator. Simulator IPAs could upload to a real-device cloud and sit on SpringBoard. App-id aliases drifted from the intended build.
- **Root cause:** Artifact type, signing, and identity were not gated before upload/install.
- **Enforceable rule:** Local x86_64 emulator → universal or x86_64 APK. Real/cloud arm64 → prefer arm64-v8a, universal as fallback. `adb install -r -d` before a suite (unchanged `versionCode` can skip reinstall). IPA must be `DTPlatformName=iphoneos`, device-signed (not simulator, not App-Store-only when the cloud cannot install it). Prefer `bs://` app ids over moving `custom_id` aliases; re-list recent apps before long reruns. Pin exact deviceName + platformVersion.
- **Acceptance criteria:** Skills refuse simulator IPAs and arm64-only APKs on x86_64 emulators before upload/install.
- **Suggested destination:** `chaosengine` (skills #7724, #7725, #7723)

### 18. Plan workbooks update when a PR merges or a new branch starts
- **What happened:** The human asked for the shared plan sheet to be updated automatically on those two events, with owner names in front of their tasks and comments reflecting current state.
- **Root cause:** The sheet lagged git, so stakeholders read stale status.
- **Enforceable rule:** On PR merge or when work starts on a new branch, update the plan sheet: status vocabulary from lesson 2, owner name prefixed, comments current, Done hours from lesson 3, totals refreshed. No extra status words.
- **Acceptance criteria:** After a merge or branch start, the sheet row matches the tracker HEAD.
- **Suggested destination:** `chaosengine` and `brain`

### 19. Verify a "dropped" artifact before redoing it
- **What happened:** Secrets looked missing at repo root while the classpath copy the suite loads was already there. Previously filed engine issues were later found closed. Redoing either would have duplicated work.
- **Root cause:** A single-path or cached check was treated as ground truth.
- **Enforceable rule:** Before restoring, re-implementing, or re-filing, check the live path, PR, issue, or page. If it exists, record it as Done with the link and do not redo it.
- **Acceptance criteria:** Status reports cite what was opened this turn. Claims not re-opened are marked unverified.
- **Suggested destination:** `chaosengine` (status-board gates, #7692) and `brain`

### 20. File abstracted upstream issues; let Chaos merge them in one PR
- **What happened:** Client-specific setup and review lessons were written twice (project README and GitHub). One-issue-per-skill is right for tracking; one PR per skill is not.
- **Root cause:** Filing and implementing were conflated.
- **Enforceable rule:** File generic, English-only GitHub issues with no client data. Implementation of related ChaosEngine skills and open CE bugs goes in one mega PR (this cycle's Phase 3), not one PR per ticket.
- **Acceptance criteria:** Public issue text has no client names, secrets, or internal hosts. Related skill work lands together.
- **Suggested destination:** `chaosengine` (delegation protocol, protocol section 4)

## Open or related SHAFT / ChaosEngine items

Verified on GitHub and Maven Central this turn (2026-11-06):

| Item | State | Notes |
| --- | --- | --- |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7680 | closed | iOS SwiftUI swipe |
| https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7681 | merged 2026-11-01 20:08 UTC | Fix for #7680; merged after Maven `10.4.20261102` (metadata lastUpdated 2026-11-01 18:08 UTC). Consumer fallback must stay until a newer release. |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7706 | closed 2026-11-02 15:19 UTC | Blank hittable + unchanged page observability. Confirm the implementation actually landed. |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7709 | not re-fetched this turn | Adversarial PASS must bind to a SHA (cited from #7726/#7728). |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7722 | closed 2026-11-02 15:19 UTC | Skill `shaft-mobile-env` |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7723 | closed 2026-11-02 15:19 UTC | Skill `shaft-browserstack-native` |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7724 | closed 2026-11-02 15:19 UTC | Skill `shaft-ios-ipa-acceptance` |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7725 | closed 2026-11-02 15:19 UTC | Skill `shaft-android-apk-select` |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7726 | closed 2026-11-02 15:19 UTC | Skill `docs-hygiene` |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7727 | closed 2026-11-02 15:19 UTC | Skill `shaft-mobile-retest-hygiene` |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7728 | closed 2026-11-02 15:19 UTC | Epic; 6/6 children marked completed. Chaos should confirm in Phase 3 that the skills landed, and not re-file them if they did. |
| Maven Central `io.github.shafthq:shaft-engine` | latest `10.4.20261102` | No newer release as of this turn. |

Please fold any still-missing skill text into the Phase 3 mega PR rather than opening one PR per closed ticket. Do not recreate #7680.
