# Lessons — Chaos (The Engineer) — 2026-11-06 (Phase 2 retrospective)

Scope: work since about 2026-10-31. Sources: `/workspace/specs/TEAM-PROTOCOL-20261106.md`, `/workspace/specs/WAYS-OF-WORKING-20261031.md`, `/workspace/status/20261106/chaos.md` (verified 2026-11-06 status for #7749, #7752, #7754, #7756, #7757, #7764, #7765, #7679, #7681, allure-java#1499, selenium#18254 and #7077–#7081), and the progress files under `/workspace/intellij-closeout/`, `/workspace/allure-1497/`, `/workspace/job-apply-skill/` and `/workspace/cleanup-2026-11-03/`. I checked live gh this turn for PRs #7662, #7668, #7672, #7676, #7682, #7731, #7735 and #7737, and for issues #7751, #7753, #7758, #7766, #7767 and #4307.
All lessons are abstracted. They contain no secrets, codes, tokens or personal data.

---

## L1. Watchers must exit and wake on ANY terminal state
- **What happened:** Long CI watchers looped only on "success → merged". When a check failed or the watcher timed out, they kept waiting or died silently. The owning bot looked stalled for hours and the human had to ask "did you crash?" several times.
- **Root cause:** The exit condition was success-only. Failure, cancelled, timeout and BLOCKED states had no wake path.
- **Rule/gate:** Every watcher script needs a bounded deadline. It must exit non-zero and wake the owner on any failed, cancelled or timed-out check, on BLOCKED/DIRTY mergeable state, on dequeue, and on its own deadline. The harness provides one shared watcher template, and the preflight rejects ad-hoc `while true` loops with no deadline.
- **Acceptance:** In a simulated failing check run, the watcher exits non-zero within one poll interval, writes the failing check name to its status file and triggers a wake. In a simulated stall, it exits at its deadline.
- **Destination:** chaosengine

## L2. Hourly watchdog for long tasks (wake notifications can be lost)
- **What happened:** A finished background executor or watcher sometimes produced no wake notification, so delivery stalled with nobody driving it.
- **Root cause:** The wake channel is best effort, and nothing independently checked task liveness.
- **Rule/gate:** Any task expected to run for more than about 30 minutes registers an hourly watchdog routine. The routine reads the progress file and the live PR state, then wakes or resumes the owner. It is deleted once the task is delivered. Executors write resumable progress files because session caps interrupt them.
- **Acceptance:** A long task has both a watchdog entry and a progress file. After merge, the watchdog no longer exists.
- **Destination:** chaosengine (rule), brain (fact: wakes are lossy)

## L3. SHAFT PR Gate reruns reuse the original event payload
- **What happened:** After a PR-body fix such as the retrieve-receipt line, rerunning the PR Gate still failed on the old body. Editing the body did not trigger the gate.
- **Root cause:** A rerun replays the original `pull_request` event payload, and the gate does not listen on `edited`.
- **Rule/gate:** After any PR-body fix, push an empty commit to retrigger the gate (no rerun). The receipt line format is `retrieve: skipped(<reason>)`, with no literal bracketed store placeholder. The preflight lints the PR body before the PR is created.
- **Acceptance:** A PR-body lint runs before `gh pr create`. The bot docs say "empty commit, not rerun", and a test fixture covers the receipt format.
- **Destination:** chaosengine (gate), graphify/brain (fact)

## L4. BLOCKED with green checks means look for unresolved review threads first
- **What happened:** PRs sat BLOCKED with every check green, and time went into re-running CI.
- **Root cause:** main requires every review thread resolved, including github-code-quality and Codacy bot threads.
- **Rule/gate:** The merge-readiness check queries unresolved review threads through GraphQL before anything else and lists them. Fix each thread, then resolve it.
- **Acceptance:** The `merge-ready` helper reports unresolved threads by author. A PR with a bot thread open is reported as "blocked: threads", not "waiting CI".
- **Destination:** chaosengine

## L5. Merge discipline: auto-merge with a head match, then confirm the queue
- **What happened:** The merge queue jammed behind Dependabot PRs whose merge_group SHAs never got a PR Gate run. A finished PR waited behind them until the stuck entries were dequeued. Several open single-bump Dependabot PRs (#7759–#7763) also kept competing for the queue.
- **Root cause:** Ungrouped Dependabot updates, and nothing detected a queue entry with no merge_group gate run.
- **Rule/gate:** Merge only with `gh pr merge N --auto --merge --match-head-commit SHA`, then confirm the PR is actually in the queue. If the queue head has no merge_group gate run after N minutes, flag it and dequeue it. Group Dependabot updates into one PR per cycle across ecosystems.
- **Acceptance:** After enqueue, the watcher logs the queue position. A stuck-head detector exists. The Dependabot config has groups.
- **Destination:** chaosengine (gate + dependabot config)

## L6. Worker session cap vs long test suites
- **What happened:** Running the full CE unittest suite in a worker's foreground hit the session cap, and parallel copies slowed each other down.
- **Root cause:** The suite takes longer than one worker slice, and nothing stopped two concurrent runs.
- **Rule/gate:** Never run the full unittest suite in the foreground. Run focused tests in the foreground and the full suite in the background with a log, a lock file (one copy at a time) and a watcher (L1).
- **Acceptance:** The suite runner refuses a second concurrent run while the lock is held and warns when started in the foreground without `--background`.
- **Destination:** chaosengine

## L7. Pins: latest stable AND LTS, with fallback plus a ticket
- **What happened:** The pin refresh (#7749, #7752) moved toolchains to the latest stable and, where an LTS line exists, to the LTS. Some upgrades were not straightforward: Python 3.15 is blocked on onnxruntime wheels (#7751), and Flutter demo CI needs Gradle work for Java 25 (#7753).
- **Root cause:** "Latest" and "LTS" were not distinguished, and blocked upgrades were mixed into the pin PR.
- **Rule/gate:** The drift watch is LTS-aware. When the latest upgrade is not straightforward, stay on the newest working version and file a separate tracked ticket with the blocking dependency. Never block the pin PR on it.
- **Acceptance:** The drift report shows latest and LTS per tool. Every held-back pin links an open issue.
- **Destination:** chaosengine

## L8. Cleanup at close: worktrees, browsers, report servers
- **What happened:** Standalone worktrees, review clones, IDE sandboxes, venvs and caches piled up across bots until the disk reached 88% used. Cleanup freed several GiB, and the remainder needed owner-approved deletes.
- **Root cause:** No delivery-close step removed task artifacts, and ownership of paths was not recorded.
- **Rule/gate:** This is partly done in #7756 (delivery_close_blockers plus an ownership ledger). Extend it: register every created worktree, clone or sandbox in the ownership ledger at creation. A task cannot be "Done" until its artifacts are removed or explicitly marked keep. Stop report servers and browsers the task started.
- **Acceptance:** `delivery_close_blockers` returns empty before the Done message. Disk-guard output is attached to the close.
- **Destination:** chaosengine

## L9. Headless tests, no Allure auto-open, no local report servers
- **What happened:** Test runs opened Chrome tabs and Allure servers that stayed alive and ate RAM.
- **Root cause:** The defaults favour interactive local use.
- **Rule/gate:** The harness forces headless execution, disables Allure auto-open and blocks `allure serve` for bots. The close gate (L8) kills any leftovers.
- **Acceptance:** Bot test-run env/properties show headless=true and auto-open=false. No `allure serve` or orphan chrome processes remain after a task.
- **Destination:** chaosengine

## L10. Mega-PRs for related tickets
- **What happened:** The IntelliJ revamp (#7754, 20 tickets) and the close-out (#7757, 14 fixed and 5 won't-do) each landed as one PR, and so did the CE megas (#7682, #7731, #7735). That beat one PR per ticket on queue time and review load. Tickets with no real value were closed as won't-do instead of being forced in.
- **Root cause:** One PR per ticket multiplies CI, queue waits and pom/config conflicts.
- **Rule/gate:** Group related tickets into one PR whose body has "closes" lines. One open PR per kind touches the build config at a time (WAYS-OF-WORKING §3). Close non-value tickets as not-planned with a reason.
- **Acceptance:** The PR body lists every closed issue. No two open bot PRs touch pom.xml or build config at once.
- **Destination:** chaosengine

## L11. PDCA: step back when a stage fails twice
- **What happened:** CI/coverage pushes chained fix after fix (several heads to reach the patch-coverage gate), and pre-push loops repeated the same failure.
- **Root cause:** No forced root-cause step. Each failure got a local patch.
- **Rule/gate:** On the second failure of the same stage, stop. Write a root-cause note (plan, then do/check/act) in the progress file before the next push.
- **Acceptance:** The progress file has an RCA entry after any stage with 2 or more consecutive failures, and the push guard warns otherwise.
- **Destination:** chaosengine

## L12. Independent adversarial review without a working external CLI
- **What happened:** The second-opinion CLI failed (deprecated endpoint or auth), so reviews fell back to two independent mechanical methods: an evidence matrix and a surface audit.
- **Root cause:** A single review provider with no documented fallback.
- **Rule/gate:** Dual independent reviews by fresh workers or methods are required. When the CLI is unavailable, record the fallback and its reason in the progress file. Never self-review in the same context.
- **Acceptance:** Every delivery has two review verdicts recorded at the final head SHA.
- **Destination:** chaosengine, brain (fact: CLI fallback)

## L13. Real end-to-end proof for IDE/plugin work
- **What happened:** Unit coverage alone left gaps. The close-out installed the Marketplace ZIP in a real IntelliJ IDEA under xvfb (headless) and exercised the core features, with evidence committed.
- **Root cause:** Plugin wiring bugs only show in a real IDE.
- **Rule/gate:** Plugin deliveries need an e2e run in a real IDE (headless display) with an evidence folder in the repo.
- **Acceptance:** The evidence directory with E2E-REPORT.md exists on main for the PR.
- **Destination:** chaosengine, graphify (evidence location)

## L14. Abstraction: no personal data in skills
- **What happened:** The job-apply (#7764) and job-search (#7765) skills were built from one person's real application run.
- **Root cause:** Source playbooks contained identities, contacts, employers and stats.
- **Rule/gate:** Skills are abstract templates. The applicant profile lives only in a private per-user store. `check_abstraction` plus scans for emails, phones and years run in preflight on skills. Any apply step that shares a profile needs explicit human approval.
- **Acceptance:** `check_abstraction` passes on every skill file, and no personal data shows up in the skills tree.
- **Destination:** chaosengine, mempalace (private profile stays private)

## L15. Delegation / status / radio-silence cycle
- **What happened:** Multi-bot cycles relied on chat details, cached status and implicit unfreezes.
- **Root cause:** No codified protocol.
- **Rule/gate:** Implement TEAM-PROTOCOL §4: one shared instruction file, fresh polls, a status template, numbered IDs, "proceed with N" applying only to its owner, radio silence, a per-bot run-state of frozen/active/halted, and the lessons → one CE PR → install → cleanup → Done cycle. This is already filed as #7767 (no duplicate).
- **Acceptance:** As defined in §4 of the protocol, a router can run the cycle with one file and one message per bot.
- **Destination:** chaosengine

## L16. External-contribution trackers
- **What happened:** The allure-java PR #1499 merged and shipped in 3.1.1. Tracker #7758 stays open until SHAFT adopts that release (the old tracker #7729 closed after its upstream merge). The Selenium PR #18254 is in CHANGES_REQUESTED (tracker #4307). Fork PRs can't set the maintainer-only label, so the label check failed.
- **Root cause:** Tracker lifecycle and the fork limits were implicit.
- **Rule/gate:** Every upstream PR gets a SHAFT tracker. Close the tracker only after upstream merge, release and adoption in SHAFT. A periodic check flags CHANGES_REQUESTED or stale upstream PRs. On fork PRs, ask maintainers for the label. Commit under the identity used for prior contributions to that project. Upstream text is in English.
- **Acceptance:** Every open tracker shows upstream state and release adoption, and stale review requests get surfaced.
- **Destination:** chaosengine (tracker rule), brain (contribution facts)

## L17. Safety rules: delete codes, sudo, force-push, tokens
- **What happened:** Deletes went through the delete-check guard. Codes are single-use and expire after 10 minutes, and some paths waited for fresh codes. A gh head-OID mismatch after a history rewrite blocked hygiene deletes.
- **Root cause:** These are by design. Only the human authorizes deletes.
- **Rule/gate:** Only codes typed by the human count; a code relayed by an agent is never approval. No sudo. No force-push without explicit approval. Never print tokens or put them in remotes, chat or files. Guards enforce all four (delete-check, a pre-push force check, a secret scan on output and commits).
- **Acceptance:** The guards refuse a relayed code, an unapproved `--force` or a token pattern in a commit.
- **Destination:** chaosengine

## L18. Self-provisioned toolchains and preflight
- **What happened:** Maven/JDK/JAVA_HOME gaps broke packaging (#7668, #7676, #7735), and a fresh Windows install hits "no python 3.11+" (#7766).
- **Root cause:** The installer assumed the host had the toolchains.
- **Rule/gate:** The installer self-provisions or clearly guides the required JDK, Maven and Python before lifecycle prompts. When something is missing, the failure UX gives what is missing, why and the one fix.
- **Acceptance:** A clean Windows/Linux install runs the first prompt without a toolchain error. #7766 is reproduced and fixed.
- **Destination:** chaosengine

## L19. Versioning and releases
- **Rule/fact:** CE versions follow `0.4.YYYYMMDD.n` and are never future-dated. No SHAFT release unless asked. For example, the iOS swipe fix #7681 is on main but not in a release.
- **Acceptance:** The version-lint rejects a date later than today.
- **Destination:** chaosengine (lint), brain (fact)

---

## Known open ChaosEngine bugs / missing gates / enhancements
- https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7766: prompt blocked by project/lifecycle: no python 3.11+ (bug, open)
- https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7767: multi-agent delegation protocol (enhancement, open, Phase 3)
- https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7751: managed Python 3.15 (blocked on onnxruntime cp315 wheels)
- Not yet filed, proposed gates from the lessons above: the shared watcher template (L1), the stuck merge-queue detector and Dependabot grouping (L5), the suite lock (L6), the PDCA push guard (L11) and the version-date lint (L19).

Related but not CE (defer from the Phase 3 PR): #7753 (Flutter CI to Java 25), #7758 (adopt allure-java 3.1.1), #4307 (Selenium tracker), #7077–#7081 (coverage), #7423.
