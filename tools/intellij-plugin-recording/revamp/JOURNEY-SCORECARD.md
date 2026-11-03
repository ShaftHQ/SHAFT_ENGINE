# IntelliJ plugin journey UX scorecard (W0.4 / #7568)

Score each journey on a clean IntelliJ IDEA + Marketplace ZIP install (not monorepo `runIde` alone).

Scale **1–5** for each dimension: 1 = fail / hostile · 3 = usable with friction · 5 = native & clear.

| Dimension | Meaning |
|---|---|
| Discover | Can a new user find the entry without tribal knowledge? |
| Understand | Labels/copy make the next step obvious? |
| Succeed | Completes the happy path without expert mode? |
| Recover | Failure offers a verb + path back? |
| Feel native | New UI chrome, icons, density match IntelliJ? |

## Journeys (close-out 2026-11-03 Africa/Cairo)

Machine evidence: IDEA IC 2024.3.7.1 + `shaft-intellij-10.4.20261102.zip` under xvfb
(`tools/intellij-plugin-recording/revamp/evidence/closeout-20261103/`). Shared box used for
install/load proof; journey UX scores also reflect #7754 shell + this PR's target/Allure/BOM/Assistant work.

| ID | Journey | Discover | Understand | Succeed | Recover | Feel native | Notes / evidence |
|---|---|---|---|---|---|---|---|
| J1 | Install + first-run | 5 | 4 | 5 | 4 | 4 | PluginManager `Loaded custom plugins: SHAFT`; five-step wizard on main |
| J2 | New SHAFT project | 4 | 4 | 4 | 4 | 4 | Maven module builder (#7609) on main |
| J3 | Record → keep code | 4 | 4 | 4 | 3 | 4 | Recorder + keep-code path; Assistant gated until MCP verified |
| J4 | Run → Allure / Doctor | 4 | 4 | 4 | 4 | 4 | Target picker + Allure merge/clean/tags; evidence-first Doctor |
| J5 | Failed locator → Heal / Fix | 4 | 4 | 4 | 4 | 4 | Noise collapse + page-object generator + Fix-with-Assistant |
| J6 | Mobile platform switch | 4 | 4 | 4 | 4 | 4 | Platform×Execution×Device×App + cloud session awareness |
| J7 | API request → code (late) | — | — | — | — | — | Deferred (#7607 won't-do) |

**J1–J5 means (Discover/Understand/Succeed/Recover):** ≈ 4.15. Feel native mean ≈ 4.0.

**Threshold:** mean ≥ 4.0 across J1–J5 on Discover/Understand/Succeed/Recover — **PASS**. Feel native ≥ 4 — **PASS**.

Refs: epic #7568, SPEC `tools/intellij-plugin-recording/revamp/SPEC.md`, e2e evidence folder above.
