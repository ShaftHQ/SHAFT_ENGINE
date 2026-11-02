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

## Journeys

| ID | Journey | Discover | Understand | Succeed | Recover | Feel native | Notes / evidence |
|---|---|---|---|---|---|---|---|
| J1 | Install + first-run | | | | | | |
| J2 | New SHAFT project | | | | | | |
| J3 | Record → keep code | | | | | | |
| J4 | Run → Allure / Doctor | | | | | | |
| J5 | Failed locator → Heal / Fix | | | | | | |
| J6 | Mobile platform switch (Phase 2b) | | | | | | |
| J7 | API request → code (late) | | | | | | |

**Threshold (release gate):** mean ≥ 4.0 across J1–J5 on Discover/Understand/Succeed/Recover; Feel native ≥ 3 until Phase 1 shell lands, then ≥ 4.

Machine: ≥8 GiB free. Shared agent box is not evidence (SPEC C7).

Human labels (O1 default until scorecard decides): Assistant / Tests / Setup ↔ Agent / Workflow+log / Setup.

Refs: epic #7568, W0.4, SPEC `tools/intellij-plugin-recording/revamp/SPEC.md`.
