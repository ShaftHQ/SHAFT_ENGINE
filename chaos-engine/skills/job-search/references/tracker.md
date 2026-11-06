# Tracker hygiene

One row per opening:

| Field | Rule |
| --- | --- |
| Stable ID | Deterministic per opening (job id or post share id) |
| Status | Found / Applied / Not applied / … — Found at discovery; Applied only after real send/submit |
| Source permalink | Channel B post permalink or job page URL (not only a short link) |
| Contact | Recruiter / apply email when present (live tracker only — never in this skill) |
| Apply method | email / external form / ATS / Easy Apply (usually out of scope for this skill) |
| Fit flags | Verbatim eligibility / region / work-mode notes |

The ledger is a cache of this tracker. Read back after every write. The sheet location stays in the private store. Before adding a row, search for the same agency +
contact + client to avoid double-apply on repeat postings.
