# Two-channel discovery

## Channel A — guest jobs API (seed)

- Endpoint shape: `jobs-guest/jobs/api/seeMoreJobPostings/search` with
  `f_WT=2` for remote.
- No login required; cheapest first pass.
- Limitation: "remote" ≈ remote within the posting's country. Combine with
  outside-home-country rules; filter on **stated eligible countries** early.
- Output: seed IDs / titles / locations only — not final eligibility.

## Channel B — recruiter posts (email / external apply)

- Hiring posts ("send CV to …") are where email-apply and many external-apply
  roles live.
- Posts search UI does not reliably expose stable permalinks.
- Permalink path: poster profile → Activity → Posts → Copy link to post →
  `posts/<slug>-share-<id>`.
- Tracker primary URL = that permalink; short links are secondary.

## Order of work

1. Seed via Channel A.
2. Budget Channel B from the start (do not wait until Channel A is empty).
3. Eligibility-first filter on both channels before deep reading.
