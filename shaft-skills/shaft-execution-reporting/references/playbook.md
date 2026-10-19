# Execution-reporting playbook

## Ten practices

1. Name the engineering audience, reporting period, test objective, and decision this report supports. [`ISTQB-TM`]
2. Record provenance: plan, run, build, commit, environment, data, configuration, start, stop, and artifact locations. [`ISTQB-TM`, `ALLURE-RESULTS`]
3. Reconcile selected, started, completed, passed, failed, broken, skipped, blocked, unknown, and not-run totals. [`ALLURE-RESULTS`, `ISTQB-CTFL`]
4. Report requirement, risk, platform, data, and test-type coverage against the approved scope. [`ISTQB-TM`]
5. Group failures by evidence-backed signature and distinguish product, test, data, environment, dependency, and unknown causes. [`ALLURE-RESULTS`, `SHAFT-REPORTING`]
6. Show retry, status-transition, flake, quarantine, duration, and infrastructure trends without hiding unstable passes. [`ALLURE-STABILITY`]
7. Link each confirmed defect, severity, owner, state, affected test, and fix-verification status. [`ISTQB-TM`]
8. Link concise evidence and exact reproduction or focused rerun instructions instead of pasting unbounded logs. [`SHAFT-REPORTING`]
9. State quality-gate values, thresholds, exceptions, blockers, residual risks, and confidence explicitly. [`ISTQB-TM`]
10. End with prioritized technical actions, owners, and next checks; never claim an unobserved execution or resolution. [`ISTQB-TM`, `GOOGLE-SRE-MONITORING`]

## Examples

- Summarize a pull-request run with exact suite counts, one new product failure, two broken tests, retry history, and artifact links.
- Compare a browser matrix by platform, duration, failure signature, unexecuted combinations, and gate outcome.
- Report a regression cycle with risk coverage, defect verification, flake trend, environment incidents, and exit criteria.

## Boundary case

- If raw evidence is empty, stale, or mismatched to the claimed build, report the run as unverified and request the missing provenance; do not calculate a pass rate.

## Output

Return provenance, reconciled results, coverage, clusters, defects, stability,
gate status, risks, evidence links, and owned next actions.

## Execution class

- `historyId` and `testCaseId` include platform, and language when language is a separate cohort, before the report is built. The same platform and language keep both ids. A parameter that already includes platform is not counted again. A shared display name across platforms or languages is a broken report, not a retry. Do not fix this by editing the generated HTML.
- Defect ids match whole tokens. `SHAFT#6940` is a token. A hostname, path, or device name that merely contains the same digits is not. The id also has to be on the throwable (#6947), not only in a report string.
- Decode base64 report blobs before a secret scan. A hit blocks publish and must not echo the secret. A plaintext grep is not success. Checking `browserstack.yml` is not enough (#6826 stays).
- Never commit a generated report. Allure HTML, `allure-results/` and `allure-report/` are build output. Attach the evidence to the PR or ticket instead. The `tools/git-hooks` secret guard blocks a report path, a known secret value (raw or in a decoded blob), a JWT-like token or a secret assignment. Do not bypass it with `--no-verify`; fix a false positive in the guard or in `.secret-guard-allow`. Audit a branch with `python3 tools/git-hooks/secret_guard.py --rev origin/main..HEAD` (#7177).
- If a leak was already pushed: make a bundle backup, then rewrite only the affected branch, with the owner's approval, and push with `--force-with-lease`. Rotate every exposed credential, because deleting a file does not un-leak it. Scan the PR or ticket attachments too. They cannot be replaced, only deleted (#7177).
- Attach with `SHAFT.Report.attach(type, name, content)`, not raw `Allure.attachment`. If raw Allure is unavoidable, its order is `(name, type, content[, options])`, the opposite of SHAFT's type-first order; a swapped call compiles and the report shows the content as the type. SHAFT warns about such calls and about non-MIME attachment types before generating the report. After adding a new attachment, open it once in the generated report (#7185).
- Allure 3 suite widgets may be base64. A plaintext miss is not an empty report. Decode the widget with a bounded scan. Do not load the whole single-file HTML as one string (#3407). A name in plaintext and in base64 counts once. An empty results directory stays unverified.
- A later run deletes `allure-results` when `allure.cleanResultsDirectory` is true (the default). Keep a previous cohort by turning that property off or by copying into the project workspace first, for example `target/allure-results-kept`.
- The delivery report lists each test name. Platform and language are distinguishable on the name or the suite. A pass count alone is not the report. A count that does not match the visible names fails the check.
