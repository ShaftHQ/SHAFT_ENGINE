# Allure Extent Report Operator

Use for report generation and CI artifact behavior. Do not treat a generated summary as valid until raw result files are populated.

## Workflow

1. Separate producers from consumers: `AllureManager`, report properties/paths, `.github/actions/post-test-report`, `.github/actions/consolidate-test-results`, workflows, and `scripts/ci/extract_allure_failures.py`.
2. Preserve the live `allure-results` directory and delete only its contents;
   replacing the root can race with Allure writers on Windows.
3. Count `*-result.json` files before trusting Allure status or `summary.json`.
4. For parser changes, run `python3 -m pytest tests/scripts/test_extract_allure_failures.py`. For Java report internals, run the narrow affected Allure manager tests.
5. Treat Extent as a separate path: verify actual Extent artifacts before claiming support.

## Output

Report producer/consumer impact, raw result count evidence, parser or Java tests run, artifact names, and remaining OS/path risk.
