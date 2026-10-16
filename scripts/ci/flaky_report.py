#!/usr/bin/env python3
"""
Flaky-test report (#7005).

Surefire reruns a failing test once
(-Dsurefire.rerunFailingTestsCount=1); a test that passes on retry is recorded
as <flakyFailure>/<flakyError>. Surface each one as a warning and in the step
summary instead of letting it go green silently. Exit 0: flakes are reported,
not blocking; a test that fails every attempt still fails the build in Maven.
"""
from __future__ import annotations

import sys
import xml.etree.ElementTree as ET  # nosec B405 - local build output only
from pathlib import Path


def flaky_tests(report_dir: Path) -> list[str]:
    found: list[str] = []
    for report in sorted(report_dir.glob("TEST-*.xml")):
        for case in ET.parse(report).getroot().iter("testcase"):  # nosec B314 - trusted local build output
            if case.find("flakyFailure") is not None or case.find("flakyError") is not None:
                found.append(f"{case.get('classname')}#{case.get('name')}")
    return found


def render(module: str, flaky: list[str]) -> str:
    if not flaky:
        return f"### Flaky tests ({module})\nNone: no test passed only on retry.\n"
    lines = [f"### Flaky tests ({module}): {len(flaky)} passed only on retry", ""]
    lines += [f"- `{name}`" for name in flaky]
    lines += ["", "Fix the test or tag it `quarantine` with an owner issue (#7005)."]
    return "\n".join(lines) + "\n"


def main(argv: list[str]) -> int:
    module = argv[1] if len(argv) > 1 else "."
    flaky = flaky_tests(Path(module, "target", "surefire-reports"))
    for name in flaky:
        print(f"::warning title=Flaky test::{name} passed only on retry ({module})")
    print(render(module, flaky), end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
