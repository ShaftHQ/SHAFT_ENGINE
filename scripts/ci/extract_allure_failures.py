#!/usr/bin/env python3
"""Extract failed and broken test cases from Allure result/report artifacts.

The script accepts one or more Allure result directories, generated report
folders, JSON files, or ZIP archives. It prints a Markdown table by default so
CI jobs and issue triage notes can include exact failing methods and reasons.
"""

import argparse
import json
import textwrap
import zipfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Iterator

FAILING_STATUSES = {"failed", "broken"}


@dataclass(frozen=True)
class AllureFailure:
    source: str
    status: str
    method: str
    reason: str
    trace_top: str


def _iter_json_payloads(path: Path) -> Iterator[tuple[str, dict]]:
    if path.is_dir():
        for json_path in sorted(path.rglob("*.json")):
            yield from _iter_json_payloads(json_path)
        return

    if zipfile.is_zipfile(path):
        with zipfile.ZipFile(path) as archive:
            for member in sorted(archive.namelist()):
                if member.endswith(".json"):
                    try:
                        yield member, json.loads(archive.read(member).decode("utf-8"))
                    except (UnicodeDecodeError, json.JSONDecodeError):
                        continue
        return

    if path.suffix == ".json":
        try:
            yield str(path), json.loads(path.read_text(encoding="utf-8"))
        except json.JSONDecodeError:
            return


def _clean_cell(value: str) -> str:
    value = " ".join(str(value or "").split())
    return value.replace("|", "\\|")


def _first_trace_line(trace: str) -> str:
    for line in str(trace or "").splitlines():
        stripped = line.strip()
        if stripped:
            return stripped
    return ""


def _status_details(payload: dict) -> tuple[str, str]:
    details = payload.get("statusDetails") or {}
    message = details.get("message") or details.get("known") or ""
    trace = details.get("trace") or ""
    reason = str(message).strip() or _first_trace_line(trace) or "No failure message recorded"
    return reason, _first_trace_line(trace)


def _method_name(payload: dict) -> str:
    labels = payload.get("labels") or []
    label_map = {label.get("name"): label.get("value") for label in labels if isinstance(label, dict)}
    package = label_map.get("package")
    test_class = label_map.get("testClass") or label_map.get("suite")
    method = label_map.get("testMethod")
    if package and test_class and method and not str(test_class).startswith(str(package)):
        return f"{package}.{test_class}.{method}"
    if test_class and method:
        return f"{test_class}.{method}"
    return payload.get("fullName") or payload.get("name") or payload.get("uid") or payload.get("uuid") or "<unknown>"


def extract_failures(paths: Iterable[Path]) -> list[AllureFailure]:
    failures: list[AllureFailure] = []
    seen: set[tuple[str, str, str, str]] = set()
    for path in paths:
        for source, payload in _iter_json_payloads(path):
            status = str(payload.get("status") or "").lower()
            if status not in FAILING_STATUSES:
                continue
            method = _method_name(payload)
            reason, trace_top = _status_details(payload)
            key = (source, status, method, reason)
            if key in seen:
                continue
            seen.add(key)
            failures.append(AllureFailure(source, status, method, reason, trace_top))
    return failures


def to_markdown(failures: list[AllureFailure]) -> str:
    if not failures:
        return "No failed or broken Allure test cases were found."

    rows = [
        "| Status | Test method | Failure reason | Top trace frame | Source |",
        "|---|---|---|---|---|",
    ]
    for failure in failures:
        rows.append(
            "| {status} | `{method}` | {reason} | `{trace}` | `{source}` |".format(
                status=_clean_cell(failure.status),
                method=_clean_cell(failure.method),
                reason=_clean_cell(failure.reason),
                trace=_clean_cell(failure.trace_top or "n/a"),
                source=_clean_cell(failure.source),
            )
        )
    return "\n".join(rows)


def to_json(failures: list[AllureFailure]) -> str:
    return json.dumps([failure.__dict__ for failure in failures], indent=2, sort_keys=True)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Extract failed and broken test methods from Allure JSON artifacts.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent(
            """
            Examples:
              scripts/ci/extract_allure_failures.py allure-results
              scripts/ci/extract_allure_failures.py artifacts/*.zip --format json
            """
        ),
    )
    parser.add_argument("paths", nargs="+", type=Path, help="Allure result/report path, JSON file, or ZIP archive")
    parser.add_argument("--format", choices=("markdown", "json"), default="markdown", help="Output format")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    failures = extract_failures(args.paths)
    print(to_json(failures) if args.format == "json" else to_markdown(failures))
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
