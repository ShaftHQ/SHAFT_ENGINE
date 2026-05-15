#!/usr/bin/env python3
"""Extract failed and broken test cases from Allure result/report artifacts.

The script accepts one or more Allure result directories, generated report
folders, JSON files, or ZIP archives. It prints a Markdown table by default so
CI jobs and issue triage notes can include exact failing methods and reasons.
Allure failed and broken results are the single source of truth for the
process exit status and Markdown failure table. Surefire XML can be parsed by
helper functions for diagnostics, but CLI Markdown output intentionally stays
limited to Allure failures so CI logs match the Allure summary.
"""

import argparse
import base64
import json
import re
import textwrap
import xml.etree.ElementTree as ET
import zipfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable, Iterator

FAILING_STATUSES = {"failed", "broken"}


@dataclass(frozen=True)
class AllureFailure:
    source: str
    status: str
    method: str
    reason: str
    trace_top: str


@dataclass(frozen=True)
class SurefireFailure:
    source: str
    status: str
    method: str
    reason: str
    trace_top: str


def _iter_embedded_html_payloads(path: Path) -> Iterator[tuple[str, dict[str, Any]]]:
    try:
        html = path.read_text(encoding="utf-8", errors="ignore")
    except OSError:
        return

    for match in re.finditer(r'd\("([^"\\]+\.json)","([A-Za-z0-9+/=]+)"\)', html):
        source, encoded_payload = match.groups()
        if not source.startswith("data/test-results/"):
            continue
        try:
            payload = json.loads(base64.b64decode(encoded_payload).decode("utf-8"))
        except (UnicodeDecodeError, json.JSONDecodeError, ValueError):
            continue
        if isinstance(payload, dict):
            yield f"{path}!{source}", payload


def _iter_json_payloads(path: Path) -> Iterator[tuple[str, dict[str, Any]]]:
    if path.is_dir():
        for json_path in sorted(path.rglob("*.json")):
            yield from _iter_json_payloads(json_path)
        for html_path in sorted(path.rglob("*.html")):
            yield from _iter_json_payloads(html_path)
        return

    if zipfile.is_zipfile(path):
        with zipfile.ZipFile(path) as archive:
            for member in sorted(archive.namelist()):
                if member.endswith(".json"):
                    try:
                        payload = json.loads(archive.read(member).decode("utf-8"))
                    except (UnicodeDecodeError, json.JSONDecodeError):
                        continue
                    if isinstance(payload, dict):
                        yield member, payload
        return

    if path.suffix == ".json":
        try:
            payload = json.loads(path.read_text(encoding="utf-8"))
            if isinstance(payload, dict):
                yield str(path), payload
        except json.JSONDecodeError:
            return
        return

    if path.suffix == ".html":
        yield from _iter_embedded_html_payloads(path)


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
    details = payload.get("statusDetails") or payload.get("error") or {}
    message = details.get("message") or details.get("known") or payload.get("statusMessage") or ""
    trace = details.get("trace") or payload.get("statusTrace") or ""
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


def _iter_surefire_xml_files(path: Path) -> Iterator[Path]:
    if path.is_dir():
        for xml_path in sorted(path.rglob("TEST-*.xml")):
            yield xml_path
        return
    if path.suffix == ".xml":
        yield path


def _surefire_case_method(testcase: ET.Element) -> str:
    class_name = testcase.get("classname") or ""
    name = testcase.get("name") or ""
    if class_name and name and not name.startswith(f"{class_name}."):
        return f"{class_name}.{name}"
    return name or class_name or "<unknown>"


def _surefire_failure_details(element: ET.Element) -> tuple[str, str]:
    message = element.get("message") or element.get("type") or ""
    trace = element.text or ""
    reason = str(message).strip() or _first_trace_line(trace) or "No failure message recorded"
    return reason, _first_trace_line(trace)


def extract_surefire_failures(paths: Iterable[Path]) -> list[SurefireFailure]:
    failures: list[SurefireFailure] = []
    seen: set[tuple[str, str, str, str]] = set()
    for path in paths:
        for xml_path in _iter_surefire_xml_files(path):
            try:
                root = ET.parse(xml_path).getroot()
            except ET.ParseError:
                continue
            for testcase in root.iter("testcase"):
                failure_element = testcase.find("failure")
                error_element = testcase.find("error")
                result_element = failure_element if failure_element is not None else error_element
                if result_element is None:
                    continue
                status = "failure" if failure_element is not None else "error"
                method = _surefire_case_method(testcase)
                reason, trace_top = _surefire_failure_details(result_element)
                key = (str(xml_path), status, method, reason)
                if key in seen:
                    continue
                seen.add(key)
                failures.append(SurefireFailure(str(xml_path), status, method, reason, trace_top))
    return failures


def _methods_match(left: str, right: str) -> bool:
    if left == right:
        return True
    return left.endswith(f".{right}") or right.endswith(f".{left}")


def missing_surefire_failures(
    allure_failures: Iterable[AllureFailure], surefire_failures: Iterable[SurefireFailure]
) -> list[SurefireFailure]:
    allure_methods = [failure.method for failure in allure_failures]
    return [
        failure
        for failure in surefire_failures
        if not any(_methods_match(failure.method, allure_method) for allure_method in allure_methods)
    ]


def _is_generated_report_non_test_result(source: str) -> bool:
    normalized = source.replace("\\", "/")
    is_report_data = normalized.startswith("data/") or "/data/" in normalized or "!data/" in normalized
    is_test_result = _is_generated_report_test_result_source(normalized)
    return is_report_data and not is_test_result


def _is_generated_report_test_result_source(source: str) -> bool:
    normalized = source.replace("\\", "/")
    return (
        normalized.startswith("data/test-results/")
        or "/data/test-results/" in normalized
        or "!data/test-results/" in normalized
    )


def _is_raw_allure_result_source(source: str) -> bool:
    normalized = source.replace("\\", "/")
    return (
        normalized.endswith("-result.json")
        or "/allure-results/" in normalized
        or normalized.startswith("allure-results/")
        or "!allure-results/" in normalized
    )


def _is_allure_test_result_payload(source: str, payload: dict[str, Any]) -> bool:
    if _is_generated_report_test_result_source(source) or _is_raw_allure_result_source(source):
        return True

    labels = payload.get("labels")
    return bool(
        payload.get("fullName")
        or payload.get("historyId")
        or payload.get("testCaseId")
        or payload.get("uuid")
        or (isinstance(labels, list) and labels)
    )


def extract_failures(paths: Iterable[Path]) -> list[AllureFailure]:
    failures: list[AllureFailure] = []
    seen: set[tuple[str, str, str, str]] = set()
    for path in paths:
        for source, payload in _iter_json_payloads(path):
            if _is_generated_report_non_test_result(source):
                continue
            if not _is_allure_test_result_payload(source, payload):
                continue
            status = str(payload.get("status") or "").lower()
            if status not in FAILING_STATUSES or payload.get("hidden") is True:
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


def to_surefire_markdown(failures: list[SurefireFailure]) -> str:
    if not failures:
        return "No Surefire failures were missing from Allure results."

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


def to_comparison_json(
    allure_failures: list[AllureFailure],
    surefire_failures: list[SurefireFailure],
    missing_failures: list[SurefireFailure],
) -> str:
    return json.dumps(
        {
            "allureFailures": [failure.__dict__ for failure in allure_failures],
            "surefireFailures": [failure.__dict__ for failure in surefire_failures],
            "missingSurefireFailures": [failure.__dict__ for failure in missing_failures],
        },
        indent=2,
        sort_keys=True,
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Extract failed and broken test methods from Allure JSON artifacts.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent(
            """
            Examples:
              scripts/ci/extract_allure_failures.py allure-results
              scripts/ci/extract_allure_failures.py allure-results --surefire-reports target/surefire-reports --format json
              scripts/ci/extract_allure_failures.py artifacts/*.zip --format json
            """
        ),
    )
    parser.add_argument("paths", nargs="+", type=Path, help="Allure result/report path, JSON file, or ZIP archive")
    parser.add_argument(
        "--surefire-reports",
        nargs="*",
        type=Path,
        default=[],
        help=(
            "Optional Surefire XML report path(s) for JSON diagnostics only; "
            "Allure remains the CLI Markdown and exit-code source of truth"
        ),
    )
    parser.add_argument("--format", choices=("markdown", "json"), default="markdown", help="Output format")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    allure_failures = extract_failures(args.paths)
    surefire_failures = extract_surefire_failures(args.surefire_reports)
    missing_failures = missing_surefire_failures(allure_failures, surefire_failures)

    if args.format == "json":
        output = (
            to_comparison_json(allure_failures, surefire_failures, missing_failures)
            if args.surefire_reports
            else to_json(allure_failures)
        )
    else:
        output = to_markdown(allure_failures)
    print(output)
    return 1 if allure_failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
