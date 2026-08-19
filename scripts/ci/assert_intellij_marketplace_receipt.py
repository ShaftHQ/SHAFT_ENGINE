#!/usr/bin/env python3
"""Fail if publishPlugin left no Gradle or Marketplace receipt for this version.

Issue #5221: a cancelled verify+publish composite looked partly healthy because
coverage uploaded `if: always()`. A successful Marketplace publish must leave a
Gradle `BUILD SUCCESSFUL` receipt and the version must appear on plugin 32529.
"""

from __future__ import annotations

import argparse
import json
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

MARKETPLACE_PLUGIN_ID = 32529
MARKETPLACE_UPDATES_URL = (
    f"https://plugins.jetbrains.com/api/plugins/{MARKETPLACE_PLUGIN_ID}/updates"
)
USER_AGENT = "SHAFT_ENGINE-ci-intellij-marketplace-receipt"


def plugin_version_from_properties(text: str) -> str:
    for line in text.splitlines():
        stripped = line.strip()
        if stripped.startswith("pluginVersion="):
            version = stripped.split("=", 1)[1].strip()
            if version:
                return version
    raise ValueError("pluginVersion is missing from gradle.properties")


def gradle_receipt_errors(log_text: str) -> list[str]:
    if "BUILD SUCCESSFUL" not in log_text:
        return ["Gradle publish log is missing BUILD SUCCESSFUL"]
    return []


def marketplace_versions(updates_json: str) -> list[str]:
    payload = json.loads(updates_json)
    if isinstance(payload, dict):
        payload = payload.get("updates") or payload.get("plugins") or payload.get("data") or []
    if not isinstance(payload, list):
        raise ValueError("Marketplace updates payload is not a list")
    versions: list[str] = []
    for item in payload:
        if isinstance(item, dict) and item.get("version"):
            versions.append(str(item["version"]))
    return versions


def marketplace_receipt_errors(updates_json: str, version: str) -> list[str]:
    try:
        versions = marketplace_versions(updates_json)
    except (ValueError, json.JSONDecodeError) as error:
        return [f"Marketplace updates JSON is unreadable: {error}"]
    if version not in versions:
        return [
            f"Marketplace plugin {MARKETPLACE_PLUGIN_ID} does not list version {version}"
        ]
    return []


def receipt_errors(log_text: str, properties_text: str, updates_json: str) -> list[str]:
    errors = gradle_receipt_errors(log_text)
    try:
        version = plugin_version_from_properties(properties_text)
    except ValueError as error:
        return errors + [str(error)]
    errors.extend(marketplace_receipt_errors(updates_json, version))
    return errors


def fetch_updates(url: str = MARKETPLACE_UPDATES_URL, timeout: int = 30) -> str:
    request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    with urllib.request.urlopen(request, timeout=timeout) as response:
        return response.read().decode("utf-8")


def fetch_updates_with_retry(
    url: str = MARKETPLACE_UPDATES_URL,
    attempts: int = 5,
    delay_seconds: float = 15,
    sleeper=time.sleep,
    fetcher=fetch_updates,
    version: str | None = None,
) -> str:
    last_error: Exception | None = None
    last_json: str | None = None
    for attempt in range(1, attempts + 1):
        try:
            updates_json = fetcher(url)
        except (urllib.error.URLError, TimeoutError, OSError) as error:
            last_error = error
            if attempt < attempts:
                sleeper(delay_seconds)
            continue
        last_json = updates_json
        if version is None:
            return updates_json
        try:
            versions = marketplace_versions(updates_json)
        except (ValueError, json.JSONDecodeError):
            return updates_json
        if version in versions:
            return updates_json
        if attempt < attempts:
            sleeper(delay_seconds)
    if last_json is not None:
        return last_json
    raise RuntimeError(f"Marketplace updates fetch failed: {last_error}") from last_error


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--log", type=Path, required=True)
    parser.add_argument("--properties", type=Path, required=True)
    parser.add_argument("--updates-json", type=Path)
    parser.add_argument("--updates-url", default=MARKETPLACE_UPDATES_URL)
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    log_text = args.log.read_text(encoding="utf-8")
    properties_text = args.properties.read_text(encoding="utf-8")
    if args.updates_json is not None:
        updates_json = args.updates_json.read_text(encoding="utf-8")
    else:
        try:
            version = plugin_version_from_properties(properties_text)
        except ValueError as error:
            print(str(error), file=sys.stderr)
            return 1
        try:
            updates_json = fetch_updates_with_retry(
                url=args.updates_url,
                version=version,
            )
        except RuntimeError as error:
            print(str(error), file=sys.stderr)
            return 1
    errors = receipt_errors(log_text, properties_text, updates_json)
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    version = plugin_version_from_properties(properties_text)
    print(
        f"Marketplace receipt confirmed for plugin {MARKETPLACE_PLUGIN_ID} version {version}."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
