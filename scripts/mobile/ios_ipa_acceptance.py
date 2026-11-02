#!/usr/bin/env python3
"""Gate an iOS IPA (or Info.plist) before cloud upload (#7724).

Fails closed when DTPlatformName is missing or not ``iphoneos``.
Never prints IPA payloads or client identifiers.
"""

from __future__ import annotations

import argparse
import plistlib
import sys
import zipfile
from pathlib import Path


REQUIRED_PLATFORM = "iphoneos"


def find_info_plist_in_ipa(ipa_path: Path) -> bytes:
    """Return raw Info.plist bytes from Payload/*.app/Info.plist inside an IPA."""
    with zipfile.ZipFile(ipa_path) as archive:
        candidates = [
            name
            for name in archive.namelist()
            if name.startswith("Payload/")
            and name.endswith(".app/Info.plist")
            and name.count("/") == 2
        ]
        if not candidates:
            raise FileNotFoundError("no Payload/*.app/Info.plist inside IPA")
        # Prefer the shortest path (top-level app bundle).
        candidates.sort(key=len)
        return archive.read(candidates[0])


def load_plist_bytes(raw: bytes) -> dict:
    return plistlib.loads(raw)


def evaluate_platform(plist_data: dict) -> tuple[bool, str]:
    """Return (ok, message) for DTPlatformName gate."""
    platform = plist_data.get("DTPlatformName")
    if platform is None or (isinstance(platform, str) and not platform.strip()):
        return False, "DTPlatformName missing; refuse upload (need iphoneos device build)"
    platform_s = str(platform).strip()
    if platform_s != REQUIRED_PLATFORM:
        return False, f"DTPlatformName={platform_s!r}; refuse upload (need {REQUIRED_PLATFORM})"
    return True, f"DTPlatformName={REQUIRED_PLATFORM}; acceptance gate passed"


def checklist_lines(ok: bool) -> list[str]:
    return [
        "checklist:",
        f"  [{'x' if ok else ' '}] real-device IPA (not simulator)",
        f"  [{'x' if ok else ' '}] DTPlatformName=iphoneos",
        "  [ ] signing: Development / Ad Hoc / Enterprise (not App-Store-only)",
        "  [ ] arm64 / thinning documented",
        "  [ ] scheme matches backend under test",
    ]


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--ipa", type=Path, help="path to an .ipa archive")
    group.add_argument("--plist", type=Path, help="path to an Info.plist file")
    args = parser.parse_args(argv)

    try:
        if args.ipa is not None:
            raw = find_info_plist_in_ipa(args.ipa)
        else:
            raw = args.plist.read_bytes()
        data = load_plist_bytes(raw)
    except Exception as exc:  # noqa: BLE001 — CLI boundary
        print(f"error: cannot read Info.plist: {exc}", file=sys.stderr)
        for line in checklist_lines(False):
            print(line, file=sys.stderr)
        return 2

    ok, message = evaluate_platform(data)
    stream = sys.stdout if ok else sys.stderr
    print(message, file=stream)
    for line in checklist_lines(ok):
        print(line, file=stream)
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
