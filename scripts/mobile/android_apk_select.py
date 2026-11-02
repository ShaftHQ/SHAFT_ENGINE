#!/usr/bin/env python3
"""Select a universal or ABI-split APK for the target device (#7725)."""

from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass


UNIVERSAL = "universal"
KNOWN_KINDS = {UNIVERSAL, "x86_64", "x86", "arm64-v8a", "armeabi-v7a"}


@dataclass(frozen=True)
class ApkCandidate:
    path: str
    kind: str


def normalize_abi(abi: str) -> str:
    value = abi.strip().lower().replace("-", "_")
    aliases = {
        "arm64_v8a": "arm64-v8a",
        "armeabi_v7a": "armeabi-v7a",
        "arm64": "arm64-v8a",
    }
    # Keep hyphenated canonical forms for arm ABIs.
    if abi.strip() in KNOWN_KINDS:
        return abi.strip()
    return aliases.get(value, abi.strip())


def select_apk(target_abi: str, candidates: list[ApkCandidate]) -> ApkCandidate:
    """Choose an APK for ``target_abi``.

    Local x86_64 → universal or x86_64 (never arm64-only).
    Real/cloud arm64 → prefer arm64-v8a, else universal.
    """
    if not candidates:
        raise ValueError("no APK candidates provided")
    abi = normalize_abi(target_abi)
    by_kind = {c.kind: c for c in candidates}

    if abi in {"x86_64", "x86"}:
        if UNIVERSAL in by_kind:
            return by_kind[UNIVERSAL]
        if abi in by_kind:
            return by_kind[abi]
        arm_only = [c for c in candidates if c.kind.startswith("arm")]
        if arm_only and len(arm_only) == len(candidates):
            raise ValueError(
                f"target abi {abi} cannot install arm64-only APKs; need universal or {abi} split"
            )
        raise ValueError(f"no suitable APK for abi {abi}")

    if abi == "arm64-v8a":
        if "arm64-v8a" in by_kind:
            return by_kind["arm64-v8a"]
        if UNIVERSAL in by_kind:
            return by_kind[UNIVERSAL]
        raise ValueError("no arm64-v8a or universal APK for arm64 target")

    # Unknown / other: prefer exact kind, then universal.
    if abi in by_kind:
        return by_kind[abi]
    if UNIVERSAL in by_kind:
        return by_kind[UNIVERSAL]
    raise ValueError(f"no suitable APK for abi {abi}")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--abi", required=True, help="target ABI, e.g. x86_64 or arm64-v8a")
    parser.add_argument(
        "--apk",
        action="append",
        nargs=2,
        metavar=("PATH", "KIND"),
        required=True,
        help="APK path and kind (universal|x86_64|arm64-v8a|...). Repeatable.",
    )
    args = parser.parse_args(argv)

    candidates: list[ApkCandidate] = []
    for path, kind in args.apk:
        kind_n = kind.strip()
        if kind_n not in KNOWN_KINDS:
            print(f"error: unknown kind {kind_n!r}; known={sorted(KNOWN_KINDS)}", file=sys.stderr)
            return 2
        candidates.append(ApkCandidate(path=path, kind=kind_n))

    try:
        chosen = select_apk(args.abi, candidates)
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1

    print(chosen.path)
    print(f"kind={chosen.kind}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
