#!/usr/bin/env python3
"""Record the owner and gate a likeness checkpoint on those recordings only.

Synthesized introductions are not training audio. This file does not download
weights, rewrite a base model, upload, or publish. With no command it prints
the passage the owner reads aloud.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import subprocess  # nosec B404 - fixed ffmpeg argv, no shell
import sys
from pathlib import Path

REFUSED_SHA256 = frozenset(
    {
        "89c5ae999910b74c4325cd06a6ba590f38305947b67367294fb7918ecb6d030b",
    }
)
REFUSED_NAME_MARKERS = ("qwen-cv-intro",)
SPEAKER_FLOOR = 0.9860
# U+0443 U+043C. Spelling the filler in that writing system moved the accent.
FORBIDDEN_SPELLING = "\u0443\u043c"

READ_ALOUD = (
    "My name is Mohab Mohie. I founded Automatest. "
    "Um. You know, the work is test automation and the voice should stay mine."
)


def read_aloud_script() -> str:
    """Return the passage the owner reads. A phonetic note is not included."""
    if FORBIDDEN_SPELLING in READ_ALOUD:
        raise ValueError("read-aloud script uses a foreign spelling")
    return READ_ALOUD


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def reject_reason(path: Path) -> str | None:
    """Return why this file cannot train a likeness, or None when it may."""
    name = path.name.casefold()
    if any(marker in name for marker in REFUSED_NAME_MARKERS):
        return "synthesized introduction names are not training audio"
    if not path.is_file():
        return "recording is missing"
    if file_sha256(path) in REFUSED_SHA256:
        return "this file is a refused synthesized introduction"
    return None


def adopt_checkpoint(base_cosine: float, candidate_cosine: float) -> bool:
    """True only when the candidate beats the same-run base and the floor."""
    return candidate_cosine > base_cosine and candidate_cosine > SPEAKER_FLOOR


def accepted_manifest(paths: list[Path]) -> dict:
    """Validate recordings and return sha256s. Raises ValueError on a refusal."""
    rows = []
    for path in paths:
        reason = reject_reason(path)
        if reason:
            raise ValueError(f"{path.name}: {reason}")
        rows.append({"name": path.name, "sha256": file_sha256(path)})
    if not rows:
        raise ValueError("no owner recordings")
    return {"recordings": rows, "floor": SPEAKER_FLOOR, "adopt": "above-base-and-floor"}


def record_argv(destination: Path) -> list[str]:
    """ffmpeg argv for one owner take. The caller decides whether to run it."""
    return [
        "ffmpeg",
        "-y",
        "-f",
        "pulse",
        "-i",
        "default",
        "-ac",
        "1",
        "-ar",
        "48000",
        str(destination),
    ]


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Owner recordings for a likeness adapter.")
    parser.add_argument("command", nargs="?", choices=("script", "check", "decide", "record"), default="script")
    parser.add_argument("paths", nargs="*")
    parser.add_argument("--base", type=float)
    parser.add_argument("--candidate", type=float)
    parser.add_argument("--out", type=Path)
    parser.add_argument("--execute-record", action="store_true")
    args = parser.parse_args(argv)
    if args.command == "script":
        print(read_aloud_script())
        return 0
    if args.command == "decide":
        if args.base is None or args.candidate is None:
            print("decide needs --base and --candidate", file=sys.stderr)
            return 2
        adopted = adopt_checkpoint(args.base, args.candidate)
        print("adopt" if adopted else "reject")
        return 0 if adopted else 2
    if args.command == "record":
        if args.out is None:
            print("record needs --out", file=sys.stderr)
            return 2
        argv_record = record_argv(args.out)
        if not args.execute_record:
            print(json.dumps(argv_record))
            return 0
        if shutil.which("ffmpeg") is None:
            print("ffmpeg is not on PATH", file=sys.stderr)
            return 2
        completed = subprocess.run(argv_record, check=False)  # nosec B603 - fixed ffmpeg argv
        return completed.returncode
    try:
        manifest = accepted_manifest([Path(item) for item in args.paths])
    except ValueError as error:
        print(str(error), file=sys.stderr)
        return 2
    print(json.dumps(manifest, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
