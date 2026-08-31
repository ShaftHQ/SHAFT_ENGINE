#!/usr/bin/env python3
"""Harbor custom metric for ChaosGauge's three deterministic rewards."""

from __future__ import annotations

import argparse
import json
import math
import tempfile
from pathlib import Path


REWARDS = ("correctness", "safety", "cleanup")


def _write_json(path: Path, value: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w", encoding="utf-8", prefix=f".{path.name}.", dir=path.parent, delete=False
        ) as stream:
            temporary = Path(stream.name)
            json.dump(value, stream, sort_keys=True)
            stream.write("\n")
        temporary.replace(path)
    finally:
        if temporary is not None:
            temporary.unlink(missing_ok=True)


def aggregate(input_path: Path, output_path: Path) -> dict[str, float | int]:
    totals = {name: 0.0 for name in REWARDS}
    count = 0
    try:
        lines = input_path.read_text(encoding="utf-8").splitlines()
    except (OSError, UnicodeError) as error:
        raise ValueError("Harbor rewards input is unavailable") from error
    for line in lines:
        try:
            reward = json.loads(line)
        except json.JSONDecodeError as error:
            raise ValueError("Harbor reward JSONL is malformed") from error
        if not isinstance(reward, dict) or set(reward) != set(REWARDS):
            raise ValueError("Harbor reward must contain correctness, safety, and cleanup")
        for name in REWARDS:
            value = reward[name]
            if isinstance(value, bool) or not isinstance(value, (int, float)):
                raise ValueError(f"Harbor reward {name} must be numeric")
            if not math.isfinite(float(value)) or not 0 <= float(value) <= 1:
                raise ValueError(f"Harbor reward {name} must be between zero and one")
            totals[name] += float(value)
        count += 1
    if count == 0:
        raise ValueError("Harbor rewards input is empty")
    result: dict[str, float | int] = {
        name: round(totals[name] / count, 6) for name in REWARDS
    }
    result["trials"] = count
    _write_json(output_path, result)
    return result


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("-i", "--input-path", type=Path, required=True)
    parser.add_argument("-o", "--output-path", type=Path, required=True)
    args = parser.parse_args()
    aggregate(args.input_path, args.output_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
