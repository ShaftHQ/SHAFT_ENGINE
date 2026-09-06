"""Shared accessibility and reduced-motion quality gates for SHAFT surfaces (#5453)."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path


WCAG_AA_NORMAL_TEXT = 4.5
WCAG_AA_LARGE_TEXT = 3.0
MIN_TARGET_PX = 24.0
MAX_LAYOUT_SHIFT = 0.1
MAX_MOTION_DURATION_MS = 500.0
REQUIRED_STATES = (
    "loading",
    "empty",
    "partial",
    "error",
    "recovery",
    "success",
)
REQUIRED_BOOLS = (
    "keyboardAccessible",
    "visibleFocus",
    "labeledControls",
    "statusMessages",
    "narrowLayoutOk",
    "reducedMotionEquivalent",
    "avoidsLayoutShiftAnimation",
    "actionLabelsConsistent",
    "hasPerformanceEvidence",
    "hasVisualRegressionEvidence",
)


def linearize(channel: float) -> float:
    channel = channel / 255.0
    return channel / 12.92 if channel <= 0.03928 else ((channel + 0.055) / 1.055) ** 2.4


def relative_luminance(rgb: tuple[int, int, int]) -> float:
    red, green, blue = (linearize(float(value)) for value in rgb)
    return 0.2126 * red + 0.7152 * green + 0.0722 * blue


def contrast_ratio(first: tuple[int, int, int], second: tuple[int, int, int]) -> float:
    lighter = max(relative_luminance(first), relative_luminance(second))
    darker = min(relative_luminance(first), relative_luminance(second))
    return (lighter + 0.05) / (darker + 0.05)


def parse_hex_color(value: str) -> tuple[int, int, int]:
    text = value.strip().lstrip("#")
    if len(text) != 6:
        raise ValueError(f"expected #RRGGBB color, got {value!r}")
    return int(text[0:2], 16), int(text[2:4], 16), int(text[4:6], 16)


def evaluate_surface(surface: dict[str, object]) -> list[str]:
    errors: list[str] = []
    name = str(surface.get("name", "<unnamed>"))
    foreground = parse_hex_color(str(surface["foreground"]))
    background = parse_hex_color(str(surface["background"]))
    large = bool(surface.get("largeText", False))
    minimum = WCAG_AA_LARGE_TEXT if large else WCAG_AA_NORMAL_TEXT
    ratio = contrast_ratio(foreground, background)
    if ratio < minimum:
        errors.append(f"{name}: contrast {ratio:.2f}:1 below WCAG AA {minimum}:1")

    states = surface.get("states", [])
    if not isinstance(states, list):
        errors.append(f"{name}: states must be a list")
        states = []
    for state in REQUIRED_STATES:
        if state not in states:
            errors.append(f"{name}: missing required state '{state}'")

    target = surface.get("minTargetPx")
    if target is None or float(target) < MIN_TARGET_PX:
        errors.append(f"{name}: target size below {MIN_TARGET_PX}px")

    for field in REQUIRED_BOOLS:
        if not bool(surface.get(field, False)):
            errors.append(f"{name}: {field} must be true")

    if bool(surface.get("motionGatesContent", False)):
        errors.append(f"{name}: motion must not gate content")

    purpose = str(surface.get("motionPurpose", "none"))
    if purpose not in {"none", "state", "causality"}:
        errors.append(f"{name}: motionPurpose must be none|state|causality")
    elif purpose == "none" and float(surface.get("maxMotionDurationMs", 0) or 0) > 0:
        errors.append(f"{name}: motionPurpose none forbids timed motion")

    layout_shift = surface.get("maxLayoutShift")
    if layout_shift is None or float(layout_shift) > MAX_LAYOUT_SHIFT:
        errors.append(f"{name}: maxLayoutShift must be <= {MAX_LAYOUT_SHIFT}")

    motion_ms = surface.get("maxMotionDurationMs")
    if motion_ms is None or float(motion_ms) > MAX_MOTION_DURATION_MS:
        errors.append(f"{name}: maxMotionDurationMs must be <= {MAX_MOTION_DURATION_MS}")

    return errors


def load_manifest(path: Path) -> list[dict[str, object]]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    surfaces = payload.get("surfaces")
    if not isinstance(surfaces, list) or not surfaces:
        raise ValueError("manifest must contain a non-empty surfaces array")
    return surfaces


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("manifest", type=Path, help="JSON manifest of representative surfaces")
    args = parser.parse_args(argv)
    errors: list[str] = []
    for surface in load_manifest(args.manifest):
        errors.extend(evaluate_surface(surface))
    if errors:
        print("accessibility quality gates failed:", file=sys.stderr)
        for error in errors:
            print(f"- {error}", file=sys.stderr)
        return 1
    print(f"accessibility quality gates passed for {args.manifest}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
