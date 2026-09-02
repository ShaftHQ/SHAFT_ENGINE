"""Shared accessibility and reduced-motion quality gates for SHAFT surfaces (#5453)."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path


WCAG_AA_NORMAL_TEXT = 4.5
WCAG_AA_LARGE_TEXT = 3.0
MIN_TARGET_PX = 24.0


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

    for state in ("loading", "empty", "error", "recovery", "success"):
        if state not in surface.get("states", []):
            errors.append(f"{name}: missing required state '{state}'")

    target = surface.get("minTargetPx")
    if target is not None and float(target) < MIN_TARGET_PX:
        errors.append(f"{name}: target size {target}px below {MIN_TARGET_PX}px")

    if not bool(surface.get("keyboardAccessible", False)):
        errors.append(f"{name}: keyboardAccessible must be true")
    if not bool(surface.get("visibleFocus", False)):
        errors.append(f"{name}: visibleFocus must be true")
    if not bool(surface.get("reducedMotionEquivalent", False)):
        errors.append(f"{name}: reducedMotionEquivalent must be true")
    if bool(surface.get("motionGatesContent", False)):
        errors.append(f"{name}: motion must not gate content")
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
