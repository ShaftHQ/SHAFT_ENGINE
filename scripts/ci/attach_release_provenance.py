#!/usr/bin/env python3
"""Attach a missing SLSA provenance bundle to an existing GitHub Release.

Scorecard Signed-Releases looks for a ``*.intoto.jsonl`` release asset. The
publish workflow stages one on a new release; reconcile does not. This script
uploads ``SHAFT_ENGINE-<version>.intoto.jsonl`` only when that release has no
``.intoto.jsonl`` asset and a bundle file is on disk. A missing bundle is not
a reconcile failure. Other release assets are left untouched (no ``--clobber``).
"""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess  # nosec B404 - fixed argv gh release calls, never shell=True.
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))


def gh_executable() -> str:
    """Return the resolved GitHub CLI executable for fixed release commands."""
    executable = shutil.which("gh")
    if executable is None:
        raise RuntimeError("GitHub CLI is required to attach release provenance")
    return executable


def asset_name(version: str) -> str:
    """Return the release asset name Scorecard expects for this version."""
    return f"SHAFT_ENGINE-{version}.intoto.jsonl"


def provenance_asset_present(version: str) -> bool:
    """Return whether the release already has any ``*.intoto.jsonl`` asset."""
    result = subprocess.run(  # nosec B603 - fixed argv, no shell.
        [gh_executable(), "release", "view", version, "--json", "assets"],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        detail = (result.stderr or result.stdout).strip()
        raise RuntimeError(f"could not inspect GitHub Release {version}: {detail}")
    try:
        assets = json.loads(result.stdout)["assets"]
        names = [str(asset["name"]) for asset in assets]
    except (KeyError, TypeError, json.JSONDecodeError) as error:
        raise RuntimeError(f"invalid GitHub Release asset data for {version}") from error
    return any(name.endswith(".intoto.jsonl") for name in names)


def resolve_version(version: str | None) -> str:
    """Use ``--version`` when set; otherwise the checked-out reactor version."""
    if version:
        return version
    from scripts.ci.reconcile_maven_central_release import read_reactor_version

    return read_reactor_version()


def attach_release_provenance(version: str, bundle: Path | None, dry_run: bool) -> int:
    """Upload ``bundle`` under the provenance asset name, or skip cleanly."""
    if provenance_asset_present(version):
        print(f"Provenance bundle already present on GitHub Release {version}.")
        return 0
    destination = asset_name(version)
    if dry_run:
        print(f"[dry-run] would run: gh release upload {version} {destination}")
        return 0
    if bundle is None or not bundle.is_file():
        print("Provenance bundle is unavailable.")
        return 0
    with tempfile.TemporaryDirectory(prefix="shaft-release-provenance-") as temp_dir:
        staged = Path(temp_dir) / destination
        shutil.copyfile(bundle, staged)
        command = [gh_executable(), "release", "upload", version, str(staged)]
        result = subprocess.run(  # nosec B603 - fixed argv, no shell, no --clobber.
            command,
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=False,
        )
        if result.returncode != 0:
            detail = (result.stderr or result.stdout).strip()
            raise RuntimeError(f"gh release upload failed (exit {result.returncode}): {detail}")
    print(f"Uploaded {destination} to GitHub Release {version}.")
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--version",
        default=None,
        help="GitHub Release tag. Default: read from the reactor via mvn help:evaluate.",
    )
    parser.add_argument(
        "--bundle",
        default=None,
        help="Attestation bundle to upload. A missing file skips the upload.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print the upload without calling gh release upload.",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    bundle = Path(args.bundle) if args.bundle else None
    return attach_release_provenance(resolve_version(args.version), bundle, args.dry_run)


if __name__ == "__main__":
    raise SystemExit(main())
