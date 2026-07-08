#!/usr/bin/env python3
"""Assemble one shard's self-contained blob for SHAFT's D8 sharded-merge pipeline.

Stages a shard's ``allure-results`` directory, ``target/shaft-traces`` directory,
and (optionally) a doctor ``execution-intelligence.json`` digest into the
``ShardBlob`` layout that ``ShardMerger``/``ShardMergeCli``/the shaft-mcp
``report_merge_shards`` tool already read:

    <output>/
      allure-results/*.json
      shaft-traces/...             (optional)
      execution-intelligence.json  (optional)

shaft-engine has no dependency on shaft-doctor -- the two are deliberately
kept apart by the reactor's ``enforce-engine-dependency-boundary`` Enforcer
rule -- so the engine cannot run ``shaft-doctor analyze`` itself once a test
run finishes. This script is the post-test orchestration step that closes
that gap. The doctor step is best-effort: pass ``--doctor-command`` with a
full command line to run it, or omit it entirely to stage a blob without
``execution-intelligence.json``; ``ShardBlob``/``ShardMerger`` already treat
that file as optional, so a missing or failing doctor step never blocks
assembling (and later merging) the blob.

Usage:
    scripts/ci/assemble_shard_blob.py --shard 1

    scripts/ci/assemble_shard_blob.py --shard 1 --zip \\
        --doctor-command "java -cp shaft-doctor.jar com.shaft.doctor.cli.DoctorCli analyze --input allure-results --allowed-root . --output-dir target/shaft-doctor"

Exit codes:
    0   blob assembled (with or without execution-intelligence.json)
    2   usage / environment error (e.g. allure-results directory missing)
"""

from __future__ import annotations

import argparse
import shlex
import shutil
import subprocess  # nosec B404
import sys
import zipfile
from pathlib import Path


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument("--shard", required=True, help="Shard label, e.g. '1' for -Dshaft.shard=1/3")
    parser.add_argument(
        "--allure-results", type=Path, default=Path("allure-results"),
        help="Allure results directory for this shard's run (default: allure-results)",
    )
    parser.add_argument(
        "--traces-dir", type=Path, default=Path("target/shaft-traces"),
        help="SHAFT trace directory for this shard's run (default: target/shaft-traces)",
    )
    parser.add_argument(
        "--doctor-command", type=str, default=None,
        help="Full 'shaft-doctor analyze' command line (shell-split) to run before staging; "
        "omit to stage the blob without execution-intelligence.json",
    )
    parser.add_argument(
        "--doctor-output", type=Path, default=Path("target/shaft-doctor"),
        help="Directory the doctor command writes execution-intelligence.json into "
        "(default: target/shaft-doctor)",
    )
    parser.add_argument(
        "--output", type=Path, default=None,
        help="Shard blob output directory (default: target/shard-blobs/shard-<shard>)",
    )
    parser.add_argument("--zip", action="store_true", help="Also produce <output>.zip alongside the directory")
    return parser


def run_doctor(command_line: str) -> bool:
    """Runs the configured shaft-doctor command; returns True on success."""
    parts = shlex.split(command_line)
    if not parts:
        return False
    executable = shutil.which(parts[0]) or parts[0]
    # List-args with no shell=True; the command line is an operator-supplied
    # CI configuration value, not attacker-controlled input.
    completed = subprocess.run(  # nosec B603
        [executable, *parts[1:]], capture_output=True, text=True, check=False
    )
    if completed.returncode != 0:
        print(
            f"WARNING: shaft-doctor analyze failed (exit {completed.returncode}), "
            f"staging blob without execution-intelligence.json:\n{completed.stderr.strip()}",
            file=sys.stderr,
        )
        return False
    return True


def stage_directory(source: Path, destination: Path) -> None:
    """Copies source into destination, replacing any existing directory."""
    if destination.exists():
        shutil.rmtree(destination)
    shutil.copytree(source, destination)


def zip_blob(output: Path) -> Path:
    """Zips the assembled blob directory into <output>.zip and returns its path."""
    zip_path = output.with_suffix(".zip")
    if zip_path.exists():
        zip_path.unlink()
    with zipfile.ZipFile(zip_path, "w", zipfile.ZIP_DEFLATED) as archive:
        for file in sorted(output.rglob("*")):
            if file.is_file():
                archive.write(file, file.relative_to(output))
    return zip_path


def assemble(args: argparse.Namespace) -> Path:
    """Stages the ShardBlob layout under args.output and returns its path."""
    output = (args.output or Path("target") / "shard-blobs" / f"shard-{args.shard}").resolve()
    if output.exists():
        shutil.rmtree(output)
    output.mkdir(parents=True)

    if not args.allure_results.is_dir():
        raise SystemExit(f"Allure results directory not found: {args.allure_results}")
    stage_directory(args.allure_results, output / "allure-results")

    if args.traces_dir.is_dir():
        stage_directory(args.traces_dir, output / "shaft-traces")
    else:
        print(f"No shaft-traces directory at {args.traces_dir}; blob will have no shaft-traces/.", file=sys.stderr)

    if args.doctor_command:
        if run_doctor(args.doctor_command):
            intelligence = args.doctor_output / "execution-intelligence.json"
            if intelligence.is_file():
                shutil.copy2(intelligence, output / "execution-intelligence.json")
            else:
                print(
                    f"shaft-doctor analyze ran but did not produce {intelligence}; "
                    "blob will have no execution-intelligence.json.",
                    file=sys.stderr,
                )
    else:
        print("No --doctor-command supplied; blob will have no execution-intelligence.json.", file=sys.stderr)

    if args.zip:
        print(f"Zipped blob: {zip_blob(output)}")

    return output


def main(argv: list[str] | None = None) -> int:
    """Runs the CLI."""
    args = build_parser().parse_args(argv)
    output = assemble(args)
    print(f"Shard {args.shard} blob assembled at {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
