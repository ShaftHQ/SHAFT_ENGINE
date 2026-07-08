#!/usr/bin/env python3
"""Run SHAFT's sharded-execution + merged-report pipeline (D8) end-to-end, locally.

Orchestrates, for a total of N shards:

  1. one Maven test invocation per shard using ``-Dshaft.shard=<i>/<N>``
     (partitioning itself is deterministic and handled entirely by
     ``ShardPartitioner``/``TestNGListener`` inside the test run -- this
     script only drives N separate invocations);
  2. ``scripts/ci/assemble_shard_blob.py`` after each shard run, staging that
     shard's ``allure-results``/``target/shaft-traces``/optional doctor
     ``execution-intelligence.json`` into the ``ShardBlob`` layout; and
  3. one merge step (``ShardMergeCli``) combining every shard blob into one
     Allure result set plus a "speedboard" HTML with cross-shard doctor
     triage.

This intentionally does not replace or touch the existing manual class-filter
CI partitioning in ``.github/workflows/e2eTests.yml`` -- it is a separate,
opt-in recipe for the ``-Dshaft.shard`` mechanism, runnable locally or wired
into a CI matrix job (see ``.github/workflows/sharded-merged-report.yml``,
which runs shards 1..N as parallel jobs and calls this same assemble/merge
machinery across job boundaries via artifact upload/download instead of
in one local process).

Usage:
    scripts/ci/run_sharded_and_merge.py --shards 3 --test "%regex[.*SmokeTest.*]"

    scripts/ci/run_sharded_and_merge.py --shards 3 \\
        --doctor-command "java -cp shaft-doctor.jar com.shaft.doctor.cli.DoctorCli analyze --input allure-results --allowed-root . --output-dir target/shaft-doctor" \\
        --merge-command "java -cp report-aggregate.jar com.shaft.reportaggregate.ShardMergeCli --output {output} {blobs}"

Exit codes:
    0   sharded runs + assembly + merge all completed
    1   a shard's Maven test invocation, blob assembly, or the merge step failed
    2   usage / environment error (e.g. --shards < 1)
"""

from __future__ import annotations

import argparse
import shlex
import shutil
import subprocess  # nosec B404
import sys
from pathlib import Path

# report-aggregate has no runnable-jar packaging (no exec-maven-plugin
# binding, no Main-Class manifest) -- this default drives ShardMergeCli via
# an ad-hoc `exec:java` goal invocation, which Maven can resolve without any
# pom changes. Override with --merge-command once a packaged CLI exists.
DEFAULT_MERGE_COMMAND = (
    'mvn -q -pl report-aggregate exec:java '
    '-Dexec.mainClass=com.shaft.reportaggregate.ShardMergeCli '
    '-Dexec.args="--output {output} {blobs}"'
)


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument("--shards", type=int, required=True, help="Total shard count (the M in -Dshaft.shard=N/M)")
    parser.add_argument("--test", type=str, default=None, help="Optional -Dtest= filter forwarded to every shard's Maven run")
    parser.add_argument(
        "--no-headless", dest="headless", action="store_false", default=True,
        help="Do not force -DheadlessExecution=true (default: headless is forced on)",
    )
    parser.add_argument("--extra-mvn-arg", action="append", default=[], help="Additional -D... argument, may be repeated")
    parser.add_argument("--doctor-command", type=str, default=None, help="Forwarded to assemble_shard_blob.py --doctor-command")
    parser.add_argument("--output", type=Path, default=Path("target/merged-report"), help="Merged report output directory")
    parser.add_argument(
        "--merge-command", type=str, default=None,
        help="Full merge command line (shell-split); {output} and {blobs} placeholders are substituted. "
        "Defaults to a report-aggregate exec:java invocation of ShardMergeCli.",
    )
    parser.add_argument(
        "--skip-tests", action="store_true",
        help="Skip running mvn test per shard (assumes allure-results/target/shaft-traces already exist "
        "from a prior run) -- useful for re-running just assembly+merge",
    )
    return parser


def run(command: list[str], step: str) -> None:
    """Runs one subprocess step to completion, raising with a labeled message on failure."""
    executable = shutil.which(command[0]) or command[0]
    # List-args with no shell=True; every argument is either a fixed literal
    # or an internally-built -D/flag value, never unsanitized external input.
    completed = subprocess.run([executable, *command[1:]])  # nosec B603
    if completed.returncode != 0:
        raise SystemExit(f"{step} failed with exit code {completed.returncode}")


def run_shard_tests(shard: int, args: argparse.Namespace) -> None:
    """Runs one shard's Maven test invocation, matching the -am shape CI already uses."""
    command = ["mvn", "-pl", "shaft-engine", "-am", "test", f"-Dshaft.shard={shard}/{args.shards}"]
    if args.headless:
        command.append("-DheadlessExecution=true")
    if args.test:
        command.append(f"-Dtest={args.test}")
    command.extend(args.extra_mvn_arg)
    run(command, f"Shard {shard} Maven test")


def assemble_shard(shard: int, args: argparse.Namespace) -> Path:
    """Stages one shard's blob via assemble_shard_blob.py and returns its directory."""
    blob_dir = Path("target") / "shard-blobs" / f"shard-{shard}"
    command = [sys.executable, "scripts/ci/assemble_shard_blob.py", "--shard", str(shard), "--output", str(blob_dir)]
    if args.doctor_command:
        command += ["--doctor-command", args.doctor_command]
    run(command, f"Shard {shard} blob assembly")
    return blob_dir


def merge_shards(blob_dirs: list[Path], args: argparse.Namespace) -> None:
    """Runs the configured (or default) merge command over every shard blob."""
    # shlex.split() below applies POSIX escaping rules, where a bare backslash
    # is an escape character -- it silently eats Windows-style path
    # separators (target\shard-blobs\shard-1 -> targetshard-blobsshard-1).
    # Rendering paths as forward slashes sidesteps that entirely; both
    # Windows and POSIX toolchains accept forward-slash paths.
    blobs = " ".join(blob.as_posix() for blob in blob_dirs)
    template = args.merge_command or DEFAULT_MERGE_COMMAND
    rendered = template.format(output=args.output.as_posix(), blobs=blobs)
    run(shlex.split(rendered), "Shard merge")


def main(argv: list[str] | None = None) -> int:
    """Runs the CLI."""
    args = build_parser().parse_args(argv)
    if args.shards < 1:
        raise SystemExit("--shards must be >= 1")

    blob_dirs = []
    for shard in range(1, args.shards + 1):
        if not args.skip_tests:
            run_shard_tests(shard, args)
        blob_dirs.append(assemble_shard(shard, args))

    merge_shards(blob_dirs, args)
    print(f"Merged {args.shards} shard(s) into {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
