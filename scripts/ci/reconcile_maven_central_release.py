#!/usr/bin/env python3
"""On-demand Maven Central release reconciliation.

Additive companion to ``verify_maven_central_release.py`` / ``check_maven_release_version.py``.
A partially-failed reactor deploy (parent POM published, a later module fails on a transient
error) leaves ``check_maven_release_version.py``'s parent-only guard reporting
``already_released=true`` forever, so the missing module is never re-uploaded and
``announce_release`` never runs. This script is meant to be run by hand (or via
``workflow_dispatch``) against a version already reported "released": it finds exactly which
packages are still missing from Central, deploys only those, and then creates the GitHub
Release/Slack announcement if they, too, never ran for this version. Every external action
(deploy, ``gh release``, Slack POST) goes through a plain module-level call site so tests can
monkeypatch it - the real network/subprocess call is never made from a test.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import tempfile
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.ci import verify_maven_central_release as verify  # noqa: E402
from scripts.ci.validate_maven_publication import PUBLIC_ARTIFACTS  # noqa: E402

RELEASE_BODY_TEMPLATE = ROOT / ".github" / "RELEASE_BODY_TEMPLATE.md"

# Reuse validate_maven_publication's packaging-aware, pom-derived artifact map instead of
# assuming "directory name == artifact name" (false for shaft-parent/root and the SHAFT_ENGINE
# relocation pom, which lives in legacy-shaft-engine/).
MODULE_DIR_BY_ARTIFACT = {
    name: str(pom_path.parent) for name, (pom_path, _packaging) in PUBLIC_ARTIFACTS.items()
}


def missing_module_dirs(missing_paths: list[str]) -> list[str]:
    """Reduce missing Central publication paths to their unique reactor module directories."""
    seen: list[str] = []
    for path in missing_paths:
        artifact = path.split("/")[3]
        if artifact not in seen:
            seen.append(artifact)
    return [MODULE_DIR_BY_ARTIFACT[artifact] for artifact in seen]


def build_deploy_command(module_dirs: list[str], gpg_keyname: str, gpg_passphrase: str) -> list[str]:
    """Build a targeted ``mvn deploy -pl`` command, mirroring mavenCentral_cd.yml's deploy flags."""
    return [
        verify.maven_executable(),
        "--batch-mode",
        "deploy",
        "-pl", ",".join(module_dirs),
        "-DskipTests",
        f"-Dgpg.keyname={gpg_keyname}",
        f"-Dgpg.passphrase={gpg_passphrase}",
    ]


def release_exists(version: str) -> bool:
    """Check whether a GitHub Release already exists for this version."""
    result = subprocess.run(["gh", "release", "view", version], cwd=ROOT, check=False)
    return result.returncode == 0


def render_release_body(version: str, template_path: Path = RELEASE_BODY_TEMPLATE) -> str:
    """Render the release body the same way announce_release's "Prepare Release Body" step does."""
    return template_path.read_text(encoding="utf-8").replace("$RELEASE_VERSION", version)


def build_release_create_command(version: str, body_file: Path) -> list[str]:
    """Build the `gh release create` invocation for a version, given a rendered body file."""
    return [
        "gh", "release", "create", version,
        "--title", version,
        "--notes-file", str(body_file),
        "--generate-notes",
    ]


def create_release(version: str) -> str:
    """Create the GitHub Release for a version and return its URL."""
    with tempfile.TemporaryDirectory(prefix="shaft-release-body-") as temp_dir:
        body_file = Path(temp_dir) / "release_body.md"
        body_file.write_text(render_release_body(version), encoding="utf-8")
        command = build_release_create_command(version, body_file)
        result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, check=False)
        if result.returncode != 0:
            raise RuntimeError(
                f"gh release create failed (exit {result.returncode}): "
                f"{result.stderr.strip() or result.stdout.strip()}"
            )
        return result.stdout.strip()


def build_slack_payload(version: str, release_url: str) -> dict:
    """Build the Slack payload, mirroring mavenCentral_cd.yml's inline Python block exactly."""
    summary = (
        f"SHAFT_ENGINE {version} is now available. The release notes are intentionally "
        "minimal and highlight only major new features, breaking changes, and new contributors."
    )
    return {
        "text": f"SHAFT_ENGINE {version} released: {release_url}",
        "blocks": [
            {
                "type": "section",
                "text": {
                    "type": "mrkdwn",
                    "text": f":tada: *SHAFT_ENGINE {version}* is now available!",
                },
            },
            {
                "type": "section",
                "text": {"type": "mrkdwn", "text": summary},
            },
            {
                "type": "actions",
                "elements": [
                    {
                        "type": "button",
                        "text": {"type": "plain_text", "text": "View release notes"},
                        "url": release_url,
                    }
                ],
            },
        ],
    }


def post_slack_notification(webhook_url: str, payload: dict) -> None:
    """POST the release announcement payload to a Slack incoming webhook."""
    request = urllib.request.Request(
        webhook_url,
        data=json.dumps(payload).encode("utf-8"),
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    with urllib.request.urlopen(request, timeout=30):
        pass


def read_reactor_version() -> str:
    """Read the reactor version the same way mavenCentral_cd.yml's release_version step does."""
    result = subprocess.run(
        [
            verify.maven_executable(), "-f", str(ROOT / "pom.xml"), "help:evaluate",
            "-Dexpression=project.version", "-q", "-DforceStdout",
        ],
        cwd=ROOT, capture_output=True, text=True, check=False,
    )
    if result.returncode != 0:
        raise RuntimeError(
            f"Could not read the reactor version: {result.stderr.strip() or result.stdout.strip()}"
        )
    return result.stdout.strip()


def reconcile_release(
    version: str, repository_url: str, gpg_keyname: str, gpg_passphrase: str, dry_run: bool
) -> int:
    """Reconcile one version: deploy missing artifacts, then announce if genuinely new."""
    missing = verify.missing_publication_paths(repository_url, version)
    module_dirs = missing_module_dirs(missing)

    if module_dirs:
        command = build_deploy_command(module_dirs, gpg_keyname, gpg_passphrase)
        if dry_run:
            print(f"[dry-run] missing Maven Central artifacts for modules: {', '.join(module_dirs)}")
            print("[dry-run] would run: " + " ".join(command))
            return 0
        print(f"Missing Maven Central artifacts for modules: {', '.join(module_dirs)}")
        result = subprocess.run(command, cwd=ROOT, check=False)
        if result.returncode != 0:
            raise RuntimeError(
                f"Targeted deploy failed (exit {result.returncode}) for modules: "
                f"{', '.join(module_dirs)}"
            )
        print(f"Deployed missing modules to Maven Central: {', '.join(module_dirs)}")
    else:
        print(f"All Maven Central artifacts already present for {version}.")

    if release_exists(version):
        print(f"GitHub Release {version} already exists.")
        return 0

    if dry_run:
        print(f"[dry-run] would create GitHub Release {version}")
        return 0

    release_url = create_release(version)
    print(f"Created GitHub Release {version}: {release_url}")

    webhook_url = os.environ.get("SLACK_WEBHOOK_URL")
    if not webhook_url:
        print("SLACK_WEBHOOK_URL is not configured; skipping Slack release announcement.")
        return 0

    post_slack_notification(webhook_url, build_slack_payload(version, release_url))
    print("Posted Slack release announcement.")
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--version", default=None,
        help="Release version to reconcile. Default: read from the reactor via mvn help:evaluate.",
    )
    parser.add_argument("--repository-url", default=verify.DEFAULT_REPOSITORY)
    parser.add_argument("--gpg-keyname", default=os.environ.get("GPG_KEYNAME", ""))
    parser.add_argument("--gpg-passphrase", default=os.environ.get("GPG_PASSPHRASE", ""))
    parser.add_argument(
        "--dry-run", action="store_true",
        help="Print the planned actions without executing them.",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    version = args.version or read_reactor_version()
    return reconcile_release(
        version=version,
        repository_url=args.repository_url,
        gpg_keyname=args.gpg_keyname,
        gpg_passphrase=args.gpg_passphrase,
        dry_run=args.dry_run,
    )


if __name__ == "__main__":
    raise SystemExit(main())
