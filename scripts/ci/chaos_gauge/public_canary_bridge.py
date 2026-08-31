#!/usr/bin/env python3
"""Fail-closed credential and evidence boundary for public ChaosGauge canaries."""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import io
import json
import os
import re
import subprocess
import tempfile
import urllib.error
import urllib.request
import zipfile
from pathlib import Path
from typing import Callable

from scripts.ci.validate_shaft_pilot_release import CREDENTIAL_PATTERNS, SECRET_CANARIES


PRIVATE_REPOSITORY = "ShaftHQ/ChaosGauge-private"
PRIVATE_COMMIT = "08551a3db4376438acddd77422554ce710a58624"
RUN_ID = re.compile(r"[1-9][0-9]{0,18}")
BUNDLE_FILES = ("raw.json", "receipt.json")


def _error(message: str) -> ValueError:
    return ValueError(f"public canary bridge: {message}")


def _metadata(token: str, opener: Callable[..., object] = urllib.request.urlopen) -> tuple[dict[str, object], str]:
    if not token:
        raise _error("GH_TOKEN is unavailable")
    request = urllib.request.Request(
        f"https://api.github.com/repos/{PRIVATE_REPOSITORY}",
        headers={
            "Accept": "application/vnd.github+json",
            "Authorization": f"Bearer {token}",
            "X-GitHub-Api-Version": "2026-03-10",
        },
    )
    try:
        with opener(request, timeout=20) as response:
            value = json.loads(response.read().decode("utf-8"))
            scopes = str(response.headers.get("X-OAuth-Scopes", ""))
    except (OSError, urllib.error.HTTPError, json.JSONDecodeError) as error:
        raise _error("GH_TOKEN metadata is unavailable") from error
    if not isinstance(value, dict):
        raise _error("GH_TOKEN metadata is invalid")
    return value, scopes


def preflight(token: str, opener: Callable[..., object] = urllib.request.urlopen) -> None:
    """Prove private read/release capability with metadata only; never probe-write."""
    metadata, scopes = _metadata(token, opener)
    permissions = metadata.get("permissions")
    if (
        metadata.get("private") is not True
        or not isinstance(permissions, dict)
        or permissions.get("pull") is not True
        or permissions.get("push") is not True
    ):
        raise _error("GH_TOKEN lacks private release capability")
    granted = {scope.strip() for scope in scopes.split(",") if scope.strip()}
    if not {"repo", "workflow"}.issubset(granted):
        raise _error("GH_TOKEN release scopes are not provable")


def _canary_module():
    path = Path(__file__).with_name("canary.py")
    spec = importlib.util.spec_from_file_location("chaos_gauge_canary", path)
    if spec is None or spec.loader is None:
        raise _error("canary validation is unavailable")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _safe_file(path: Path) -> bytes:
    if not path.is_file() or path.is_symlink():
        raise _error("evidence file is unavailable")
    return path.read_bytes()


def _scan(label: str, content: bytes) -> None:
    if any(marker in content for marker in SECRET_CANARIES) or any(pattern.search(content) for pattern in CREDENTIAL_PATTERNS):
        raise _error(f"{label} contains a secret-shaped value")


def _validate_contents(raw_content: bytes, receipt_content: bytes, repository: Path) -> None:
    _scan("raw evidence", raw_content)
    _scan("sanitized receipt", receipt_content)
    try:
        _canary_module().validate_public_evidence(json.loads(receipt_content.decode("utf-8")), repository=repository)
    except (OSError, UnicodeDecodeError, ValueError, json.JSONDecodeError) as error:
        raise _error("sanitized receipt is invalid") from error


def validate(raw: Path, receipt: Path, repository: Path) -> None:
    """Reject unsafe raw data and receipt drift before any upload."""
    _validate_contents(_safe_file(raw), _safe_file(receipt), repository)


def _tag(run_id: str) -> str:
    if not RUN_ID.fullmatch(run_id):
        raise _error("run ID is invalid")
    return f"chaosgauge-canary-{run_id}"


def _bundle_name(run_id: str) -> str:
    return f"{_tag(run_id)}-evidence.zip"


def _zip_entry(archive: zipfile.ZipFile, name: str, content: bytes) -> None:
    info = zipfile.ZipInfo(name, date_time=(1980, 1, 1, 0, 0, 0))
    info.compress_type = zipfile.ZIP_DEFLATED
    info.external_attr = 0o100600 << 16
    archive.writestr(info, content, compress_type=zipfile.ZIP_DEFLATED, compresslevel=9)


def bundle(raw: Path, receipt: Path, destination: Path, run_id: str) -> Path:
    """Build one deterministic private evidence bundle with a hash manifest."""
    content = {"raw.json": _safe_file(raw), "receipt.json": _safe_file(receipt)}
    manifest = {
        "schemaVersion": 1,
        "files": {name: hashlib.sha256(content[name]).hexdigest() for name in BUNDLE_FILES},
    }
    payload = io.BytesIO()
    with zipfile.ZipFile(payload, "w", compression=zipfile.ZIP_DEFLATED, compresslevel=9, strict_timestamps=True) as archive:
        _zip_entry(archive, "manifest.json", json.dumps(manifest, sort_keys=True, separators=(",", ":")).encode("utf-8"))
        for name in BUNDLE_FILES:
            _zip_entry(archive, name, content[name])
    target = Path(destination) / _bundle_name(run_id)
    target.write_bytes(payload.getvalue())
    return target


def bundle_contents(path: Path) -> tuple[dict[str, object], dict[str, bytes]]:
    """Read only fixed evidence entries and verify declared hashes."""
    payload = _safe_file(path)
    try:
        with zipfile.ZipFile(io.BytesIO(payload)) as archive:
            names = [entry.filename for entry in archive.infolist()]
            if len(names) != len(set(names)) or set(names) != {"manifest.json", *BUNDLE_FILES}:
                raise _error("private evidence bundle entries are invalid")
            manifest = json.loads(archive.read("manifest.json").decode("utf-8"))
            content = {name: archive.read(name) for name in BUNDLE_FILES}
    except (OSError, UnicodeDecodeError, ValueError, zipfile.BadZipFile, json.JSONDecodeError) as error:
        raise _error("private evidence bundle is invalid") from error
    expected = {"schemaVersion": 1, "files": {name: hashlib.sha256(content[name]).hexdigest() for name in BUNDLE_FILES}}
    if manifest != expected:
        raise _error("private evidence bundle hashes are invalid")
    return manifest, content


def _run_json(arguments: list[str], run: Callable[..., object]) -> dict[str, object]:
    result = run(arguments, check=True, capture_output=True, text=True)
    output = result if isinstance(result, str) else getattr(result, "stdout", "")
    try:
        value = json.loads(output)
    except (TypeError, json.JSONDecodeError) as error:
        raise _error("private release metadata is invalid") from error
    if not isinstance(value, dict):
        raise _error("private release metadata is invalid")
    return value


def _release_view(tag: str, run: Callable[..., object]) -> dict[str, object]:
    return _run_json(
        ["gh", "release", "view", tag, "--repo", PRIVATE_REPOSITORY, "--json", "tagName,isDraft,targetCommitish,name,assets"], run,
    )


def _release(tag: str, run_id: str, run: Callable[..., object], *, create: bool) -> dict[str, object]:
    try:
        value = _release_view(tag, run)
    except subprocess.CalledProcessError:
        if not create:
            raise _error("private draft release is unavailable")
        run(
            [
                "gh", "release", "create", tag, "--repo", PRIVATE_REPOSITORY, "--target", PRIVATE_COMMIT,
                "--draft", "--title", f"ChaosGauge excluded canary {run_id}",
                "--notes", "Private raw evidence and validated sanitized receipt.",
            ], check=True, capture_output=True, text=True,
        )
        value = _release_view(tag, run)
    if (
        value.get("tagName") != tag
        or value.get("isDraft") is not True
        or value.get("targetCommitish") != PRIVATE_COMMIT
        or value.get("name") != f"ChaosGauge excluded canary {run_id}"
        or not isinstance(value.get("assets"), list)
    ):
        raise _error("private draft release is invalid")
    return value


def _asset(release: dict[str, object], name: str) -> dict[str, object] | None:
    assets = release["assets"]
    if not isinstance(assets, list):
        raise _error("private draft release is invalid")
    if not assets:
        return None
    if len(assets) != 1 or not isinstance(assets[0], dict) or assets[0].get("name") != name:
        raise _error("private draft release is incomplete")
    digest = assets[0].get("digest")
    if not isinstance(digest, str) or not re.fullmatch(r"sha256:[0-9a-f]{64}", digest):
        raise _error("private evidence asset digest is invalid")
    return assets[0]


def _download(tag: str, name: str, destination: Path, run: Callable[..., object]) -> Path:
    run(
        ["gh", "release", "download", tag, "--repo", PRIVATE_REPOSITORY, "--pattern", name, "--dir", str(destination)],
        check=True, capture_output=True, text=True,
    )
    path = destination / name
    if not path.is_file() or path.is_symlink():
        raise _error("private evidence asset is unavailable")
    return path


def _verify_bundle(path: Path, digest: str, repository: Path) -> bytes:
    if digest != f"sha256:{hashlib.sha256(_safe_file(path)).hexdigest()}":
        raise _error("private evidence asset digest mismatch")
    _, content = bundle_contents(path)
    _validate_contents(content["raw.json"], content["receipt.json"], repository)
    return content["receipt.json"]


def _write_exclusive(path: Path, content: bytes) -> None:
    target = Path(path)
    if not target.parent.is_dir() or target.parent.is_symlink() or target.is_symlink():
        raise _error("receipt output is unavailable")
    try:
        descriptor = os.open(target, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    except FileExistsError as error:
        raise _error("receipt output already exists") from error
    with os.fdopen(descriptor, "wb") as output:
        output.write(content)
        output.flush()
        os.fsync(output.fileno())


def prepare(
    repository: Path, run_id: str, token: str, *, receipt_out: Path | None = None,
    run: Callable[..., object] = subprocess.run,
) -> str:
    """Create/reconcile storage before provider work; recover a complete prior bundle."""
    tag, name = _tag(run_id), _bundle_name(run_id)
    preflight(token)
    release = _release(tag, run_id, run, create=True)
    asset = _asset(release, name)
    if asset is None:
        return "run"
    if receipt_out is None:
        raise _error("receipt recovery output is unavailable")
    with tempfile.TemporaryDirectory() as directory:
        receipt = _verify_bundle(_download(tag, name, Path(directory), run), str(asset["digest"]), repository)
    _write_exclusive(receipt_out, receipt)
    return "recover"


def publish(raw: Path, receipt: Path, repository: Path, run_id: str, token: str, run: Callable[..., object] = subprocess.run) -> None:
    """Replace one private bundle and verify remote copy before public recovery."""
    tag, name = _tag(run_id), _bundle_name(run_id)
    preflight(token)
    validate(raw, receipt, repository)
    _asset(_release(tag, run_id, run, create=False), name)
    archive = bundle(raw, receipt, raw.parent, run_id)
    try:
        run(
            ["gh", "release", "upload", tag, str(archive), "--repo", PRIVATE_REPOSITORY, "--clobber"],
            check=True, capture_output=True, text=True,
        )
        asset = _asset(_release(tag, run_id, run, create=False), name)
        if asset is None:
            raise _error("private evidence asset is unavailable")
        with tempfile.TemporaryDirectory() as directory:
            _verify_bundle(_download(tag, name, Path(directory), run), str(asset["digest"]), repository)
    finally:
        if archive.exists():
            archive.unlink()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("preflight")
    prepare_parser = commands.add_parser("prepare")
    validate_parser = commands.add_parser("validate")
    publish_parser = commands.add_parser("publish")
    prepare_parser.add_argument("--repository", type=Path, required=True)
    prepare_parser.add_argument("--run-id", required=True)
    prepare_parser.add_argument("--receipt-out", type=Path, required=True)
    for command in (validate_parser, publish_parser):
        command.add_argument("--raw", type=Path, required=True)
        command.add_argument("--receipt", type=Path, required=True)
        command.add_argument("--repository", type=Path, required=True)
    publish_parser.add_argument("--run-id", required=True)
    args = parser.parse_args()
    token = os.environ.get("GH_TOKEN", "")
    if args.command == "preflight":
        preflight(token)
    elif args.command == "prepare":
        print(prepare(args.repository, args.run_id, token, receipt_out=args.receipt_out))
    elif args.command == "validate":
        validate(args.raw, args.receipt, args.repository)
    else:
        publish(args.raw, args.receipt, args.repository, args.run_id, token)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
