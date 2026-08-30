#!/usr/bin/env python3
"""Install the portable ChaosEngine tree into a consumer project."""

from __future__ import annotations

import argparse
import base64
from contextlib import contextmanager, nullcontext
import hashlib
import json
import os
import re
import runpy
import secrets
import shutil
import subprocess  # nosec B404 - fixed list-form Maven build commands.
import sys
import tempfile
import types
import zipfile
from pathlib import Path, PurePosixPath


INSTALL_DIRECTORY = ".chaos-engine"
MANIFEST_NAME = "manifest.json"
SCHEMA_VERSION = 1
DIAGNOSTIC_SCHEMA_VERSION = 2
CANONICAL_IDENTITY = "chaos-engine"
DEFAULT_DISTRIBUTION = "portable"
DISTRIBUTIONS_NAME = "distributions.json"
COMMIT_PATTERN = re.compile(r"[0-9a-f]{40}")
REPOSITORY_PATTERN = re.compile(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+")
BRANCH_PATTERN = re.compile(r"[^\x00-\x20\x7f~^:?*\\\[\]]+")
LOCK_NAME = ".chaos-engine.lock"
BACKUP_NAME = ".chaos-engine.backup"
JOURNAL_NAME = ".chaos-engine.transaction.json"
LOCK_MAGIC = b"chaos-engine-lock-v1\n"
NEXT_BACKUP_NAME = ".chaos-engine.backup.next"
OLD_BACKUP_NAME = ".chaos-engine.backup.old"
UNINSTALL_ARCHIVE_NAME = ".chaos-engine-uninstall-recovery.zip"
UNINSTALL_CURRENT_NAME = ".chaos-engine-uninstall-current"
UNINSTALL_OLD_BACKUP_NAME = ".chaos-engine-uninstall-old-backup"
DEPENDENCY_LOCK_MAGIC = b"chaos-engine-dependencies-lock-v1\n"
CROSS_ROLLBACK_JOURNAL_NAME = ".chaos-engine-cross-rollback"
CAPABILITY_FIELDS = {"owner", "scope", "lifecycle", "taskImpact"}
CAPABILITY_ENUMS = {
    "owner": {"installer", "project", "user"},
    "scope": {"project", "repository", "user"},
    "lifecycle": {"receipt-owned", "persistent-data", "derived-single-writer", "user-managed-cache"},
    "taskImpact": {"required", "advisory", "optional"},
}
CAPABILITY_COMPONENTS = {
    "core", "skills", "playbooks", "hooks", "plugins", "roles", "mcps",
    "retrieval-config", "projection-policy", "tools", "memory", "mempalace",
    "graphify", "maven-tools-mcp",
}
PROJECT_SETUP_OUTPUTS = (
    ".agents/skills/graphify",
    "graphify-out",
)
MEMPALACE_STATE_OUTPUT = ".chaos-engine-state/mempalace"


def legacy_capability_policy() -> dict[str, dict[str, str]]:
    """Return the immutable schema-v1 compatibility view; new installs use tracked contracts."""
    result = {
        name: {
            "owner": "installer",
            "scope": "project",
            "lifecycle": "receipt-owned",
            "taskImpact": "required",
        }
        for name in CAPABILITY_COMPONENTS
    }
    result["playbooks"]["scope"] = "repository"
    result["mcps"]["owner"] = "project"
    result["retrieval-config"].update(owner="project", lifecycle="persistent-data")
    result["memory"].update(owner="project", lifecycle="persistent-data", taskImpact="advisory")
    result["mempalace"].update(owner="project", lifecycle="persistent-data", taskImpact="advisory")
    result["graphify"].update(
        owner="project", scope="repository", lifecycle="derived-single-writer", taskImpact="advisory"
    )
    result["maven-tools-mcp"].update(
        owner="installer", scope="user", lifecycle="receipt-owned", taskImpact="optional"
    )
    return _validated_capabilities(result)


def file_sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def valid_branch(branch: str) -> bool:
    parts = branch.split("/")
    return (
        BRANCH_PATTERN.fullmatch(branch) is not None
        and not branch.startswith("-")
        and not branch.startswith("/")
        and not branch.endswith(("/", "."))
        and "//" not in branch
        and ".." not in branch
        and "@{" not in branch
        and branch != "HEAD"
        and all(part and not part.startswith(".") and not part.endswith(".lock") for part in parts)
    )


def normalize_source_record(source: object) -> dict[str, str]:
    if not isinstance(source, dict) or not all(
        isinstance(key, str) and isinstance(value, str) for key, value in source.items()
    ):
        raise ValueError("ChaosEngine source record is invalid")
    if (
        set(source) == {"commit", "kind"}
        and source.get("kind") == "local"
        and COMMIT_PATTERN.fullmatch(source.get("commit", "")) is not None
    ):
        return dict(source)
    if (
        set(source) == {"commit", "kind", "repositorySha256", "branchSha256"}
        and source.get("kind") == "git-digest"
        and COMMIT_PATTERN.fullmatch(source.get("commit", "")) is not None
        and re.fullmatch(r"[0-9a-f]{64}", source.get("repositorySha256", "")) is not None
        and re.fullmatch(r"[0-9a-f]{64}", source.get("branchSha256", "")) is not None
    ):
        return dict(source)
    repository = source.get("repository", "")
    branch = source.get("branch", "")
    components = repository.split("/")
    if not (
        set(source) == {"commit", "kind", "repository", "branch"}
        and source.get("kind") == "git"
        and COMMIT_PATTERN.fullmatch(source.get("commit", "")) is not None
        and REPOSITORY_PATTERN.fullmatch(repository) is not None
        and len(components) == 2
        and all(component not in {".", ".."} for component in components)
        and valid_branch(branch)
    ):
        raise ValueError("ChaosEngine source record is invalid")
    result = dict(source)
    result["repository"] = repository.casefold()
    return result


def is_link_or_reparse(path: Path) -> bool:
    if path.is_symlink():
        return True
    try:
        attributes = getattr(os.lstat(path), "st_file_attributes", 0)
    except FileNotFoundError:
        return False
    return bool(attributes & 0x400)


def dependency_tombstone_entries(root: Path) -> list[Path]:
    entries: list[Path] = []
    pending = [root]
    while pending:
        directory = pending.pop()
        with os.scandir(directory) as children:
            paths = sorted((Path(child.path) for child in children), reverse=True)
        for path in paths:
            entries.append(path)
            if not is_link_or_reparse(path) and path.is_dir():
                pending.append(path)
    return sorted(entries)


def reject_link_or_reparse(path: Path) -> None:
    if is_link_or_reparse(path):
        raise ValueError(f"path is a link or reparse point: {path}")


def require_absent(path: Path, label: str) -> None:
    if path.exists() or is_link_or_reparse(path):
        raise ValueError(f"{label} collision: {path}")


_XML_COMMENT = re.compile(r"<!--.*?-->", re.DOTALL)
_XML_TAG = re.compile(
    r"<(/?)(?:[\w.-]+:)?([A-Za-z_][\w.-]*)(?:\s[^>]*)?(/?)>",
    re.DOTALL,
)
_MAX_POM_BYTES = 2_000_000
_MAVEN_ID_PARENTS = {
    ("project", "artifactId"),
    ("modules", "module"),
    ("dependency", "artifactId"),
}


def maven_coordinate_ids(pom: Path) -> set[str]:
    """Return project, module, and dependency artifact ids from one POM."""
    try:
        raw = pom.read_bytes()
    except OSError:
        return set()
    if len(raw) > _MAX_POM_BYTES:
        return set()
    try:
        text = _XML_COMMENT.sub("", raw.decode("utf-8"))
    except UnicodeDecodeError:
        return set()
    ids: set[str] = set()
    stack: list[str] = []
    pending_parent: str | None = None
    last_end = 0
    for match in _XML_TAG.finditer(text):
        if pending_parent is not None:
            value = text[last_end:match.start()].strip()
            if value:
                ids.add(value)
            pending_parent = None
        closing, name, self_close = match.group(1), match.group(2), match.group(3)
        last_end = match.end()
        if closing:
            if stack and stack[-1] == name:
                stack.pop()
            continue
        if self_close:
            continue
        stack.append(name)
        if len(stack) >= 2 and (stack[-2], stack[-1]) in _MAVEN_ID_PARENTS:
            pending_parent = stack[-2]
    return ids


def project_maven_ids(project: Path) -> set[str]:
    pom = project / "pom.xml"
    if not pom.is_file():
        return set()
    return maven_coordinate_ids(pom)


def profile_install_predicate(profile: dict[str, object]) -> set[str]:
    when = profile.get("installWhen")
    if when is None:
        return set()
    if not isinstance(when, dict):
        raise ValueError("profile installWhen must be an object")
    raw = when.get("mavenArtifactIds")
    if raw is None:
        return set()
    if not isinstance(raw, list) or not all(isinstance(item, str) and item.strip() for item in raw):
        raise ValueError("profile installWhen.mavenArtifactIds must be a list of ids")
    return {item.strip() for item in raw}


def detect_distribution(project: Path, source: Path) -> str:
    """Select a distribution from profile predicates; default stays portable."""
    catalog = json.loads((source / DISTRIBUTIONS_NAME).read_text(encoding="utf-8"))
    distributions = catalog.get("distributions")
    if not isinstance(distributions, dict):
        raise ValueError("ChaosEngine distribution catalog is invalid")
    declared = project_maven_ids(project)
    matches: list[str] = []
    for name, policy in distributions.items():
        if not isinstance(name, str) or not isinstance(policy, dict):
            continue
        if name == DEFAULT_DISTRIBUTION:
            continue
        profile_name = policy.get("profile")
        if not isinstance(profile_name, str):
            continue
        profile_path = source / "profiles" / profile_name / "profile.json"
        if not profile_path.is_file():
            continue
        profile = json.loads(profile_path.read_text(encoding="utf-8"))
        if not isinstance(profile, dict):
            raise ValueError(f"invalid ChaosEngine profile: {profile_name}")
        wanted = profile_install_predicate(profile)
        if wanted and wanted & declared:
            matches.append(name)
    if len(matches) > 1:
        raise ValueError("multiple ChaosEngine distributions match this project")
    if len(matches) == 1:
        return matches[0]
    return DEFAULT_DISTRIBUTION


def load_distribution(source: Path, distribution: str) -> tuple[dict[str, object], str]:
    try:
        catalog = json.loads((source / DISTRIBUTIONS_NAME).read_text(encoding="utf-8"))
        policy = catalog["distributions"][distribution]
    except (OSError, json.JSONDecodeError, KeyError, TypeError) as error:
        raise ValueError(f"unknown ChaosEngine distribution: {distribution}") from error
    if not isinstance(policy, dict):
        raise ValueError(f"unknown ChaosEngine distribution: {distribution}")
    profile = policy.get("profile")
    forbidden_tokens = policy.get("forbiddenTokens")
    runtime_files = policy.get("runtimeFiles")
    if (
        not isinstance(profile, str)
        or re.fullmatch(r"[a-z0-9][a-z0-9-]*", profile) is None
        or not isinstance(forbidden_tokens, list)
        or not all(
        isinstance(token, str) and token for token in forbidden_tokens
        )
        or not isinstance(runtime_files, list)
        or not all(
            isinstance(relative, str)
            and PurePosixPath(relative).parts
            and not PurePosixPath(relative).is_absolute()
            and all(part not in {"", ".", ".."} for part in PurePosixPath(relative).parts)
            for relative in runtime_files
        )
    ):
        raise ValueError("ChaosEngine distribution policy is invalid")
    profile_root = source / "profiles" / profile
    if not all((profile_root / name).is_file() for name in ("entrypoint.md", "profile.json")):
        raise ValueError(f"ChaosEngine distribution profile is incomplete: {profile}")
    encoded = json.dumps(policy, sort_keys=True, separators=(",", ":")).encode()
    return policy, hashlib.sha256(encoded).hexdigest()


def _validated_capabilities(value: object) -> dict[str, dict[str, str]]:
    if not isinstance(value, dict):
        raise ValueError("ChaosEngine capability policy is invalid")
    result: dict[str, dict[str, str]] = {}
    for name, descriptor in value.items():
        if (
            not isinstance(name, str)
            or not isinstance(descriptor, dict)
            or set(descriptor) != CAPABILITY_FIELDS
            or any(descriptor.get(field) not in allowed for field, allowed in CAPABILITY_ENUMS.items())
        ):
            raise ValueError("ChaosEngine capability policy is invalid")
        result[name] = {field: str(descriptor[field]) for field in sorted(CAPABILITY_FIELDS)}
    return result


def load_capability_policy(source: Path, distribution: str) -> tuple[dict[str, dict[str, str]], str]:
    policy, _ = load_distribution(source, distribution)
    profile_name = str(policy["profile"])
    try:
        profile = json.loads((source / "profiles" / profile_name / "profile.json").read_text(encoding="utf-8"))
        dependencies = json.loads((source / "dependencies.json").read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError, TypeError) as error:
        raise ValueError("ChaosEngine capability policy is invalid") from error
    if not isinstance(profile, dict) or not isinstance(dependencies, dict):
        raise ValueError("ChaosEngine capability policy is invalid")
    merged: dict[str, dict[str, str]] = {}
    for raw in (policy.get("components"), profile.get("components"), dependencies.get("components")):
        selected = _validated_capabilities(raw)
        if set(merged) & set(selected):
            raise ValueError("ChaosEngine capability policy contains duplicate components")
        merged.update(selected)
    if set(merged) != CAPABILITY_COMPONENTS:
        raise ValueError("ChaosEngine capability policy does not cover every component")
    encoded = json.dumps(merged, sort_keys=True, separators=(",", ":")).encode()
    return merged, hashlib.sha256(encoded).hexdigest()


def is_origin_only(relative: Path) -> bool:
    return relative.parts[:2] == ("assets", "brand") or relative.as_posix() in {
        "README.md",
        "INSTALL.md",
        "RESEARCH.md",
        "STANDALONE.md",
    }


def source_files(source: Path, distribution: str = DEFAULT_DISTRIBUTION) -> tuple[Path, ...]:
    reject_link_or_reparse(source)
    if not (source / "skills/chaos-engine/SKILL.md").is_file():
        raise ValueError(f"source is not a portable ChaosEngine tree: {source}")
    if (source / MANIFEST_NAME).exists():
        raise ValueError(f"source contains the reserved manifest path: {MANIFEST_NAME}")
    policy, _ = load_distribution(source, distribution)
    selected_profile = str(policy["profile"])
    forbidden_tokens = tuple(str(token).casefold() for token in policy["forbiddenTokens"])
    runtime_files = frozenset(str(relative) for relative in policy["runtimeFiles"])
    files: list[Path] = []
    for path in sorted(source.rglob("*")):
        relative = path.relative_to(source)
        if is_generated_python_cache(relative):
            continue
        if relative.as_posix() == DISTRIBUTIONS_NAME:
            continue
        if (
            len(relative.parts) >= 3
            and relative.parts[0] == "profiles"
            and relative.parts[1] != selected_profile
        ):
            continue
        if is_origin_only(relative):
            continue
        if is_link_or_reparse(path):
            raise ValueError(f"source contains a link or reparse point: {relative}")
        if path.is_file():
            relative_text = relative.as_posix().casefold()
            content = path.read_text(encoding="utf-8", errors="ignore").casefold()
            if any(token in relative_text or token in content for token in forbidden_tokens):
                raise ValueError(
                    f"distribution policy rejected forbidden content: {relative.as_posix()}"
                )
            files.append(path)
    packaged = {path.relative_to(source).as_posix() for path in files}
    if not runtime_files <= packaged:
        raise ValueError("ChaosEngine distribution runtime inventory is incomplete")
    return tuple(files)


def verify_staged_payload(stage: Path, ownership: dict[str, str]) -> None:
    staged = {
        path.relative_to(stage).as_posix(): file_sha256(path)
        for path in sorted(stage.rglob("*"))
        if path.is_file()
    }
    if staged != ownership:
        raise ValueError("staged payload does not match the immutable ownership plan")


def load_manifest(target: Path) -> dict[str, object]:
    try:
        manifest = json.loads((target / MANIFEST_NAME).read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError(f"ChaosEngine manifest is missing or invalid: {target}") from error
    if not isinstance(manifest, dict) or manifest.get("schemaVersion") != SCHEMA_VERSION:
        raise ValueError("ChaosEngine manifest schema is unsupported")
    source = manifest.get("source")
    files = manifest.get("files")
    host_token = manifest.get("hostToken")
    distribution = manifest.get("distribution")
    capabilities = manifest.get("capabilities")
    capability_digest = manifest.get("capabilityPolicySha256")
    if distribution is None and manifest.get("schemaVersion") == 1:
        distribution = {"id": "legacy", "policySha256": "0" * 64}
        manifest["distribution"] = distribution
    try:
        normalized_source = normalize_source_record(source)
    except ValueError as error:
        raise ValueError("ChaosEngine manifest has an invalid ownership record") from error
    if (
        not isinstance(files, dict)
        or not isinstance(distribution, dict)
        or not isinstance(distribution.get("id"), str)
        or re.fullmatch(r"[0-9a-f]{64}", str(distribution.get("policySha256", ""))) is None
        or not isinstance(host_token, str)
        or re.fullmatch(r"[0-9a-f]{64}", host_token) is None
        or any(not isinstance(path, str) or not isinstance(digest, str) for path, digest in files.items())
        or ((capabilities is None) != (capability_digest is None))
    ):
        raise ValueError("ChaosEngine manifest has an invalid ownership record")
    if capabilities is not None:
        validated = _validated_capabilities(capabilities)
        encoded = json.dumps(validated, sort_keys=True, separators=(",", ":")).encode()
        if (
            set(validated) != CAPABILITY_COMPONENTS
            or capability_digest != hashlib.sha256(encoded).hexdigest()
        ):
            raise ValueError("ChaosEngine manifest has an invalid capability policy")
        manifest["capabilities"] = validated
    manifest["source"] = normalized_source
    return manifest


def is_generated_python_cache(relative: Path) -> bool:
    return "__pycache__" in relative.parts or relative.suffix == ".pyc"


def installed_payload(target: Path) -> dict[str, str]:
    reject_link_or_reparse(target)
    payload: dict[str, str] = {}
    for path in sorted(target.rglob("*")):
        reject_link_or_reparse(path)
        relative = path.relative_to(target)
        if is_generated_python_cache(relative):
            continue
        if path.is_file() and relative.as_posix() != MANIFEST_NAME:
            payload[relative.as_posix()] = file_sha256(path)
    return payload


def verify_install(target: Path) -> dict[str, object]:
    manifest = load_manifest(target)
    if installed_payload(target) != manifest["files"]:
        raise ValueError(f"ChaosEngine ownership drift detected: {target}")
    return manifest


def _is_link_or_reparse_error(error: BaseException) -> bool:
    return str(error).startswith("path is a link or reparse point:")


def try_verify_install(target: Path) -> dict[str, object] | None:
    try:
        return verify_install(target)
    except ValueError as error:
        if _is_link_or_reparse_error(error):
            raise
        return None


def inspect_current_install(target: Path) -> dict[str, object] | None:
    reject_link_or_reparse(target)
    if not target.is_dir():
        raise ValueError(f"ChaosEngine install path is not a directory: {target}")
    return try_verify_install(target)


def peek_host_token(target: Path) -> str | None:
    try:
        manifest = load_manifest(target)
    except ValueError:
        return None
    token = manifest.get("hostToken")
    if isinstance(token, str) and re.fullmatch(r"[0-9a-f]{64}", token) is not None:
        return token
    return None


def remove_repairable_tree(path: Path) -> None:
    if path.is_dir():
        shutil.rmtree(path)
    elif path.exists():
        path.unlink()


def project_setup_output_files(project: Path, relative: str) -> tuple[bool, dict[str, str]]:
    """Return one Graphify output tree's exact regular-file digests."""
    root = project / relative
    reject_link_or_reparse(root)
    if not root.exists():
        return False, {}
    if not root.is_dir():
        raise ValueError(f"project setup output is not a directory: {root}")
    files: dict[str, str] = {}
    for child in root.rglob("*"):
        reject_link_or_reparse(child)
        if child.is_file():
            files[child.relative_to(root).as_posix()] = file_sha256(child)
        elif not child.is_dir():
            raise ValueError(f"project setup output is not a file or directory: {child}")
    return True, files


def snapshot_project_setup_outputs(
    project: Path,
) -> tuple[Path, dict[str, tuple[bool, dict[str, str]]]]:
    """Keep Graphify preimages until host setup commits under the project lock."""
    snapshot = Path(tempfile.mkdtemp(prefix="chaos-engine-project-setup-"))
    before: dict[str, tuple[bool, dict[str, str]]] = {}
    try:
        for relative in PROJECT_SETUP_OUTPUTS:
            original = project / relative
            exists, files = project_setup_output_files(project, relative)
            before[relative] = (exists, files)
            if not exists:
                continue
            saved = snapshot / relative
            saved.parent.mkdir(parents=True, exist_ok=True)
            shutil.copytree(original, saved)
    except BaseException:
        shutil.rmtree(snapshot, ignore_errors=True)
        raise
    return snapshot, before


def project_setup_after_images(project: Path) -> dict[str, dict[str, str]]:
    """Capture exact setup-owned Graphify files just before host publication."""
    return {
        relative: project_setup_output_files(project, relative)[1]
        for relative in PROJECT_SETUP_OUTPUTS
    }


def mempalace_rollback_image(
    before: tuple[bool, dict[str, str]], after: tuple[bool, dict[str, str]]
) -> dict[str, object]:
    """Record candidate state only when the immutable base had no local palace."""
    return {
        "beforeAbsent": not before[0],
        "after": dict(after[1]) if not before[0] else {},
    }


def restore_candidate_mempalace_state(project: Path, image: dict[str, object]) -> None:
    """Remove only byte-identical candidate state that was absent in the base."""
    before_absent = image.get("beforeAbsent")
    after = image.get("after")
    if not isinstance(before_absent, bool) or not isinstance(after, dict) or any(
        not isinstance(relative, str)
        or not isinstance(digest, str)
        or re.fullmatch(r"[0-9a-f]{64}", digest) is None
        for relative, digest in after.items()
    ):
        raise ValueError("account rollback has invalid MemPalace state image")
    if not before_absent:
        return
    palace = project / MEMPALACE_STATE_OUTPUT
    exists, current = project_setup_output_files(project, MEMPALACE_STATE_OUTPUT)
    expected = dict(after)
    if not exists:
        if expected:
            raise ValueError("candidate MemPalace state changed before rollback")
        return
    if current != expected or any(child.is_dir() for child in palace.iterdir()):
        raise ValueError("candidate MemPalace state changed before rollback")
    for relative in expected:
        (palace / relative).unlink()
    palace.rmdir()
    state_root = palace.parent
    if state_root.exists() and not any(state_root.iterdir()):
        state_root.rmdir()


def restore_project_setup_outputs(
    project: Path,
    snapshot: Path,
    before: dict[str, tuple[bool, dict[str, str]]],
    after: dict[str, dict[str, str]],
) -> None:
    """Undo only unchanged transaction output files; foreign residue always wins."""
    for relative in PROJECT_SETUP_OUTPUTS:
        prior_exists, prior_files = before[relative]
        published_files = after[relative]
        _current_exists, current_files = project_setup_output_files(project, relative)
        root = project / relative
        for child, published_digest in published_files.items():
            path = root / child
            current_digest = current_files.get(child)
            prior_digest = prior_files.get(child)
            if prior_digest is None:
                if current_digest == published_digest:
                    path.unlink()
                continue
            if current_digest == published_digest and prior_digest != published_digest:
                saved = snapshot / relative / child
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(saved.read_bytes())
        if not prior_exists and root.exists() and root.is_dir():
            for directory in sorted(
                (item for item in root.rglob("*") if item.is_dir()),
                key=lambda item: len(item.parts), reverse=True,
            ):
                if not any(directory.iterdir()):
                    directory.rmdir()
            if not any(root.iterdir()):
                root.rmdir()


@contextmanager
def project_lock(project: Path):
    project = project.resolve()
    lock_path = project / LOCK_NAME
    flags = os.O_RDWR | getattr(os, "O_BINARY", 0)
    created = False
    try:
        descriptor = os.open(lock_path, flags | os.O_CREAT | os.O_EXCL, 0o600)
        created = True
    except FileExistsError:
        reject_link_or_reparse(lock_path)
        descriptor = os.open(lock_path, flags)
    try:
        lock_file = os.fdopen(descriptor, "r+b", closefd=True)
    except BaseException:
        os.close(descriptor)
        raise
    try:
        opened = os.fstat(lock_file.fileno())
        named = os.stat(lock_path, follow_symlinks=False)
        if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino):
            raise ValueError(f"ChaosEngine lock collision: {lock_path}")
        if created:
            lock_file.write(LOCK_MAGIC)
            lock_file.flush()
            os.fsync(lock_file.fileno())
        else:
            lock_file.seek(0)
            try:
                lock_contents = lock_file.read()
            except PermissionError as error:
                raise RuntimeError("another ChaosEngine operation is already running") from error
            if lock_contents != LOCK_MAGIC:
                raise ValueError(f"ChaosEngine lock collision: {lock_path}")
        lock_file.seek(0)
        if os.name == "nt":
            import msvcrt  # pylint: disable=import-outside-toplevel

            msvcrt.locking(lock_file.fileno(), msvcrt.LK_NBLCK, 1)
        else:
            import fcntl  # pylint: disable=import-outside-toplevel,import-error

            fcntl.flock(lock_file.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError as error:
        lock_file.close()
        raise RuntimeError("another ChaosEngine operation is already running") from error
    except BaseException:
        lock_file.close()
        raise
    try:
        yield
    finally:
        try:
            lock_file.seek(0)
            if os.name == "nt":
                msvcrt.locking(lock_file.fileno(), msvcrt.LK_UNLCK, 1)
            else:
                fcntl.flock(lock_file.fileno(), fcntl.LOCK_UN)
        finally:
            lock_file.close()


@contextmanager
def dependency_runtime_lock(runtime: Path):
    lock_path = runtime.with_name(f"{runtime.name}.lock")
    flags = os.O_RDWR | getattr(os, "O_BINARY", 0)
    created = False
    try:
        descriptor = os.open(lock_path, flags | os.O_CREAT | os.O_EXCL, 0o600)
        created = True
    except FileExistsError:
        reject_link_or_reparse(lock_path)
        descriptor = os.open(lock_path, flags)
    try:
        stream = os.fdopen(descriptor, "r+b", closefd=True)
    except BaseException:
        os.close(descriptor)
        raise
    try:
        opened = os.fstat(stream.fileno())
        named = os.stat(lock_path, follow_symlinks=False)
        if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino):
            raise ValueError(f"dependency lock collision: {lock_path}")
        if created:
            stream.write(DEPENDENCY_LOCK_MAGIC)
            stream.flush()
            os.fsync(stream.fileno())
        else:
            stream.seek(0)
            try:
                contents = stream.read()
            except PermissionError as error:
                raise RuntimeError("another dependency runtime operation is already running") from error
            if contents != DEPENDENCY_LOCK_MAGIC:
                raise ValueError(f"dependency lock collision: {lock_path}")
        stream.seek(0)
        if os.name == "nt":
            import msvcrt

            msvcrt.locking(stream.fileno(), msvcrt.LK_NBLCK, 1)
        else:
            import fcntl

            fcntl.flock(stream.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError as error:
        stream.close()
        raise RuntimeError("another dependency runtime operation is already running") from error
    except BaseException:
        stream.close()
        raise
    try:
        yield
    finally:
        try:
            stream.seek(0)
            if os.name == "nt":
                msvcrt.locking(stream.fileno(), msvcrt.LK_UNLCK, 1)
            else:
                fcntl.flock(stream.fileno(), fcntl.LOCK_UN)
        finally:
            stream.close()


def write_journal(project: Path, operation: str, commit: str) -> Path:
    journal = project / JOURNAL_NAME
    temporary = journal.with_suffix(journal.suffix + ".tmp")
    reject_link_or_reparse(journal)
    payload = (
        json.dumps({"schemaVersion": 1, "operation": operation, "commit": commit}, sort_keys=True)
        + "\n"
    ).encode()
    flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0)
    try:
        descriptor = os.open(temporary, flags, 0o600)
    except FileExistsError as error:
        raise ValueError(f"transaction journal scratch path collision: {temporary}") from error
    with os.fdopen(descriptor, "wb") as handle:
        handle.write(payload)
        handle.flush()
        os.fsync(handle.fileno())
        opened = os.fstat(handle.fileno())
        named = os.stat(temporary, follow_symlinks=False)
        if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino):
            raise ValueError(f"transaction journal scratch path collision: {temporary}")
    temporary.replace(journal)
    return journal


def write_cross_rollback_journal(
    project: Path,
    desired_commit: str,
    prior_commit: str,
    *,
    prior_host_receipt: bytes | None = None,
) -> Path:
    if COMMIT_PATTERN.fullmatch(desired_commit) is None or COMMIT_PATTERN.fullmatch(prior_commit) is None:
        raise ValueError("rollback commits are invalid")
    transaction = project / CROSS_ROLLBACK_JOURNAL_NAME
    journal = transaction / "journal.json"
    reject_link_or_reparse(transaction)
    if transaction.exists():
        raise ValueError(f"cross-resource rollback transaction collision: {transaction}")
    target = project / INSTALL_DIRECTORY
    host_controller = load_installed_controller(target, "hosts") if target.exists() else None
    if prior_host_receipt is None and host_controller is not None:
        historical_receipt = getattr(host_controller, "rollback_previous_receipt", None)
        if callable(historical_receipt):
            prior_host_receipt = historical_receipt(project, desired_commit)
    if prior_host_receipt is not None:
        if not target.exists():
            raise ValueError("cross-resource rollback journal has no installed controller")
        validate_prior_host_receipt(
            project,
            host_controller,
            prior_host_receipt,
            desired_commit,
        )
    if host_controller is not None:
        host_controller.set_rollback_intent(
            project, desired_commit, prior_commit
        )
    body: dict[str, object] = {
        "schemaVersion": 1,
        "desiredCommit": desired_commit,
        "priorCommit": prior_commit,
    }
    if prior_host_receipt is not None:
        body["priorHostReceipt"] = base64.b64encode(prior_host_receipt).decode("ascii")
    body["integritySha256"] = hashlib.sha256(
        json.dumps(body, sort_keys=True, separators=(",", ":")).encode()
    ).hexdigest()
    payload = (json.dumps(body, sort_keys=True) + "\n").encode()
    transaction.mkdir()
    descriptor = os.open(
        journal,
        os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0),
        0o600,
    )
    with os.fdopen(descriptor, "wb") as stream:
        stream.write(payload)
        stream.flush()
        os.fsync(stream.fileno())
    return journal


def validate_prior_host_receipt(
    project: Path, controller, raw: bytes, desired_commit: str
) -> dict[str, object]:
    """Validate exact pre-upgrade receipt retained for rollback recovery."""
    try:
        receipt = json.loads(raw.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("cross-resource rollback journal is invalid") from error
    if (
        not isinstance(receipt, dict)
        or receipt.get("schemaVersion") != controller.SCHEMA_VERSION
        or receipt.get("phase") != "installed"
        or receipt.get("coreCommit") != desired_commit
        or receipt.get("hosts") != controller.host_routes()
        or receipt.get("rollbackIntent") is not None
        or controller.receipt_bytes(receipt, project) != raw
    ):
        raise ValueError("cross-resource rollback journal is invalid")
    controller.decode_images(receipt.get("before"), nullable=True)
    controller.decode_images(receipt.get("after"), nullable=True)
    return receipt


def read_cross_rollback_journal(project: Path) -> dict[str, str | bytes] | None:
    transaction = project / CROSS_ROLLBACK_JOURNAL_NAME
    journal = transaction / "journal.json"
    reject_link_or_reparse(transaction)
    if not transaction.exists():
        return None
    if not transaction.is_dir():
        raise ValueError("cross-resource rollback transaction is invalid")
    for child in transaction.iterdir():
        reject_link_or_reparse(child)
        if child.name != "journal.json":
            raise ValueError("cross-resource rollback transaction contains unknown state")
    target = project / INSTALL_DIRECTORY
    if not target.exists():
        raise ValueError("cross-resource rollback journal has no installed controller")
    verify_install(target)
    host_controller = load_installed_controller(target, "hosts")
    receipt, _ = host_controller.read_receipt(project)
    intent = receipt.get("rollbackIntent")
    if not journal.exists():
        if not isinstance(intent, dict):
            raise ValueError("cross-resource rollback transaction has no authenticated intent")
        body: dict[str, object] = {"schemaVersion": 1, **intent}
        body["integritySha256"] = hashlib.sha256(
            json.dumps(body, sort_keys=True, separators=(",", ":")).encode()
        ).hexdigest()
        journal.write_text(json.dumps(body, sort_keys=True) + "\n", encoding="utf-8")
    try:
        value = json.loads(journal.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        if not isinstance(intent, dict):
            raise ValueError("cross-resource rollback journal is invalid") from error
        body = {"schemaVersion": 1, **intent}
        body["integritySha256"] = hashlib.sha256(
            json.dumps(body, sort_keys=True, separators=(",", ":")).encode()
        ).hexdigest()
        journal.write_text(json.dumps(body, sort_keys=True) + "\n", encoding="utf-8")
        value = body
    integrity = value.get("integritySha256") if isinstance(value, dict) else None
    body = {key: item for key, item in value.items() if key != "integritySha256"} if isinstance(value, dict) else {}
    if (
        not isinstance(value, dict)
        or value.get("schemaVersion") != 1
        or COMMIT_PATTERN.fullmatch(str(value.get("desiredCommit", ""))) is None
        or COMMIT_PATTERN.fullmatch(str(value.get("priorCommit", ""))) is None
        or integrity
        != hashlib.sha256(json.dumps(body, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
    ):
        raise ValueError("cross-resource rollback journal is invalid")
    if receipt.get("rollbackIntent") != {
        "desiredCommit": str(value["desiredCommit"]),
        "priorCommit": str(value["priorCommit"]),
    }:
        raise ValueError("cross-resource rollback journal intent is not authenticated")
    result: dict[str, str | bytes] = {
        "desiredCommit": str(value["desiredCommit"]),
        "priorCommit": str(value["priorCommit"]),
    }
    prior_host_receipt = value.get("priorHostReceipt")
    if prior_host_receipt is not None:
        if not isinstance(prior_host_receipt, str):
            raise ValueError("cross-resource rollback journal is invalid")
        try:
            decoded = base64.b64decode(prior_host_receipt, validate=True)
        except (ValueError, TypeError) as error:
            raise ValueError("cross-resource rollback journal is invalid") from error
        if base64.b64encode(decoded).decode("ascii") != prior_host_receipt:
            raise ValueError("cross-resource rollback journal is invalid")
        validate_prior_host_receipt(
            project, host_controller, decoded, str(value["desiredCommit"])
        )
        result["priorHostReceipt"] = decoded
    return result


def publish_staged_tree(stage: Path, target: Path, displaced: Path) -> None:
    previous = target.exists()
    if previous:
        require_absent(displaced, "update displaced-tree path")
        target.replace(displaced)
    try:
        stage.replace(target)
    except BaseException:
        if previous and displaced.exists() and not target.exists():
            displaced.replace(target)
        raise


def archive_install(source: Path, archive: Path) -> None:
    require_absent(archive, "uninstall recovery archive")
    temporary = archive.with_suffix(archive.suffix + ".tmp")
    require_absent(temporary, "uninstall recovery archive scratch path")
    manifest = verify_install(source)
    try:
        with zipfile.ZipFile(temporary, "x", compression=zipfile.ZIP_DEFLATED) as bundle:
            bundle.writestr(MANIFEST_NAME, (source / MANIFEST_NAME).read_bytes())
            for relative in manifest["files"]:  # type: ignore[union-attr]
                bundle.write(source / str(relative), str(relative))
        with zipfile.ZipFile(temporary) as bundle:
            archived = {
                name: hashlib.sha256(bundle.read(name)).hexdigest()
                for name in bundle.namelist()
                if name != MANIFEST_NAME
            }
        if archived != manifest["files"]:
            raise ValueError("uninstall recovery archive failed verification")
        temporary.replace(archive)
    finally:
        temporary.unlink(missing_ok=True)


def restore_archive(archive: Path, target: Path) -> None:
    require_absent(target, "uninstall restore target")
    stage = Path(tempfile.mkdtemp(prefix=f"{INSTALL_DIRECTORY}-restore-", dir=target.parent))
    try:
        with zipfile.ZipFile(archive) as bundle:
            bundle.extractall(stage)
        verify_install(stage)
        stage.replace(target)
    finally:
        if stage.exists():
            shutil.rmtree(stage)


def _recover_transaction(  # noqa: MC0001 - one auditable recovery state machine.
    project: Path,
) -> None:
    journal = project / JOURNAL_NAME
    if not journal.exists():
        return
    reject_link_or_reparse(journal)
    try:
        record = json.loads(journal.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError("transaction journal is invalid") from error
    operation = record.get("operation") if isinstance(record, dict) else None
    target = project / INSTALL_DIRECTORY
    backup = project / BACKUP_NAME
    swap = project / f"{INSTALL_DIRECTORY}-rollback"
    displaced = project / NEXT_BACKUP_NAME
    old_backup = project / OLD_BACKUP_NAME
    uninstall_archive = project / UNINSTALL_ARCHIVE_NAME
    uninstall_current = project / UNINSTALL_CURRENT_NAME
    uninstall_old_backup = project / UNINSTALL_OLD_BACKUP_NAME
    for path in (
        target,
        backup,
        swap,
        displaced,
        old_backup,
        uninstall_archive,
        uninstall_current,
        uninstall_old_backup,
    ):
        reject_link_or_reparse(path)
    if operation in ("install", "update"):
        if not target.exists() and displaced.exists():
            if try_verify_install(displaced) is None:
                remove_repairable_tree(displaced)
            else:
                displaced.replace(target)
        if not target.exists() and backup.exists() and not displaced.exists():
            verify_install(backup)
            backup.replace(target)
        if not target.exists() and not backup.exists() and not displaced.exists():
            journal.unlink()
            return
        if try_verify_install(target) is None:
            if displaced.exists() and try_verify_install(displaced) is not None:
                remove_repairable_tree(target)
                displaced.replace(target)
            else:
                if displaced.exists():
                    remove_repairable_tree(displaced)
                journal.unlink()
                return
        if backup.exists():
            verify_install(backup)
        if displaced.exists():
            if try_verify_install(displaced) is None:
                remove_repairable_tree(displaced)
            else:
                if backup.exists():
                    require_absent(old_backup, "obsolete backup path")
                    backup.replace(old_backup)
                displaced.replace(backup)
                verify_install(backup)
        if old_backup.exists():
            shutil.rmtree(old_backup)
    elif operation == "rollback":
        if swap.exists() and not target.exists() and backup.exists():
            verify_install(swap)
            verify_install(backup)
            swap.replace(target)
        elif swap.exists() and target.exists() and not backup.exists():
            verify_install(swap)
            verify_install(target)
            swap.replace(backup)
        elif swap.exists():
            raise ValueError("rollback recovery has an ambiguous mixed state")
        verify_install(target)
        verify_install(backup)
    elif operation == "uninstall":
        if target.exists():
            verify_install(target)
        elif uninstall_current.exists():
            try:
                verify_install(uninstall_current)
            except ValueError:
                if not uninstall_archive.exists():
                    raise
                shutil.rmtree(uninstall_current)
                restore_archive(uninstall_archive, target)
            else:
                uninstall_current.replace(target)
        elif uninstall_archive.exists():
            restore_archive(uninstall_archive, target)
        else:
            raise ValueError("uninstall recovery has no verified tree")
        verify_install(target)
        if uninstall_old_backup.exists() and not backup.exists():
            shutil.rmtree(uninstall_old_backup)
        if uninstall_current.exists():
            shutil.rmtree(uninstall_current)
        if uninstall_archive.exists():
            uninstall_archive.unlink()
    else:
        raise ValueError("transaction journal operation is unsupported")
    journal.unlink()


def recover_transaction(project: Path) -> None:
    project = project.resolve()
    with project_lock(project):
        _recover_transaction(project)


def install(  # noqa: MC0001 - publication and compensation form one transaction.
    project: Path,
    source: Path,
    commit: str,
    _locked: bool = False,
    source_record: dict[str, str] | None = None,
    distribution: str = DEFAULT_DISTRIBUTION,
) -> Path:
    project = project.resolve()
    reject_link_or_reparse(source.absolute())
    source = source.resolve()
    if not project.is_dir():
        raise ValueError(f"project is not a directory: {project}")
    if COMMIT_PATTERN.fullmatch(commit) is None:
        raise ValueError("commit must be a lowercase 40-hex revision")
    desired_source = normalize_source_record({"commit": commit, "kind": "local"})
    if source_record is not None:
        desired_source = normalize_source_record(source_record)
        if desired_source.get("commit") != commit or desired_source.get("kind") not in {
            "git",
            "git-digest",
        }:
            raise ValueError("ChaosEngine source record is invalid")
    if source == project or source.is_relative_to(project) or project.is_relative_to(source):
        raise ValueError("ChaosEngine source and project trees must be disjoint")

    if (source / MANIFEST_NAME).exists():
        raise ValueError(f"source contains the reserved manifest path: {MANIFEST_NAME}")
    _, policy_digest = load_distribution(source, distribution)
    capabilities, capability_digest = load_capability_policy(source, distribution)
    files = source_files(source, distribution)
    ownership = {path.relative_to(source).as_posix(): file_sha256(path) for path in files}
    target = project / INSTALL_DIRECTORY
    backup = project / BACKUP_NAME
    displaced = project / NEXT_BACKUP_NAME
    old_backup = project / OLD_BACKUP_NAME
    with (nullcontext() if _locked else project_lock(project)):
        _recover_transaction(project)
        if read_cross_rollback_journal(project) is not None:
            raise ValueError("rollback recovery is required before install")
        current = inspect_current_install(target) if target.exists() else None
        if current is not None:
            current_commit = current["source"]["commit"]  # type: ignore[index]
            current_distribution = current.get("distribution")
            legacy_distribution = (
                isinstance(current_distribution, dict)
                and current_distribution.get("id") == "legacy"
            )
            if (
                not isinstance(current_distribution, dict)
                or (
                    current_distribution.get("id") != distribution
                    and not (legacy_distribution and distribution == DEFAULT_DISTRIBUTION)
                )
            ):
                raise ValueError(
                    "installed ChaosEngine distribution differs; uninstall before changing it"
                )
            if current_commit == commit:
                if (
                    not legacy_distribution
                    and current_distribution.get("policySha256") != policy_digest
                ):
                    raise ValueError(
                        "same commit resolved to a different ChaosEngine distribution policy"
                    )
                if current["files"] != ownership:
                    raise ValueError("same commit resolved to a different ChaosEngine payload")
                if current["source"] == desired_source:
                    current_capabilities = current.get("capabilities")
                    current_capability_digest = current.get("capabilityPolicySha256")
                    manifest_changed = False
                    if legacy_distribution:
                        current["distribution"] = {
                            "id": distribution,
                            "policySha256": policy_digest,
                        }
                        manifest_changed = True
                    if current_capabilities is None and current_capability_digest is None:
                        current["capabilities"] = capabilities
                        current["capabilityPolicySha256"] = capability_digest
                        manifest_changed = True
                    elif (
                        current_capabilities != capabilities
                        or current_capability_digest != capability_digest
                    ):
                        raise ValueError("same commit resolved to a different capability policy")
                    if manifest_changed:
                        temporary_manifest = target / f"{MANIFEST_NAME}.upgrade-{secrets.token_hex(8)}"
                        try:
                            temporary_manifest.write_text(
                                json.dumps(current, indent=2, sort_keys=True) + "\n",
                                encoding="utf-8",
                            )
                            temporary_manifest.replace(target / MANIFEST_NAME)
                        finally:
                            if temporary_manifest.exists():
                                temporary_manifest.unlink()
                    return target
                if current["source"].get("kind") != "local":  # type: ignore[union-attr]
                    raise ValueError("same commit resolved from different ChaosEngine provenance")
        if backup.exists():
            require_absent(old_backup, "obsolete backup path")
        stage = Path(tempfile.mkdtemp(prefix=f"{INSTALL_DIRECTORY}-stage-", dir=project))
        try:
            for path in files:
                relative = path.relative_to(source)
                destination = stage / relative
                destination.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(path, destination)
            verify_staged_payload(stage, ownership)
            manifest = {
                "schemaVersion": SCHEMA_VERSION,
                "distribution": {"id": distribution, "policySha256": policy_digest},
                "capabilities": capabilities,
                "capabilityPolicySha256": capability_digest,
                "source": desired_source,
                "files": ownership,
                "hostToken": (
                    str(current["hostToken"])
                    if current is not None
                    else (peek_host_token(target) if target.exists() else None)
                    or secrets.token_hex(32)
                ),
            }
            (stage / MANIFEST_NAME).write_text(
                json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )
            journal = write_journal(project, "update" if target.exists() else "install", commit)
            publish_staged_tree(stage, target, displaced)
            verify_install(target)
            if displaced.exists():
                if try_verify_install(displaced) is None:
                    remove_repairable_tree(displaced)
                else:
                    if backup.exists():
                        verify_install(backup)
                        require_absent(old_backup, "obsolete backup path")
                        backup.replace(old_backup)
                    displaced.replace(backup)
                    verify_install(backup)
            if old_backup.exists():
                shutil.rmtree(old_backup)
            journal.unlink()
        finally:
            if stage.exists():
                shutil.rmtree(stage)
    return target


def status(project: Path) -> dict[str, str]:
    project = project.resolve()
    with project_lock(project):
        manifest = verify_install(project / INSTALL_DIRECTORY)
        state = "recovery-required" if (project / JOURNAL_NAME).exists() else "healthy"
        return {
            "status": state,
            "commit": str(manifest["source"]["commit"]),  # type: ignore[index]
            "distribution": str(manifest["distribution"]["id"]),  # type: ignore[index]
        }


def rollback(  # noqa: MC0001 - cross-resource rollback is one journaled state machine.
    project: Path, _locked: bool = False, provisioner=None
) -> Path:
    project = project.resolve()
    target = project / INSTALL_DIRECTORY
    backup = project / BACKUP_NAME
    swap = project / f"{INSTALL_DIRECTORY}-rollback"
    if not _locked:
        with project_lock(project):
            _recover_transaction(project)
            pending = read_cross_rollback_journal(project)
            receipt_controller = None
            receipt_value = None
            if target.exists():
                verify_install(target)
                receipt_controller = load_installed_controller(target, "hosts")
                host_receipt_path = project / ".chaos-engine-hosts.json"
                if host_receipt_path.exists() or is_link_or_reparse(host_receipt_path):
                    receipt_value, _ = receipt_controller.read_receipt(project)
                    if pending is None and receipt_value.get("rollbackIntent") is not None:
                        pending = receipt_value["rollbackIntent"]
            if pending is None and (not target.exists() or not backup.exists()):
                return rollback(project, _locked=True)
            verify_install(target)
            verify_install(backup)
            host_receipt = project / ".chaos-engine-hosts.json"
            if not host_receipt.exists() and not is_link_or_reparse(host_receipt):
                return rollback(project, _locked=True)
            if pending is None:
                current_commit = str(verify_install(target)["source"]["commit"])
                previous_commit = str(verify_install(backup)["source"]["commit"])
                previous_dependencies = load_dependency_controller(backup)
                account_receipt_path = project / ".chaos-engine-dependencies.json"
                account_rollback = (
                    hasattr(previous_dependencies, "install_account_dependencies")
                    and account_receipt_path.is_file()
                )
                previous_receipt = getattr(
                    receipt_controller, "rollback_previous_receipt", None
                )
                if account_rollback:
                    if is_link_or_reparse(account_receipt_path):
                        raise ValueError(
                            "account rollback has no exact prior dependency receipt"
                        )
                    if not callable(previous_receipt):
                        raise ValueError("account rollback has no exact prior host receipt")
                    raw = previous_receipt(project, previous_commit)
                    if not isinstance(raw, bytes):
                        raise ValueError("account rollback has no exact prior host receipt")
                    previous_account_receipt = getattr(
                        receipt_controller, "rollback_previous_account_receipt", None
                    )
                    if not callable(previous_account_receipt):
                        raise ValueError(
                            "account rollback has no exact prior dependency receipt"
                        )
                    account_raw = previous_account_receipt(project, previous_commit)
                    if not isinstance(account_raw, bytes):
                        raise ValueError(
                            "account rollback has no exact prior dependency receipt"
                        )
                    previous_mempalace_state = getattr(
                        receipt_controller, "rollback_previous_mempalace_state", None
                    )
                    if not callable(previous_mempalace_state):
                        raise ValueError("account rollback has no prior MemPalace state image")
                    mempalace_state = previous_mempalace_state(project, previous_commit)
                    if not isinstance(mempalace_state, dict):
                        raise ValueError("account rollback has no prior MemPalace state image")
                    pending = {
                        "desiredCommit": previous_commit,
                        "priorCommit": current_commit,
                        "priorHostReceipt": raw,
                        "priorAccountReceipt": account_raw,
                        "priorMempalaceState": mempalace_state,
                    }
                else:
                    write_cross_rollback_journal(project, previous_commit, current_commit)
                    pending = {"desiredCommit": previous_commit, "priorCommit": current_commit}
            desired_commit = pending["desiredCommit"]
            prior_commit = pending["priorCommit"]
            target_commit = str(verify_install(target)["source"]["commit"])
            backup_commit = str(verify_install(backup)["source"]["commit"])
            if {target_commit, backup_commit} != {desired_commit, prior_commit}:
                raise ValueError("rollback trees do not match the recorded generations")
            host_controller = load_installed_controller(target, "hosts")
            host_receipt_value, _ = host_controller.read_receipt(project)
            host_commit = host_receipt_value.get("coreCommit")
            intent = host_receipt_value.get("rollbackIntent")
            if intent is None and target_commit == prior_commit and host_commit == prior_commit:
                host_controller.set_rollback_intent(project, desired_commit, prior_commit)
                host_receipt_value, _ = host_controller.read_receipt(project)
                intent = host_receipt_value.get("rollbackIntent")
            if intent != {"desiredCommit": desired_commit, "priorCommit": prior_commit}:
                raise ValueError("rollback state does not match the recorded phase")
            valid_phase = (
                target_commit == prior_commit and backup_commit == desired_commit and host_commit == prior_commit
            ) or (
                target_commit == desired_commit
                and backup_commit == prior_commit
                and host_commit in {prior_commit, desired_commit}
            )
            if not valid_phase:
                raise ValueError("rollback state does not match the recorded phase")
            generation_previous = None
            generation_pointer_already_published = False
            previous_specification_sha256 = None
            previous_core_sha256 = None
            current_dependencies = load_dependency_controller(target)
            desired_root = target if target_commit == desired_commit else backup
            previous_dependencies = load_dependency_controller(desired_root)
            account_receipt_path = project / ".chaos-engine-dependencies.json"
            previous_account_mode = (
                hasattr(previous_dependencies, "install_account_dependencies")
                and account_receipt_path.is_file()
            )
            if previous_account_mode and is_link_or_reparse(account_receipt_path):
                raise ValueError("account rollback has no exact prior dependency receipt")
            if not previous_account_mode and hasattr(current_dependencies, "validated_previous") and hasattr(
                previous_dependencies, "publish_pointer"
            ):
                previous_specification = previous_dependencies.load_specification(
                    desired_root / "dependencies.json"
                )
                previous_specification_sha256 = (
                    previous_dependencies.specification_digest(previous_specification)
                )
                previous_core_sha256 = file_sha256(desired_root / MANIFEST_NAME)
                pointer = current_dependencies.pointer_records(project)
                active_record = pointer["active"]
                if (
                    active_record["specificationSha256"]
                    == previous_specification_sha256
                    and active_record["coreSha256"] == previous_core_sha256
                ):
                    _, authenticated_pointer = current_dependencies.active_generation(
                        project,
                        expected_specification_sha256=previous_specification_sha256,
                        expected_core_sha256=previous_core_sha256,
                    )
                    if authenticated_pointer["active"] != active_record:
                        raise ValueError("dependency pointer changed during rollback recovery")
                    generation_previous = active_record
                    generation_pointer_already_published = True
                else:
                    generation_previous = current_dependencies.validated_previous(
                        project,
                        previous_specification_sha256,
                        previous_core_sha256,
                    )
            if target_commit != desired_commit:
                rollback(project, _locked=True)
            try:
                previous_hosts = load_installed_controller(target, "hosts")
                prior_host_receipt = pending.get("priorHostReceipt")
                prior_host_receipt_value = (
                    validate_prior_host_receipt(
                        project, previous_hosts, prior_host_receipt, desired_commit
                    )
                    if isinstance(prior_host_receipt, bytes)
                    else None
                )
                prior_account_receipt = pending.get("priorAccountReceipt")
                prior_account_receipt_value = (
                    prior_account_receipt
                    if isinstance(prior_account_receipt, bytes)
                    else None
                )
                if previous_account_mode and prior_account_receipt_value is None:
                    raise ValueError("account rollback has no exact prior dependency receipt")
                prior_mempalace_state = pending.get("priorMempalaceState")
                if previous_account_mode and not isinstance(prior_mempalace_state, dict):
                    raise ValueError("account rollback has no prior MemPalace state image")
                restored_prior_host_receipt = (
                    previous_account_mode and prior_host_receipt_value is not None
                )
                if restored_prior_host_receipt:
                    restore_candidate_mempalace_state(project, prior_mempalace_state)
                    _current_receipt, current_raw = previous_hosts.read_receipt(project)
                    current_images = previous_hosts.current_images(project)
                    prior_after = previous_hosts.decode_images(
                        prior_host_receipt_value["after"], nullable=True
                    )
                    previous_hosts.reconcile(
                        project, prior_after, (current_images, prior_after)
                    )
                    previous_hosts.write_receipt(
                        project, prior_host_receipt_value, current_raw
                    )
                else:
                    desired_manifest = verify_install(target)
                    previous_hosts.install(
                        project,
                        core_commit=desired_commit,
                        capability_policy_digest=desired_manifest.get("capabilityPolicySha256"),
                        dependency_runtime=(
                            project / getattr(previous_dependencies, "GENERATIONS_NAME", ".chaos-engine-runtime-generations") / generation_previous["generationId"]
                            if generation_previous is not None else None
                        ),
                    )
                    restored_host_receipt, _ = previous_hosts.read_receipt(project)
                    if isinstance(restored_host_receipt.get("clientActivation"), dict):
                        previous_hosts.activate_detected_plugins(project)
                    if generation_previous is not None:
                        restored_host_receipt, restored_host_raw = previous_hosts.read_receipt(
                            project
                        )
                        previous_hosts.apply_hook_receipt(
                            restored_host_receipt,
                            previous_hosts.decode_images(
                                restored_host_receipt["before"], nullable=True
                            ),
                            previous_hosts.decode_images(
                                restored_host_receipt["after"], nullable=True
                            ),
                        )
                        previous_hosts.write_receipt(
                            project, restored_host_receipt, restored_host_raw
                        )
                previous_dependencies = load_dependency_controller(target)
                if previous_account_mode:
                    current_account_receipt = account_receipt_path.read_bytes()
                    previous_hosts.atomic_write(
                        project,
                        account_receipt_path,
                        prior_account_receipt_value,
                        current_account_receipt,
                    )
                    if account_receipt_path.read_bytes() != prior_account_receipt_value:
                        raise ValueError("account dependency receipt changed during rollback")
                if (
                    generation_previous is not None
                    and not generation_pointer_already_published
                ):
                    previous_dependencies.publish_pointer(
                        project,
                        generation_previous,
                        expected_specification_sha256=previous_specification_sha256,
                        expected_core_sha256=previous_core_sha256,
                    )
                elif not previous_account_mode:
                    runtime = project / ".chaos-engine-runtime"
                    removed_newer_runtime = False
                    if not hasattr(previous_dependencies, "RUNTIME_CONTRACT_VERSION"):
                        removed_newer_runtime = not runtime.exists()
                        if runtime.exists():
                            current_dependencies = load_dependency_controller(backup)
                            current_receipt = current_dependencies.read_receipt(runtime)
                            ownership = current_receipt.get("ownership")
                            links = (
                                ownership.get("links", [])
                                if isinstance(ownership, dict)
                                else None
                            )
                            if links:
                                current_dependencies.remove(
                                    runtime,
                                    current_dependencies.load_specification(
                                        backup / "dependencies.json"
                                    ),
                                )
                                removed_newer_runtime = True
                    if not removed_newer_runtime:
                        repair = provisioner or previous_dependencies.repair
                        repair(
                            runtime,
                            previous_dependencies.load_specification(
                                target / "dependencies.json"
                            ),
                        )
            except BaseException:
                raise
            transaction_path = project / CROSS_ROLLBACK_JOURNAL_NAME
            journal_path = transaction_path / "journal.json"
            if journal_path.exists():
                journal_path.unlink()
            if transaction_path.exists():
                transaction_path.rmdir()
            if restored_prior_host_receipt:
                pass
            elif prior_host_receipt_value is not None:
                prior_after = previous_hosts.decode_images(
                    prior_host_receipt_value["after"], nullable=True
                )
                if previous_hosts.current_images(project) == prior_after:
                    _, current_raw = previous_hosts.read_receipt(project)
                    previous_hosts.write_receipt(
                        project, prior_host_receipt_value, current_raw
                    )
                else:
                    previous_hosts.clear_rollback_intent(
                        project, desired_commit, prior_commit
                    )
            else:
                previous_hosts.clear_rollback_intent(project, desired_commit, prior_commit)
            return target
    with (nullcontext() if _locked else project_lock(project)):
        _recover_transaction(project)
        if not target.exists() and backup.exists():
            verify_install(backup)
            backup.replace(target)
            return target
        current = verify_install(target)
        previous = verify_install(backup)
        require_absent(swap, "rollback scratch path")
        journal = write_journal(project, "rollback", str(previous["source"]["commit"]))  # type: ignore[index]
        target.replace(swap)
        try:
            backup.replace(target)
            swap.replace(backup)
        except BaseException:
            raise
        verify_install(target)
        verify_install(backup)
        journal.unlink()
        del current
    return target


def uninstall(
    project: Path,
    expected_commit: str | None = None,
    _locked: bool = False,
) -> None:
    project = project.resolve()
    target = project / INSTALL_DIRECTORY
    backup = project / BACKUP_NAME
    archive = project / UNINSTALL_ARCHIVE_NAME
    removed = project / UNINSTALL_CURRENT_NAME
    old_backup = project / UNINSTALL_OLD_BACKUP_NAME
    with (nullcontext() if _locked else project_lock(project)):
        _recover_transaction(project)
        if not target.exists():
            if backup.exists():
                verify_install(backup)
            return
        manifest = verify_install(target)
        if expected_commit is not None and manifest["source"]["commit"] != expected_commit:  # type: ignore[index]
            raise RuntimeError("installed ChaosEngine changed before uninstall commit")
        require_absent(archive, "uninstall recovery archive")
        require_absent(
            archive.with_suffix(archive.suffix + ".tmp"),
            "uninstall recovery archive scratch path",
        )
        require_absent(removed, "uninstall current-tree path")
        require_absent(old_backup, "uninstall old-backup path")
        if backup.exists():
            verify_install(backup)
        journal = write_journal(project, "uninstall", str(manifest["source"]["commit"]))  # type: ignore[index]
        target.replace(removed)
        verify_install(removed)
        archive_install(removed, archive)
        if backup.exists():
            backup.replace(old_backup)
        try:
            shutil.rmtree(removed)
        except BaseException:
            if not target.exists():
                restore_archive(archive, target)
            if old_backup.exists() and not backup.exists():
                old_backup.replace(backup)
            raise
        if old_backup.exists():
            shutil.rmtree(old_backup)
        archive.unlink()
        journal.unlink()


def preflight_uninstall(project: Path) -> None:
    project = project.resolve()
    target = project / INSTALL_DIRECTORY
    if not target.exists():
        return
    verify_install(target)
    require_absent(project / UNINSTALL_ARCHIVE_NAME, "uninstall recovery archive")
    archive = project / UNINSTALL_ARCHIVE_NAME
    require_absent(
        archive.with_suffix(archive.suffix + ".tmp"),
        "uninstall recovery archive scratch path",
    )
    require_absent(project / UNINSTALL_CURRENT_NAME, "uninstall current-tree path")
    require_absent(project / UNINSTALL_OLD_BACKUP_NAME, "uninstall old-backup path")


def load_installed_controller(installed_root: Path, name: str):
    path = installed_root / f"{name}.py"
    return types.SimpleNamespace(**runpy.run_path(str(path)))


def load_source_controller(name: str):
    return load_installed_controller(Path(__file__).resolve().parent, name)


@contextmanager
def staged_candidate_host_controller(
    project: Path,
    source: Path,
    commit: str,
    source_record: dict[str, str] | None,
    distribution: str,
):
    """Load preflight hosts only from an ownership-verified candidate stage."""
    source = source.resolve()
    if source == project or source.is_relative_to(project) or project.is_relative_to(source):
        raise ValueError("ChaosEngine source and project trees must be disjoint")
    desired_source = normalize_source_record({"commit": commit, "kind": "local"})
    if source_record is not None:
        desired_source = normalize_source_record(source_record)
        if desired_source.get("commit") != commit or desired_source.get("kind") not in {
            "git",
            "git-digest",
        }:
            raise ValueError("ChaosEngine source record is invalid")
    if (source / MANIFEST_NAME).exists():
        raise ValueError(f"source contains the reserved manifest path: {MANIFEST_NAME}")
    _, policy_digest = load_distribution(source, distribution)
    capabilities, capability_digest = load_capability_policy(source, distribution)
    files = source_files(source, distribution)
    ownership = {path.relative_to(source).as_posix(): file_sha256(path) for path in files}
    with tempfile.TemporaryDirectory(prefix=f"{INSTALL_DIRECTORY}-candidate-", dir=project) as name:
        stage = Path(name)
        for path in files:
            destination = stage / path.relative_to(source)
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(path, destination)
        verify_staged_payload(stage, ownership)
        (stage / MANIFEST_NAME).write_text(
            json.dumps(
                {
                    "schemaVersion": SCHEMA_VERSION,
                    "distribution": {"id": distribution, "policySha256": policy_digest},
                    "capabilities": capabilities,
                    "capabilityPolicySha256": capability_digest,
                    "source": desired_source,
                    "files": ownership,
                    "hostToken": secrets.token_hex(32),
                },
                indent=2,
                sort_keys=True,
            ) + "\n",
            encoding="utf-8",
        )
        verify_install(stage)
        yield load_installed_controller(stage, "hosts")


def load_dependency_controller(installed_root: Path):
    return load_installed_controller(installed_root, "dependencies")


def ensure_maven_tools(  # noqa: MC0001 - cross-resource provisioning is one transaction.
    target: Path, specification: dict[str, object], *, runner=subprocess.run,
    reporter=None, confirmer=None, opener=None, mode: str = "native",
) -> tuple[Path, Path] | dict[str, str]:
    hosts = load_installed_controller(target, "hosts")
    dependencies = load_dependency_controller(target)
    contract = specification["dependencies"]["maven-tools-mcp"]
    resolver_options = {} if opener is None else {"opener": opener}
    version = dependencies.resolve_stable_version(
        "maven-tools-mcp", contract, **resolver_options
    )
    if mode == "docker":
        docker = shutil.which("docker")
        if not docker:
            raise ValueError("explicit Maven Tools Docker mode requires healthy Docker")
        probe = runner(
            [docker, "version", "--format", "{{.Server.Version}}"],
            capture_output=True, text=True, check=False, timeout=30,
        )
        if probe.returncode != 0:
            raise ValueError("explicit Maven Tools Docker mode requires healthy Docker")
        return {
            "mode": "docker",
            "command": str(Path(docker).resolve()),
            "image": f"arvindand/maven-tools-mcp:{version}",
        }
    if mode != "native":
        raise ValueError("unsupported Maven Tools mode")
    tag = f"v{version}"
    cache_status = hosts.maven_tools_cache_status(version)
    if cache_status["status"] == "healthy":
        existing = hosts.discover_maven_tools_runtime()
        if existing is not None and hosts.probe_maven_tools_runtime(*existing):
            return existing
    elif cache_status["status"] != "absent":
        raise ValueError("Maven Tools MCP cache is invalid")
    java_candidates = []
    configured = os.environ.get("CHAOSENGINE_JAVA")
    java_home = os.environ.get("JAVA_HOME")
    path_java = shutil.which("java")
    if configured:
        java_candidates.append(Path(configured).expanduser())
    if java_home:
        java_candidates.append(Path(java_home) / "bin" / ("java.exe" if os.name == "nt" else "java"))
    if path_java:
        java_candidates.append(Path(path_java))
    java = next(
        (
            item.resolve()
            for item in java_candidates
            if item.is_file() and hosts.java_major(item.resolve()) == 25
        ),
        None,
    )
    if java is None:
        raise ValueError("system Temurin Java 25 is required for Maven Tools MCP")
    cache_root = hosts.maven_tools_cache_root()
    cache_root.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix=".maven-tools-source-") as source_name:
        source = Path(source_name) / "source"
        git = shutil.which("git")
        if not git:
            raise ValueError("git is required to install Maven Tools MCP")
        if confirmer is not None:
            confirmer(f"Download Maven Tools {tag} source with git")
        if reporter is not None:
            reporter.start("Install Maven Tools", detail=tag)
        runner([
            git, "clone", "--branch", tag, "--depth", "1",
            "https://github.com/arvindand/maven-tools-mcp.git", str(source),
        ], check=True, timeout=300)
        revision = runner(
            [git, "-C", str(source), "rev-parse", "HEAD"], check=True,
            capture_output=True, text=True, timeout=30,
        ).stdout.strip().casefold()
        if re.fullmatch(r"[0-9a-f]{40}", revision) is None:
            raise ValueError("Maven Tools stable tag did not resolve to an immutable commit")
        wrapper = source / ("mvnw.cmd" if os.name == "nt" else "mvnw")
        if not wrapper.is_file():
            raise ValueError("Maven Tools upstream wrapper is missing")
        environment = os.environ.copy()
        environment["JAVA_HOME"] = str(java.parent.parent)
        if confirmer is not None:
            confirmer("Build and install Maven Tools")
        runner([str(wrapper), "-B", "clean", "package", "-Pci"], cwd=source, env=environment, check=True, timeout=900)
        built = source / f"target/maven-tools-mcp-{version}.jar"
        if not built.is_file() or built.stat().st_size == 0:
            raise ValueError("Maven Tools build did not produce the pinned JAR")
        with tempfile.TemporaryDirectory(prefix=".publishing-", dir=cache_root) as staging_name:
            staging = Path(staging_name)
            jar = staging / built.name
            shutil.copyfile(built, jar)
            receipt = {"version": version, "commit": revision, "jar": jar.name, "sha256": file_sha256(jar)}
            (staging / hosts.MAVEN_TOOLS_MCP_RECEIPT).write_text(json.dumps(receipt, sort_keys=True) + "\n", encoding="utf-8")
            hosts.publish_maven_tools_cache(staging)
    runtime = hosts.discover_maven_tools_runtime()
    if runtime is None or not hosts.probe_maven_tools_runtime(*runtime):
        raise ValueError("Maven Tools MCP publication probe failed")
    return runtime


def installed_kernel_status(installed_root: Path) -> dict[str, object]:
    """Report canonical kernel health and declared host coverage."""
    try:
        kernel = load_installed_controller(installed_root / "hooks", "kernel")
        errors = [*kernel.validate_lifecycle(), *kernel.validate_rules(kernel.RULES)]
        hosts = sorted(kernel.HOST_CAPABILITIES)
        if hosts != ["claude", "codex", "copilot", "gemini", "grok"]:
            errors.append("kernel host capability matrix is incomplete")
        return {
            "status": "healthy" if not errors else "recovery-required",
            "schemaVersion": kernel.SCHEMA_VERSION,
            "hosts": hosts,
            "capabilities": {
                host: {
                    "events": list(kernel.HOST_CAPABILITIES[host].supported_events),
                    "strictJsonStdout": getattr(
                        kernel.HOST_CAPABILITIES[host], "strict_json_stdout", True
                    ),
                    "liveGate": getattr(kernel.HOST_CAPABILITIES[host], "live_gate", False),
                    "staticSurfaces": list(
                        getattr(kernel.HOST_CAPABILITIES[host], "static_surfaces", ())
                    ),
                }
                for host in hosts
            },
            "errors": errors,
        }
    except (OSError, RuntimeError, ValueError) as error:
        return {"status": "recovery-required", "errors": [str(error)]}


def install_with_dependencies(  # noqa: MC0001 - owned resources share one compensation boundary.
    project: Path,
    source: Path,
    commit: str,
    provisioner=None,
    source_record: dict[str, str] | None = None,
    distribution: str = DEFAULT_DISTRIBUTION,
    with_maven_tools: bool = False,
    maven_tools_mode: str = "native",
    reporter=None,
    confirmer=None,
) -> Path:
    project = project.resolve()
    source = source.absolute()
    reject_link_or_reparse(source)
    source = source.resolve()
    with_maven_tools = with_maven_tools or (project / "pom.xml").is_file()
    source_dependencies = load_dependency_controller(source)
    account_mode = provisioner is None and hasattr(
        source_dependencies, "install_account_dependencies"
    )
    generation_mode = provisioner is None and not account_mode
    with project_lock(project):
        if read_cross_rollback_journal(project) is not None:
            raise ValueError("rollback recovery is required before install")
        current = project / INSTALL_DIRECTORY
        old_commit = None
        old_manifest = None
        host_snapshot = None
        old_specification_sha256 = None
        old_core_sha256 = None
        account_receipt_path = project / ".chaos-engine-dependencies.json"
        account_receipt_before = (
            account_receipt_path.read_bytes()
            if account_mode and account_receipt_path.is_file()
            and not is_link_or_reparse(account_receipt_path) else None
        )
        project_setup_snapshot = None
        project_setup_before: dict[str, tuple[bool, dict[str, str]]] = {}
        project_setup_after: dict[str, dict[str, str]] | None = None
        mempalace_state_before = (False, {})
        rollback_mempalace_state = None
        if current.exists():
            old_manifest = inspect_current_install(current)
            if old_manifest is not None:
                old_commit = str(old_manifest["source"]["commit"])
                with staged_candidate_host_controller(
                    project, source, commit, source_record, distribution
                ) as candidate_host_controller:
                    host_receipt_path = project / candidate_host_controller.RECEIPT_NAME
                    if host_receipt_path.exists() or is_link_or_reparse(host_receipt_path):
                        try:
                            host_snapshot = candidate_host_controller.preflight(project)
                        except ValueError as error:
                            raise ValueError(
                                f"ChaosEngine host adapter drift detected (receipt integrity drift): {project}"
                            ) from error
                if generation_mode:
                    try:
                        old_dependencies = load_dependency_controller(current)
                        old_specification = old_dependencies.load_specification(
                            current / "dependencies.json"
                        )
                        old_specification_sha256 = old_dependencies.specification_digest(
                            old_specification
                        )
                        old_core_sha256 = file_sha256(current / MANIFEST_NAME)
                        old_dependencies.active_generation(
                            project,
                            expected_specification_sha256=old_specification_sha256,
                            expected_core_sha256=old_core_sha256,
                        )
                    except (OSError, ValueError):
                        old_specification_sha256 = None
                        old_core_sha256 = None
        if account_mode:
            project_setup_snapshot, project_setup_before = snapshot_project_setup_outputs(
                project
            )
            mempalace_state_before = project_setup_output_files(
                project, MEMPALACE_STATE_OUTPUT
            )
        try:
            target = install(
                project,
                source,
                commit,
                _locked=True,
                source_record=source_record,
                distribution=distribution,
            )
        except BaseException:
            if project_setup_snapshot is not None:
                shutil.rmtree(project_setup_snapshot, ignore_errors=True)
            raise
        installed_manifest = verify_install(target)
        core_changed = old_manifest is None or {
            key: value
            for key, value in installed_manifest.items()
            if key not in {"capabilities", "capabilityPolicySha256"}
        } != {
            key: value
            for key, value in old_manifest.items()
            if key not in {"capabilities", "capabilityPolicySha256"}
        }
        host_controller = load_installed_controller(target, "hosts")
        host_receipt = project / host_controller.RECEIPT_NAME
        host_existed = host_receipt.exists() or is_link_or_reparse(host_receipt)
        host_created = False
        runtime = project / ".chaos-engine-runtime"
        runtime_existed = runtime.exists() or is_link_or_reparse(runtime)
        controller = None
        specification = None
        candidate = None
        retired_generation = None
        candidate_published = False
        account_receipt_after = None
        try:
            controller = load_dependency_controller(target)
            specification = controller.load_specification(target / "dependencies.json")
            maven_docker = None
            if with_maven_tools:
                maven_setup = ensure_maven_tools(
                    target, specification, reporter=reporter, confirmer=confirmer,
                    mode=maven_tools_mode,
                )
                if isinstance(maven_setup, dict):
                    maven_docker = (
                        str(maven_setup["command"]), str(maven_setup["image"])
                    )
            dependency_generation = None
            account_commands = None
            if generation_mode:
                specification_sha256 = controller.specification_digest(specification)
                core_sha256 = file_sha256(target / MANIFEST_NAME)
                try:
                    controller.active_generation(
                        project,
                        expected_specification_sha256=specification_sha256,
                        expected_core_sha256=core_sha256,
                    )
                except (OSError, ValueError):
                    callback_options = {}
                    if reporter is not None:
                        callback_options["reporter"] = reporter
                    if confirmer is not None:
                        callback_options["confirmer"] = confirmer
                    candidate = controller.prepare_candidate(
                        project, specification, core_sha256, **callback_options
                    )
                if candidate is not None:
                    dependency_generation = project / getattr(
                        controller,
                        "GENERATIONS_NAME",
                        ".chaos-engine-runtime-generations",
                    ) / candidate["generationId"]
                else:
                    dependency_generation = controller.active_generation(
                        project,
                        expected_specification_sha256=specification_sha256,
                        expected_core_sha256=core_sha256,
                    )[0]
            elif account_mode:
                account_receipt = controller.install_account_dependencies(
                    project, specification
                )
                account_receipt_after = (
                    account_receipt_path.read_bytes()
                    if account_receipt_path.is_file() else None
                )
                account_commands = account_receipt.get("commands")
                project_setup_after = project_setup_after_images(project)
                mempalace_state_after = project_setup_output_files(
                    project, MEMPALACE_STATE_OUTPUT
                )
                if old_manifest is not None:
                    rollback_mempalace_state = mempalace_rollback_image(
                        mempalace_state_before, mempalace_state_after
                    )
            host_controller.install(
                project,
                core_commit=commit,
                capability_policy_digest=installed_manifest.get("capabilityPolicySha256"),
                dependency_runtime=dependency_generation,
                account_commands=account_commands,
                maven_docker=maven_docker,
                **(
                    {"rollback_account_receipt": account_receipt_before}
                    if account_mode and account_receipt_before is not None
                    else {}
                ),
                **(
                    {"rollback_mempalace_state": rollback_mempalace_state}
                    if rollback_mempalace_state is not None else {}
                ),
                **({"upgrade_snapshot": host_snapshot} if host_snapshot is not None else {}),
            )
            host_created = not host_existed
            if not account_mode and not generation_mode:
                provisioner(runtime, specification)
            if not account_mode:
                host_controller.initialize_mempalace_runtime(project)
            if candidate is not None:
                try:
                    retired_generation = controller.pointer_records(project).get(
                        "previous"
                    )
                except (OSError, ValueError):
                    retired_generation = None
                published_pointer = controller.publish_pointer(
                    project,
                    candidate,
                    expected_specification_sha256=specification_sha256,
                    expected_core_sha256=core_sha256,
                )
                candidate_published = True
                if retired_generation is not None and retired_generation not in (
                    published_pointer["active"],
                    published_pointer.get("previous"),
                ):
                    controller.remove_generation(project, retired_generation)
            if old_commit is not None:
                client_plugins = host_controller.detected_plugin_status(project)
                if any(item.get("plugin") == "stale" for item in client_plugins.values()):
                    host_controller.activate_detected_plugins(project)
        except BaseException as error:
            if isinstance(error, (KeyboardInterrupt, SystemExit)):
                raise
            compensation_errors: list[BaseException] = []
            restore_generation = None
            if candidate is not None and not candidate_published:
                try:
                    pointer = controller.pointer_records(project)
                    candidate_published = pointer.get("active") == candidate
                except (OSError, ValueError):
                    # Compensation continues from the last authenticated state.
                    pass
            if candidate_published and old_specification_sha256 and old_core_sha256:
                try:
                    restore_generation = controller.validated_previous(
                        project,
                        expected_specification_sha256=old_specification_sha256,
                        expected_core_sha256=old_core_sha256,
                    )
                except BaseException as cleanup_error:
                    compensation_errors.append(cleanup_error)
            can_compensate = not candidate_published or restore_generation is not None
            if candidate is not None and not candidate_published:
                try:
                    controller.remove_generation(project, candidate)
                except BaseException as cleanup_error:
                    compensation_errors.append(cleanup_error)
            if can_compensate and (
                not runtime_existed
                and runtime.exists()
                and controller is not None
                and specification is not None
            ):
                try:
                    controller.remove(runtime, specification)
                except BaseException as cleanup_error:
                    compensation_errors.append(cleanup_error)
            if can_compensate and host_snapshot is not None:
                try:
                    host_controller.restore_snapshot(project, host_snapshot)
                except BaseException as cleanup_error:
                    compensation_errors.append(cleanup_error)
            if can_compensate and account_mode and account_receipt_after is not None:
                try:
                    if account_receipt_path.read_bytes() != account_receipt_after:
                        raise ValueError("account dependency receipt changed during compensation")
                    if account_receipt_before is None:
                        account_receipt_path.unlink()
                    else:
                        account_receipt_path.write_bytes(account_receipt_before)
                except BaseException as cleanup_error:
                    compensation_errors.append(cleanup_error)
            elif can_compensate and host_created:
                try:
                    host_controller.uninstall(project)
                except BaseException as cleanup_error:
                    compensation_errors.append(cleanup_error)
            if (
                can_compensate
                and project_setup_snapshot is not None
                and project_setup_after is not None
            ):
                try:
                    restore_project_setup_outputs(
                        project,
                        project_setup_snapshot,
                        project_setup_before,
                        project_setup_after,
                    )
                except BaseException as cleanup_error:
                    compensation_errors.append(cleanup_error)
            if can_compensate:
                try:
                    backup_path = project / BACKUP_NAME
                    if old_commit is None and try_verify_install(backup_path) is not None:
                        rollback(project, _locked=True)
                    elif old_commit is None:
                        uninstall(project, expected_commit=commit, _locked=True)
                    elif core_changed and backup_path.exists():
                        rollback(project, _locked=True)
                    if restore_generation is not None:
                        controller.publish_pointer(
                            project,
                            restore_generation,
                            expected_specification_sha256=old_specification_sha256,
                            expected_core_sha256=old_core_sha256,
                        )
                except BaseException as cleanup_error:
                    compensation_errors.append(cleanup_error)
            elif old_commit is not None:
                try:
                    prior_host_receipt = (
                        host_snapshot.get("raw") if isinstance(host_snapshot, dict) else None
                    )
                    write_cross_rollback_journal(
                        project,
                        old_commit,
                        commit,
                        prior_host_receipt=(
                            prior_host_receipt if isinstance(prior_host_receipt, bytes) else None
                        ),
                    )
                except BaseException as cleanup_error:
                    compensation_errors.append(cleanup_error)
            if compensation_errors:
                if len(compensation_errors) == 1:
                    raise compensation_errors[0] from error
                details = "; ".join(str(item) for item in compensation_errors)
                raise RuntimeError(f"ChaosEngine compensation failures: {details}") from error
            raise
        finally:
            if project_setup_snapshot is not None:
                shutil.rmtree(project_setup_snapshot, ignore_errors=True)
        return target


def attach_component_status(
    result: dict[str, object],
    project: Path,
    target: Path,
    dependency_health: str,
    host_controller: object,
    *,
    inspect_retrieval_state: bool = True,
) -> None:
    manifest = load_manifest(target)
    capabilities = manifest.get("capabilities")
    if not isinstance(capabilities, dict):
        capabilities = legacy_capability_policy()
    component_paths = {
        "core": [
            target / "skills/chaos-engine/SKILL.md",
            target / "vendor/caveman/PIN.json",
            target / "vendor/ponytail/PIN.json",
        ],
        "skills": [
            project / ".agents/skills/chaos-engine/SKILL.md",
            project / "plugins/caveman/skills/caveman/SKILL.md",
            project / "plugins/ponytail/skills/ponytail/SKILL.md",
        ],
        "playbooks": [target / "references/work-github-playbook.md"],
        "hooks": [
            target / "hooks/guard.py",
            target / "hooks/kernel.py",
            target / "hooks/launch.js",
            target / "hooks/lifecycle.py",
            target / "hooks/reflection.py",
            project / ".codex/hooks.json",
            project / ".grok/hooks/lifecycle.json",
            project / ".gemini/settings.json",
            project / ".github/hooks/chaos-engine.json",
            project / "plugins/chaos-engine/hooks/hooks.json",
            project / "plugins/chaos-engine/hooks/launch.js",
            project / "plugins/caveman/src/hooks/caveman-activate.js",
            project / "plugins/caveman/src/hooks/caveman-mode-tracker.js",
            project / "plugins/ponytail/hooks/ponytail-activate.js",
            project / "plugins/ponytail/hooks/ponytail-mode-tracker.js",
        ],
        "plugins": [
            project / ".agents/plugins/marketplace.json",
            project / ".claude-plugin/marketplace.json",
            project / "plugins/chaos-engine/.codex-plugin/plugin.json",
            project / "plugins/chaos-engine/.claude-plugin/plugin.json",
            project / "plugins/caveman/.codex-plugin/plugin.json",
            project / "plugins/caveman/.claude-plugin/plugin.json",
            project / "plugins/ponytail/.codex-plugin/plugin.json",
            project / "plugins/ponytail/.claude-plugin/plugin.json",
        ],
        "roles": [
            *(project / ".claude/agents").glob("chaos-engine-*"),
            *(project / ".codex/agents").glob("chaos-engine-*"),
        ],
        "mcps": [project / ".mcp.json", project / ".codex/config.toml"],
        "retrieval-config": [
            project / ".memory/config.json",
            project / "mempalace.yaml",
        ],
        "projection-policy": [target / MANIFEST_NAME],
    }
    components: dict[str, dict[str, object]] = {}
    for name, paths in component_paths.items():
        expected_count = 10 if name == "roles" else len(paths)
        healthy = len(paths) == expected_count and all(path.is_file() for path in paths)
        if name == "retrieval-config" and healthy:
            healthy = bool(host_controller.retrieval_configs_healthy(project))
        components[name] = {"status": "healthy" if healthy else "absent", **capabilities[name]}
    for name in ("tools", "memory", "mempalace", "graphify"):
        components[name] = {"status": dependency_health, **capabilities[name]}
    if inspect_retrieval_state:
        mempalace_state = host_controller.mempalace_runtime_status(project)
        if mempalace_state.get("status") != "healthy":
            components["mempalace"] = {**mempalace_state, **capabilities["mempalace"]}
    cache_state = host_controller.maven_tools_cache_status()
    components["maven-tools-mcp"] = {
        **cache_state,
        **capabilities["maven-tools-mcp"],
    }
    result["components"] = components
    if any(
        item["status"] != "healthy" and item["taskImpact"] != "optional"
        for item in components.values()
    ):
        result["status"] = "recovery-required"


def status_with_dependencies(project: Path, *, active_probes: bool = False) -> dict[str, object]:
    project = project.resolve()
    runtime = project / ".chaos-engine-runtime"
    with project_lock(project):
        with dependency_runtime_lock(runtime):
            target = project / INSTALL_DIRECTORY
            manifest = verify_install(target)
            state = (
                "recovery-required"
                if (project / JOURNAL_NAME).exists()
                or (project / CROSS_ROLLBACK_JOURNAL_NAME).exists()
                else "healthy"
            )
            result: dict[str, object] = {
                "status": state,
                "commit": str(manifest["source"]["commit"]),  # type: ignore[index]
                "distribution": str(manifest["distribution"]["id"]),  # type: ignore[index]
                "policySha256": str(manifest["distribution"]["policySha256"]),  # type: ignore[index]
            }
            result["kernel"] = installed_kernel_status(target)
            if result["kernel"]["status"] != "healthy":  # type: ignore[index]
                result["status"] = "recovery-required"
            host_controller = load_installed_controller(target, "hosts")
            pending_rollback = read_cross_rollback_journal(project)
            if pending_rollback is not None:
                result["hosts"] = host_controller.verify(project)
                result["hosts"]["status"] = "recovery-required"  # type: ignore[index]
            else:
                result["hosts"] = host_controller.verify(
                    project,
                    core_commit=str(manifest["source"]["commit"]),  # type: ignore[index]
                )
            if result["hosts"]["status"] != "healthy":  # type: ignore[index]
                result["status"] = "recovery-required"
            if active_probes:
                result["hosts"]["grok"] = host_controller.grok_runtime_status(project)  # type: ignore[index]
                if result["hosts"]["grok"]["status"] == "recovery-required":  # type: ignore[index]
                    result["hosts"]["status"] = "recovery-required"  # type: ignore[index]
                    result["status"] = "recovery-required"
            removing = project / ".chaos-engine-runtime.removing"
            backup = project / ".chaos-engine-runtime.backup"
            building = project / ".chaos-engine-runtime.building"
            if any(
                path.exists() or is_link_or_reparse(path)
                for path in (removing, backup, building)
            ):
                result["dependencies"] = {"status": "recovery-required"}
                attach_component_status(
                    result, project, target, "recovery-required", host_controller
                )
                return result
            controller = load_dependency_controller(target)
            account_receipt = project / getattr(
                controller,
                "ACCOUNT_RECEIPT_NAME",
                ".chaos-engine-dependencies.json",
            )
            if account_receipt.is_file() and not is_link_or_reparse(account_receipt):
                controller = load_dependency_controller(target)
                receipt = controller.read_account_receipt(project)
                records = receipt.get("components", {})
                healthy = isinstance(records, dict) and all(
                    isinstance(item, dict)
                    and item.get("status") == "healthy"
                    and item.get("action") != "blocked"
                    for item in records.values()
                )
                result["dependencies"] = {
                    "status": "healthy" if healthy else "recovery-required",
                    "schemaVersion": receipt["schemaVersion"],
                    "checkedAt": receipt.get("checkedAt"),
                    "scope": receipt.get("scope"),
                    "components": records,
                }
                attach_component_status(
                    result,
                    project,
                    target,
                    str(result["dependencies"]["status"]),
                    host_controller,
                    inspect_retrieval_state=False,
                )
                return result
            pointer_path = project / ".chaos-engine-runtime-current.json"
            if pointer_path.exists() or is_link_or_reparse(pointer_path):
                controller = load_dependency_controller(target)
                specification = controller.load_specification(
                    target / "dependencies.json"
                )
                generation, pointer = controller.active_generation(
                    project,
                    expected_specification_sha256=controller.specification_digest(
                        specification
                    ),
                    expected_core_sha256=file_sha256(target / MANIFEST_NAME),
                )
                if active_probes:
                    controller.probe_active(
                        project,
                        controller.specification_digest(specification),
                        file_sha256(target / MANIFEST_NAME),
                    )
                result["dependencies"] = {
                    "status": "healthy",
                    "generationId": pointer["active"]["generationId"],
                    "path": str(generation),
                }
                receipt = json.loads((generation / getattr(controller, "RECEIPT_NAME", "receipt.json")).read_text(encoding="utf-8"))
                if isinstance(receipt.get("runtimes"), dict):
                    result["dependencies"]["runtimes"] = receipt["runtimes"]
                attach_component_status(
                    result, project, target, "healthy", host_controller
                )
                return result
            if not runtime.exists():
                result["dependencies"] = {"status": "absent"}
                attach_component_status(result, project, target, "absent", host_controller)
                return result
            controller = load_dependency_controller(target)
            dependency_check = controller.doctor if active_probes else controller.status
            result["dependencies"] = dependency_check(
                runtime,
                specification=controller.load_specification(target / "dependencies.json"),
            )
            dependency_health = str(result["dependencies"].get("status"))  # type: ignore[union-attr]
            attach_component_status(
                result, project, target, dependency_health, host_controller
            )
            return result


def doctor_with_dependencies(
    project: Path, *, verify_clients: bool = True
) -> dict[str, object]:
    """Verify installed files and actively execute every dependency entrypoint probe."""
    result = status_with_dependencies(project, active_probes=True)
    target = project.resolve() / INSTALL_DIRECTORY
    host_controller = load_installed_controller(target, "hosts")
    retrieval = host_controller.retrieval_runtime_status(project.resolve())
    if retrieval.get("status") != "healthy":
        result["status"] = "recovery-required"
        components = result.get("components")
        if isinstance(components, dict) and isinstance(components.get("memory"), dict):
            components["memory"]["status"] = retrieval["status"]
            if retrieval.get("reason"):
                components["memory"]["reason"] = retrieval["reason"]
    dependency = result.get("dependencies")
    generation_path = dependency.get("path") if isinstance(dependency, dict) else None
    account_commands = None
    account_receipt = project.resolve() / ".chaos-engine-dependencies.json"
    if account_receipt.is_file() and not is_link_or_reparse(account_receipt):
        controller = load_dependency_controller(target)
        receipt = controller.read_account_receipt(project.resolve())
        commands = receipt.get("commands")
        if isinstance(commands, dict):
            account_commands = commands
    scripts = "Scripts" if os.name == "nt" else "bin"
    python_name = "python.exe" if os.name == "nt" else "python"
    managed_python = (
        Path(generation_path) / "uv-tools/mempalace" / scripts / python_name
        if isinstance(generation_path, str) else None
    )
    if (
        managed_python is None and account_commands is None
    ) or not host_controller.mcp_runtime_healthy(
        project.resolve(), managed_python, account_commands
    ):
        result["status"] = "recovery-required"
        components = result.get("components")
        if isinstance(components, dict) and isinstance(components.get("mcps"), dict):
            components["mcps"]["status"] = "recovery-required"
    hook_python = managed_python or Path(sys.executable).resolve()
    if not host_controller.hook_runtime_healthy(project.resolve(), hook_python):
        result["status"] = "recovery-required"
        components = result.get("components")
        if isinstance(components, dict) and isinstance(components.get("hooks"), dict):
            components["hooks"]["status"] = "recovery-required"
    if not verify_clients:
        return result
    clients = host_controller.detected_plugin_status(project.resolve())
    result["clients"] = clients
    if any(item.get("status") != "healthy" for item in clients.values()):
        result["status"] = "recovery-required"
        components = result.get("components")
        if isinstance(components, dict) and isinstance(components.get("plugins"), dict):
            components["plugins"]["status"] = "recovery-required"
    return result


_SECRET_FIELD = re.compile(
    r"(?:authorization|credential|password|private.?key|secret|token)", re.IGNORECASE
)
_URL_CREDENTIAL = re.compile(r"(?P<scheme>[a-z][a-z0-9+.-]*://)[^/@\s]+@", re.IGNORECASE)
_DIAGNOSTIC_FIELDS = {
    "status": {
        "schemaVersion", "identity", "kind", "status", "commit", "distribution",
        "policySha256", "kernel", "hosts", "dependencies", "components",
    },
    "doctor": {
        "schemaVersion", "identity", "kind", "status", "commit", "distribution",
        "policySha256", "kernel", "hosts", "dependencies", "components", "clients",
    },
    "explain": {
        "schemaVersion", "identity", "kind", "host", "event", "phase", "decision",
        "diagnosticCode", "reason", "remedy", "factsUsed", "effects", "terminalReason",
    },
}
_DIAGNOSTIC_REQUIRED_FIELDS = {
    kind: frozenset(fields) for kind, fields in _DIAGNOSTIC_FIELDS.items()
}


def _diagnostic_value(value: object, project: Path) -> object:
    """Return bounded, canonical JSON data without paths or credential-shaped fields."""
    if isinstance(value, dict):
        result: dict[str, object] = {}
        for key in sorted(str(item) for item in value):
            if key in {"path", "root"} or _SECRET_FIELD.search(key):
                continue
            result[key] = _diagnostic_value(value[key], project)
        return result
    if isinstance(value, list):
        return [_diagnostic_value(item, project) for item in value]
    if isinstance(value, tuple):
        return [_diagnostic_value(item, project) for item in value]
    if isinstance(value, str):
        rendered = _URL_CREDENTIAL.sub(r"\g<scheme><redacted>@", value)
        replacements = ((str(project.resolve()), "<project>"), (str(Path.home()), "<home>"))
        for source, replacement in replacements:
            if source:
                rendered = rendered.replace(source, replacement)
                rendered = rendered.replace(source.replace("\\", "/"), replacement)
        return rendered[:2048]
    if value is None or type(value) in {bool, int, float}:
        return value
    return type(value).__name__


def validate_diagnostic_json(document: object) -> dict[str, object]:
    """Reject schema drift in the current diagnostic contract."""
    if not isinstance(document, dict) or document.get("schemaVersion") != 2:
        raise ValueError("diagnostic JSON schema v2 is invalid")
    kind = document.get("kind")
    allowed = _DIAGNOSTIC_FIELDS.get(str(kind))
    if allowed is None or set(document) - allowed:
        raise ValueError("diagnostic JSON contains unknown or removed fields")
    required = {"schemaVersion", "identity", "kind"}
    if not required <= set(document) or document.get("identity") != CANONICAL_IDENTITY:
        raise ValueError("diagnostic JSON identity is invalid")
    missing = _DIAGNOSTIC_REQUIRED_FIELDS[str(kind)] - set(document)
    if missing:
        raise ValueError(
            "diagnostic JSON is missing required fields: " + ", ".join(sorted(missing))
        )
    return document


def status_json(project: Path, *, active_probes: bool = False) -> dict[str, object]:
    """Expose the stable secret-free status JSON v2 contract."""
    state = (
        doctor_with_dependencies(project)
        if active_probes
        else status_with_dependencies(project)
    )
    return validate_diagnostic_json({
        "schemaVersion": DIAGNOSTIC_SCHEMA_VERSION,
        "identity": CANONICAL_IDENTITY,
        "kind": "doctor" if active_probes else "status",
        **_diagnostic_value(state, project.resolve()),  # type: ignore[arg-type]
    })


def explain_json(
    project: Path,
    event: str,
    *,
    host: str,
    session_id: str = "diagnostic-session",
    phase: str = "ReadOnly",
    target_phase: str = "",
    cancelled: bool = False,
    timed_out: bool = False,
) -> dict[str, object]:
    """Evaluate one declarative event without reading ambient session state."""
    target = project.resolve() / INSTALL_DIRECTORY
    verify_install(target)
    kernel = load_installed_controller(target / "hooks", "kernel")
    if host not in kernel.HOST_CAPABILITIES:
        raise ValueError("unsupported diagnostic host")
    normalized = kernel.normalize_event(
        {
            "hook_event_name": event,
            "session_id": session_id,
            "phase": phase,
            "target_phase": target_phase,
            "cancelled": cancelled,
            "timed_out": timed_out,
        },
        host,
    )
    report = kernel.evaluate(normalized).to_dict()
    return validate_diagnostic_json({
        **_diagnostic_value(report, project.resolve()),  # type: ignore[arg-type]
        "schemaVersion": DIAGNOSTIC_SCHEMA_VERSION,
        "identity": CANONICAL_IDENTITY,
        "kind": "explain",
    })


def uninstall_with_dependencies(  # noqa: MC0001 - coordinated host, runtime, and core teardown.
    project: Path,
) -> None:
    project = project.resolve()
    with project_lock(project):
        _recover_transaction(project)
        if read_cross_rollback_journal(project) is not None:
            raise ValueError("rollback recovery is required before uninstall")
        target = project / INSTALL_DIRECTORY
        runtime = project / ".chaos-engine-runtime"
        removing = project / ".chaos-engine-runtime.removing"
        backup = project / ".chaos-engine-runtime.backup"
        building = project / ".chaos-engine-runtime.building"
        if any(path.exists() or is_link_or_reparse(path) for path in (backup, building)):
            raise ValueError("dependency recovery is required before uninstall")
        if not target.exists():
            host_receipt = project / ".chaos-engine-hosts.json"
            host_controller = None
            if host_receipt.exists() or is_link_or_reparse(host_receipt):
                host_controller = load_source_controller("hosts")
                receipt, _ = host_controller.read_receipt(project)
                if receipt["phase"] != "removing":
                    raise ValueError("host removal is not prepared for absent-core recovery")
            generation_controller = load_source_controller("dependencies")
            generation_pointer = project / generation_controller.POINTER_NAME
            generation_removing = project / generation_controller.POINTER_REMOVING_NAME
            if generation_pointer.exists() or is_link_or_reparse(generation_pointer):
                raise ValueError("dependency generation removal is not prepared")
            if generation_removing.exists() or is_link_or_reparse(generation_removing):
                generation_controller.finalize_generation_remove(project)
            with dependency_runtime_lock(runtime):
                if removing.exists():
                    finalize_dependency_tombstone(removing)
                if runtime.exists():
                    finalize_dependency_tombstone(runtime)
            if host_controller is not None:
                host_controller.finalize_uninstall(project)
            uninstall(project, _locked=True)
            return
        verify_install(target)
        controller = load_dependency_controller(target)
        host_controller = load_installed_controller(target, "hosts")
        specification = controller.load_specification(target / "dependencies.json")
        generation_pointer = project / controller.POINTER_NAME
        generation_removing = project / controller.POINTER_REMOVING_NAME
        if generation_removing.exists() or is_link_or_reparse(generation_removing):
            raise ValueError("dependency generation removal recovery is required")
        generation_mode = generation_pointer.exists() or is_link_or_reparse(generation_pointer)
        if generation_mode and (runtime.exists() or is_link_or_reparse(runtime)):
            raise ValueError("mixed dependency runtime ownership requires recovery")
        preflight_uninstall(project)
        manifest = verify_install(target)
        commit = str(manifest["source"]["commit"])
        prepared = False
        generation_prepared = False
        host_prepared = False
        try:
            host_controller.prepare_uninstall(project)
            host_prepared = True
            if generation_mode:
                controller.prepare_generation_remove(
                    project,
                    expected_specification_sha256=controller.specification_digest(
                        specification
                    ),
                    expected_core_sha256=file_sha256(target / MANIFEST_NAME),
                )
                generation_prepared = True
            elif runtime.exists() or is_link_or_reparse(runtime):
                controller.prepare_remove(runtime, specification)
                prepared = True
            uninstall(project, expected_commit=commit, _locked=True)
        except BaseException as error:
            if isinstance(error, (KeyboardInterrupt, SystemExit)):
                raise
            compensation_errors: list[BaseException] = []
            if generation_prepared:
                try:
                    controller.cancel_generation_remove(project)
                except BaseException as cleanup_error:
                    compensation_errors.append(cleanup_error)
            if prepared:
                try:
                    controller.cancel_remove(runtime)
                except BaseException as cleanup_error:
                    compensation_errors.append(cleanup_error)
            if host_prepared:
                try:
                    host_controller.cancel_uninstall(project)
                except BaseException as cleanup_error:
                    compensation_errors.append(cleanup_error)
            if compensation_errors:
                if len(compensation_errors) == 1:
                    raise compensation_errors[0] from error
                details = "; ".join(str(item) for item in compensation_errors)
                raise RuntimeError(f"ChaosEngine uninstall compensation failures: {details}") from error
            raise
        if prepared or removing.exists():
            controller.finalize_remove(runtime, specification)
        if generation_prepared:
            controller.finalize_generation_remove(project)
        host_controller.finalize_uninstall(project)


def finalize_dependency_tombstone(removing: Path) -> None:
    if is_link_or_reparse(removing):
        raise ValueError("dependency removal path is a link or reparse point")
    if not any(removing.iterdir()):
        removing.rmdir()
        return
    entries = dependency_tombstone_entries(removing)
    for path in entries:
        if is_link_or_reparse(path) and not path.is_symlink():
            raise ValueError("dependency removal contains an unsupported reparse point")
    receipt_path = removing / "receipt.json"
    receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
    integrity = receipt.pop("receiptIntegritySha256", None)
    encoded = json.dumps(receipt, sort_keys=True, separators=(",", ":")).encode()
    if integrity != hashlib.sha256(encoded).hexdigest():
        raise ValueError("dependency removal receipt integrity drift detected")
    ownership = receipt.get("ownership")
    files = ownership.get("files") if isinstance(ownership, dict) else None
    directories = ownership.get("directories") if isinstance(ownership, dict) else None
    links = ownership.get("links", []) if isinstance(ownership, dict) else None
    if (
        not isinstance(files, dict)
        or not isinstance(directories, list)
        or not isinstance(links, list)
        or not all(
            isinstance(link, dict)
            and set(link) == {"path", "target"}
            and isinstance(link["path"], str)
            and isinstance(link["target"], str)
            for link in links
        )
    ):
        raise ValueError("dependency removal ownership record is invalid")
    expected_links = {link["path"]: link["target"] for link in links}
    present_links = {
        path.relative_to(removing).as_posix(): os.readlink(path)
        for path in entries
        if path.is_symlink()
    }
    if not set(present_links) <= set(expected_links) or any(
        expected_links[path] != target for path, target in present_links.items()
    ):
        raise ValueError("dependency removal link ownership drift detected")
    present_files = {
        path.relative_to(removing).as_posix()
        for path in entries
        if not is_link_or_reparse(path) and path.is_file() and path != receipt_path
    }
    if not present_files <= set(files):
        raise ValueError("dependency removal contains an unowned file")
    for relative in sorted(present_files):
        path = removing / relative
        if file_sha256(path) != files[relative]:
            raise ValueError("dependency removal ownership drift detected")
        path.unlink()
    for relative in sorted(present_links):
        (removing / relative).unlink()
    present_directories = {
        path.relative_to(removing).as_posix()
        for path in entries
        if not is_link_or_reparse(path) and path.is_dir()
    }
    if not present_directories <= set(directories):
        raise ValueError("dependency removal directory ownership drift detected")
    for directory in sorted(
        (
            path
            for path in entries
            if not is_link_or_reparse(path) and path.is_dir()
        ),
        key=lambda path: len(path.parts),
        reverse=True,
    ):
        directory.rmdir()
    receipt_path.unlink()
    removing.rmdir()


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    commands = result.add_subparsers(dest="command", required=True)
    install_command = commands.add_parser("install")
    install_command.add_argument("--project", required=True, type=Path)
    install_command.add_argument("--source", required=True, type=Path)
    install_command.add_argument("--commit", required=True)
    install_command.add_argument("--distribution", default=DEFAULT_DISTRIBUTION)
    install_command.add_argument("--skip-tools", action="store_true")
    install_command.add_argument("--with-maven-tools", action="store_true")
    install_command.add_argument(
        "--maven-tools-mode", choices=("native", "docker"), default="native"
    )
    for name in ("status", "doctor", "rollback", "uninstall"):
        command = commands.add_parser(name)
        command.add_argument("--project", required=True, type=Path)
        if name in {"status", "doctor"}:
            command.add_argument("--json", action="store_true")
    explain = commands.add_parser("explain")
    explain.add_argument("event")
    explain.add_argument("--project", required=True, type=Path)
    explain.add_argument(
        "--host", choices=("codex", "claude", "gemini", "grok", "copilot"), required=True
    )
    explain.add_argument("--session", default="diagnostic-session")
    explain.add_argument("--phase", default="ReadOnly")
    explain.add_argument("--target-phase", default="")
    explain.add_argument("--cancelled", action="store_true")
    explain.add_argument("--timeout", action="store_true")
    explain.add_argument("--json", action="store_true")
    cache = commands.add_parser("cache")
    cache_commands = cache.add_subparsers(dest="cache_command", required=True)
    cache_status = cache_commands.add_parser("status")
    cache_status.add_argument("--component", choices=("maven-tools-mcp",), required=True)
    cache_purge = cache_commands.add_parser("purge")
    cache_purge.add_argument("--component", choices=("maven-tools-mcp",), required=True)
    cache_purge.add_argument("--version", required=True)
    return result


def validate_install_options(args: argparse.Namespace) -> None:
    if getattr(args, "skip_tools", False) and getattr(args, "with_maven_tools", False):
        raise ValueError("--with-maven-tools cannot be combined with --skip-tools")


def main() -> int:
    args = parser().parse_args()
    try:
        validate_install_options(args)
        if args.command == "install":
            target = (
                install(
                    args.project,
                    args.source,
                    args.commit,
                    distribution=args.distribution,
                )
                if args.skip_tools
                else install_with_dependencies(
                    args.project,
                    args.source,
                    args.commit,
                    distribution=args.distribution,
                    with_maven_tools=args.with_maven_tools,
                    maven_tools_mode=args.maven_tools_mode,
                )
            )
            result: object = {"status": "installed", "root": str(target)}
        elif args.command == "cache":
            controller = load_source_controller("hosts")
            result = (
                controller.maven_tools_cache_status()
                if args.cache_command == "status"
                else controller.purge_maven_tools_cache(args.version)
            )
        elif args.command == "status":
            result = status_json(args.project)
        elif args.command == "doctor":
            result = status_json(args.project, active_probes=True)
        elif args.command == "explain":
            result = explain_json(
                args.project,
                args.event,
                host=args.host,
                session_id=args.session,
                phase=args.phase,
                target_phase=args.target_phase,
                cancelled=args.cancelled,
                timed_out=args.timeout,
            )
        elif args.command == "rollback":
            result = {"status": "rolled-back", "root": str(rollback(args.project))}
        else:
            uninstall_with_dependencies(args.project)
            result = {"status": "uninstalled"}
    except (OSError, RuntimeError, ValueError) as error:
        if getattr(args, "json", False):
            print(
                json.dumps(
                    {
                        "schemaVersion": DIAGNOSTIC_SCHEMA_VERSION,
                        "identity": CANONICAL_IDENTITY,
                        "kind": args.command,
                        "status": "Blocked",
                        "diagnosticCode": "CE_DIAGNOSTIC_UNAVAILABLE",
                        "terminalReason": "blocked",
                    },
                    sort_keys=True,
                    separators=(",", ":"),
                )
            )
        else:
            print(str(error), file=sys.stderr)
        return 1
    print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
