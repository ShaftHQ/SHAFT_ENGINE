"""One terminal Learning Session with redacted, quarantined evidence."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import stat
import tempfile
import threading
import time
from contextlib import contextmanager
from datetime import datetime, timedelta, timezone
from pathlib import Path, PurePosixPath, PureWindowsPath


SIGNAL_KINDS = frozenset(
    {
        "user_correction",
        "tool_failure",
        "test_failure",
        "guard_block",
        "review_finding",
        "invalidated_assumption",
        "novel_success",
        "behavior_change",
    }
)
ORIGINS = frozenset({"user", "tool", "reviewer", "agent"})
RISK_TIERS = frozenset({"ordinary", "kernel"})
EVIDENCE_KINDS = frozenset({"test", "guard", "review", "tool", "issue", "trace", "file"})
NO_LEARNING_REASONS = frozenset(
    {
        "no_new_evidence",
        "duplicate_evidence",
        "trivial_interaction",
        "already_routed",
        "store_degraded",
    }
)
RUNTIME_DISPOSITIONS = frozenset({"fixed-now", "existing", "new", "blocked"})
SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
GIT_REF_RE = re.compile(r"^[0-9a-f]{40}$")
OPERATION_ID_RE = re.compile(r"^[A-Za-z0-9_-]{8,64}$")
TRACKING_ISSUE_URL_RE = re.compile(
    r"^https://github\.com/ShaftHQ/SHAFT_ENGINE/issues/[1-9][0-9]*$"
)
RECEIPT_KEYS = frozenset(
    {
        "schema_version",
        "receipt_id",
        "session_hash",
        "signal_kind",
        "incident_hash",
        "origin",
        "evidence",
        "task_ref_hash",
        "occurred_at",
        "observed_outcome",
        "trust",
        "assessment",
    }
)
_LOCAL_LOCKS: dict[str, threading.Lock] = {}
_LOCAL_LOCKS_GUARD = threading.Lock()


def _hash_text(value: str) -> str:
    return hashlib.sha256(value.encode("utf-8")).hexdigest()


def _hash_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(65536), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _valid_utc_timestamp(value: object) -> bool:
    if not isinstance(value, str):
        return False
    try:
        parsed = datetime.fromisoformat(value)
    except ValueError:
        return False
    return parsed.tzinfo is not None and parsed.utcoffset() == timedelta(0)


def _canonical(value: object) -> str:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True)


def _session_hash(session_id: str) -> str:
    if not isinstance(session_id, str) or not session_id.strip():
        raise ValueError("session_id must be a nonempty string")
    return _hash_text(session_id.strip())


def incident_hash(incident_id: str) -> str:
    """Return the normalized, non-reversible incident identity."""
    if not isinstance(incident_id, str) or not incident_id.strip():
        raise ValueError("incident_id must be a nonempty string")
    return _hash_text(incident_id.strip())


def default_state_dir() -> Path:
    """Return the one user-local runtime root; callers cannot redirect CLI writes."""
    return Path(tempfile.gettempdir()) / "chaosengine-learning-v1"


def _safe_relative_file(value: str) -> bool:
    if not isinstance(value, str) or not value:
        return False
    posix = PurePosixPath(value)
    windows = PureWindowsPath(value)
    return not (
        posix.is_absolute()
        or windows.is_absolute()
        or windows.drive
        or windows.root
        or ".." in posix.parts
        or ".." in windows.parts
        or value.endswith(("/", "\\"))
    )


def _is_reparse_point(path: Path) -> bool:
    try:
        attributes = getattr(path.stat(follow_symlinks=False), "st_file_attributes", 0)
    except OSError:
        return False
    is_junction = getattr(os.path, "isjunction", lambda _path: False)
    return bool(
        path.is_symlink()
        or is_junction(path)
        or attributes & getattr(stat, "FILE_ATTRIBUTE_REPARSE_POINT", 0)
    )


def _secure_root(state: Path) -> Path:
    root = Path(state)
    for candidate in (root, *root.parents):
        if candidate.exists() and _is_reparse_point(candidate):
            raise ValueError("learning state root or ancestor is a link/reparse point")
    root.mkdir(parents=True, exist_ok=True)
    if _is_reparse_point(root):
        raise ValueError("learning state root is a link/reparse point")
    before = root.stat(follow_symlinks=False)
    resolved_root = root.resolve(strict=True)
    after = root.stat(follow_symlinks=False)
    target = resolved_root.stat()
    if (
        _is_reparse_point(root)
        or (before.st_dev, before.st_ino) != (after.st_dev, after.st_ino)
        or (after.st_dev, after.st_ino) != (target.st_dev, target.st_ino)
    ):
        raise ValueError("learning state root changed during validation")
    return resolved_root


def _contained_directory(state: Path, name: str) -> Path:
    root = Path(state)
    resolved_root = _secure_root(root)
    child = resolved_root / name
    if child.exists() and _is_reparse_point(child):
        raise ValueError(f"state path is a link/reparse point: {name}")
    child.mkdir(exist_ok=True)
    if _is_reparse_point(child):
        raise ValueError(f"state path is a link/reparse point: {name}")
    before = child.stat(follow_symlinks=False)
    resolved_child = child.resolve(strict=True)
    after = child.stat(follow_symlinks=False)
    target = resolved_child.stat()
    if (
        _is_reparse_point(child)
        or (before.st_dev, before.st_ino) != (after.st_dev, after.st_ino)
        or (after.st_dev, after.st_ino) != (target.st_dev, target.st_ino)
    ):
        raise ValueError(f"state path changed during validation: {name}")
    try:
        resolved_child.relative_to(resolved_root)
    except ValueError as error:
        raise ValueError(f"state path escapes runtime root: {name}") from error
    return resolved_child


@contextmanager
def _state_lock(state: Path, key: str):
    resolved_root = _secure_root(Path(state))
    lock = resolved_root / f".{key}.lock"
    with _LOCAL_LOCKS_GUARD:
        local_lock = _LOCAL_LOCKS.setdefault(str(lock), threading.Lock())
    with local_lock:
        deadline = time.monotonic() + 5.0
        if lock.exists() and (_is_reparse_point(lock) or lock.stat().st_nlink != 1):
            raise ValueError("learning lock file is a link")
        descriptor: int | None = None
        handle = None
        acquired = False
        try:
            try:
                descriptor = os.open(lock, os.O_CREAT | os.O_EXCL | os.O_RDWR, 0o600)
            except FileExistsError:
                descriptor = os.open(lock, os.O_RDWR)
            handle = os.fdopen(descriptor, "r+b")
            descriptor = None  # fdopen owns and closes the descriptor from here.
            opened = os.fstat(handle.fileno())
            current = lock.stat(follow_symlinks=False)
            if (
                _is_reparse_point(lock)
                or opened.st_nlink != 1
                or (opened.st_dev, opened.st_ino) != (current.st_dev, current.st_ino)
            ):
                raise ValueError("learning lock file is a link or changed identity")
            while not acquired:
                try:
                    handle.seek(0)
                    if os.name == "nt":
                        import msvcrt

                        if lock.stat().st_size == 0:
                            handle.write(b"0")
                            handle.flush()
                            handle.seek(0)
                        if os.fstat(handle.fileno()).st_nlink != 1:
                            raise ValueError("learning lock file gained a hard link")
                        msvcrt.locking(handle.fileno(), msvcrt.LK_NBLCK, 1)
                    else:
                        import fcntl

                        fcntl.flock(handle.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
                    acquired = True
                except OSError:
                    if time.monotonic() >= deadline:
                        raise ValueError("learning state is busy")
                    time.sleep(0.01)
            yield
        finally:
            try:
                if acquired and handle is not None:
                    handle.seek(0)
                    if os.name == "nt":
                        import msvcrt

                        msvcrt.locking(handle.fileno(), msvcrt.LK_UNLCK, 1)
                    else:
                        import fcntl

                        fcntl.flock(handle.fileno(), fcntl.LOCK_UN)
            finally:
                if handle is not None:
                    handle.close()
                elif descriptor is not None:
                    os.close(descriptor)


def _validate_evidence(  # noqa: MC0001  # One fail-closed pass keeps artifact validation atomic.
    evidence: object, evidence_root: Path, state: Path
) -> list[dict[str, str]]:
    if not isinstance(evidence, list) or not evidence:
        raise ValueError("evidence must be a nonempty list")
    root = Path(evidence_root).resolve(strict=True)
    validated: list[dict[str, str]] = []
    for item in evidence:
        if not isinstance(item, dict) or set(item) != {"kind", "id", "sha256"}:
            raise ValueError("evidence entries require only kind, id, and sha256")
        if not all(isinstance(item[key], str) and item[key].strip() for key in item):
            raise ValueError("evidence values must be nonempty strings")
        if item["kind"] not in EVIDENCE_KINDS:
            raise ValueError("evidence kind is not an allowed provenance class")
        digest = item["sha256"].lower()
        if not SHA256_RE.fullmatch(digest):
            raise ValueError("evidence sha256 must be 64 lowercase hexadecimal characters")
        if not _safe_relative_file(item["id"]):
            raise ValueError("evidence id must be a safe relative artifact path")
        try:
            artifact = (root / item["id"]).resolve(strict=True)
            artifact.relative_to(root)
        except (FileNotFoundError, ValueError) as error:
            raise ValueError("evidence artifact must stay inside evidence root") from error
        if not artifact.is_file():
            raise ValueError("evidence artifact must be a regular file")
        if _hash_file(artifact) != digest:
            raise ValueError("evidence digest does not match the artifact")
        byte_length = artifact.stat().st_size
        evidence_store = _contained_directory(Path(state), "evidence")
        stored_artifact = evidence_store / f"{digest}.proof.json"
        proof = {"schema_version": 1, "sha256": digest, "byte_length": byte_length}
        with _state_lock(Path(state), f"evidence-{digest}"):
            if _is_reparse_point(evidence_store):
                raise ValueError("evidence state path changed before proof write")
            if stored_artifact.exists():
                try:
                    existing_proof = json.loads(stored_artifact.read_text(encoding="utf-8"))
                except (OSError, ValueError, UnicodeError) as error:
                    raise ValueError("content-addressed evidence proof is invalid") from error
                if _is_reparse_point(stored_artifact) or existing_proof != proof:
                    raise ValueError("content-addressed evidence is invalid")
            else:
                with stored_artifact.open("x", encoding="utf-8") as handle:
                    handle.write(_canonical(proof) + "\n")
                if _is_reparse_point(stored_artifact):
                    raise ValueError("content-addressed evidence copy failed verification")
        validated.append(
            {
                "kind": item["kind"].strip(),
                "id_hash": _hash_text(item["id"]),
                "sha256": digest,
                "byte_length": byte_length,
            }
        )
    return validated


def _receipt_identity(receipt: dict) -> dict:
    return {
        key: receipt[key]
        for key in (
            "session_hash",
            "signal_kind",
            "incident_hash",
            "origin",
            "evidence",
            "task_ref_hash",
        )
    }


def _valid_receipt(value: object, expected_session_hash: str, state: Path) -> bool:
    if not isinstance(value, dict) or set(value) != RECEIPT_KEYS:
        return False
    evidence = value.get("evidence")
    structurally_valid = bool(
        value.get("schema_version") == 1
        and value.get("session_hash") == expected_session_hash
        and value.get("signal_kind") in SIGNAL_KINDS
        and value.get("origin") in ORIGINS
        and value.get("observed_outcome") == "unknown"
        and value.get("trust") == "quarantined"
        and value.get("assessment") == "pending"
        and _valid_utc_timestamp(value.get("occurred_at"))
        and SHA256_RE.fullmatch(str(value.get("incident_hash", "")))
        and (
            value.get("task_ref_hash") is None
            or SHA256_RE.fullmatch(str(value.get("task_ref_hash")))
        )
        and isinstance(evidence, list)
        and bool(evidence)
        and all(
            isinstance(item, dict)
            and set(item) == {"kind", "id_hash", "sha256", "byte_length"}
            and item["kind"] in EVIDENCE_KINDS
            and SHA256_RE.fullmatch(str(item["id_hash"]))
            and SHA256_RE.fullmatch(str(item["sha256"]))
            and isinstance(item["byte_length"], int)
            and item["byte_length"] >= 0
            for item in evidence
        )
        and value.get("receipt_id") == _hash_text(_canonical(_receipt_identity(value)))
    )
    if not structurally_valid:
        return False
    try:
        evidence_store = _contained_directory(Path(state), "evidence")
        for item in evidence:
            proof_path = evidence_store / f"{item['sha256']}.proof.json"
            if not proof_path.is_file() or _is_reparse_point(proof_path):
                return False
            proof = json.loads(proof_path.read_text(encoding="utf-8"))
            if (
                not isinstance(proof, dict)
                or set(proof) != {"schema_version", "sha256", "byte_length"}
                or proof.get("schema_version") != 1
                or proof.get("sha256") != item["sha256"]
                or proof.get("byte_length") != item["byte_length"]
            ):
                return False
        return True
    except (OSError, ValueError):
        return False


def _receipt_path(state: Path, session_hash: str) -> Path:
    return _contained_directory(Path(state), "receipts") / f"{session_hash}.jsonl"


def load_receipts(state: Path, session_id: str) -> list[dict]:
    """Return only complete, hash-valid receipts for one session."""
    expected = _session_hash(session_id)
    try:
        path = _receipt_path(Path(state), expected)
    except (OSError, ValueError):
        return []
    if not path.is_file():
        return []
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except (OSError, UnicodeError):
        return []
    receipts: list[dict] = []
    identifiers: set[str] = set()
    for line in lines:
        try:
            item = json.loads(line)
        except (ValueError, TypeError):
            continue
        if _valid_receipt(item, expected, Path(state)) and item["receipt_id"] not in identifiers:
            identifiers.add(item["receipt_id"])
            receipts.append(item)
    return receipts


def record_signal(
    state: Path,
    *,
    session_id: str,
    kind: str,
    incident_id: str,
    origin: str,
    evidence: list[dict[str, str]],
    evidence_root: Path,
    task_ref: str | None = None,
) -> dict:
    """Append one redacted meaningful-event receipt, deduplicated by incident."""
    if load_session_completion(Path(state), session_id) is not None:
        raise ValueError("learning session is already complete")
    if kind not in SIGNAL_KINDS:
        raise ValueError(f"unknown signal kind: {kind}")
    if origin not in ORIGINS:
        raise ValueError(f"unknown signal origin: {origin}")
    if task_ref is not None and (not isinstance(task_ref, str) or not task_ref.strip()):
        raise ValueError("task_ref must be a nonempty string when supplied")
    session_hash = _session_hash(session_id)
    normalized_incident_hash = incident_hash(incident_id)
    validated_evidence = _validate_evidence(evidence, Path(evidence_root), Path(state))
    with _state_lock(Path(state), session_hash):
        prior = load_receipts(Path(state), session_id)
        for receipt in prior:
            if receipt["incident_hash"] == normalized_incident_hash:
                return receipt
        attestation = _attestation_path(Path(state), session_id)
        if attestation.is_file():
            attestation.unlink()
        identity = {
            "session_hash": session_hash,
            "signal_kind": kind,
            "incident_hash": normalized_incident_hash,
            "origin": origin,
            "evidence": validated_evidence,
            "task_ref_hash": _hash_text(task_ref.strip()) if task_ref else None,
        }
        receipt = {
            "schema_version": 1,
            "receipt_id": _hash_text(_canonical(identity)),
            **identity,
            "occurred_at": datetime.now(timezone.utc).isoformat(),
            "observed_outcome": "unknown",
            "trust": "quarantined",
            "assessment": "pending",
        }
        path = _receipt_path(Path(state), session_hash)
        with path.open("a", encoding="utf-8") as handle:
            handle.write(_canonical(receipt) + "\n")
        return receipt


def _validate_candidate_spec(
    *,
    hypothesis: str,
    owner: str,
    baseline_ref: str,
    allowed_paths: list[str],
    red_command: str,
    success_predicates: list[str],
    invariants: list[str],
    risk_tier: str,
    tracking_issue_urls: list[str] | None,
) -> None:
    if any(
        not isinstance(value, str) or not value.strip()
        for value in (hypothesis, owner, red_command)
    ):
        raise ValueError("hypothesis, owner, and red_command must be nonempty strings")
    if not GIT_REF_RE.fullmatch(baseline_ref):
        raise ValueError("baseline_ref must be a full lowercase commit SHA")
    if not allowed_paths or not all(_safe_relative_file(path) for path in allowed_paths):
        raise ValueError("allowed_paths must contain safe relative files")
    if not success_predicates or not all(
        isinstance(item, str) and item.strip() for item in success_predicates
    ):
        raise ValueError("success_predicates must be nonempty strings")
    if not invariants or not all(isinstance(item, str) and item.strip() for item in invariants):
        raise ValueError("invariants must be nonempty strings")
    if risk_tier not in RISK_TIERS:
        raise ValueError(f"unknown risk tier: {risk_tier}")
    if not tracking_issue_urls or not all(
        isinstance(url, str) and TRACKING_ISSUE_URL_RE.fullmatch(url)
        for url in tracking_issue_urls
    ):
        raise ValueError("tracking issue URLs must identify standalone ShaftHQ/SHAFT_ENGINE issues")
    if len(set(tracking_issue_urls)) != len(tracking_issue_urls):
        raise ValueError("each actionable incident requires a distinct tracking issue")


def _atomic_json(path: Path, value: dict) -> None:
    descriptor, temporary_name = tempfile.mkstemp(prefix=path.name, dir=path.parent)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
            handle.write(json.dumps(value, indent=2, sort_keys=True) + "\n")
        os.replace(temporary_name, path)
    except BaseException:
        try:
            os.unlink(temporary_name)
        except OSError:
            pass  # Preserve the original write/replace failure; cleanup is best-effort.
        raise


def _candidate_identity(candidate: dict) -> dict:
    return {
        key: candidate[key]
        for key in (
            "receipt_ids",
            "incident_hash",
            "hypothesis_hash",
            "owner",
            "baseline_ref",
            "allowed_paths",
            "red_command_hash",
            "success_predicate_hashes",
            "invariant_hashes",
            "risk_tier",
            "tracking_issue_url",
        )
    }


def _valid_candidate(candidate: object) -> bool:
    if not isinstance(candidate, dict):
        return False
    expected = {
        "schema_version",
        "candidate_id",
        "receipt_ids",
        "incident_hash",
        "hypothesis_hash",
        "owner",
        "baseline_ref",
        "allowed_paths",
        "red_command_hash",
        "success_predicate_hashes",
        "invariant_hashes",
        "risk_tier",
        "tracking_issue_url",
        "status",
    }
    try:
        return bool(
            set(candidate) == expected
            and candidate["schema_version"] == 2
            and candidate["status"] == "quarantined"
            and candidate["risk_tier"] in RISK_TIERS
            and TRACKING_ISSUE_URL_RE.fullmatch(candidate["tracking_issue_url"])
            and isinstance(candidate["owner"], str)
            and bool(candidate["owner"].strip())
            and GIT_REF_RE.fullmatch(candidate["baseline_ref"])
            and isinstance(candidate["allowed_paths"], list)
            and bool(candidate["allowed_paths"])
            and all(_safe_relative_file(path) for path in candidate["allowed_paths"])
            and isinstance(candidate["receipt_ids"], list)
            and bool(candidate["receipt_ids"])
            and all(SHA256_RE.fullmatch(item) for item in candidate["receipt_ids"])
            and SHA256_RE.fullmatch(candidate["incident_hash"])
            and SHA256_RE.fullmatch(candidate["hypothesis_hash"])
            and SHA256_RE.fullmatch(candidate["red_command_hash"])
            and isinstance(candidate["success_predicate_hashes"], list)
            and bool(candidate["success_predicate_hashes"])
            and all(SHA256_RE.fullmatch(item) for item in candidate["success_predicate_hashes"])
            and isinstance(candidate["invariant_hashes"], list)
            and bool(candidate["invariant_hashes"])
            and all(SHA256_RE.fullmatch(item) for item in candidate["invariant_hashes"])
            and candidate["candidate_id"] == _hash_text(_canonical(_candidate_identity(candidate)))
        )
    except (KeyError, TypeError):
        return False


def load_candidates(state: Path) -> list[dict]:
    try:
        directory = _contained_directory(Path(state), "candidates")
    except (OSError, ValueError):
        return []
    candidates: list[dict] = []
    for path in sorted(directory.glob("*.json")):
        try:
            candidate = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, ValueError, UnicodeError):
            continue
        if _valid_candidate(candidate):
            candidates.append(candidate)
    return candidates


def assess(
    state: Path,
    *,
    session_id: str,
    hypothesis: str,
    owner: str,
    baseline_ref: str,
    allowed_paths: list[str],
    red_command: str,
    success_predicates: list[str],
    invariants: list[str],
    risk_tier: str,
    tracking_issue_urls: list[str] | None = None,
) -> list[dict]:
    """Create one complete, quarantined candidate for every session incident."""
    if load_session_completion(Path(state), session_id) is not None:
        raise ValueError("learning session is already complete")
    _validate_candidate_spec(
        hypothesis=hypothesis,
        owner=owner,
        baseline_ref=baseline_ref,
        allowed_paths=allowed_paths,
        red_command=red_command,
        success_predicates=success_predicates,
        invariants=invariants,
        risk_tier=risk_tier,
        tracking_issue_urls=tracking_issue_urls,
    )
    receipts = load_receipts(Path(state), session_id)
    if not receipts:
        raise ValueError("assessment requires at least one meaningful signal")
    if len(tracking_issue_urls or []) != len(receipts):
        raise ValueError("assessment requires one distinct tracking issue per incident")
    candidates: list[dict] = []
    with _state_lock(Path(state), "candidate-tracking-issues"):
        directory = _contained_directory(Path(state), "candidates")
        existing_candidates = load_candidates(Path(state))
        for receipt, tracking_issue_url in zip(receipts, tracking_issue_urls or []):
            identity = {
                "receipt_ids": [receipt["receipt_id"]],
                "incident_hash": receipt["incident_hash"],
                "hypothesis_hash": _hash_text(hypothesis.strip()),
                "owner": owner.strip(),
                "baseline_ref": baseline_ref,
                "allowed_paths": list(dict.fromkeys(allowed_paths)),
                "red_command_hash": _hash_text(red_command.strip()),
                "success_predicate_hashes": [_hash_text(item) for item in success_predicates],
                "invariant_hashes": [_hash_text(item) for item in invariants],
                "risk_tier": risk_tier,
                "tracking_issue_url": tracking_issue_url,
            }
            candidate = {
                "schema_version": 2,
                "candidate_id": _hash_text(_canonical(identity)),
                **identity,
                "status": "quarantined",
            }
            incident_candidate = next(
                (
                    item
                    for item in existing_candidates
                    if item["incident_hash"] == receipt["incident_hash"]
                ),
                None,
            )
            if incident_candidate is not None and incident_candidate != candidate:
                raise ValueError("incident already bound to a different tracking issue or candidate")
            issue_candidate = next(
                (
                    item
                    for item in existing_candidates
                    if item["tracking_issue_url"] == tracking_issue_url
                ),
                None,
            )
            if issue_candidate is not None and issue_candidate["incident_hash"] != receipt["incident_hash"]:
                raise ValueError("tracking issue already belongs to a different incident")
            path = directory / f"{candidate['candidate_id']}.json"
            if path.is_file():
                try:
                    existing = json.loads(path.read_text(encoding="utf-8"))
                except (OSError, ValueError, UnicodeError) as error:
                    raise ValueError("existing candidate is unreadable") from error
                if not _valid_candidate(existing) or existing != candidate:
                    raise ValueError("existing candidate does not match its identity")
                candidates.append(existing)
                continue
            _atomic_json(path, candidate)
            existing_candidates.append(candidate)
            candidates.append(candidate)
    return candidates


def _attestation_path(state: Path, session_id: str) -> Path:
    return _contained_directory(Path(state), "attestations") / f"{_session_hash(session_id)}.json"


def load_attestation(state: Path, session_id: str) -> dict | None:
    try:
        path = _attestation_path(Path(state), session_id)
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError, UnicodeError):
        return None
    required_keys = {
        "schema_version",
        "session_hash",
        "reason_code",
        "attestation_id",
        "assessed_at",
    }
    expected = {
        "schema_version": 1,
        "session_hash": _session_hash(session_id),
        "reason_code": value.get("reason_code") if isinstance(value, dict) else None,
    }
    if (
        not isinstance(value, dict)
        or set(value) != required_keys
        or value.get("schema_version") != 1
        or value.get("session_hash") != _session_hash(session_id)
        or not _valid_utc_timestamp(value.get("assessed_at"))
        or value.get("reason_code") not in NO_LEARNING_REASONS
        or value.get("attestation_id") != _hash_text(_canonical(expected))
    ):
        return None
    return value


def attest_no_learning(state: Path, session_id: str, reason_code: str) -> dict:
    """Record a structured no-learning result only for a signal-free session."""
    if load_session_completion(Path(state), session_id) is not None:
        raise ValueError("learning session is already complete")
    if reason_code not in NO_LEARNING_REASONS:
        raise ValueError(f"unknown no-learning reason: {reason_code}")
    session_hash = _session_hash(session_id)
    identity = {
        "schema_version": 1,
        "session_hash": session_hash,
        "reason_code": reason_code,
    }
    value = {
        **identity,
        "attestation_id": _hash_text(_canonical(identity)),
        "assessed_at": datetime.now(timezone.utc).isoformat(),
    }
    with _state_lock(Path(state), session_hash):
        if load_receipts(Path(state), session_id):
            raise ValueError("cannot attest no learning while meaningful signals remain")
        _atomic_json(_attestation_path(Path(state), session_id), value)
    return value


def _session_completion_path(state: Path, session_id: str) -> Path:
    return _contained_directory(Path(state), "sessions") / f"{_session_hash(session_id)}.json"


def _session_completion_identity(value: dict) -> dict:
    return {
        key: value[key]
        for key in (
            "kind",
            "session_hash",
            "disposition",
            "incident_hashes",
            "candidate_ids",
            "reason_code",
        )
    }


def load_session_completion(state: Path, session_id: str) -> dict | None:
    """Load the one immutable terminal completion for a root session."""
    try:
        value = json.loads(
            _session_completion_path(Path(state), session_id).read_text(encoding="utf-8")
        )
    except (OSError, ValueError, UnicodeError):
        return None
    required = {
        "schema_version",
        "completion_id",
        "kind",
        "session_hash",
        "disposition",
        "incident_hashes",
        "candidate_ids",
        "reason_code",
        "completed_at",
    }
    try:
        valid = bool(
            isinstance(value, dict)
            and set(value) == required
            and value["schema_version"] == 1
            and value["kind"] == "learning-session-complete"
            and value["session_hash"] == _session_hash(session_id)
            and value["disposition"] in {"assessed", "nothing-durable"}
            and isinstance(value["incident_hashes"], list)
            and all(SHA256_RE.fullmatch(item) for item in value["incident_hashes"])
            and isinstance(value["candidate_ids"], list)
            and all(SHA256_RE.fullmatch(item) for item in value["candidate_ids"])
            and _valid_utc_timestamp(value["completed_at"])
            and value["completion_id"]
            == _hash_text(_canonical(_session_completion_identity(value)))
        )
    except (KeyError, TypeError):
        return None
    return value if valid else None


def finalize_session(state: Path, session_id: str) -> dict:
    """Atomically close the only terminal Learning Session; retries are reads."""
    session_hash = _session_hash(session_id)
    with _state_lock(Path(state), f"session-{session_hash}"):
        existing = load_session_completion(Path(state), session_id)
        if existing is not None:
            return existing
        receipts = load_receipts(Path(state), session_id)
        receipt_incidents = {item["incident_hash"] for item in receipts}
        candidates = [
            item
            for item in load_candidates(Path(state))
            if item["incident_hash"] in receipt_incidents
        ]
        candidate_incidents = {item["incident_hash"] for item in candidates}
        reason_code = None
        if receipts:
            if candidate_incidents != receipt_incidents:
                raise ValueError("every learning signal must be assessed before finalization")
            disposition = "assessed"
        else:
            attestation = load_attestation(Path(state), session_id)
            if attestation is None:
                raise ValueError("learning session requires assessed signals or no-learning attestation")
            disposition = "nothing-durable"
            reason_code = attestation["reason_code"]
        identity = {
            "kind": "learning-session-complete",
            "session_hash": session_hash,
            "disposition": disposition,
            "incident_hashes": sorted(receipt_incidents),
            "candidate_ids": sorted(item["candidate_id"] for item in candidates),
            "reason_code": reason_code,
        }
        value = {
            "schema_version": 1,
            "completion_id": _hash_text(_canonical(identity)),
            **identity,
            "completed_at": datetime.now(timezone.utc).isoformat(),
        }
        _atomic_json(_session_completion_path(Path(state), session_id), value)
        return value


def _runtime_completion_path(state: Path, root_session_id: str) -> Path:
    name = f"{_session_hash(root_session_id)}.json"
    return _contained_directory(Path(state), "runtime-completions") / name


def finalize_runtime_session(
    state: Path,
    *,
    root_session_id: str,
    participant_session_ids: list[str],
    dispositions: dict[str, str],
) -> dict:
    """Close one root-owned runtime after considering root and delegate receipts."""
    participants = sorted(participant_session_ids)
    if len(set(participants)) != len(participants):
        raise ValueError("runtime participants must appear exactly once")
    if root_session_id not in participants:
        raise ValueError("runtime participants must include the root session")
    if not participants or any(
        not isinstance(item, str) or not item.strip() for item in participants
    ):
        raise ValueError("runtime participants must be nonempty session identifiers")
    receipts = {
        receipt["incident_hash"]: receipt
        for session_id in participants
        for receipt in load_receipts(Path(state), session_id)
    }
    incident_hashes = set(receipts)
    if set(dispositions) != incident_hashes:
        raise ValueError("every runtime incident requires exactly one disposition")
    if any(
        not SHA256_RE.fullmatch(key) or value not in RUNTIME_DISPOSITIONS
        for key, value in dispositions.items()
    ):
        raise ValueError("runtime disposition is invalid")
    identity = {
        "kind": "learning-runtime-complete",
        "root_session_hash": _session_hash(root_session_id),
        "disposition": "assessed" if incident_hashes else "no-durable",
        "participant_hashes": sorted(_session_hash(item) for item in participants),
        "incidents": [
            {"incident_hash": key, "disposition": dispositions[key]}
            for key in sorted(dispositions)
        ],
    }
    value = {
        "schema_version": 1,
        "completion_id": _hash_text(_canonical(identity)),
        **identity,
        "completed_at": datetime.now(timezone.utc).isoformat(),
    }
    with _state_lock(Path(state), f"runtime-{identity['root_session_hash']}"):
        path = _runtime_completion_path(Path(state), root_session_id)
        if path.is_file():
            try:
                existing = json.loads(path.read_text(encoding="utf-8"))
            except (OSError, ValueError, UnicodeError) as error:
                raise ValueError("runtime completion is invalid") from error
            if {key: existing.get(key) for key in identity} != identity:
                raise ValueError("runtime learning session is already complete")
            return existing
        _atomic_json(path, value)
    return value


def _completion_identity(value: dict) -> dict:
    return {
        key: value[key]
        for key in (
            "session_hash",
            "operation_id_hash",
            "operation",
            "incident_hashes",
            "reason_code",
        )
    }


def _completion_path(state: Path, operation_id: str) -> Path:
    if not OPERATION_ID_RE.fullmatch(operation_id):
        raise ValueError("operation_id must be 8-64 letters, digits, underscores, or hyphens")
    return _contained_directory(Path(state), "completions") / f"{_hash_text(operation_id)}.json"


def record_completion(
    state: Path,
    session_id: str,
    operation_id: str,
    operation: str,
    incident_hashes: list[str] | None = None,
) -> dict:
    """Bind one successful CLI invocation to the artifacts it actually produced."""
    if operation not in {"signal", "assess", "attest-none"}:
        raise ValueError("unknown learning completion operation")
    session_hash = _session_hash(session_id)
    receipts = load_receipts(Path(state), session_id)
    receipt_incidents = {receipt["incident_hash"] for receipt in receipts}
    supplied = list(dict.fromkeys(incident_hashes or []))
    reason_code = None
    if operation == "signal":
        if len(supplied) != 1 or supplied[0] not in receipt_incidents:
            raise ValueError("signal completion is not bound to a valid receipt")
    elif operation == "assess":
        valid_candidates = {
            candidate["incident_hash"]
            for candidate in load_candidates(Path(state))
            if candidate["incident_hash"] in receipt_incidents
            and candidate["receipt_ids"]
            and candidate["receipt_ids"][0]
            in {receipt["receipt_id"] for receipt in receipts}
        }
        if not supplied or set(supplied) != valid_candidates:
            raise ValueError("assessment completion is not bound to every valid candidate")
    else:
        attestation = load_attestation(Path(state), session_id)
        if attestation is None or supplied:
            raise ValueError("attestation completion is not bound to a valid attestation")
        reason_code = attestation["reason_code"]
    identity = {
        "session_hash": session_hash,
        "operation_id_hash": _hash_text(operation_id),
        "operation": operation,
        "incident_hashes": supplied,
        "reason_code": reason_code,
    }
    value = {
        "schema_version": 1,
        "completion_id": _hash_text(_canonical(identity)),
        **identity,
        "completed_at": datetime.now(timezone.utc).isoformat(),
    }
    with _state_lock(Path(state), f"completion-{session_hash}"):
        path = _completion_path(Path(state), operation_id)
        if path.is_file():
            existing = load_completion(Path(state), session_id, operation_id)
            if existing is None or _completion_identity(existing) != identity:
                raise ValueError("operation_id already belongs to a different completion")
            return existing
        _atomic_json(path, value)
    return value


def load_completion(state: Path, session_id: str, operation_id: str) -> dict | None:
    try:
        path = _completion_path(Path(state), operation_id)
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError, UnicodeError):
        return None
    required = {
        "schema_version",
        "completion_id",
        "session_hash",
        "operation_id_hash",
        "operation",
        "incident_hashes",
        "reason_code",
        "completed_at",
    }
    try:
        valid = bool(
            isinstance(value, dict)
            and set(value) == required
            and value["schema_version"] == 1
            and value["session_hash"] == _session_hash(session_id)
            and value["operation_id_hash"] == _hash_text(operation_id)
            and value["operation"] in {"signal", "assess", "attest-none"}
            and isinstance(value["incident_hashes"], list)
            and all(SHA256_RE.fullmatch(item) for item in value["incident_hashes"])
            and _valid_utc_timestamp(value["completed_at"])
            and value["completion_id"] == _hash_text(_canonical(_completion_identity(value)))
        )
    except (KeyError, TypeError):
        return None
    return value if valid else None


def _report_expectations(report: object) -> dict[str, tuple[list[str], list[dict]]]:
    if (
        not isinstance(report, dict)
        or set(report) != {"episodes", "rules", "guard_metrics", "unmeasured_rule_ids"}
        or report.get("unmeasured_rule_ids") != []
        or not isinstance(report.get("rules"), dict)
        or not isinstance(report.get("guard_metrics"), dict)
    ):
        raise ValueError("evaluation reports must be complete with zero unmeasured rules")
    episodes = report.get("episodes")
    if not isinstance(episodes, dict) or not episodes:
        raise ValueError("evaluation report requires episodes")
    normalized: dict[str, tuple[list[str], list[dict]]] = {}
    for identifier, episode in episodes.items():
        if not isinstance(identifier, str) or not isinstance(episode, dict):
            raise ValueError("evaluation episode is invalid")
        rule_ids = episode.get("rule_ids")
        expectations = episode.get("expectations")
        strict_pass = episode.get("strict_episode_pass")
        if (
            set(episode) != {"rule_ids", "strict_episode_pass", "expectations"}
            or
            not isinstance(rule_ids, list)
            or not all(isinstance(item, str) and item for item in rule_ids)
            or not isinstance(expectations, list)
            or not expectations
            or not all(
                isinstance(item, dict)
                and set(item) == {"kind", "passed"}
                and isinstance(item.get("kind"), str)
                and isinstance(item.get("passed"), bool)
                for item in expectations
            )
            or not isinstance(strict_pass, bool)
            or strict_pass != all(item["passed"] for item in expectations)
        ):
            raise ValueError("evaluation episode shape is invalid")
        normalized[identifier] = (rule_ids, expectations)
    return normalized


def _evaluation_path(state: Path, candidate_id: str) -> Path:
    if not SHA256_RE.fullmatch(candidate_id):
        raise ValueError("candidate_id must be a SHA-256 identifier")
    return _contained_directory(Path(state), "evaluations") / f"{candidate_id}.json"


_EVALUATION_IDENTITY_KEYS = (
    "candidate_id", "baseline_sha", "candidate_sha", "corpus_sha256",
    "baseline_report_hash", "candidate_report_hash", "target_rule_id_hashes",
    "changed_paths", "review_key_hashes", "review_lenses", "run_id_hashes", "risk_tier",
)


def _valid_evaluation(value: object, candidate_id: str) -> bool:
    required = {
        "schema_version", "evaluation_id", *_EVALUATION_IDENTITY_KEYS, "status", "evaluated_at"
    }
    try:
        identity = {key: value[key] for key in _EVALUATION_IDENTITY_KEYS}
        return bool(
            isinstance(value, dict)
            and set(value) == required
            and value["schema_version"] == 1
            and value["candidate_id"] == candidate_id
            and SHA256_RE.fullmatch(value["candidate_id"])
            and value["status"] == "evaluated"
            and GIT_REF_RE.fullmatch(value["baseline_sha"])
            and GIT_REF_RE.fullmatch(value["candidate_sha"])
            and value["baseline_sha"] != value["candidate_sha"]
            and SHA256_RE.fullmatch(value["corpus_sha256"])
            and SHA256_RE.fullmatch(value["baseline_report_hash"])
            and SHA256_RE.fullmatch(value["candidate_report_hash"])
            and isinstance(value["target_rule_id_hashes"], list)
            and bool(value["target_rule_id_hashes"])
            and all(SHA256_RE.fullmatch(item) for item in value["target_rule_id_hashes"])
            and isinstance(value["changed_paths"], list)
            and bool(value["changed_paths"])
            and all(_safe_relative_file(item) for item in value["changed_paths"])
            and isinstance(value["review_key_hashes"], list)
            and bool(value["review_key_hashes"])
            and all(SHA256_RE.fullmatch(item) for item in value["review_key_hashes"])
            and isinstance(value["review_lenses"], list)
            and bool(value["review_lenses"])
            and set(value["review_lenses"]).issubset({"correctness", "reproduction", "safety"})
            and isinstance(value["run_id_hashes"], list)
            and bool(value["run_id_hashes"])
            and all(SHA256_RE.fullmatch(item) for item in value["run_id_hashes"])
            and value["risk_tier"] in RISK_TIERS
            and (
                value["risk_tier"] == "ordinary"
                or (
                    len(set(value["review_key_hashes"])) >= 2
                    and set(value["review_lenses"])
                    == {"correctness", "reproduction", "safety"}
                    and len(set(value["run_id_hashes"])) >= 2
                )
            )
            and _valid_utc_timestamp(value["evaluated_at"])
            and value["evaluation_id"] == _hash_text(_canonical(identity))
        )
    except (KeyError, TypeError):
        return False


def _load_json_object(path: Path) -> dict | None:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError, UnicodeError):
        return None
    return value if isinstance(value, dict) else None


def evaluate_candidate(  # noqa: MC0001  # One fail-closed gate keeps evaluation invariants together.
    state: Path,
    *,
    candidate: dict,
    baseline_report: dict,
    candidate_report: dict,
    target_rule_ids: list[str],
    candidate_sha: str,
    corpus_sha256: str,
    changed_paths: list[str],
    tests_passed: bool,
    reviews: list[dict[str, str]],
    run_ids: list[str],
) -> dict:
    """Record a fail-closed consistency comparison for a quarantined candidate."""
    if not _valid_candidate(candidate) or candidate not in load_candidates(Path(state)):
        raise ValueError("candidate is not a valid quarantined state object")
    if not GIT_REF_RE.fullmatch(candidate_sha) or candidate_sha == candidate["baseline_ref"]:
        raise ValueError("candidate_sha must be a new full commit SHA")
    if not SHA256_RE.fullmatch(corpus_sha256):
        raise ValueError("corpus_sha256 must be a SHA-256 identifier")
    if tests_passed is not True:
        raise ValueError("all required tests must pass")
    if not target_rule_ids or not all(isinstance(item, str) and item for item in target_rule_ids):
        raise ValueError("target_rule_ids must be nonempty strings")
    if not changed_paths or not all(_safe_relative_file(path) for path in changed_paths):
        raise ValueError("changed_paths must be safe relative paths")
    allowed = candidate["allowed_paths"]
    if any(
        not any(path == root or path.startswith(root.rstrip("/") + "/") for root in allowed)
        for path in changed_paths
    ):
        raise ValueError("candidate changed a path outside its declared scope")

    baseline = _report_expectations(baseline_report)
    proposed = _report_expectations(candidate_report)
    if set(baseline) != set(proposed):
        raise ValueError("baseline and candidate reports use different episodes")
    improved = False
    for identifier, (baseline_rules, baseline_expectations) in baseline.items():
        candidate_rules, candidate_expectations = proposed[identifier]
        if baseline_rules != candidate_rules or len(baseline_expectations) != len(candidate_expectations):
            raise ValueError("baseline and candidate report shapes differ")
        for before, after in zip(baseline_expectations, candidate_expectations):
            if before["kind"] != after["kind"]:
                raise ValueError("baseline and candidate expectation kinds differ")
            if before["passed"] is True and after["passed"] is not True:
                raise ValueError("candidate regresses a previously passing expectation")
            if (
                set(baseline_rules).intersection(target_rule_ids)
                and before["passed"] is not True
                and after["passed"] is True
            ):
                improved = True
    if not improved:
        raise ValueError("candidate must strictly improve a targeted rule")
    baseline_false = baseline_report.get("guard_metrics", {}).get("false_block_count")
    candidate_false = candidate_report.get("guard_metrics", {}).get("false_block_count")
    if not isinstance(baseline_false, int) or not isinstance(candidate_false, int):
        raise ValueError("reports require integer guard false-block counts")
    if candidate_false > baseline_false:
        raise ValueError("candidate increases false guard blocks")

    if not isinstance(reviews, list) or not reviews:
        raise ValueError("at least one independent approval is required")
    if any(
        not isinstance(review, dict)
        or set(review) != {"key", "lens", "decision"}
        or not all(isinstance(review[field], str) and review[field] for field in review)
        or review["decision"] != "approve"
        for review in reviews
    ):
        raise ValueError("reviews must be complete approvals")
    if not isinstance(run_ids, list) or not run_ids or not all(
        isinstance(item, str) and item for item in run_ids
    ):
        raise ValueError("run_ids must identify successful evaluation runs")
    reviewer_keys = {review["key"] for review in reviews}
    lenses = {review["lens"] for review in reviews}
    if not lenses.issubset({"correctness", "reproduction", "safety"}):
        raise ValueError("review lenses must be correctness, reproduction, or safety")
    if candidate["risk_tier"] == "kernel" and (
        len(reviewer_keys) < 2
        or not {"correctness", "reproduction", "safety"}.issubset(lenses)
        or len(set(run_ids)) < 2
    ):
        raise ValueError("kernel candidates require two reviewer keys, three lenses, and two runs")

    identity = {
        "candidate_id": candidate["candidate_id"],
        "baseline_sha": candidate["baseline_ref"],
        "candidate_sha": candidate_sha,
        "corpus_sha256": corpus_sha256,
        "baseline_report_hash": _hash_text(_canonical(baseline_report)),
        "candidate_report_hash": _hash_text(_canonical(candidate_report)),
        "target_rule_id_hashes": sorted(_hash_text(item) for item in set(target_rule_ids)),
        "changed_paths": list(dict.fromkeys(changed_paths)),
        "review_key_hashes": sorted(_hash_text(item) for item in reviewer_keys),
        "review_lenses": sorted(lenses),
        "run_id_hashes": sorted(_hash_text(item) for item in set(run_ids)),
        "risk_tier": candidate["risk_tier"],
    }
    result = {
        "schema_version": 1,
        "evaluation_id": _hash_text(_canonical(identity)),
        **identity,
        "status": "evaluated",
        "evaluated_at": datetime.now(timezone.utc).isoformat(),
    }
    with _state_lock(Path(state), f"evaluation-{candidate['candidate_id']}"):
        path = _evaluation_path(Path(state), candidate["candidate_id"])
        existing = _load_json_object(path) if path.is_file() else None
        if existing is not None:
            comparable = {key: existing.get(key) for key in identity}
            if (
                not _valid_evaluation(existing, candidate["candidate_id"])
                or comparable != identity
                or existing.get("evaluation_id") != result["evaluation_id"]
            ):
                raise ValueError("candidate already has a different evaluation")
            return existing
        _atomic_json(path, result)
    return result


def promote_candidate(
    state: Path, candidate_id: str, *, head_sha: str, branch: str, pr_number: int
) -> dict:
    """Record promotion intent for the exact evaluated commit and declared PR."""
    candidates = [item for item in load_candidates(Path(state)) if item["candidate_id"] == candidate_id]
    if len(candidates) != 1:
        raise ValueError("promotion candidate is missing or invalid")
    candidate = candidates[0]
    evaluation = _load_json_object(_evaluation_path(Path(state), candidate_id))
    if not _valid_evaluation(evaluation, candidate_id):
        raise ValueError("candidate has no valid evaluation record")
    if (
        evaluation["baseline_sha"] != candidate["baseline_ref"]
        or evaluation["risk_tier"] != candidate["risk_tier"]
        or any(
            not any(
                path == root or path.startswith(root.rstrip("/") + "/")
                for root in candidate["allowed_paths"]
            )
            for path in evaluation["changed_paths"]
        )
    ):
        raise ValueError("evaluation is not bound to the quarantined candidate")
    if head_sha != evaluation.get("candidate_sha"):
        raise ValueError("promotion head does not match the evaluated commit")
    if not isinstance(branch, str) or not branch.startswith("ChaosEngine/"):
        raise ValueError("promotion requires an isolated ChaosEngine branch")
    if not isinstance(pr_number, int) or isinstance(pr_number, bool) or pr_number <= 0:
        raise ValueError("promotion requires a positive pull request number")
    promotion_identity = {
        "candidate_id": candidate_id,
        "evaluation_id": evaluation["evaluation_id"],
        "candidate_sha": head_sha,
        "branch_hash": _hash_text(branch),
        "pr_number": pr_number,
    }
    value = {
        "schema_version": 1,
        "promotion_id": _hash_text(_canonical(promotion_identity)),
        **promotion_identity,
        "status": "promotion-intent",
        "repairs_attempted": 0,
        "frozen": False,
        "updated_at": datetime.now(timezone.utc).isoformat(),
    }
    with _state_lock(Path(state), f"promotion-{candidate_id}"):
        path = _contained_directory(Path(state), "promotions") / f"{candidate_id}.json"
        existing = _load_json_object(path) if path.is_file() else None
        if existing is not None:
            if _valid_promotion(existing, candidate_id) and all(
                existing.get(key) == value[key] for key in promotion_identity
            ):
                return existing
            raise ValueError("candidate already has a different promotion")
        _atomic_json(path, value)
    return value


def _valid_promotion(value: object, candidate_id: str) -> bool:
    required = {
        "schema_version", "promotion_id", "candidate_id", "evaluation_id", "candidate_sha",
        "branch_hash", "pr_number", "status", "repairs_attempted", "frozen", "updated_at",
    }
    try:
        identity = {
            key: value[key]
            for key in ("candidate_id", "evaluation_id", "candidate_sha", "branch_hash", "pr_number")
        }
        valid_state = (
            (value["status"] == "promotion-intent" and value["repairs_attempted"] == 0 and not value["frozen"])
            or (value["status"] == "repair-required" and value["repairs_attempted"] == 1 and not value["frozen"])
            or (value["status"] == "revert-required" and value["repairs_attempted"] == 1 and value["frozen"])
        )
        return bool(
            isinstance(value, dict)
            and set(value) == required
            and value["schema_version"] == 1
            and value["candidate_id"] == candidate_id
            and SHA256_RE.fullmatch(value["candidate_id"])
            and SHA256_RE.fullmatch(value["evaluation_id"])
            and GIT_REF_RE.fullmatch(value["candidate_sha"])
            and SHA256_RE.fullmatch(value["branch_hash"])
            and isinstance(value["pr_number"], int)
            and not isinstance(value["pr_number"], bool)
            and value["pr_number"] > 0
            and valid_state
            and _valid_utc_timestamp(value["updated_at"])
            and value["promotion_id"] == _hash_text(_canonical(identity))
        )
    except (KeyError, TypeError):
        return False


def repair_or_revert(state: Path, candidate_id: str) -> dict:
    """Record one repair requirement, then a frozen revert requirement on recurrence."""
    if not SHA256_RE.fullmatch(candidate_id):
        raise ValueError("candidate_id must be a SHA-256 identifier")
    with _state_lock(Path(state), f"promotion-{candidate_id}"):
        path = _contained_directory(Path(state), "promotions") / f"{candidate_id}.json"
        value = _load_json_object(path)
        if not _valid_promotion(value, candidate_id) or value["status"] not in {
            "promotion-intent", "repair-required"
        }:
            raise ValueError("candidate is not in a recoverable promoted state")
        candidates = [
            item for item in load_candidates(Path(state)) if item["candidate_id"] == candidate_id
        ]
        evaluation = _load_json_object(_evaluation_path(Path(state), candidate_id))
        if (
            len(candidates) != 1
            or not _valid_evaluation(evaluation, candidate_id)
            or value["evaluation_id"] != evaluation["evaluation_id"]
            or value["candidate_sha"] != evaluation["candidate_sha"]
            or evaluation["baseline_sha"] != candidates[0]["baseline_ref"]
            or evaluation["risk_tier"] != candidates[0]["risk_tier"]
        ):
            raise ValueError("promotion is detached from its candidate evaluation")
        if value["status"] == "promotion-intent" and value.get("repairs_attempted") == 0:
            value.update(status="repair-required", repairs_attempted=1)
        else:
            value.update(status="revert-required", frozen=True)
        value["updated_at"] = datetime.now(timezone.utc).isoformat()
        _atomic_json(path, value)
    return value


def _parse_evidence(value: str) -> dict[str, str]:
    parts = value.split(":", 2)
    if len(parts) != 3:
        raise argparse.ArgumentTypeError("evidence must be kind:relative-artifact:sha256")
    return {"kind": parts[0], "id": parts[1], "sha256": parts[2]}


def _parse_disposition(value: str) -> tuple[str, str]:
    incident, separator, disposition = value.partition("=")
    if (
        not separator
        or not SHA256_RE.fullmatch(incident)
        or disposition not in RUNTIME_DISPOSITIONS
    ):
        raise argparse.ArgumentTypeError(
            "disposition must be SHA256=fixed-now|existing|new|blocked"
        )
    return incident, disposition


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    signal = commands.add_parser("signal")
    signal.add_argument("--session-id", required=True)
    signal.add_argument("--operation-id", required=True)
    signal.add_argument("--kind", choices=sorted(SIGNAL_KINDS), required=True)
    signal.add_argument("--incident-id", required=True)
    signal.add_argument("--origin", choices=sorted(ORIGINS), required=True)
    signal.add_argument("--evidence", action="append", type=_parse_evidence, required=True)
    signal.add_argument("--task-ref")
    candidate = commands.add_parser("assess")
    candidate.add_argument("--session-id", required=True)
    candidate.add_argument("--operation-id", required=True)
    candidate.add_argument("--hypothesis", required=True)
    candidate.add_argument("--owner", required=True)
    candidate.add_argument("--baseline-ref", required=True)
    candidate.add_argument("--allowed-path", action="append", required=True)
    candidate.add_argument("--red-command", required=True)
    candidate.add_argument("--success-predicate", action="append", required=True)
    candidate.add_argument("--invariant", action="append", required=True)
    candidate.add_argument("--risk-tier", choices=sorted(RISK_TIERS), required=True)
    candidate.add_argument("--tracking-issue-url", action="append", required=True)
    none = commands.add_parser("attest-none")
    none.add_argument("--session-id", required=True)
    none.add_argument("--operation-id", required=True)
    none.add_argument("--reason-code", choices=sorted(NO_LEARNING_REASONS), required=True)
    final = commands.add_parser("finalize")
    final.add_argument("--session-id", required=True)
    runtime = commands.add_parser("finalize-runtime")
    runtime.add_argument("--session-id", required=True)
    runtime.add_argument("--participant-session-id", action="append", required=True)
    runtime.add_argument("--disposition", action="append", type=_parse_disposition, default=[])
    evaluate = commands.add_parser("evaluate")
    evaluate.add_argument("--candidate-id", required=True)
    evaluate.add_argument("--manifest", type=Path, required=True)
    promote = commands.add_parser("promote")
    promote.add_argument("--candidate-id", required=True)
    promote.add_argument("--head-sha", required=True)
    promote.add_argument("--branch", required=True)
    promote.add_argument("--pr-number", type=int, required=True)
    recovery = commands.add_parser("repair-or-revert")
    recovery.add_argument("--candidate-id", required=True)
    return parser


def main(arguments: list[str] | None = None) -> int:
    options = build_parser().parse_args(arguments)
    state = default_state_dir()
    try:
        if options.command == "signal":
            result = record_signal(
                state,
                session_id=options.session_id,
                kind=options.kind,
                incident_id=options.incident_id,
                origin=options.origin,
                evidence=options.evidence,
                evidence_root=Path.cwd(),
                task_ref=options.task_ref,
            )
            record_completion(
                state,
                options.session_id,
                options.operation_id,
                "signal",
                [result["incident_hash"]],
            )
        elif options.command == "assess":
            result = assess(
                state,
                session_id=options.session_id,
                hypothesis=options.hypothesis,
                owner=options.owner,
                baseline_ref=options.baseline_ref,
                allowed_paths=options.allowed_path,
                red_command=options.red_command,
                success_predicates=options.success_predicate,
                invariants=options.invariant,
                risk_tier=options.risk_tier,
                tracking_issue_urls=options.tracking_issue_url,
            )
            record_completion(
                state,
                options.session_id,
                options.operation_id,
                "assess",
                [candidate["incident_hash"] for candidate in result],
            )
        elif options.command == "attest-none":
            result = attest_no_learning(state, options.session_id, options.reason_code)
            record_completion(
                state, options.session_id, options.operation_id, "attest-none"
            )
        elif options.command == "finalize":
            result = finalize_session(state, options.session_id)
        elif options.command == "finalize-runtime":
            result = finalize_runtime_session(
                state,
                root_session_id=options.session_id,
                participant_session_ids=options.participant_session_id,
                dispositions=dict(options.disposition),
            )
        elif options.command == "evaluate":
            manifest = _load_json_object(options.manifest)
            if manifest is None:
                raise ValueError("evaluation manifest must be a JSON object")
            required = {
                "baseline_report", "candidate_report", "target_rule_ids", "candidate_sha",
                "corpus_sha256", "changed_paths", "tests_passed", "reviews", "run_ids",
            }
            if set(manifest) != required:
                raise ValueError("evaluation manifest has an invalid schema")
            candidate = next(
                (item for item in load_candidates(state) if item["candidate_id"] == options.candidate_id),
                None,
            )
            if candidate is None:
                raise ValueError("candidate is not present in quarantined state")
            result = evaluate_candidate(state, candidate=candidate, **manifest)
        elif options.command == "promote":
            result = promote_candidate(
                state, options.candidate_id, head_sha=options.head_sha,
                branch=options.branch, pr_number=options.pr_number,
            )
        else:
            result = repair_or_revert(state, options.candidate_id)
    except (OSError, ValueError) as error:
        build_parser().error(str(error))
    print(json.dumps(result, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
