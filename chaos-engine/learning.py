#!/usr/bin/env python3
"""Queue privacy-safe reusable learnings and submit only confirmed GitHub issues."""

from __future__ import annotations

import argparse
from contextlib import contextmanager
import hashlib
import json
import os
import re
import subprocess  # nosec B404 - fixed list-form GitHub CLI calls, never a shell.
import sys
import tempfile
import threading
import time
from pathlib import Path
from urllib.parse import urlencode


SCHEMA_VERSION = 1
QUEUE_NAME = "queue.json"
LOCK_NAME = ".queue.lock"
LOCK_TIMEOUT_SECONDS = 5.0
ALLOWED_KEYS = {
    "category",
    "title",
    "lesson",
    "proposedChange",
    "benefit",
    "estimatedTokens",
}
CATEGORIES = {"guidance", "tooling", "portability", "reliability", "security"}
UPSTREAM = re.compile(r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")
PRIVATE = (
    re.compile(r"(?i)(?:gh[oprsu]_|github_pat_|sk-|api[_-]?key|password|secret|token)[A-Za-z0-9_:=./+\-]{8,}"),
    re.compile(r"(?i)\b(?:token|password|secret|api[_-]?key)\b\s*(?::|=|is|used)\s*(?:bearer\s+)?[A-Za-z0-9_./+\-]{8,}"),
    re.compile(r"(?i)\bauthorization\s*:\s*bearer\s+[A-Za-z0-9_./+\-]{8,}"),
    re.compile(r"(?i)\b(?:token|password|secret|api[_ -]?key)\b\s*(?:(?::|=)\s*|(?:was|is|used|value)\s+)[^\r\n]{8,}"),
    re.compile(r"(?i)(?:[A-Z]:\\|/(?:home|users|root|private|opt)/)"),
    re.compile(r"(?i)https?://"),
    re.compile(r"(?i)\b(?:raw\s+)?(?:system\s+)?prompt\b|\btranscript\b|\b[^\s]*\.log\b|\btraceback\b"),
    re.compile(r"`"),
    re.compile(r"\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}\b", re.IGNORECASE),
    re.compile(r"(?i)(?:^|\s)(?:\.{0,2}[\\/]|[A-Za-z0-9_.-]+[\\/])[^\s]+"),
    re.compile(r"(?i)(?:^|\n)\s*(?:def|class|import|from)\s+[A-Za-z_]|(?:=>|[{};])"),
)
LIMITS = {"title": 100, "lesson": 400, "proposedChange": 300, "benefit": 300}
QUEUED_KEYS = ALLOWED_KEYS | {"id", "status", "upstream"}
OPTIONAL_ITEM_KEYS = {"lastError", "issueUrl", "fallbackUrl"}
THREAD_LOCK = threading.RLock()


def is_link_or_reparse(path: Path) -> bool:
    if path.is_symlink():
        return True
    try:
        attributes = getattr(os.lstat(path), "st_file_attributes", 0)
    except FileNotFoundError:
        return False
    return bool(attributes & 0x400)


def reject_linked_ancestors(path: Path) -> None:
    absolute = path.absolute()
    for candidate in (*reversed(absolute.parents), absolute):
        if (candidate.exists() or is_link_or_reparse(candidate)) and is_link_or_reparse(candidate):
            raise ValueError("ChaosEngine learning path has a linked or reparse ancestor")


@contextmanager
def _file_learning_lock(state: Path):
    reject_linked_ancestors(state)
    state.mkdir(parents=True, exist_ok=True)
    reject_linked_ancestors(state)
    lock = state / LOCK_NAME
    if is_link_or_reparse(lock):
        raise ValueError("ChaosEngine learning lock is a link or reparse point")
    flags = os.O_RDWR | os.O_CREAT | getattr(os, "O_BINARY", 0) | getattr(os, "O_NOFOLLOW", 0)
    descriptor = os.open(lock, flags, 0o600)
    try:
        stream = os.fdopen(descriptor, "r+b", closefd=True)
    except BaseException:
        os.close(descriptor)
        raise
    try:
        opened = os.fstat(stream.fileno())
        named = os.stat(lock, follow_symlinks=False)
        if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino):
            raise ValueError("ChaosEngine learning lock collision")
        if os.name == "nt":
            import msvcrt

            stream.seek(0)
            deadline = time.monotonic() + LOCK_TIMEOUT_SECONDS
            while True:
                try:
                    msvcrt.locking(stream.fileno(), msvcrt.LK_NBLCK, 1)
                    break
                except OSError as error:
                    if time.monotonic() >= deadline:
                        raise TimeoutError(
                            f"ChaosEngine learning lock timed out after {LOCK_TIMEOUT_SECONDS}s"
                        ) from error
                    time.sleep(min(0.05, max(0.0, deadline - time.monotonic())))
        else:
            import fcntl

            deadline = time.monotonic() + LOCK_TIMEOUT_SECONDS
            while True:
                try:
                    fcntl.flock(stream.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
                    break
                except BlockingIOError as error:
                    if time.monotonic() >= deadline:
                        raise TimeoutError(
                            f"ChaosEngine learning lock timed out after {LOCK_TIMEOUT_SECONDS}s"
                        ) from error
                    time.sleep(min(0.05, max(0.0, deadline - time.monotonic())))
        named = os.stat(lock, follow_symlinks=False)
        if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino):
            raise ValueError("ChaosEngine learning lock ownership drift detected")
    except BaseException:
        stream.close()
        raise
    try:
        yield
    finally:
        try:
            if os.name == "nt":
                stream.seek(0)
                msvcrt.locking(stream.fileno(), msvcrt.LK_UNLCK, 1)
            else:
                fcntl.flock(stream.fileno(), fcntl.LOCK_UN)
        finally:
            stream.close()


@contextmanager
def learning_lock(state: Path):
    if not THREAD_LOCK.acquire(timeout=LOCK_TIMEOUT_SECONDS):
        raise TimeoutError(
            f"ChaosEngine learning thread lock timed out after {LOCK_TIMEOUT_SECONDS}s"
        )
    try:
        with _file_learning_lock(state):
            yield
    finally:
        THREAD_LOCK.release()


def canonical(value: object) -> bytes:
    return json.dumps(value, sort_keys=True, separators=(",", ":")).encode("utf-8")


def issue_url_matches(url: object, upstream: str) -> bool:
    return isinstance(url, str) and re.fullmatch(
        rf"https://github\.com/{re.escape(upstream)}/issues/[1-9][0-9]*", url, re.IGNORECASE
    ) is not None


def validate_candidate(candidate: dict[str, object], upstream: str) -> dict[str, object]:
    if not isinstance(candidate, dict) or set(candidate) != ALLOWED_KEYS:
        raise ValueError("learning privacy gate requires the minimal structured schema")
    if not isinstance(upstream, str) or UPSTREAM.fullmatch(upstream) is None:
        raise ValueError("learning upstream must be an explicit owner/repository")
    result: dict[str, object] = {}
    upstream_terms = {
        term.casefold()
        for term in upstream.split("/")
        if term
    }
    for key in ("category", "title", "lesson", "proposedChange", "benefit"):
        value = candidate.get(key)
        if not isinstance(value, str) or not value.strip() or value != value.strip():
            raise ValueError(f"learning privacy gate rejected {key}")
        if key in LIMITS and len(value) > LIMITS[key]:
            raise ValueError(f"learning privacy gate rejected oversized {key}")
        normalized = re.sub(r"\s+", " ", value)
        if any(pattern.search(normalized) for pattern in PRIVATE):
            raise ValueError(f"learning privacy gate rejected private or raw {key}")
        words = {word.casefold() for word in re.findall(r"[A-Za-z0-9_.-]+", value)}
        if words & upstream_terms:
            raise ValueError(f"learning privacy gate rejected repository identity in {key}")
        result[key] = value
    if result["category"] not in CATEGORIES:
        raise ValueError("learning privacy gate rejected category")
    tokens = candidate.get("estimatedTokens")
    if isinstance(tokens, bool) or not isinstance(tokens, int) or not 1 <= tokens <= 10000:
        raise ValueError("learning privacy gate rejected estimatedTokens")
    result["estimatedTokens"] = tokens
    result["upstream"] = upstream
    return result


def queue_document(state: Path) -> dict[str, object]:
    path = state / QUEUE_NAME
    if not path.exists():
        if is_link_or_reparse(path):
            raise ValueError("ChaosEngine learning queue is a link or reparse point")
        return {"schemaVersion": SCHEMA_VERSION, "items": []}
    if is_link_or_reparse(path):
        raise ValueError("ChaosEngine learning queue is a link or reparse point")
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError("ChaosEngine learning queue is invalid") from error
    if (
        not isinstance(value, dict)
        or value.get("schemaVersion") != SCHEMA_VERSION
        or not isinstance(value.get("items"), list)
    ):
        raise ValueError("ChaosEngine learning queue is invalid")
    ids: set[str] = set()
    for item in value["items"]:
        if (
            not isinstance(item, dict)
            or not QUEUED_KEYS <= set(item) <= QUEUED_KEYS | OPTIONAL_ITEM_KEYS
            or item.get("status") not in {"queued", "submitted"}
            or not isinstance(item.get("id"), str)
            or not re.fullmatch(r"[0-9a-f]{64}", item["id"])
        ):
            raise ValueError("ChaosEngine learning queue is invalid")
        safe = validate_candidate({key: item.get(key) for key in ALLOWED_KEYS}, item.get("upstream"))
        expected = hashlib.sha256(canonical(safe)).hexdigest()
        if item["id"] != expected:
            raise ValueError("ChaosEngine learning queue is invalid")
        if item["id"] in ids:
            raise ValueError("ChaosEngine learning queue is invalid")
        ids.add(item["id"])
        if "lastError" in item and (
            item["lastError"] != "submission unavailable" or item["status"] != "queued"
        ):
            raise ValueError("ChaosEngine learning queue is invalid")
        has_issue_url = "issueUrl" in item
        if (item["status"] == "submitted") != has_issue_url:
            raise ValueError("ChaosEngine learning queue is invalid")
        if has_issue_url and not issue_url_matches(item["issueUrl"], str(item["upstream"])):
            raise ValueError("ChaosEngine learning queue is invalid")
        if "fallbackUrl" in item and item["fallbackUrl"] != enhancement_url(item):
            raise ValueError("ChaosEngine learning queue is invalid")
    return value


def write_queue(state: Path, document: dict[str, object]) -> None:
    state.mkdir(parents=True, exist_ok=True)
    if is_link_or_reparse(state):
        raise ValueError("ChaosEngine learning state must not be a link")
    path = state / QUEUE_NAME
    if is_link_or_reparse(path):
        raise ValueError("ChaosEngine learning queue is a link or reparse point")
    descriptor, temporary_name = tempfile.mkstemp(prefix=".queue-", suffix=".json", dir=state)
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "wb") as stream:
            stream.write(json.dumps(document, indent=2, sort_keys=True).encode("utf-8") + b"\n")
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        if temporary.exists():
            temporary.unlink()


def queue_learning(state: Path, candidate: dict[str, object], upstream: str) -> dict[str, object]:
    state = Path(state)
    safe = validate_candidate(candidate, upstream)
    learning_id = hashlib.sha256(canonical(safe)).hexdigest()
    with learning_lock(state):
        document = queue_document(state)
        items = document["items"]
        if not isinstance(items, list):
            raise ValueError("ChaosEngine learning queue is invalid")
        for item in items:
            if isinstance(item, dict) and item.get("id") == learning_id:
                return item
        item = {"id": learning_id, "status": "queued", **safe}
        items.append(item)
        write_queue(state, document)
        return item


def contribution_prompt(candidate: dict[str, object]) -> str:
    tokens = candidate.get("estimatedTokens")
    if isinstance(tokens, bool) or not isinstance(tokens, int):
        raise ValueError("estimatedTokens is required")
    return f"Share this reusable learning upstream for {tokens} estimated tokens? [y/N]"


def issue_body(item: dict[str, object]) -> str:
    return (
        "## Reusable learning\n\n"
        f"Category: {item['category']}\n\n"
        f"Lesson: {item['lesson']}\n\n"
        f"Proposed change: {item['proposedChange']}\n\n"
        f"Benefit: {item['benefit']}\n\n"
        f"Estimated contribution cost: {item['estimatedTokens']} tokens\n\n"
        f"<!-- chaos-engine-learning:{item['id']} -->\n"
    )


def enhancement_url(item: dict[str, object]) -> str:
    """Return a prefilled browser fallback without exposing local runtime data."""
    upstream = str(item["upstream"])
    if UPSTREAM.fullmatch(upstream) is None:
        raise ValueError("learning upstream must be an explicit owner/repository")
    query = urlencode(
        {
            "title": f"ChaosEngine learning: {item['title']}",
            "body": issue_body(item),
        }
    )
    return f"https://github.com/{upstream}/issues/new?{query}"


def _mark_unavailable(
    item: dict[str, object], document: dict[str, object], state: Path
) -> dict[str, object]:
    item["lastError"] = "submission unavailable"
    item["fallbackUrl"] = enhancement_url(item)
    write_queue(state, document)
    return item


def submit_learning(state: Path, learning_id: str, *, confirmed: bool, runner=subprocess.run) -> dict[str, object]:
    if not confirmed:
        raise ValueError("learning submission requires explicit confirmation")
    state = Path(state)
    with learning_lock(state):
        return _submit_learning_locked(state, learning_id, runner=runner)


def _submit_learning_locked(state: Path, learning_id: str, *, runner=subprocess.run) -> dict[str, object]:
    document = queue_document(state)
    items = document["items"]
    if not isinstance(items, list):
        raise ValueError("ChaosEngine learning queue is invalid")
    item = next(
        (value for value in items if isinstance(value, dict) and value.get("id") == learning_id),
        None,
    )
    if item is None:
        raise ValueError("learning candidate is not queued")
    if item.get("status") == "submitted":
        return item
    safe = validate_candidate({key: item[key] for key in ALLOWED_KEYS}, str(item.get("upstream")))
    del safe
    upstream = str(item["upstream"])
    marker = f"chaos-engine-learning:{learning_id}"
    try:
        search = runner(
            ["gh", "issue", "list", "--repo", upstream, "--search", f"{marker} in:body", "--json", "url"],
            capture_output=True,
            text=True,
            check=False,
            timeout=30,
        )
    except (OSError, subprocess.TimeoutExpired):
        search = None
    if search is None or search.returncode != 0:
        return _mark_unavailable(item, document, state)
    try:
        matches = json.loads(search.stdout or "[]")
    except json.JSONDecodeError:
        return _mark_unavailable(item, document, state)
    if not isinstance(matches, list) or any(
        not isinstance(match, dict) or not isinstance(match.get("url"), str)
        for match in matches
    ):
        return _mark_unavailable(item, document, state)
    if matches:
        url = matches[0].get("url") if isinstance(matches[0], dict) else None
    else:
        try:
            created = runner(
                [
                    "gh",
                    "issue",
                    "create",
                    "--repo",
                    upstream,
                    "--title",
                    f"ChaosEngine learning: {item['title']}",
                    "--body",
                    issue_body(item),
                ],
                capture_output=True,
                text=True,
                check=False,
                timeout=30,
            )
        except (OSError, subprocess.TimeoutExpired):
            created = None
        if created is None or created.returncode != 0:
            return _mark_unavailable(item, document, state)
        url = created.stdout.strip()
    if not issue_url_matches(url, upstream):
        return _mark_unavailable(item, document, state)
    item["status"] = "submitted"
    item["issueUrl"] = url
    item.pop("lastError", None)
    item.pop("fallbackUrl", None)
    write_queue(state, document)
    return item


def default_learning_state(project: Path | None = None) -> Path:
    here = (project or Path.cwd()).resolve()
    for candidate in (here, *here.parents):
        if (candidate / ".chaos-engine" / "install.py").is_file() or (
            candidate / "chaos-engine" / "install.py"
        ).is_file():
            return candidate / ".chaos-engine-state" / "learning"
    return here / ".chaos-engine-state" / "learning"


def _count_learning_sessions(project: Path) -> dict[str, object]:
    root = project / ".chaos-engine-state" / "learning-session"
    if not root.is_dir():
        return {"completions": 0, "dispositions": {}}
    completions = 0
    dispositions: dict[str, int] = {}
    for path in sorted(root.glob("*.completion.json")):
        try:
            document = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, UnicodeDecodeError, json.JSONDecodeError):
            continue
        if not isinstance(document, dict):
            continue
        completions += 1
        disposition = str(document.get("disposition") or "unknown")
        dispositions[disposition] = dispositions.get(disposition, 0) + 1
    return {"completions": completions, "dispositions": dispositions}


def _queue_metric_counts(document: dict[str, object]) -> dict[str, object]:
    queued = 0
    submitted = 0
    estimated_tokens = 0
    categories: dict[str, int] = {}
    issue_numbers: list[int] = []
    items = document.get("items") if isinstance(document, dict) else []
    if not isinstance(items, list):
        items = []
    for item in items:
        if not isinstance(item, dict):
            continue
        status = item.get("status")
        if status == "queued":
            queued += 1
        elif status == "submitted":
            submitted += 1
        tokens = item.get("estimatedTokens")
        if isinstance(tokens, int) and not isinstance(tokens, bool):
            estimated_tokens += tokens
        category = item.get("category")
        if isinstance(category, str):
            categories[category] = categories.get(category, 0) + 1
        url = item.get("issueUrl")
        if isinstance(url, str):
            match = re.search(r"/issues/([1-9][0-9]*)$", url)
            if match:
                issue_numbers.append(int(match.group(1)))
    total = queued + submitted
    return {
        "queued": queued,
        "submitted": submitted,
        "total": total,
        "submittedRate": round((submitted / total) if total else 0.0, 4),
        "estimatedTokens": estimated_tokens,
        "categories": categories,
        "issueNumbers": issue_numbers[-32:],
    }


def _load_optional_summary(module_name: str, attribute: str, root: Path) -> dict[str, object]:
    path = Path(__file__).resolve().with_name(module_name)
    if not path.is_file():
        return {}
    try:
        import importlib.util as _ilu

        spec = _ilu.spec_from_file_location(f"chaos_engine_{module_name}_metrics", path)
        if spec is None or spec.loader is None:
            return {}
        mod = _ilu.module_from_spec(spec)
        spec.loader.exec_module(mod)
        value = getattr(mod, attribute)(root)
        return value if isinstance(value, dict) else {}
    except (OSError, RuntimeError, ValueError, AttributeError):
        return {}


def learning_metrics(
    state: Path | None = None,
    *,
    project: Path | None = None,
) -> dict[str, object]:
    """Zero-LLM closed-loop metrics over queue + session ledgers (#5653)."""
    root = (project or Path.cwd()).resolve()
    for candidate in (root, *root.parents):
        if (candidate / ".chaos-engine" / "install.py").is_file() or (
            candidate / "chaos-engine" / "install.py"
        ).is_file():
            root = candidate
            break
    learning_state = Path(state) if state is not None else default_learning_state(root)
    try:
        document = queue_document(learning_state)
    except ValueError:
        document = {"items": []}
    counts = _queue_metric_counts(document)
    sessions = _count_learning_sessions(root)
    counters = _load_optional_summary("learning_counters.py", "load_counters", root)
    heuristics = _load_optional_summary("heuristics.py", "doctor_heuristics_summary", root)
    return {
        "schemaVersion": 1,
        "kind": "learning-metrics",
        **counts,
        "learningSessions": sessions,
        "sessionStartBytesLast": counters.get("sessionStartBytesLast", 0),
        "sessionStartBytesMax": counters.get("sessionStartBytesMax", 0),
        "sessionStartCount": counters.get("sessionStartCount", 0),
        "denials": counters.get("denials", 0),
        "deliveryDigests": counters.get("deliveryDigests", []),
        "learningSessionDigests": counters.get("learningSessionDigests", []),
        "heuristics": heuristics,
        "status": "healthy" if counts["total"] or sessions.get("completions") else "absent",
    }


def doctor_learning_metrics(project: Path | None = None) -> dict[str, object]:
    """Bounded doctor --json learningMetrics surface (no secrets)."""
    metrics = learning_metrics(project=project)
    return {
        "schemaVersion": metrics.get("schemaVersion", 1),
        "status": metrics.get("status"),
        "queued": metrics.get("queued"),
        "submitted": metrics.get("submitted"),
        "submittedRate": metrics.get("submittedRate"),
        "estimatedTokens": metrics.get("estimatedTokens"),
        "learningSessions": metrics.get("learningSessions"),
        "sessionStartBytesLast": metrics.get("sessionStartBytesLast"),
        "sessionStartBytesMax": metrics.get("sessionStartBytesMax"),
        "denials": metrics.get("denials"),
        "heuristics": metrics.get("heuristics"),
        "deliveryDigestCount": len(metrics.get("deliveryDigests") or []),
        "learningSessionDigestCount": len(metrics.get("learningSessionDigests") or []),
    }



def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    commands = result.add_subparsers(dest="command", required=True)
    queue = commands.add_parser("queue")
    queue.add_argument("--state", required=True, type=Path)
    queue.add_argument("--upstream", required=True)
    queue.add_argument("--candidate", required=True, type=Path)
    submit = commands.add_parser("submit")
    submit.add_argument("--state", required=True, type=Path)
    submit.add_argument("--id", required=True)
    submit.add_argument("--yes", action="store_true")
    metrics = commands.add_parser("metrics", aliases=("summary",))
    metrics.add_argument("--state", type=Path, default=None)
    metrics.add_argument("--project", type=Path, default=None)
    return result


def main() -> int:
    args = parser().parse_args()
    try:
        if args.command == "queue":
            candidate = json.loads(args.candidate.read_text(encoding="utf-8"))
            result = queue_learning(args.state, candidate, args.upstream)
        elif args.command in {"metrics", "summary"}:
            result = learning_metrics(args.state, project=args.project)
        else:
            result = submit_learning(args.state, args.id, confirmed=args.yes)
    except (OSError, ValueError, json.JSONDecodeError) as error:
        print(str(error), file=sys.stderr)
        return 1
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
