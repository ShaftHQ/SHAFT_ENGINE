"""Project identity.md seed, heal, and Learning Truth protection (#5807)."""

from __future__ import annotations

from pathlib import Path

IDENTITY_RELATIVE = "identity.md"
OVERLAY_IDENTITY = ".chaos-engine/identity.md"
TRUTH_START = "<!-- CHAOSENGINE-IDENTITY-TRUTH:START -->"
TRUTH_END = "<!-- CHAOSENGINE-IDENTITY-TRUTH:END -->"

SEED = """# ChaosEngine identity

Editable project personality. Hosts inject a pointer to this file so every
adapter sees the same durable self-model.

<!-- CHAOSENGINE-IDENTITY-TRUTH:START -->
## Truth

- Speak unabridged truth: do not lie, hide, skew, manipulate, or omit material facts.
- Prefer evidence over inference; say what you know and what you do not.
- When uncertain, say so plainly; never pad confidence.
<!-- CHAOSENGINE-IDENTITY-TRUTH:END -->

## Stance

- Pragmatic, calculating, and confident.
- Kind mentor and honest worker: teach clearly, execute cleanly, no theatrics.

## Working style

- Size the work, pick one surface, finish it.
- Prefer the laziest correct implementation (Ponytail) and compressed chat (Caveman)
  on implementation paths.
- Protect this Truth section: Learning Session may propose refinements elsewhere,
  but must not silently rewrite the marked Truth block without explicit owner review.
"""


def seed_bytes() -> bytes:
    return SEED.encode("utf-8")


def identity_path(project: Path) -> Path:
    return project.resolve() / OVERLAY_IDENTITY


def source_identity_path() -> Path:
    return Path(__file__).resolve().with_name(IDENTITY_RELATIVE)


def ensure_identity_file(project: Path, *, heal: bool = True) -> dict[str, object]:
    """Create `.chaos-engine/identity.md` from seed when missing."""
    path = identity_path(project)
    if path.is_file():
        return {"status": "healthy", "path": str(path), "detail": "present"}
    if not heal:
        return {
            "status": "sync-advisory",
            "path": str(path),
            "detail": "identity-missing",
            "fixNext": f"Create {OVERLAY_IDENTITY} from the ChaosEngine seed (rerun doctor/activate).",
        }
    source = source_identity_path()
    body = source.read_bytes() if source.is_file() else seed_bytes()
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(body)
    return {"status": "healthy", "path": str(path), "detail": "created-from-seed", "changed": True}


def truth_span(text: str) -> str | None:
    if TRUTH_START not in text or TRUTH_END not in text:
        return None
    begin = text.index(TRUTH_START)
    finish = text.index(TRUTH_END, begin) + len(TRUTH_END)
    return text[begin:finish]


def protect_truth_section(before: str, after: str) -> str:
    """If Learning rewrote the Truth span, restore the prior marked block."""
    prior = truth_span(before)
    if prior is None:
        return after
    current = truth_span(after)
    if current == prior:
        return after
    if current is None:
        # Re-insert prior truth after title if possible.
        lines = after.splitlines(keepends=True)
        insert_at = 0
        for index, line in enumerate(lines):
            if line.startswith("#"):
                insert_at = index + 1
                break
        lines.insert(insert_at, "\n" + prior + "\n")
        return "".join(lines)
    return after.replace(current, prior, 1)


def learning_may_write_identity(before: bytes | None, after: bytes) -> bytes:
    """Gate Learning writes: preserve Truth markers from the previous file."""
    if before is None:
        return after
    try:
        previous = before.decode("utf-8")
        proposed = after.decode("utf-8")
    except UnicodeDecodeError:
        return before
    protected = protect_truth_section(previous, proposed)
    return protected.encode("utf-8")


def session_start_identity_locator(project: Path | None = None) -> str:
    """Compact SessionStart pointer — no body inline."""
    return (
        f"Identity: read and follow `{OVERLAY_IDENTITY}` "
        "(Truth section is protected from silent Learning rewrites)."
    )


def instruction_identity_sentence() -> str:
    return f"Load project identity from `{OVERLAY_IDENTITY}`."
