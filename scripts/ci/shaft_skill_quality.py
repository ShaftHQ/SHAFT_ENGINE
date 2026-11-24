#!/usr/bin/env python3
"""Generate and validate SHAFT skill inventory and quality evidence (#4640, #8156)."""

import argparse
import json
import re
from pathlib import Path

try:
    from scripts.ci.assemble_shaft_skills_plugin import package_path_for_source
    from scripts.ci.validate_agent_plugins import parse_skill_frontmatter
except ModuleNotFoundError:  # Direct script execution places scripts/ci on sys.path.
    from assemble_shaft_skills_plugin import package_path_for_source
    from validate_agent_plugins import parse_skill_frontmatter

ROOT = Path(__file__).resolve().parents[2]
SKILLS_DIRECTORY = "shaft-skills"
REVIEW_FILE = "quality-review.json"
INVENTORY_FILE = "INVENTORY.md"
QUALITY_FILE = "QUALITY.md"
HOSTS_FILE = Path("scripts/mcp/agent_hosts.json")
ENTRYPOINT = "shaft-developer"
EXCLUDED_CHILDREN = {"ce-pack", "ce-addons", "references"}
DESCRIPTION_CEILING = 180
LISTING_CEILING = 500
SUPPORT_REFERENCE = "journey-coverage-map.md"
QUALITY_DIMENSIONS = (
    "trigger_precision",
    "sibling_overlap",
    "dependency_depth",
    "reference_freshness",
    "runnable_evidence",
    "context_cost",
)
LINK = re.compile(r"\[[^]]*]\((?P<target>[^)]+)\)")
ROUTED_SPECIALIST = re.compile(
    r"^\| (?P<output>.+?) \| \[`(?P<name>shaft-[a-z0-9-]+)`\]\((?P<target>specialists/[^)#]+)",
    re.M,
)


def _skills_root(root: Path) -> Path:
    return Path(root) / SKILLS_DIRECTORY


def _discover(root: Path) -> list[dict]:
    """Read directly discoverable skill metadata from canonical sources."""
    root = Path(root)
    skills_root = _skills_root(root)
    skills: list[dict] = []
    if not skills_root.is_dir():
        return skills
    for child in sorted(skills_root.iterdir(), key=lambda path: path.name):
        skill_path = child / "SKILL.md"
        if child.name in EXCLUDED_CHILDREN or not child.is_dir() or not skill_path.is_file():
            continue
        content = skill_path.read_text(encoding="utf-8")
        frontmatter = parse_skill_frontmatter(content) or {}
        description = frontmatter.get("description", "")
        references: list[str] = []
        for match in LINK.finditer(content):
            target = match.group("target").strip().strip("<>")
            if not target or target.startswith(("#", "http://", "https://", "mailto:")):
                continue
            target = target.split("#", 1)[0].split("?", 1)[0]
            if target:
                try:
                    references.append((child / target).resolve().relative_to(skills_root.resolve()).as_posix())
                except (OSError, ValueError):
                    references.append(target)
        skills.append(
            {
                "name": child.name,
                "description": description if isinstance(description, str) else "",
                "canonical_path": skill_path.relative_to(root).as_posix(),
                "package_path": package_path_for_source(skills_root, skill_path).as_posix(),
                "references": sorted(set(references)),
                "line_count": len(content.splitlines()),
                "skill_path": skill_path,
                "content": content,
            }
        )
    return skills


def _load_review(root: Path) -> dict:
    path = _skills_root(root) / REVIEW_FILE
    try:
        review = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return {}
    return review if isinstance(review, dict) else {}


def _escape(value: object) -> str:
    return str(value).replace("|", "\\|").replace("\r", " ").replace("\n", " ")


def _routed_specialist_rows(root: Path) -> list[dict]:
    """Inventory rows for logical specialists that share the one entrypoint."""
    root = Path(root)
    skills_root = _skills_root(root)
    routing = skills_root / ENTRYPOINT / "references" / "routing.md"
    if not routing.is_file():
        return []
    rows = []
    for match in ROUTED_SPECIALIST.finditer(routing.read_text(encoding="utf-8")):
        target = match.group("target")
        source = skills_root / ENTRYPOINT / "references" / target
        rows.append(
            {
                "name": match.group("name"),
                "description": " ".join(match.group("output").split()),
                "canonical_path": source.relative_to(root).as_posix(),
                "package_path": (Path("skills") / source.relative_to(skills_root)).as_posix(),
                "references": [source.relative_to(skills_root).as_posix()],
            }
        )
    return rows


def _inventory_rows(root: Path) -> list[dict]:
    return [*_discover(root), *_routed_specialist_rows(root)]


def render_inventory(root: Path = ROOT) -> str:
    """Render the authoritative source-to-package inventory."""
    review = _load_review(root)
    package = review.get("package", "")
    clients = review.get("supported_clients", [])
    client_text = "; ".join(clients) if isinstance(clients, list) else ""
    lines = [
        "# SHAFT skill inventory",
        "",
        "Generated by `scripts/ci/shaft_skill_quality.py --write`; do not edit manually.",
        "",
        "| Skill | Canonical source | Package path | Package | Supported clients | Description | Linked local references |",
        "| --- | --- | --- | --- | --- | --- | --- |",
    ]
    for skill in _inventory_rows(root):
        references = "; ".join(skill["references"]) or "—"
        lines.append(
            "| {name} | `{canonical}` | `{package_path}` | {package} | {clients} | {description} | {references} |".format(
                name=_escape(skill["name"]),
                canonical=_escape(skill["canonical_path"]),
                package_path=_escape(skill["package_path"]),
                package=_escape(package),
                clients=_escape(client_text),
                description=_escape(skill["description"]),
                references=_escape(references),
            )
        )
    return "\n".join(lines) + "\n"


def render_quality_report(root: Path = ROOT) -> str:
    """Render reviewed quality scores alongside live context measurements."""
    review = _load_review(root)
    rows = review.get("skills", {})
    rows = rows if isinstance(rows, dict) else {}
    labels = [dimension.replace("_", " ").capitalize() for dimension in QUALITY_DIMENSIONS]
    lines = [
        "# SHAFT skill quality scorecard",
        "",
        "Generated by `scripts/ci/shaft_skill_quality.py --write`; scores are reviewed in `quality-review.json`.",
        "",
        "| Skill | " + " | ".join(labels) + " | Description characters | Confusion checks | Review notes |",
        "| --- | " + " | ".join(["---:"] * len(labels)) + " | ---: | --- | --- |",
    ]
    for skill in _discover(root):
        row = rows.get(skill["name"], {})
        row = row if isinstance(row, dict) else {}
        scores = row.get("scores", {})
        scores = scores if isinstance(scores, dict) else {}
        confusion = row.get("confusion_with", [])
        confusion_text = "; ".join(confusion) if isinstance(confusion, list) else ""
        values = [str(scores.get(dimension, "—")) for dimension in QUALITY_DIMENSIONS]
        lines.append(
            "| {name} | {scores} | {characters} | {confusion} | {notes} |".format(
                name=_escape(skill["name"]),
                scores=" | ".join(_escape(value) for value in values),
                characters=len(skill["description"]),
                confusion=_escape(confusion_text or "—"),
                notes=_escape(row.get("notes", "")),
            )
        )
    return "\n".join(lines) + "\n"


def write_generated(root: Path = ROOT) -> None:
    """Refresh both checked-in generated quality artifacts."""
    skills_root = _skills_root(root)
    skills_root.mkdir(parents=True, exist_ok=True)
    (skills_root / INVENTORY_FILE).write_text(render_inventory(root), encoding="utf-8")
    (skills_root / QUALITY_FILE).write_text(render_quality_report(root), encoding="utf-8")


def _integer_budget(budgets: dict, field: str, defects: list[str]) -> int | None:
    value = budgets.get(field)
    if not isinstance(value, int) or isinstance(value, bool) or value < 1:
        defects.append(f"quality-config: context_budget.{field} must be a positive integer")
        return None
    return value


def _allowed_confusion_names(review: dict, skill_names: set[str]) -> set[str]:
    allowed = set(skill_names)
    routing = review.get("routing_reviews")
    if isinstance(routing, dict):
        allowed.update(name for name in routing if isinstance(name, str))
    return allowed


def _score_defects(name: str, scores: object) -> list[str]:
    if not isinstance(scores, dict) or set(scores) != set(QUALITY_DIMENSIONS):
        return [f"quality-dimensions: {name} must score exactly {', '.join(QUALITY_DIMENSIONS)}"]
    defects = []
    for dimension, score in scores.items():
        if not isinstance(score, int) or isinstance(score, bool) or not 1 <= score <= 5:
            defects.append(f"quality-score: {name}.{dimension} must be an integer from 1 to 5")
    return defects


def _confusion_defects(name: str, confusion: object, allowed: set[str]) -> list[str]:
    if not isinstance(confusion, list) or not all(isinstance(item, str) for item in confusion):
        return [f"confusion-skill: {name}.confusion_with must be an array of skill names"]
    invalid = sorted(item for item in confusion if item not in allowed or item == name)
    if invalid:
        return [f"confusion-skill: {name} has invalid targets {', '.join(invalid)}"]
    return []


def _review_row_defects(name: str, row: object, allowed: set[str]) -> list[str]:
    if not isinstance(row, dict):
        return [f"quality-row: {name} review must be an object"]
    defects = _score_defects(name, row.get("scores"))
    defects.extend(_confusion_defects(name, row.get("confusion_with"), allowed))
    notes = row.get("notes")
    if not isinstance(notes, str) or len(notes.strip()) < 20:
        defects.append(f"quality-notes: {name} must record a substantive review note")
    return defects


def _review_set_defects(reviews: dict, skill_names: set[str], allowed: set[str]) -> list[str]:
    review_names = set(reviews)
    defects = []
    if review_names != skill_names:
        missing = ", ".join(sorted(skill_names - review_names)) or "none"
        unknown = ", ".join(sorted(review_names - skill_names)) or "none"
        defects.append(f"review-skill-set: missing [{missing}]; unknown [{unknown}]")
    for name in sorted(skill_names & review_names):
        defects.extend(_review_row_defects(name, reviews[name], allowed))
    return defects


def _config_defects(review: dict, skill_names: set[str]) -> tuple[list[str], dict]:
    defects: list[str] = []
    package = review.get("package")
    clients = review.get("supported_clients")
    if not isinstance(package, str) or not package.strip():
        defects.append("quality-config: package must be a non-empty string")
    if not isinstance(clients, list) or not clients or not all(isinstance(client, str) and client for client in clients):
        defects.append("quality-config: supported_clients must contain non-empty strings")
    budgets = review.get("context_budget")
    if not isinstance(budgets, dict):
        budgets = {}
        defects.append("quality-config: context_budget must be an object")
    reviews = review.get("skills")
    if not isinstance(reviews, dict):
        reviews = {}
        defects.append("quality-config: skills must be an object")
    defects.extend(_review_set_defects(reviews, skill_names, _allowed_confusion_names(review, skill_names)))
    return defects, budgets


def _budget_ceiling_defects(budgets: dict) -> list[str]:
    defects = []
    description = budgets.get("max_description_chars")
    total = budgets.get("max_total_description_chars")
    if isinstance(description, int) and not isinstance(description, bool) and description > DESCRIPTION_CEILING:
        defects.append(
            f"description-ceiling: context_budget.max_description_chars must be <= {DESCRIPTION_CEILING}"
        )
    if isinstance(total, int) and not isinstance(total, bool) and total > LISTING_CEILING:
        defects.append(
            f"listing-ceiling: context_budget.max_total_description_chars must be <= {LISTING_CEILING}"
        )
    return defects


def _link_target(raw: str) -> str:
    target = raw.strip().strip("<>")
    if not target or target.startswith(("#", "http://", "https://", "mailto:")):
        return ""
    return target.split("#", 1)[0].split("?", 1)[0]


def _contained_link_defects(label: str, path: Path, skills_root: Path, *, prefix: str) -> list[str]:
    defects = []
    try:
        content = path.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        return [f"{prefix}: {label} is not readable"]
    skills_resolved = skills_root.resolve()
    for match in LINK.finditer(content):
        target = _link_target(match.group("target"))
        if not target:
            continue
        candidate = (path.parent / target).resolve()
        try:
            candidate.relative_to(skills_resolved)
        except (OSError, ValueError):
            defects.append(f"{prefix}-containment: {label} link escapes shaft-skills: {target}")
            continue
        if not candidate.is_file():
            defects.append(f"{prefix}: {label} link does not resolve to a file: {target}")
    return defects


def _measurement_defects(root: Path, skills: list[dict], budgets: dict) -> list[str]:
    defects: list[str] = []
    max_description = _integer_budget(budgets, "max_description_chars", defects)
    max_total = _integer_budget(budgets, "max_total_description_chars", defects)
    max_lines = _integer_budget(budgets, "max_skill_md_lines", defects)
    defects.extend(_budget_ceiling_defects(budgets))
    total_description = 0
    skills_root = _skills_root(root)
    for skill in skills:
        description_length = len(skill["description"])
        total_description += description_length
        if max_description is not None and description_length > max_description:
            defects.append(
                f"description-budget: {skill['name']} uses {description_length}/{max_description} description characters"
            )
        if max_lines is not None and skill["line_count"] > max_lines:
            defects.append(
                f"progressive-disclosure-budget: {skill['name']} uses {skill['line_count']}/{max_lines} SKILL.md lines"
            )
        defects.extend(
            _contained_link_defects(skill["name"], skill["skill_path"], skills_root, prefix="linked-reference")
        )
    if max_total is not None and total_description > max_total:
        defects.append(f"listing-budget: descriptions use {total_description}/{max_total} characters")
    return defects


def _product_skill_files(root: Path) -> list[Path]:
    skills_root = _skills_root(root)
    if not skills_root.is_dir():
        return []
    found = []
    for path in sorted(skills_root.rglob("SKILL.md")):
        relative = path.relative_to(skills_root)
        if relative.parts and relative.parts[0] in {"ce-pack", "ce-addons"}:
            continue
        found.append(path)
    return found


def _entrypoint_defects(root: Path) -> list[str]:
    files = _product_skill_files(root)
    expected = _skills_root(root) / ENTRYPOINT / "SKILL.md"
    strict = (expected.parent / "references" / "specialists").is_dir() or (Path(root) / HOSTS_FILE).is_file()
    if strict and files != [expected]:
        found = ", ".join(path.relative_to(root).as_posix() for path in files) or "none"
        return [f"entrypoint-count: expected shaft-skills/{ENTRYPOINT}/SKILL.md only, found {found}"]
    if not strict and len(files) != 1:
        found = ", ".join(path.parent.name for path in files) or "none"
        return [
            "entrypoint-count: expected exactly 1 product SKILL.md outside ce-pack and ce-addons, "
            f"found {len(files)} ({found})"
        ]
    return []


def _specialist_directory(root: Path) -> Path:
    return _skills_root(root) / ENTRYPOINT / "references" / "specialists"


def _routing_linked_specialists(routing_text: str) -> set[str]:
    return set(re.findall(r"specialists/([a-z0-9-]+\.md)", routing_text))


def _specialist_file_defects(root: Path) -> list[str]:
    specialists = _specialist_directory(root)
    if not specialists.is_dir():
        return []
    routing = specialists.parent / "routing.md"
    if not routing.is_file():
        return ["specialist-routing: shaft-developer/references/routing.md is missing"]
    linked = _routing_linked_specialists(routing.read_text(encoding="utf-8"))
    files = {path.name: path for path in specialists.glob("*.md") if path.is_file()}
    defects: list[str] = []
    skills_root = _skills_root(root)
    for name in sorted(linked):
        path = files.get(name)
        if path is None:
            defects.append(f"specialist-missing: routing.md links missing specialists/{name}")
            continue
        if not path.read_text(encoding="utf-8").strip():
            defects.append(f"specialist-empty: {name}")
        defects.extend(_contained_link_defects(name, path, skills_root, prefix="specialist-link"))
    for name, path in sorted(files.items()):
        if name == SUPPORT_REFERENCE:
            continue
        if name not in linked:
            defects.append(f"specialist-unrouted: {name} is not linked from routing.md")
    return defects


def _support_reference_defects(root: Path) -> list[str]:
    specialists = _specialist_directory(root)
    if not specialists.is_dir():
        return []
    support = specialists / SUPPORT_REFERENCE
    if not support.is_file() or not support.read_text(encoding="utf-8").strip():
        return [f"specialist-support: {SUPPORT_REFERENCE} must be a non-empty shared reference"]
    mentioned = False
    for path in specialists.glob("*.md"):
        if path.name != SUPPORT_REFERENCE and SUPPORT_REFERENCE in path.read_text(encoding="utf-8"):
            mentioned = True
            break
    if not mentioned:
        return [f"specialist-support: {SUPPORT_REFERENCE} is not linked from a specialist reference"]
    return _contained_link_defects(SUPPORT_REFERENCE, support, _skills_root(root), prefix="specialist-link")


def _manifest_defects(root: Path, review: dict) -> list[str]:
    specialists = _specialist_directory(root)
    if not specialists.is_dir():
        return []
    declared = review.get("specialist_references")
    live = sorted(path.relative_to(_skills_root(root)).as_posix() for path in specialists.glob("*.md"))
    if not isinstance(declared, list) or sorted(item for item in declared if isinstance(item, str)) != live:
        return ["specialist-manifest: specialist_references must match specialists/*.md"]
    if any(not isinstance(item, str) for item in declared):
        return ["specialist-manifest: specialist_references must match specialists/*.md"]
    return []


def _specialist_quality_defects(root: Path, review: dict) -> list[str]:
    return [
        *_specialist_file_defects(root),
        *_support_reference_defects(root),
        *_manifest_defects(root, review),
    ]


def registry_display_names(root: Path = ROOT) -> list[str] | None:
    """Host display names from the canonical registry, or None when it is absent."""
    path = Path(root) / HOSTS_FILE
    if not path.is_file():
        return None
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return []
    hosts = payload.get("hosts") if isinstance(payload, dict) else None
    if not isinstance(hosts, list):
        return []
    names = []
    for host in hosts:
        if not isinstance(host, dict) or not isinstance(host.get("displayName"), str) or not host["displayName"].strip():
            return []
        names.append(host["displayName"])
    return names


def _host_client_defects(root: Path, clients: object) -> list[str]:
    expected = registry_display_names(root)
    if expected is None:
        return []
    if clients != expected:
        return ["supported-clients: quality-review.json supported_clients drifted from scripts/mcp/agent_hosts.json"]
    return []


def _generated_artifact_defects(root: Path) -> list[str]:
    skills_root = _skills_root(root)
    defects = []
    inventory_path = skills_root / INVENTORY_FILE
    quality_path = skills_root / QUALITY_FILE
    if not inventory_path.is_file() or inventory_path.read_text(encoding="utf-8") != render_inventory(root):
        defects.append(f"inventory-drift: refresh {SKILLS_DIRECTORY}/{INVENTORY_FILE} with --write")
    if not quality_path.is_file() or quality_path.read_text(encoding="utf-8") != render_quality_report(root):
        defects.append(f"quality-report-drift: refresh {SKILLS_DIRECTORY}/{QUALITY_FILE} with --write")
    return defects


def validate_repository(root: Path = ROOT) -> list[str]:
    """Return stable validation defects for the canonical SHAFT skill corpus."""
    root = Path(root)
    review = _load_review(root)
    if not review:
        return [f"quality-config: {SKILLS_DIRECTORY}/{REVIEW_FILE} must contain valid JSON"]
    skills = _discover(root)
    defects, budgets = _config_defects(review, {skill["name"] for skill in skills})
    defects.extend(_measurement_defects(root, skills, budgets))
    defects.extend(_entrypoint_defects(root))
    defects.extend(_specialist_quality_defects(root, review))
    defects.extend(_host_client_defects(root, review.get("supported_clients")))
    defects.extend(_generated_artifact_defects(root))
    return defects


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write", action="store_true", help="refresh generated inventory and quality reports")
    parser.add_argument("--check", action="store_true", help="validate the single-entrypoint quality contract")
    arguments = parser.parse_args()
    if arguments.write:
        write_generated(ROOT)
    defects = validate_repository(ROOT)
    for defect in defects:
        print(f"error: {defect}")
    return 1 if defects else 0


if __name__ == "__main__":
    raise SystemExit(main())
