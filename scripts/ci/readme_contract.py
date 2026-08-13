"""Shared trust checks for the repository landing README.

This is intentionally a small parser for the constructs used by this README:
inline links and linked images, reference definitions, HTTP(S) autolinks,
HTML ``href``/``src``, backtick or tilde fences, inline backtick code, HTML
comments, and Mermaid flowcharts. It does not claim full CommonMark support.
"""

from __future__ import annotations

import re
from urllib.parse import urlsplit

USER_GUIDE_ROOT = "https://shafthq.github.io/"
USER_GUIDE_HOST = "shafthq.github.io"
BUILD_BADGE_SOURCE = (
    "https://img.shields.io/github/actions/workflow/status/"
    "ShaftHQ/SHAFT_ENGINE/pr-gate.yml"
)
BUILD_WORKFLOW_URL = (
    "https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/pr-gate.yml"
)

_HTML_COMMENT = re.compile(r"<!--.*?-->", flags=re.DOTALL)
_FENCE_OPEN = re.compile(r"^[ \t]{0,3}(`{3,}|~{3,})([^\r\n]*)$")
_INLINE_CODE = re.compile(r"`+[^`\n]*`+")
_MARKDOWN_DESTINATION = re.compile(r"\]\(\s*<?([^\s)>]+)>?")
_REFERENCE_DESTINATION = re.compile(
    r"^[ \t]{0,3}\[([^\]\n]+)\]:[ \t]*<?([^>\s]+)>?(?:[ \t]+.*)?$",
    flags=re.MULTILINE,
)
_FULL_REFERENCE = re.compile(r"!?\[([^\]\n]+)\]\[([^\]\n]*)\]")
_SHORTCUT_REFERENCE = re.compile(r"(?<!!)\[([^\]\n]+)\](?![\[(])")
_AUTOLINK_DESTINATION = re.compile(r"<(https?://[^<>\s]+)>", flags=re.IGNORECASE)
_HTML_ATTRIBUTE = re.compile(
    r"\b(href|src|width)\s*=\s*(?:\"([^\"]*)\"|'([^']*)'|([^\s\"'=<>`]+))",
    flags=re.IGNORECASE,
)
_HTML_TAG = re.compile(r"<([A-Za-z][\w:-]*)\b([^>]*)>", flags=re.DOTALL)
_IMG_TAG = re.compile(r"<img\b([^>]*)>", flags=re.DOTALL | re.IGNORECASE)
_GENERATOR_LINK = re.compile(
    r"\[Generate[^\]]*project\]\(https://shafthq\.github\.io/\)",
    flags=re.IGNORECASE,
)
_BUILD_BADGE = re.compile(
    r"\[!\[[^\]\n]*\]\("
    + re.escape(BUILD_BADGE_SOURCE)
    + r"(?:\?[^)\s]*)?\)\]\("
    + re.escape(BUILD_WORKFLOW_URL)
    + r"\)",
    flags=re.IGNORECASE,
)
_MERMAID_NODE = re.compile(r"\b([A-Za-z][\w-]*)\s*\[([^\]]+)\]")
_MERMAID_EDGE = re.compile(
    r"\b([A-Za-z][\w-]*)(?:\[[^\]]+\])?\s*-->\s*([A-Za-z][\w-]*)"
)


def _without_comments(markup: str) -> str:
    return _HTML_COMMENT.sub("", markup)


def _scan_fences(markup: str) -> tuple[str, tuple[tuple[str, str], ...]]:
    """Strip supported fences and return closed blocks as ``(info, body)``."""
    outside: list[str] = []
    blocks: list[tuple[str, str]] = []
    marker = ""
    info = ""
    body: list[str] = []

    for line in markup.splitlines(keepends=True):
        content = line.rstrip("\r\n")
        if not marker:
            opening = _FENCE_OPEN.fullmatch(content)
            if opening:
                marker = opening.group(1)
                info = opening.group(2).strip()
                body = []
                outside.append("\n" if line.endswith(("\n", "\r")) else "")
            else:
                outside.append(line)
            continue

        closing = re.fullmatch(
            rf"[ \t]{{0,3}}{re.escape(marker[0])}{{{len(marker)},}}[ \t]*",
            content,
        )
        if closing:
            blocks.append((info, "".join(body)))
            marker = ""
            info = ""
            body = []
            outside.append("\n" if line.endswith(("\n", "\r")) else "")
        else:
            body.append(line)

    return "".join(outside), tuple(blocks)


def _link_scan_markup(markup: str) -> str:
    without_comments = _without_comments(markup)
    without_fences, _ = _scan_fences(without_comments)
    return _INLINE_CODE.sub("", without_fences)


def _visible_markup(markup: str) -> str:
    without_comments = _without_comments(markup)
    without_fences, _ = _scan_fences(without_comments)
    return without_fences


def _attributes(fragment: str) -> dict[str, str]:
    attributes: dict[str, str] = {}
    for match in _HTML_ATTRIBUTE.finditer(fragment):
        value = next(group for group in match.groups()[1:] if group is not None)
        attributes[match.group(1).lower()] = value
    return attributes


def _reference_destinations(markup: str) -> list[str]:
    definitions = {
        " ".join(match.group(1).split()).casefold(): match.group(2)
        for match in _REFERENCE_DESTINATION.finditer(markup)
    }
    without_definitions = _REFERENCE_DESTINATION.sub("", markup)
    used_labels = {
        " ".join((label or text).split()).casefold()
        for text, label in _FULL_REFERENCE.findall(without_definitions)
    }
    used_labels.update(
        " ".join(label.split()).casefold()
        for label in _SHORTCUT_REFERENCE.findall(without_definitions)
    )
    return [definitions[label] for label in used_labels if label in definitions]


def destinations(markup: str) -> tuple[str, ...]:
    """Return rendered destinations from the README's supported constructs."""
    rendered = _link_scan_markup(markup)
    markdown = [match.group(1) for match in _MARKDOWN_DESTINATION.finditer(rendered)]
    references = _reference_destinations(rendered)
    autolinks = [
        match.group(1) for match in _AUTOLINK_DESTINATION.finditer(rendered)
    ]
    html = []
    for tag in _HTML_TAG.finditer(rendered):
        attributes = _attributes(tag.group(2))
        html.extend(
            attributes[name] for name in ("href", "src") if name in attributes
        )
    return tuple(markdown + references + autolinks + html)


def _has_accessible_evidence_workflow(markup: str) -> bool:
    uncommented = _without_comments(markup)
    _, blocks = _scan_fences(uncommented)
    for info, body in blocks:
        if not info or info.split()[0].casefold() != "mermaid":
            continue
        live_lines = [
            line for line in body.splitlines()
            if not line.lstrip().startswith("%%")
        ]
        block = "\n".join(live_lines)
        if re.search(r"^\s*accTitle:\s*\S", block, flags=re.MULTILINE) is None:
            continue
        if re.search(r"^\s*accDescr:\s*\S", block, flags=re.MULTILINE) is None:
            continue

        nodes = {
            label.strip().lower(): identifier
            for identifier, label in _MERMAID_NODE.findall(block)
        }
        required_labels = (
            "test intent",
            "configuration",
            "shaft orchestration",
            "web",
            "mobile",
            "api",
            "native, cli, and database",
            "unified evidence",
        )
        if any(label not in nodes for label in required_labels):
            continue

        edges = set(_MERMAID_EDGE.findall(block))
        orchestration = nodes["shaft orchestration"]
        evidence = nodes["unified evidence"]
        required_edges = {
            (nodes["test intent"], orchestration),
            (nodes["configuration"], orchestration),
            *(
                (orchestration, nodes[label])
                for label in required_labels[3:7]
            ),
            *(
                (nodes[label], evidence)
                for label in required_labels[3:7]
            ),
        }
        if required_edges.issubset(edges):
            return True
    return False


def validate_readme_contract(readme: str) -> list[str]:
    """Validate the visible, developer-facing trust contract owned by README.md."""
    errors: list[str] = []
    link_scan = _link_scan_markup(readme)
    visible = _visible_markup(readme)
    readme_destinations = set(destinations(readme))

    if USER_GUIDE_ROOT not in readme_destinations:
        errors.append(f"README.md is missing the user-guide landing page: {USER_GUIDE_ROOT}")
    if any(
        (urlsplit(destination).hostname or "").lower() == USER_GUIDE_HOST
        and destination != USER_GUIDE_ROOT
        for destination in readme_destinations
    ):
        errors.append(
            f"README.md user-guide links must target {USER_GUIDE_ROOT} exactly"
        )

    prominent_logo = False
    for image in _IMG_TAG.finditer(link_scan):
        attributes = _attributes(image.group(1))
        if not attributes.get("src", "").endswith("shaft_standard.png"):
            continue
        try:
            prominent_logo = int(attributes.get("width", "0")) >= 240
        except ValueError:
            prominent_logo = False
        if prominent_logo:
            break
    if not prominent_logo:
        errors.append("README.md must display the SHAFT S logo prominently")
    if _GENERATOR_LINK.search(link_scan) is None:
        errors.append("README.md is missing the generator-first journey")

    prose = link_scan.lower()
    prose_markers = (
        "test intent and configuration",
        "orchestration",
        "execution surfaces",
        "unified evidence",
    )
    if (
        any(marker not in prose for marker in prose_markers)
        or not _has_accessible_evidence_workflow(readme)
    ):
        errors.append("README.md is missing the accessible Mermaid evidence workflow")
    if "io.github.shafthq:shaft-engine" not in visible:
        errors.append("README.md is missing the canonical Maven coordinate")
    if _BUILD_BADGE.search(link_scan) is None:
        errors.append("README.md is missing the pr-gate.yml build badge")

    return errors
