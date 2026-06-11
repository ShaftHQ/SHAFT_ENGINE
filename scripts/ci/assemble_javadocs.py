#!/usr/bin/env python3
"""Assemble one publishable JavaDocs site from every Java-bearing SHAFT module."""

from __future__ import annotations

import html
import shutil
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
MODULES = {
    "shaft-engine": "Core engine",
    "shaft-pilot-core": "SHAFT Pilot contracts and security",
    "shaft-capture": "Deterministic browser capture model",
    "shaft-ai": "Optional direct AI providers",
    "shaft-browserstack": "BrowserStack integration",
    "shaft-video": "Desktop video integration",
    "shaft-visual": "Visual processing integration",
    "shaft-mcp": "Model Context Protocol server",
}


def project_version(root: Path = ROOT) -> str:
    value = ET.parse(root / "pom.xml").getroot().findtext("m:version", namespaces=NS)
    if not value:
        raise ValueError("root pom.xml does not declare a project version")
    return value.strip()


def assemble_javadocs(root: Path = ROOT, destination: Path | None = None) -> Path:
    destination = destination or root / "target" / "javadocs"
    sources = {
        module: root / module / "target" / "reports" / "apidocs"
        for module in MODULES
    }
    missing = [str(source / "index.html") for source in sources.values() if not (source / "index.html").is_file()]
    if missing:
        raise FileNotFoundError("missing module JavaDocs: " + ", ".join(missing))

    if destination.exists():
        shutil.rmtree(destination)
    destination.mkdir(parents=True)

    links = []
    for module, label in MODULES.items():
        shutil.copytree(sources[module], destination / module)
        links.append(f'<li><a href="{module}/index.html">{html.escape(label)}</a></li>')

    version = html.escape(project_version(root))
    (destination / "index.html").write_text(
        "<!doctype html>\n"
        '<html lang="en"><head><meta charset="utf-8">'
        f"<title>SHAFT {version} JavaDocs</title></head><body>"
        f"<h1>SHAFT {version} JavaDocs</h1><ul>{''.join(links)}</ul>"
        "</body></html>\n",
        encoding="utf-8",
    )
    return destination


def main() -> int:
    destination = assemble_javadocs()
    print(f"Assembled JavaDocs site: {destination}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
