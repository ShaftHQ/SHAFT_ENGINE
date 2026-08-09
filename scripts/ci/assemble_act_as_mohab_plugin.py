"""Assemble the portable act-as-mohab Agent Plugin from canonical sources."""

import argparse
import shutil
from pathlib import Path


SCHEMA_URL = "https://agent-plugins.org/schemas/1.0.0/plugin.schema.json"
SKILLS = ("act-as-mohab", "consult-first", "retrieve-first")


def copy_tree(source: Path, destination: Path) -> None:
    """Copy one canonical source tree without preserving host-specific metadata."""
    for path in sorted(source.rglob("*")):
        if not path.is_file():
            continue
        target = destination / path.relative_to(source)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(path, target)


def assemble(repository_root: Path, package_root: Path) -> None:
    """Create a new portable package from the canonical skill sources."""
    repository_root = Path(repository_root).resolve()
    package_root = Path(package_root)
    if package_root.exists():
        raise FileExistsError(f"refusing to overwrite package output: {package_root}")

    canonical_skills = repository_root / ".agents/skills"
    package_root.mkdir(parents=True)
    (package_root / "plugin.json").write_text(
        f'{{"$schema":"{SCHEMA_URL}","name":"act-as-mohab"}}\n', encoding="utf-8"
    )
    for skill in SKILLS:
        source = canonical_skills / skill
        destination = package_root / "skills" / skill
        destination.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source / "SKILL.md", destination / "SKILL.md")
    copy_tree(
        canonical_skills / "act-as-mohab/references",
        package_root / "skills/act-as-mohab/references",
    )
    (package_root / "skills/README.md").write_text(
        "# Act as Mohab portable skills\n\n"
        "This package contains the maintainer workflow entrypoint and its required companion skills.\n",
        encoding="utf-8",
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path, help="new package output directory")
    parser.add_argument("--repository-root", type=Path, default=Path(__file__).resolve().parents[2])
    arguments = parser.parse_args()
    assemble(arguments.repository_root, arguments.output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
