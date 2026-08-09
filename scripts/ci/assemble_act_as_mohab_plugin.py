"""Assemble the portable act-as-mohab Agent Plugin from canonical sources."""

import argparse
import shutil
import subprocess
from pathlib import Path


SCHEMA_URL = "https://agent-plugins.org/schemas/1.0.0/plugin.schema.json"
SKILLS = ("act-as-mohab", "consult-first", "retrieve-first")
PORTABLE_REFERENCE_SUFFIXES = {".md", ".LICENSE"}


def require_contained(root: Path, candidate: Path, label: str) -> Path:
    """Resolve one source path and reject a link that leaves its canonical root."""
    try:
        resolved = candidate.resolve(strict=True)
        resolved.relative_to(root.resolve())
    except (FileNotFoundError, ValueError) as error:
        raise ValueError(f"{label} must stay inside canonical skill sources: {candidate}") from error
    return resolved


def tracked_source_files(repository_root: Path) -> set[Path]:
    """Return the reviewed canonical files permitted to enter the package."""
    result = subprocess.run(
        [
            "git",
            "ls-files",
            "-z",
            "--",
            ".agents/skills/act-as-mohab",
            ".agents/skills/consult-first/SKILL.md",
            ".agents/skills/retrieve-first/SKILL.md",
        ],
        cwd=repository_root,
        check=True,
        capture_output=True,
    )
    return {
        repository_root / Path(path.decode("utf-8"))
        for path in result.stdout.split(b"\0")
        if path
    }


def copy_tree(source: Path, destination: Path, allowed_files: set[Path]) -> None:
    """Copy reviewed portable references without preserving host-specific metadata."""
    source = require_contained(source.parent, source, "canonical reference directory")
    for path in sorted(path for path in allowed_files if path.is_relative_to(source)):
        path = require_contained(source, path, "canonical reference")
        if path.suffix not in PORTABLE_REFERENCE_SUFFIXES:
            continue
        target = destination / path.relative_to(source)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(path, target)


def assemble(repository_root: Path, package_root: Path) -> None:
    """Create a new portable package from the canonical skill sources."""
    repository_root = Path(repository_root).resolve()
    package_root = Path(package_root)
    canonical_skills = repository_root / ".agents/skills"
    try:
        package_root.resolve(strict=False).relative_to(canonical_skills.resolve())
    except ValueError:
        pass
    else:
        raise ValueError("package output must not overlap canonical skill sources")
    if package_root.exists():
        raise FileExistsError(f"refusing to overwrite package output: {package_root}")

    allowed_files = tracked_source_files(repository_root)
    package_root.mkdir(parents=True)
    (package_root / "plugin.json").write_text(
        f'{{"$schema":"{SCHEMA_URL}","name":"act-as-mohab"}}\n', encoding="utf-8"
    )
    for skill in SKILLS:
        source = canonical_skills / skill
        destination = package_root / "skills" / skill
        destination.mkdir(parents=True, exist_ok=True)
        source = require_contained(canonical_skills, source, f"canonical skill directory {skill}")
        skill_file = source / "SKILL.md"
        if skill_file not in allowed_files:
            raise ValueError(f"canonical skill must be tracked: {skill}")
        skill_file = require_contained(canonical_skills, skill_file, f"canonical skill {skill}")
        shutil.copyfile(skill_file, destination / "SKILL.md")
    copy_tree(
        canonical_skills / "act-as-mohab/references",
        package_root / "skills/act-as-mohab/references",
        allowed_files,
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
