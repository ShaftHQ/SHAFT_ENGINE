"""Assemble the portable shaft-skills Agent Plugin from canonical sources."""

import argparse
import shutil
import subprocess
from pathlib import Path


SCHEMA_URL = "https://agent-plugins.org/schemas/1.0.0/plugin.schema.json"
CANONICAL_SKILLS_DIRECTORY = "shaft-skills"


def git_executable() -> str:
    """Find Git before executing the fixed repository inventory command."""
    executable = shutil.which("git")
    if executable is None:
        raise RuntimeError("Git is required to assemble the portable plugin")
    return executable


def require_contained(root: Path, candidate: Path, label: str) -> Path:
    """Resolve a source path and reject a link that leaves its canonical root."""
    try:
        resolved = candidate.resolve(strict=True)
        resolved.relative_to(root.resolve())
    except (FileNotFoundError, ValueError) as error:
        raise ValueError(f"{label} must stay inside canonical skill sources: {candidate}") from error
    return resolved


def tracked_source_files(repository_root: Path) -> set[Path]:
    """Return the reviewed canonical files permitted to enter the package."""
    result = subprocess.run(
        [git_executable(), "ls-files", "-z", "--", CANONICAL_SKILLS_DIRECTORY],
        cwd=repository_root,
        check=True,
        capture_output=True,  # nosec B603: fixed Git command and arguments; shell is disabled.
    )
    return {
        repository_root / Path(path.decode("utf-8"))
        for path in result.stdout.split(b"\0")
        if path
    }


def write_adapters(package_root: Path) -> None:
    """Write small native discovery adapters around the portable core package."""
    claude_adapter = package_root / ".claude-plugin"
    claude_adapter.mkdir()
    (claude_adapter / "plugin.json").write_text(
        '{"name":"shaft-skills","version":"1.0.0",'
        '"description":"User-facing SHAFT test-automation skills.",'
        '"author":{"name":"ShaftHQ","url":"https://github.com/ShaftHQ/SHAFT_ENGINE"}}\n',
        encoding="utf-8",
    )
    codex_adapter = package_root / ".codex-plugin"
    codex_adapter.mkdir()
    (codex_adapter / "plugin.json").write_text(
        '{"name":"shaft-skills","version":"1.0.0","skills":"./skills/"}\n', encoding="utf-8"
    )
    marketplace = package_root / ".agents/plugins"
    marketplace.mkdir(parents=True)
    (marketplace / "marketplace.json").write_text(
        '{"name":"shaft-skills","plugins":['
        '{"name":"shaft-skills","source":{"source":"local","path":"./"}}]}\n',
        encoding="utf-8",
    )


def assemble(repository_root: Path, package_root: Path) -> None:
    """Create a new portable package from the canonical user-skill sources."""
    repository_root = Path(repository_root).resolve()
    canonical_skills = repository_root / CANONICAL_SKILLS_DIRECTORY
    package_root = Path(package_root)
    try:
        package_root.resolve(strict=False).relative_to(canonical_skills.resolve())
    except ValueError:
        pass
    else:
        raise ValueError("package output must not overlap canonical skill sources")
    if package_root.exists():
        raise FileExistsError(f"refusing to overwrite package output: {package_root}")

    package_root.mkdir(parents=True)
    (package_root / "plugin.json").write_text(
        f'{{"$schema":"{SCHEMA_URL}","name":"shaft-skills","version":"1.0.0",'
        '"description":"User-facing SHAFT test-automation skills.",'
        '"author":{"name":"ShaftHQ","url":"https://github.com/ShaftHQ/SHAFT_ENGINE"},'
        '"repository":"https://github.com/ShaftHQ/SHAFT_ENGINE","license":"MIT"}\n',
        encoding="utf-8",
    )
    write_adapters(package_root)
    for source in sorted(tracked_source_files(repository_root)):
        source = require_contained(canonical_skills, source, "canonical skill source")
        target = package_root / "skills" / source.relative_to(canonical_skills)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source, target)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path, help="new package output directory")
    parser.add_argument("--repository-root", type=Path, default=Path(__file__).resolve().parents[2])
    arguments = parser.parse_args()
    assemble(arguments.repository_root, arguments.output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
