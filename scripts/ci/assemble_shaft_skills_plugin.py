"""Assemble the portable shaft-skills Agent Plugin from canonical sources."""

import argparse
import shutil
import subprocess
from pathlib import Path


SCHEMA_URL = "https://agent-plugins.org/schemas/1.0.0/plugin.schema.json"
CANONICAL_SKILLS_DIRECTORY = "shaft-skills"
RELEASE_FILES = (Path("LICENSE"), Path("agent-plugins/CHANGELOG.md"))


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


def tracked_release_file(repository_root: Path, relative: Path) -> Path:
    """Return a tracked public release file that remains inside the repository."""
    source = repository_root / relative
    try:
        source.resolve(strict=True).relative_to(repository_root)
    except (FileNotFoundError, ValueError) as error:
        raise ValueError(f"release file must stay inside repository root: {relative}") from error
    tracked = subprocess.run(
        [git_executable(), "ls-files", "--error-unmatch", "--", relative.as_posix()],
        cwd=repository_root,
        capture_output=True,
    )
    if tracked.returncode:
        raise ValueError(f"release file must be tracked: {relative}")
    return source


def write_adapters(package_root: Path, version: str) -> None:
    """Write small native discovery adapters around the portable core package."""
    claude_adapter = package_root / ".claude-plugin"
    claude_adapter.mkdir()
    (claude_adapter / "plugin.json").write_text(
        f'{{"name":"shaft-skills","version":"{version}",'
        '"description":"User-facing SHAFT test-automation skills.",'
        '"author":{"name":"ShaftHQ","url":"https://github.com/ShaftHQ/SHAFT_ENGINE"}}\n',
        encoding="utf-8",
    )
    codex_adapter = package_root / ".codex-plugin"
    codex_adapter.mkdir()
    (codex_adapter / "plugin.json").write_text(
        f'{{"name":"shaft-skills","version":"{version}","skills":"./skills/"}}\n', encoding="utf-8"
    )
    marketplace = package_root / ".agents/plugins"
    marketplace.mkdir(parents=True)
    (marketplace / "marketplace.json").write_text(
        '{"name":"shaft-skills","plugins":['
        '{"name":"shaft-skills","source":{"source":"local","path":"./"}}]}\n',
        encoding="utf-8",
    )


def assemble(repository_root: Path, package_root: Path, version: str = "1.0.0") -> None:
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
        f'{{"$schema":"{SCHEMA_URL}","name":"shaft-skills","version":"{version}",'
        '"description":"User-facing SHAFT test-automation skills.",'
        '"author":{"name":"ShaftHQ","url":"https://github.com/ShaftHQ/SHAFT_ENGINE"},'
        '"repository":"https://github.com/ShaftHQ/SHAFT_ENGINE","license":"MIT"}\n',
        encoding="utf-8",
    )
    write_adapters(package_root, version)
    for source in sorted(tracked_source_files(repository_root)):
        source = require_contained(canonical_skills, source, "canonical skill source")
        target = package_root / "skills" / source.relative_to(canonical_skills)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source, target)
    for relative in RELEASE_FILES:
        source = tracked_release_file(repository_root, relative)
        shutil.copyfile(source, package_root / ("CHANGELOG.md" if relative.name == "CHANGELOG.md" else relative.name))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path, help="new package output directory")
    parser.add_argument("--repository-root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--version", default="1.0.0")
    arguments = parser.parse_args()
    assemble(arguments.repository_root, arguments.output, arguments.version)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
