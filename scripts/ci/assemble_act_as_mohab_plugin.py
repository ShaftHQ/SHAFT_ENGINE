"""Assemble the portable act-as-mohab Agent Plugin from canonical sources."""

import argparse
import shutil
import subprocess
from pathlib import Path


SCHEMA_URL = "https://agent-plugins.org/schemas/1.0.0/plugin.schema.json"
SKILLS = ("act-as-mohab", "consult-first", "retrieve-first")
PORTABLE_REFERENCE_SUFFIXES = {".md", ".LICENSE"}
RELEASE_FILES = (Path("LICENSE"), Path("agent-plugins/CHANGELOG.md"))


def git_executable() -> str:
    """Find Git before executing the fixed repository inventory command."""
    executable = shutil.which("git")
    if executable is None:
        raise RuntimeError("Git is required to assemble the portable plugin")
    return executable


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
            git_executable(),
            "ls-files",
            "-z",
            "--",
            ".agents/skills/act-as-mohab",
            ".agents/skills/consult-first/SKILL.md",
            ".agents/skills/retrieve-first/SKILL.md",
        ],
        cwd=repository_root,
        check=True,
        capture_output=True,  # nosec B603: fixed Git command and arguments; shell is disabled.
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


def copy_release_files(repository_root: Path, package_root: Path) -> None:
    """Copy tracked public release files into a portable package root."""
    for relative in RELEASE_FILES:
        source = require_contained(repository_root, repository_root / relative, f"release file {relative}")
        if not source.is_file():
            raise ValueError(f"release file must be a file: {relative}")
        tracked = subprocess.run(
            [git_executable(), "ls-files", "--error-unmatch", "--", relative.as_posix()],
            cwd=repository_root,
            capture_output=True,
        )
        if tracked.returncode:
            raise ValueError(f"release file must be tracked: {relative}")
        shutil.copyfile(source, package_root / ("CHANGELOG.md" if relative.name == "CHANGELOG.md" else relative.name))


def assemble(repository_root: Path, package_root: Path, version: str = "1.0.0") -> None:
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
        f'{{"$schema":"{SCHEMA_URL}","name":"act-as-mohab","version":"{version}",'
        '"description":"Maintainer workflow and harness skills for SHAFT.",'
        '"author":{"name":"ShaftHQ","url":"https://github.com/ShaftHQ/SHAFT_ENGINE"},'
        '"repository":"https://github.com/ShaftHQ/SHAFT_ENGINE","license":"MIT"}\n',
        encoding="utf-8",
    )
    claude_adapter = package_root / ".claude-plugin"
    claude_adapter.mkdir()
    (claude_adapter / "plugin.json").write_text(
        f'{{"name":"act-as-mohab","version":"{version}",'
        '"description":"Maintainer workflow and harness skills for SHAFT.",'
        '"author":{"name":"ShaftHQ","url":"https://github.com/ShaftHQ/SHAFT_ENGINE"}}\n',
        encoding="utf-8",
    )
    codex_adapter = package_root / ".codex-plugin"
    codex_adapter.mkdir()
    (codex_adapter / "plugin.json").write_text(
        f'{{"name":"act-as-mohab","version":"{version}",'
        '"description":"Maintainer workflow and harness skills for SHAFT.",'
        '"skills":"./skills/"}\n',
        encoding="utf-8",
    )
    codex_marketplace = package_root / ".agents/plugins"
    codex_marketplace.mkdir(parents=True)
    (codex_marketplace / "marketplace.json").write_text(
        '{"name":"act-as-mohab","plugins":['
        '{"name":"act-as-mohab","source":{"source":"local","path":"./"}}]}\n',
        encoding="utf-8",
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
    copy_release_files(repository_root, package_root)
    (package_root / "skills/README.md").write_text(
        "# Act as Mohab portable skills\n\n"
        "This package contains the maintainer workflow entrypoint and its required companion skills.\n",
        encoding="utf-8",
    )


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
