"""Assemble the portable act-as-mohab Agent Plugin from canonical sources."""

import argparse
import json
import shutil
import subprocess
import zipfile
from pathlib import Path

try:
    from scripts.ci.agent_plugin_release import release_version
except ModuleNotFoundError:
    from agent_plugin_release import release_version


SCHEMA_URL = "https://agent-plugins.org/schemas/1.0.0/plugin.schema.json"
SKILLS = ("act-as-mohab",)
PORTABLE_REFERENCE_SUFFIXES = {".md", ".LICENSE"}
RELEASE_FILES = (
    (Path("LICENSE"), Path("LICENSE")),
    (Path("agent-plugins/act-as-mohab/CHANGELOG.md"), Path("CHANGELOG.md")),
    (Path("agent-plugins/act-as-mohab/COMPATIBILITY.md"), Path("COMPATIBILITY.md")),
)
RUNTIME_SOURCES = (
    Path("scripts/agents/act_as_mohab_cli.py"),
    Path("scripts/agents/planning_contract.py"),
    Path("scripts/agents/repository_context.py"),
    Path("scripts/agents/watch_pr_checks.py"),
)
RUNTIME_MAIN = b"from act_as_mohab_cli import main\nraise SystemExit(main())\n"


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


def tracked_release_file(repository_root: Path, relative: Path) -> Path:
    """Return a tracked regular release file without following repository links."""
    candidate = repository_root / relative
    current = candidate
    while current != repository_root:
        if current.is_symlink():
            raise ValueError(f"release file must not be a symlink: {relative}")
        current = current.parent
    source = require_contained(repository_root, candidate, f"release file {relative}")
    if not source.is_file():
        raise ValueError(f"release file must be a file: {relative}")
    tracked = subprocess.run(
        [git_executable(), "ls-files", "--error-unmatch", "--", relative.as_posix()],
        cwd=repository_root,
        capture_output=True,  # nosec B603: fixed Git command and arguments; shell is disabled.
    )
    if tracked.returncode:
        raise ValueError(f"release file must be tracked: {relative}")
    return source


def copy_release_files(repository_root: Path, package_root: Path) -> None:
    """Copy tracked public release files into a portable package root."""
    for relative, target in RELEASE_FILES:
        shutil.copyfile(tracked_release_file(repository_root, relative), package_root / target)


def build_runtime(repository_root: Path, package_root: Path) -> None:
    """Build the deterministic stdlib zipapp from tracked canonical modules."""
    destination = package_root / "bin/act-as-mohab.pyz"
    destination.parent.mkdir()
    entries = [("__main__.py", RUNTIME_MAIN)]
    for relative in RUNTIME_SOURCES:
        source = tracked_release_file(repository_root, relative)
        entries.append((source.name, source.read_bytes().replace(b"\r\n", b"\n")))
    with zipfile.ZipFile(destination, "w", compression=zipfile.ZIP_STORED) as archive:
        for name, content in sorted(entries):
            entry = zipfile.ZipInfo(name, (1980, 1, 1, 0, 0, 0))
            entry.create_system = 3
            entry.external_attr = 0o100644 << 16
            entry.compress_type = zipfile.ZIP_STORED
            archive.writestr(entry, content)


def assemble(repository_root: Path, package_root: Path, version: str | None = None) -> None:
    """Create a new portable package from the canonical skill sources."""
    repository_root = Path(repository_root).resolve()
    version = release_version(repository_root, "act-as-mohab", version)
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
        '"author":{"name":"ShaftHQ","url":"https://github.com/ShaftHQ"},'
        '"repository":"https://github.com/ShaftHQ/SHAFT_ENGINE","license":"MIT",'
        '"skills":"./skills/","mcpServers":"./.mcp.json",'
        '"interface":{"displayName":"Act as Mohab",'
        '"shortDescription":"Portable ChaosEngine maintainer workflow",'
        '"longDescription":"Repository-aware maintainer guidance and bounded delivery operations.",'
        '"developerName":"ShaftHQ","category":"Developer Tools",'
        '"capabilities":["Repository context","Pull request checks","MCP"],'
        '"defaultPrompt":["Resolve the current repository context."]}}\n',
        encoding="utf-8",
    )
    (claude_adapter / "marketplace.json").write_text(
        f'{{"name":"act-as-mohab","owner":{{"name":"ShaftHQ"}},'
        '"description":"Portable ChaosEngine maintainer workflow.","plugins":['
        f'{{"name":"act-as-mohab","source":"./",'
        f'"description":"Maintainer workflow and harness skills for SHAFT.",'
        f'"version":"{version}"}}]}}\n',
        encoding="utf-8",
    )
    (package_root / ".mcp.json").write_text(
        json.dumps(
            {
                "mcpServers": {
                "chaosengine": {
                    "command": "python",
                    "args": ["./bin/act-as-mohab.pyz", "mcp"],
                    "cwd": ".",
                }
                }
            },
            separators=(",", ":"),
        )
        + "\n",
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
    build_runtime(repository_root, package_root)
    (package_root / "skills/README.md").write_text(
        "# Act as Mohab portable skills\n\n"
        "This package contains the maintainer workflow entrypoint and its internal references.\n",
        encoding="utf-8",
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path, help="new package output directory")
    parser.add_argument("--repository-root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--version")
    arguments = parser.parse_args()
    assemble(arguments.repository_root, arguments.output, arguments.version)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
