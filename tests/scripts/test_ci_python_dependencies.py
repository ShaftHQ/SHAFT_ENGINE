"""Contract tests for Python dependencies installed by CI workflows (#4650)."""

from __future__ import annotations

import re
import shlex
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
REQUIREMENTS = ROOT / "requirements-ci.txt"
CHAOS_GAUGE_REQUIREMENTS = ROOT / "scripts/ci/chaos_gauge/requirements.lock"
EXPECTED_PINS = {
    "attrs": "26.1.0",
    "jsonschema": "4.26.0",
    "jsonschema-specifications": "2025.9.1",
    "pyyaml": "6.0.3",
    "referencing": "0.37.0",
    "rpds-py": "2026.6.3",
}
EXPECTED_INSTALLS = {
    ".github/workflows/agent-plugin-acceptance.yml": (
        "python -m pip install --no-deps --requirement requirements-ci.txt --quiet",
        "python -m pip install --no-deps --requirement requirements-ci.txt --quiet",
    ),
    ".github/workflows/mavenCentral_cd.yml": (
        "python3 -m pip install --no-deps --requirement requirements-ci.txt --quiet",
    ),
    ".github/workflows/pr-gate.yml": (
        "python3 -m pip install --no-deps --requirement requirements-ci.txt --quiet",
        "python -m pip install --no-deps --requirement requirements-ci.txt --quiet",
        "python3 -m pip install --no-deps --requirement requirements-ci.txt --quiet",
    ),
    ".github/workflows/publish-shaft-mcp.yml": (
        "python3 -m pip install --no-deps --requirement ../requirements-ci.txt --quiet",
    ),
    ".github/workflows/chaos-gauge-public-canary.yml": (
        "pip install --system --require-hashes --requirement scripts/ci/chaos_gauge/requirements.lock",
    ),
}
EXPECTED_SETUP_PYTHON = {
    **{path: ("actions/setup-python@v7", "3.13") for path in EXPECTED_INSTALLS if path != ".github/workflows/chaos-gauge-public-canary.yml"},
    ".github/workflows/chaos-gauge-public-canary.yml": (
        "actions/setup-python@ece7cb06caefa5fff74198d8649806c4678c61a1", "3.12",
    ),
}
EXPECTED_REQUIREMENTS = {
    **{path: REQUIREMENTS for path in EXPECTED_INSTALLS if path != ".github/workflows/chaos-gauge-public-canary.yml"},
    ".github/workflows/chaos-gauge-public-canary.yml": CHAOS_GAUGE_REQUIREMENTS,
}
EXPECTED_WORKING_DIRECTORIES = {
    ".github/workflows/chaos-gauge-public-canary.yml": "public",
}


class CiPythonDependenciesTest(unittest.TestCase):
    def test_workflow_dependencies_are_exact_centralized_and_automated(self):
        self.assertTrue(REQUIREMENTS.is_file(), "requirements-ci.txt must own CI Python versions")
        entries = [
            line.strip()
            for line in REQUIREMENTS.read_text(encoding="utf-8").splitlines()
            if line.strip() and not line.lstrip().startswith("#")
        ]
        parsed: dict[str, str] = {}
        for entry in entries:
            self.assertEqual(entry.count("=="), 1, f"CI dependency must use one exact pin: {entry}")
            name, version = entry.split("==", 1)
            self.assertTrue(name and version and not re.search(r"\s", entry), entry)
            parsed[name.lower()] = version
        self.assertEqual(len(parsed), len(entries), "CI dependency pins must not be duplicated")
        self.assertEqual(parsed, EXPECTED_PINS)
        self.assertEqual(
            entries,
            sorted(entries, key=lambda entry: entry.split("==", 1)[0].lower()),
            "CI dependency pins must stay sorted by package name",
        )

        actual_installs: dict[str, tuple[str, ...]] = {}
        workflow_root = ROOT / ".github/workflows"
        workflow_paths = set(workflow_root.glob("*.yml")) | set(workflow_root.glob("*.yaml"))
        for path in sorted(workflow_paths):
            commands = tuple(
                match.group(0).strip()
                for match in re.finditer(
                    r"(?:(?:python|python3) -m pip|pip3?) install[^\r\n]*",
                    path.read_text(encoding="utf-8"),
                )
            )
            if commands:
                actual_installs[path.relative_to(ROOT).as_posix()] = commands
        self.assertEqual(actual_installs, EXPECTED_INSTALLS)

        dependabot = (ROOT / ".github/dependabot.yml").read_text(encoding="utf-8")
        self.assertRegex(
            dependabot,
            r'(?ms)- package-ecosystem: "pip"\s+directory: "/"\s+schedule:\s+'
            r'interval: "daily"\s+time: "00:00"\s+timezone: "Africa/Cairo"\s+'
            r'labels:\s+- "dependencies"',
        )

    def workflow_install_steps(self):
        yaml = __import__("yaml")
        for relative_path in EXPECTED_INSTALLS:
            workflow_path = ROOT / relative_path
            workflow = yaml.safe_load(workflow_path.read_text(encoding="utf-8"))
            for job_name, job in workflow["jobs"].items():
                job_working_directory = (
                    job.get("defaults", {}).get("run", {}).get("working-directory", ".")
                )
                steps = job["steps"]
                for index, step in enumerate(steps):
                    for command in str(step.get("run", "")).splitlines():
                        if re.search(r"(?:(?:python|python3) -m pip|pip3?) install", command):
                            yield (
                                relative_path,
                                job_name,
                                steps,
                                index,
                                step.get("working-directory", job_working_directory),
                                command.strip(),
                            )

    def test_requirement_paths_exist_from_each_effective_working_directory(self):
        installs = list(self.workflow_install_steps())
        for relative_path, job_name, _, _, working_directory, command in installs:
            with self.subTest(workflow=relative_path, job=job_name):
                arguments = shlex.split(command)
                requirement = arguments[arguments.index("--requirement") + 1]
                if relative_path in EXPECTED_WORKING_DIRECTORIES:
                    self.assertEqual(working_directory, EXPECTED_WORKING_DIRECTORIES[relative_path])
                source_root = ROOT if working_directory == "public" else ROOT / working_directory
                resolved = (source_root / requirement).resolve()
                self.assertEqual(resolved, EXPECTED_REQUIREMENTS[relative_path].resolve())
                self.assertTrue(resolved.is_file())
                if resolved == CHAOS_GAUGE_REQUIREMENTS:
                    self.assertIn("--require-hashes", arguments)

    def test_each_install_job_sets_up_supported_python_before_installing(self):
        for relative_path, job_name, steps, install_index, _, _ in self.workflow_install_steps():
            with self.subTest(workflow=relative_path, job=job_name):
                expected_action, expected_version = EXPECTED_SETUP_PYTHON[relative_path]
                setup_steps = [
                    step
                    for step in steps[:install_index]
                    if step.get("uses") == expected_action
                ]
                self.assertTrue(setup_steps, "install job must set up repository-standard Python")
                self.assertEqual(setup_steps[-1].get("with", {}).get("python-version"), expected_version)

    def test_chaos_gauge_installs_dependencies_before_contracts(self):
        yaml = __import__("yaml")
        workflow = yaml.safe_load(
            (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        )
        steps = workflow["jobs"]["chaos-gauge"]["steps"]
        install_index = next(
            index
            for index, step in enumerate(steps)
            if "--requirement requirements-ci.txt" in str(step.get("run", ""))
        )
        test_index = next(
            index
            for index, step in enumerate(steps)
            if "tests.scripts.test_chaos_gauge_contracts" in str(step.get("run", ""))
        )
        self.assertLess(install_index, test_index)


if __name__ == "__main__":
    unittest.main()
