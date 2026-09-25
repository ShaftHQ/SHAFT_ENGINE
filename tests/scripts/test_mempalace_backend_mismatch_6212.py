"""MemPalace backend mismatch is detected, pinned, and never silent (#6212).

An ambient ``MEMPALACE_BACKEND=chroma`` (or a caller that passes ``--palace``
without ``--backend``) made the MemPalace CLI refuse the sqlite_exact palace.
Retrieve reported a degraded store and the agent quietly used Graphify only.
"""

from __future__ import annotations

import importlib.util
import os
import subprocess
import tempfile
import unittest
import unittest.mock
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


stores = _load("stores_6212", ROOT / "chaos-engine/stores.py")
retrieve = _load("retrieve_6212", ROOT / "chaos-engine/retrieve.py")
overlay_match = _load("overlay_match_6212", ROOT / "chaos-engine/overlay_match.py")

MISMATCH = (
    "Backend mismatch at palace: palace contains 'sqlite_exact' backend "
    "artifacts, but 'chroma' was selected"
)


class BackendPinTest(unittest.TestCase):
    def test_explicit_palace_without_backend_still_pins_sqlite_exact(self):
        palace = Path("/palace")
        arguments = stores.inject_mempalace_arguments(
            ["--palace", "/elsewhere", "search", "q"], palace
        )
        self.assertIn("--backend", arguments)
        self.assertEqual("sqlite_exact", arguments[arguments.index("--backend") + 1])
        self.assertEqual("/elsewhere", arguments[arguments.index("--palace") + 1])

    def test_caller_selected_backend_is_left_alone(self):
        arguments = ["--palace", "/p", "--backend", "sqlite_exact", "search", "q"]
        self.assertEqual(arguments, stores.inject_mempalace_arguments(arguments, Path("/p")))

    def test_retrieve_overrides_an_ambient_chroma_selection(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            tool = project / ".chaos-engine/tool.py"
            tool.parent.mkdir(parents=True)
            tool.write_text("", encoding="utf-8")
            seen = {}

            def fake_run(args, **kwargs):
                seen.update(kwargs["env"])
                return subprocess.CompletedProcess(args, 0, "hit.md:1 retrieve gate\n", "")

            with unittest.mock.patch.dict(os.environ, {"MEMPALACE_BACKEND": "chroma"}), unittest.mock.patch.object(
                retrieve.subprocess, "run", side_effect=fake_run
            ):
                retrieve._run_store(project, "mempalace", "retrieve gate")
            self.assertEqual("sqlite_exact", seen.get("MEMPALACE_BACKEND"))


class LoudMismatchTest(unittest.TestCase):
    def test_mismatch_receipt_names_the_repair_instead_of_falling_back(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            tool = project / ".chaos-engine/tool.py"
            tool.parent.mkdir(parents=True)
            tool.write_text("", encoding="utf-8")
            failed = subprocess.CompletedProcess([], 1, "", MISMATCH)
            with unittest.mock.patch.object(retrieve.subprocess, "run", return_value=failed):
                receipt = retrieve._run_store(project, "mempalace", "q")
        self.assertEqual("backend-mismatch", receipt["reason"])
        self.assertTrue(receipt["blocking"])
        self.assertIn("doctor", receipt["fixNext"])
        self.assertIn("--component mempalace", receipt["fixNext"])
        self.assertIn("not a fallback", receipt["fixNext"])


class RecheckTest(unittest.TestCase):
    def test_recheck_clears_a_recorded_mismatch_and_re_probes(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            tool = project / ".chaos-engine/tool.py"
            tool.parent.mkdir(parents=True)
            tool.write_text("", encoding="utf-8")
            hooks = project / ".chaos-engine/hooks"
            hooks.mkdir()
            source = ROOT / "chaos-engine/hooks/retrieve_justification.py"
            (hooks / source.name).write_text(source.read_text(encoding="utf-8"), encoding="utf-8")
            failed = subprocess.CompletedProcess([], 1, "", MISMATCH)
            healthy = subprocess.CompletedProcess([], 0, "a/b.md:1 hit\n", "")
            with unittest.mock.patch.object(
                retrieve.subprocess, "run", side_effect=[failed, healthy]
            ) as run:
                first = retrieve.retrieve("history", store="mempalace", project=project)
                cached = retrieve.retrieve("history", store="mempalace", project=project)
                rechecked = retrieve.retrieve(
                    "history", store="mempalace", project=project, recheck=True
                )
        self.assertEqual("backend-mismatch", first["reason"])
        self.assertFalse(cached["scheduled"])
        self.assertIn("--recheck", cached["fixNext"])
        self.assertEqual("used", rechecked["status"])
        self.assertEqual(2, run.call_count)


class DoctorBackendSelectionTest(unittest.TestCase):
    def test_doctor_flags_ambient_non_sqlite_exact_selection(self):
        result = {"status": "healthy", "components": {}}
        with tempfile.TemporaryDirectory() as temporary:
            overlay_match.apply_mempalace_backend_doctor(
                result, Path(temporary), environ={"MEMPALACE_BACKEND": "chroma"}
            )
        row = result["components"]["mempalace-backend"]
        self.assertEqual("backend-mismatch", row["reason"])
        self.assertEqual("chroma", row["selected"])
        self.assertEqual("sqlite_exact", row["expected"])
        self.assertIn("unset MEMPALACE_BACKEND", row["fixNext"])
        self.assertEqual("healthy", result["status"], "ambient env alone is advisory")

    def test_doctor_blocks_a_host_config_that_selects_another_backend(self):
        result = {"status": "healthy", "components": {}}
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".mcp.json").write_text(
                '{"mcpServers": {"chaosengine-mempalace": {"command": "x",'
                ' "env": {"MEMPALACE_BACKEND": "chroma"}}}}',
                encoding="utf-8",
            )
            overlay_match.apply_mempalace_backend_doctor(result, project, environ={})
        row = result["components"]["mempalace-backend"]
        self.assertEqual("recovery-required", row["status"])
        self.assertIn(".mcp.json", row["source"])
        self.assertIn("--component hosts", row["fixNext"])
        self.assertEqual("recovery-required", result["status"])

    def test_consistent_selection_adds_no_row(self):
        result = {"status": "healthy", "components": {}}
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".mcp.json").write_text(
                '{"mcpServers": {"chaosengine-mempalace": {"command": "x",'
                ' "env": {"MEMPALACE_BACKEND": "sqlite_exact"}}}}',
                encoding="utf-8",
            )
            overlay_match.apply_mempalace_backend_doctor(
                result, project, environ={"MEMPALACE_BACKEND": "sqlite_exact"}
            )
        self.assertNotIn("mempalace-backend", result["components"])
        self.assertEqual("healthy", result["status"])

    def test_install_doctor_wires_the_backend_check(self):
        install = (ROOT / "chaos-engine/install.py").read_text(encoding="utf-8")
        self.assertIn("apply_mempalace_backend_doctor", install)


if __name__ == "__main__":
    unittest.main()
