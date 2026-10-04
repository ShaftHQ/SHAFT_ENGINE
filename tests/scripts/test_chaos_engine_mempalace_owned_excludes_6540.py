"""#6540: the project palace never mines installed harness copies."""
from __future__ import annotations

import importlib.util
import tempfile
import contextlib
import io
import json
import unittest
import unittest.mock as mock
from pathlib import Path
from types import SimpleNamespace

ROOT = Path(__file__).resolve().parents[2]


def load(name: str):
    spec = importlib.util.spec_from_file_location(f"ce_{name}_6540", ROOT / "chaos-engine" / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


INIT_GENERATED = (
    "wing: proj\nrooms:\n- name: general\n  description: Files that don't fit other rooms\n"
    "  keywords: []\n"
)


class OwnedExcludesTest(unittest.TestCase):
    def setUp(self):
        self.dependencies = load("dependencies")

    def test_hosts_default_mirrors_dependency_list(self):
        self.assertEqual(self.dependencies.MEMPALACE_OWNED_EXCLUDES, load("hosts").MEMPALACE_OWNED_EXCLUDES)

    def test_list_covers_harness_copies(self):
        owned = self.dependencies.MEMPALACE_OWNED_EXCLUDES
        for pattern in (".chaos-engine/**", "plugins/chaos-engine/**", "**/skills/chaos-engine/**"):
            self.assertIn(pattern, owned)

    def test_installer_created_config_gets_valid_owned_excludes(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "mempalace.yaml").write_text(INIT_GENERATED, encoding="utf-8")
            self.assertTrue(self.dependencies.apply_owned_mempalace_excludes(project))
            content = (project / "mempalace.yaml").read_bytes()
            load("hosts").validate_mempalace_config(content)
            self.assertEqual([], self.dependencies.missing_owned_mempalace_excludes(project))
            self.assertFalse(self.dependencies.apply_owned_mempalace_excludes(project))
            self.assertEqual(content, (project / "mempalace.yaml").read_bytes())

    def test_existing_exclude_key_is_never_rewritten(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            config = INIT_GENERATED + "exclude_patterns: [docs/**]\n"
            (project / "mempalace.yaml").write_text(config, encoding="utf-8")
            self.assertFalse(self.dependencies.apply_owned_mempalace_excludes(project))
            self.assertEqual(config, (project / "mempalace.yaml").read_text(encoding="utf-8"))
            self.assertIn(".chaos-engine/**", self.dependencies.missing_owned_mempalace_excludes(project))

    def _install(self, project: Path, command: list[str], on_run=None) -> str:
        module = self.dependencies
        specification = json.loads((ROOT / "chaos-engine/dependencies.json").read_text(encoding="utf-8"))
        commands = {"mempalace": "/tools/mempalace", "uv": "/tools/uv", "npm": "/tools/npm"}
        local = {
            name: {"healthy": True, "version": "1.0", "detail": "passed"}
            for name in ("uv", "python", "node", "java", "mempalace", "graphify", "memory", "context7")
        }

        def run(argv, **kwargs):
            if on_run:
                on_run(argv)
            return SimpleNamespace(returncode=0, stdout="", stderr="")

        stderr = io.StringIO()
        with mock.patch.object(
            module, "discover_account_commands", side_effect=((local, commands), (local, commands))
        ), mock.patch.object(
            module, "resolve_account_actions", return_value={n: {"action": "reused"} for n in local}
        ), mock.patch.object(
            module, "project_setup_plan", return_value=[command]
        ), mock.patch.object(
            module, "_run_transient_mempalace_mine", side_effect=lambda c, p, **k: run(c)
        ), contextlib.redirect_stderr(stderr):
            module.install_account_dependencies(project, specification, runner=run, allow_root=True)
        return stderr.getvalue()

    def test_init_created_config_is_given_owned_excludes(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            init = self.dependencies.mempalace_project_cli("/tools/mempalace", "init", project)
            self._install(
                project, init,
                on_run=lambda argv: (project / "mempalace.yaml").write_text(INIT_GENERATED, encoding="utf-8"),
            )
            self.assertEqual([], self.dependencies.missing_owned_mempalace_excludes(project))

    def test_user_config_stays_byte_identical_with_advisory(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "mempalace.yaml").write_text(INIT_GENERATED, encoding="utf-8")
            mine = self.dependencies.mempalace_project_cli("/tools/mempalace", "mine", project)
            with mock.patch.dict("os.environ", {"CHAOS_ENGINE_MEMPALACE_MINE": "foreground"}):
                stderr = self._install(project, mine)
            self.assertEqual(INIT_GENERATED, (project / "mempalace.yaml").read_text(encoding="utf-8"))
            self.assertIn(".chaos-engine/**", stderr)


if __name__ == "__main__":
    unittest.main()
