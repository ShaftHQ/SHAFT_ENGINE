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

    def test_list_covers_installer_artifacts_mempalace_cannot_see_as_ignored(self):
        # Consumer installs hide these via .git/info/exclude, which MemPalace's
        # .gitignore-only matcher does not read (#7246 measured 5,464 backup and
        # 4,117 host-receipt drawers in one consumer palace).
        owned = self.dependencies.MEMPALACE_OWNED_EXCLUDES
        for pattern in (".chaos-engine.backup/**", ".chaos-engine.backup.*/**",
                        ".chaos-engine-*.json", ".chaos-engine.*"):
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

    def test_existing_exclude_key_merges_missing_owned_keeps_user(self):
        """#7465: partial exclude_patterns keeps user entries and gains owned ones."""
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            config = INIT_GENERATED + "exclude_patterns: [docs/**]\n"
            (project / "mempalace.yaml").write_text(config, encoding="utf-8")
            self.assertTrue(self.dependencies.apply_owned_mempalace_excludes(project))
            updated = (project / "mempalace.yaml").read_text(encoding="utf-8")
            self.assertIn("docs/**", updated)
            self.assertEqual([], self.dependencies.missing_owned_mempalace_excludes(project))
            self.assertFalse(self.dependencies.apply_owned_mempalace_excludes(project))

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

    def test_user_config_merges_owned_excludes_on_mine(self):
        """#7465: mine merges owned excludes into a user/consumer mempalace.yaml."""
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "mempalace.yaml").write_text(INIT_GENERATED, encoding="utf-8")
            mine = self.dependencies.mempalace_project_cli("/tools/mempalace", "mine", project)
            with mock.patch.dict("os.environ", {"CHAOS_ENGINE_MEMPALACE_MINE": "foreground"}):
                stderr = self._install(project, mine)
            self.assertEqual([], self.dependencies.missing_owned_mempalace_excludes(project))
            self.assertIn("merged missing MemPalace owned exclude_patterns", stderr)

    def test_block_style_partial_excludes_merge_preserves_placeholder(self):
        """Placeholders only: user private path stays; owned gaps fill (#7465)."""
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            config = (
                INIT_GENERATED
                + "exclude_patterns:\n"
                + "  - 'user-private-placeholder/**'\n"
                + "  - mempalace.yaml\n"
            )
            (project / "mempalace.yaml").write_text(config, encoding="utf-8")
            self.assertTrue(self.dependencies.apply_owned_mempalace_excludes(project))
            updated = (project / "mempalace.yaml").read_text(encoding="utf-8")
            self.assertIn("user-private-placeholder/**", updated)
            self.assertEqual([], self.dependencies.missing_owned_mempalace_excludes(project))

    def test_hosts_install_plan_merges_owned_into_existing_config(self):
        hosts = load("hosts")
        before = (INIT_GENERATED + "exclude_patterns:\n  - 'user-kept-placeholder/**'\n").encode()
        after = hosts.merge_owned_mempalace_excludes_bytes(before)
        text = after.decode()
        self.assertIn("user-kept-placeholder/**", text)
        for pattern in hosts.MEMPALACE_OWNED_EXCLUDES:
            self.assertIn(pattern, text)

    def test_doctor_flags_missing_owned_excludes(self):
        overlay = load("overlay_match")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "mempalace.yaml").write_text(
                INIT_GENERATED + "exclude_patterns:\n  - 'user-only-placeholder/**'\n",
                encoding="utf-8",
            )
            result = {"status": "healthy", "components": {}}
            overlay.apply_mempalace_owned_excludes_doctor(result, project)
            row = result["components"]["mempalace-owned-excludes"]
            self.assertEqual("degraded", row["status"])
            self.assertEqual("owned-excludes-missing", row["reason"])
            self.assertGreater(row["missingCount"], 0)


    def test_unquoted_owned_lines_are_not_duplicated(self):
        """#7465 coach: unquoted owned entries must count as present (no quoted dupes)."""
        hosts = load("hosts")
        # Unquoted forms of two owned patterns plus a user placeholder.
        before = (
            INIT_GENERATED
            + "exclude_patterns:\n"
            + "  - mempalace.yaml\n"
            + "  - .memory/**\n"
            + "  - user-private-placeholder/**\n"
        ).encode()
        after = hosts.merge_owned_mempalace_excludes_bytes(before)
        text = after.decode()
        self.assertEqual(1, text.count("mempalace.yaml"))
        self.assertEqual(1, text.count(".memory/**"))
        self.assertIn("user-private-placeholder/**", text)
        # Every owned pattern present exactly once as a list item.
        for pattern in hosts.MEMPALACE_OWNED_EXCLUDES:
            matches = [
                line for line in text.splitlines()
                if line.lstrip().startswith("-") and pattern in line
            ]
            self.assertEqual(1, len(matches), pattern)

    def test_removed_owned_exclude_is_re_added_by_design(self):
        """#7465: a user who deletes an owned pattern gets it restored on merge."""
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            # Full owned list minus one harness path.
            owned = list(self.dependencies.MEMPALACE_OWNED_EXCLUDES)
            kept = [p for p in owned if p != ".chaos-engine/**"]
            body = INIT_GENERATED + "exclude_patterns:\n" + "".join(
                f"  - '{p}'\n" for p in kept
            )
            (project / "mempalace.yaml").write_text(body, encoding="utf-8")
            self.assertIn(".chaos-engine/**", self.dependencies.missing_owned_mempalace_excludes(project))
            self.assertTrue(self.dependencies.apply_owned_mempalace_excludes(project))
            self.assertEqual([], self.dependencies.missing_owned_mempalace_excludes(project))
            self.assertIn(".chaos-engine/**", (project / "mempalace.yaml").read_text(encoding="utf-8"))


    def test_mine_decision_merges_owned_excludes_before_mine_7436(self):
        """#7436: user-owned yaml still gets harness excludes before mine."""
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "mempalace.yaml").write_text(
                INIT_GENERATED + "exclude_patterns:\n  - 'custom-only/**'\n",
                encoding="utf-8",
            )
            mode, _reason = self.dependencies.mempalace_mine_decision(
                project, command=["mempalace", "mine", "."]
            )
            text = (project / "mempalace.yaml").read_text(encoding="utf-8")
            self.assertIn(".chaos-engine/**", text)
            self.assertIn("custom-only/**", text)
            self.assertIn(mode, {"full", "incremental"})


if __name__ == "__main__":
    unittest.main()
