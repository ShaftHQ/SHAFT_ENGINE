"""Coverage and orchestration tests for repository-owned Graphify maintenance."""

import importlib.util
import json
import shutil
import subprocess  # nosec B404 - tests run fixed repository-owned commands.
import sys
import tempfile
from pathlib import Path
from unittest import TestCase, main, mock


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "tools/repository-map/graphify_maintenance.py"


class GraphifyMaintenanceTest(TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.repository = Path(self.temporary.name) / "portable-repository"
        self.graph_out = self.repository / "cache" / "map"
        self.graph_out.mkdir(parents=True)

    def write_cache(self, paths, covered=()):
        manifest = {path: {"ast_hash": "", "semantic_hash": ""} for path in paths}
        graph = {
            "nodes": [
                {"id": f"node-{index}", "source_file": source}
                for index, source in enumerate(covered)
            ],
            "links": [],
        }
        (self.graph_out / "manifest.json").write_text(
            json.dumps(manifest), encoding="utf-8"
        )
        (self.graph_out / "graph.json").write_text(
            json.dumps(graph), encoding="utf-8"
        )

    def command(self, *args, cwd=None):
        return subprocess.run(  # nosec B603 - current interpreter and repository-owned script.
            [sys.executable, str(SCRIPT), *args],
            cwd=cwd or self.repository.parent,
            check=False,
            capture_output=True,
            text=True,
        )

    def load_module(self):
        spec = importlib.util.spec_from_file_location("graphify_maintenance", SCRIPT)
        self.assertIsNotNone(spec)
        self.assertIsNotNone(spec.loader)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module

    def write_default_marker(self):
        output = self.repository / "graphify-out"
        output.mkdir(exist_ok=True)
        marker = output / ".shaft-source-revision.json"
        marker.write_text("{}", encoding="utf-8")
        return marker

    def test_audit_classifies_every_manifest_path_and_fails_on_parser_gaps(self):
        paths = ["src/covered.py", "data/config.json", "db/schema.sql", "src/gap.java"]
        self.write_cache(paths, covered=("src\\covered.py",))

        completed = self.command(
            "audit", "--root", str(self.repository), "--graph-out", "cache/map"
        )

        self.assertEqual(1, completed.returncode)
        report = json.loads(completed.stdout)
        self.assertEqual(["src/covered.py"], report["covered"])
        self.assertEqual(["data/config.json"], report["expected_data_only"])
        self.assertEqual(["db/schema.sql"], report["missing_optional_parser"])
        self.assertEqual(["src/gap.java"], report["unexpected_parser_gap"])
        self.assertEqual(4, report["total_manifest_paths"])

    def test_88_json_data_files_are_visible_but_nonfatal(self):
        paths = [f"fixtures/data-{index:02}.json" for index in range(88)]
        self.write_cache(paths)

        completed = self.command(
            "audit", "--root", str(self.repository), "--graph-out", "cache/map"
        )

        self.assertEqual(0, completed.returncode, completed.stderr)
        report = json.loads(completed.stdout)
        self.assertEqual(paths, report["expected_data_only"])
        self.assertEqual([], report["missing_optional_parser"])
        self.assertEqual([], report["unexpected_parser_gap"])

    def test_non_json_sibling_of_expected_data_is_actionable(self):
        self.write_cache(["fixtures/data.json", "fixtures/data.yaml"])

        completed = self.command(
            "audit", "--root", str(self.repository), "--graph-out", "cache/map"
        )

        self.assertEqual(1, completed.returncode)
        report = json.loads(completed.stdout)
        self.assertEqual(["fixtures/data.json"], report["expected_data_only"])
        self.assertEqual(["fixtures/data.yaml"], report["unexpected_parser_gap"])

    def test_normalization_preserves_leading_dot_directories(self):
        self.write_cache(["./.github/config.json"], covered=(".github\\config.json",))

        completed = self.command(
            "audit", "--root", str(self.repository), "--graph-out", "cache/map"
        )

        self.assertEqual(0, completed.returncode, completed.stderr)
        self.assertEqual([".github/config.json"], json.loads(completed.stdout)["covered"])

    def test_refresh_runs_ephemeral_sql_build_audit_cluster_then_marker(self):
        source = SCRIPT.read_text(encoding="utf-8")
        refresh_source = source[source.index("def refresh(") : source.index("def parser(")]

        self.assertIn('"--with",\n        "tree-sitter-sql"', source)
        self.assertIn('"--from",\n        "graphifyy",\n        "graphify"', source)
        self.assertLess(refresh_source.index('"build",'), refresh_source.index('run_audit('))
        self.assertLess(refresh_source.index('run_audit('), refresh_source.index('run_stage("cluster"'))
        self.assertLess(refresh_source.index('run_stage("cluster"'), refresh_source.index('"record",'))

    def test_build_failure_stops_before_audit_cluster_and_marker(self):
        marker = self.write_default_marker()
        module = self.load_module()
        stages = []

        def fail_build(name, command, root):
            stages.append(name)
            raise RuntimeError("build failed")

        with mock.patch.object(
            module, "require_primary_checkout", return_value=self.repository
        ), mock.patch.object(
            module.shutil, "which", return_value="uv"
        ), mock.patch.object(module, "run_stage", side_effect=fail_build):
            with self.assertRaisesRegex(RuntimeError, "build failed"):
                module.refresh(self.repository, Path("graphify-out"))

        self.assertEqual(["build"], stages)
        self.assertFalse(marker.exists())
        with module.refresh_lock(self.repository):
            pass

    def test_refresh_lock_contention_stops_before_marker_or_stages(self):
        module = self.load_module()
        marker = self.write_default_marker()
        lock = self.repository / "shaft-graphify-refresh.lock"
        stages = []

        with module.refresh_lock(self.repository):
            with mock.patch.object(
                module, "require_primary_checkout", return_value=self.repository
            ), mock.patch.object(module.shutil, "which", return_value="uv"), mock.patch.object(
                module, "run_stage", side_effect=lambda name, command, root: stages.append(name)
            ):
                with self.assertRaisesRegex(RuntimeError, "refresh is already running"):
                    module.refresh(self.repository, Path("graphify-out"))

        self.assertEqual([], stages)
        self.assertTrue(marker.exists())
        self.assertTrue(lock.exists())

    def test_refresh_rejects_a_custom_output_before_any_stage(self):
        module = self.load_module()
        stages = []

        with mock.patch.object(module.shutil, "which", return_value="uv"), mock.patch.object(
            module, "run_stage", side_effect=lambda name, command, root: stages.append(name)
        ):
            with self.assertRaisesRegex(ValueError, "fixed graphify-out"):
                module.refresh(self.repository, Path("cache/map"))

        self.assertEqual([], stages)

    def test_refresh_rejects_a_non_git_root_before_any_stage(self):
        module = self.load_module()
        stages = []

        with mock.patch.object(
            module, "run_stage", side_effect=lambda name, command, root: stages.append(name)
        ):
            with self.assertRaisesRegex(ValueError, "primary Git checkout"):
                module.refresh(self.repository, Path("graphify-out"))

        self.assertEqual([], stages)

    def test_git_executable_disappearing_is_a_fail_closed_preflight_error(self):
        module = self.load_module()
        missing_git = self.repository / "missing-git.exe"

        with mock.patch.object(module.shutil, "which", return_value=str(missing_git)):
            try:
                module.require_primary_checkout(self.repository)
            except ValueError as error:
                raised = error
            except OSError as error:
                self.fail(f"preflight leaked platform OSError: {error}")
            else:
                self.fail("missing Git executable was accepted")

        self.assertRegex(str(raised), "primary Git checkout")
        self.assertIsInstance(raised.__cause__, OSError)

    def test_refresh_preflight_rejects_a_linked_worktree(self):
        git = shutil.which("git")
        self.assertIsNotNone(git)
        primary = Path(self.temporary.name) / "primary"
        linked = Path(self.temporary.name) / "linked"
        primary.mkdir()

        def run_git(*args, cwd):
            return subprocess.run(  # nosec B603 - resolved Git and controlled fixture paths.
                [git, *args], cwd=cwd, check=True, capture_output=True, text=True
            )

        run_git("init", cwd=primary)
        run_git("config", "user.email", "graphify@example.invalid", cwd=primary)
        run_git("config", "user.name", "Graphify Test", cwd=primary)
        (primary / "tracked.txt").write_text("tracked\n", encoding="utf-8")
        run_git("add", "tracked.txt", cwd=primary)
        run_git("commit", "-m", "fixture", cwd=primary)
        run_git("worktree", "add", "-b", "linked-test", str(linked), cwd=primary)
        module = self.load_module()

        module.require_primary_checkout(primary)
        with self.assertRaisesRegex(ValueError, "linked worktree"):
            module.require_primary_checkout(linked)

    def test_audit_failure_stops_before_cluster_and_marker(self):
        module = self.load_module()
        stages = []
        marker = self.write_default_marker()

        with mock.patch.object(
            module, "require_primary_checkout", return_value=self.repository
        ), mock.patch.object(
            module.shutil, "which", return_value="uv"
        ), mock.patch.object(
            module, "run_stage", side_effect=lambda name, command, root: stages.append(name)
        ), mock.patch.object(module, "run_audit", side_effect=RuntimeError("audit failed")):
            with self.assertRaisesRegex(RuntimeError, "audit failed"):
                module.refresh(self.repository, Path("graphify-out"))

        self.assertEqual(["build"], stages)
        self.assertFalse(marker.exists())

    def test_cluster_failure_stops_before_marker(self):
        module = self.load_module()
        stages = []
        marker = self.write_default_marker()

        def fail_cluster(name, command, root):
            stages.append(name)
            if name == "cluster":
                raise RuntimeError("cluster failed")

        with mock.patch.object(
            module, "require_primary_checkout", return_value=self.repository
        ), mock.patch.object(
            module.shutil, "which", return_value="uv"
        ), mock.patch.object(
            module, "run_stage", side_effect=fail_cluster
        ), mock.patch.object(module, "run_audit", return_value={}):
            with self.assertRaisesRegex(RuntimeError, "cluster failed"):
                module.refresh(self.repository, Path("graphify-out"))

        self.assertEqual(["build", "cluster"], stages)
        self.assertFalse(marker.exists())

    def test_success_records_only_after_build_audit_and_cluster(self):
        module = self.load_module()
        events = []
        commands = []

        def record_stage(name, command, root):
            events.append(name)
            commands.append(command)

        def record_audit(root, graph_out):
            events.append("audit")
            return {}

        with mock.patch.object(
            module, "require_primary_checkout", return_value=self.repository
        ), mock.patch.object(
            module.shutil, "which", return_value="uv"
        ), mock.patch.object(
            module, "run_stage", side_effect=record_stage
        ), mock.patch.object(module, "run_audit", side_effect=record_audit):
            module.refresh(self.repository, Path("graphify-out"))

        self.assertEqual(["build", "audit", "cluster", "record"], events)
        self.assertEqual(
            [
                "uv",
                "tool",
                "run",
                "--with",
                "tree-sitter-sql",
                "--from",
                "graphifyy",
                "graphify",
                "extract",
                ".",
                "--code-only",
                "--no-cluster",
            ],
            commands[0],
        )

    def test_wrapper_is_thin_and_has_no_checkout_specific_path(self):
        wrapper = (ROOT / "tools/agent-infra/graphify-refresh.cmd").read_text(
            encoding="utf-8"
        )

        self.assertIn("tools\\repository-map\\graphify_maintenance.py refresh", wrapper)
        self.assertNotIn("call graphify", wrapper.lower())
        self.assertNotIn("shafthq.github.io", wrapper.lower())
        self.assertNotIn("C:\\Users\\", wrapper)


if __name__ == "__main__":
    main()
