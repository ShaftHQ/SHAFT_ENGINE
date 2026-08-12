"""Portable ChaosEngine host adapter tests (#4795)."""

from __future__ import annotations

import importlib.util
import errno
import json
import os
import tempfile
import unittest
import unittest.mock as mock
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
HOSTS = ROOT / "chaos-engine/hosts.py"
TOOL = ROOT / "chaos-engine/tool.py"


def load(path: Path, name: str):
    specification = importlib.util.spec_from_file_location(name, path)
    if specification is None or specification.loader is None:
        raise RuntimeError("host controller test module could not be loaded")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


class ChaosEngineHostsTest(unittest.TestCase):
    def test_validate_path_rejects_a_path_outside_the_project(self):
        module = load(HOSTS, "chaos_engine_hosts_outside_path")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            with self.assertRaisesRegex(ValueError, "escapes the project"):
                module.validate_path(project, root / "outside.txt")

    def test_five_host_adapters_route_to_one_installed_skill(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            core = project / ".chaos-engine"
            core.joinpath("skills/chaos-engine").mkdir(parents=True)
            core.joinpath("skills/chaos-engine/SKILL.md").write_text("# ChaosEngine\n")

            receipt = module.install(project)

            expected = {
                "codex": project / ".agents/skills/chaos-engine/SKILL.md",
                "claude": project / ".claude/skills/chaos-engine/SKILL.md",
                "grok": project / "AGENTS.md",
                "gemini": project / ".gemini/skills/chaos-engine/SKILL.md",
                "copilot": project / ".github/skills/chaos-engine/SKILL.md",
            }
            self.assertEqual(set(expected), set(receipt["hosts"]))
            for path in expected.values():
                self.assertIn(".chaos-engine", path.read_text(encoding="utf-8"))

    def test_host_configs_use_project_relative_local_runtime_launchers(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            module.install(project)

            claude = json.loads(project.joinpath(".mcp.json").read_text(encoding="utf-8"))
            gemini = json.loads(
                project.joinpath(".gemini/settings.json").read_text(encoding="utf-8")
            )
            codex = project.joinpath(".codex/config.toml").read_text(encoding="utf-8")

            for config in (claude, gemini):
                servers = config["mcpServers"]
                self.assertEqual(
                    {"chaosengine-memory", "chaosengine-mempalace"}, set(servers)
                )
                for server in servers.values():
                    self.assertEqual("py" if os.name == "nt" else "python3", server["command"])
                    if os.name == "nt":
                        self.assertEqual("-3", server["args"][0])
                    self.assertIn(".chaos-engine/tool.py", server["args"])
            self.assertIn('[mcp_servers."chaosengine-memory"]', codex)
            self.assertIn('".chaos-engine/tool.py", "memory-mcp"]', codex)

    def test_launcher_rendering_is_explicit_for_windows_and_posix(self):
        module = load(HOSTS, "chaos_engine_hosts")

        windows = module.owned_servers("nt")
        posix = module.owned_servers("posix")

        for server in windows.values():
            self.assertEqual("py", server["command"])
            self.assertEqual("-3", server["args"][0])
        for server in posix.values():
            self.assertEqual("python3", server["command"])
            self.assertNotEqual("-3", server["args"][0])

    def test_existing_unrelated_config_is_preserved_and_owned_collision_fails_closed(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            project.joinpath(".mcp.json").write_text(
                json.dumps({"mcpServers": {"other": {"command": "other"}}}),
                encoding="utf-8",
            )
            module.install(project)
            merged = json.loads(project.joinpath(".mcp.json").read_text(encoding="utf-8"))
            self.assertEqual("other", merged["mcpServers"]["other"]["command"])

            before = project.joinpath(".mcp.json").read_bytes()
            merged["mcpServers"]["chaosengine-memory"] = {"command": "user-owned"}
            project.joinpath(".mcp.json").write_text(json.dumps(merged), encoding="utf-8")
            collision = project.joinpath(".mcp.json").read_bytes()
            with self.assertRaisesRegex(ValueError, "drift"):
                module.install(project)
            self.assertEqual(collision, project.joinpath(".mcp.json").read_bytes())
            self.assertNotEqual(before, collision)

    def test_late_host_collision_leaves_the_project_unchanged(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            project.joinpath(".codex").mkdir()
            project.joinpath(".codex/config.toml").write_text(
                '[mcp_servers."chaosengine-memory"]\ncommand = "mine"\n',
                encoding="utf-8",
            )
            before = {
                path.relative_to(project).as_posix(): path.read_bytes()
                for path in project.rglob("*")
                if path.is_file()
            }

            with self.assertRaisesRegex(ValueError, "collision"):
                module.install(project)

            after = {
                path.relative_to(project).as_posix(): path.read_bytes()
                for path in project.rglob("*")
                if path.is_file()
            }
            self.assertEqual(before, after)

    def test_failed_install_never_deletes_a_concurrent_adapter_replacement(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            first = project / ".agents/skills/chaos-engine/SKILL.md"
            second = project / ".claude/skills/chaos-engine/SKILL.md"
            original = module.atomic_write

            def interleave(root, path, content, expected):
                if path == second:
                    raise RuntimeError("later write failed")
                original(root, path, content, expected)
                if path == first:
                    path.write_text("concurrent user content\n", encoding="utf-8")

            with mock.patch.object(module, "atomic_write", side_effect=interleave):
                with self.assertRaises(Exception):
                    module.install(project)

            self.assertEqual("concurrent user content\n", first.read_text(encoding="utf-8"))

    def test_atomic_write_does_not_overwrite_a_last_moment_replacement(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            module.host_anchor(project, create=True)
            target = project / "AGENTS.md"
            target.write_bytes(b"before\n")
            original = os.link

            def interleave(source, destination, *args, **kwargs):
                if Path(destination) == target:
                    target.write_bytes(b"concurrent\n")
                return original(source, destination, *args, **kwargs)

            with mock.patch.object(os, "link", interleave):
                with self.assertRaises(Exception):
                    module.atomic_write(project, target, b"owned\n", b"before\n")

            self.assertEqual(b"concurrent\n", target.read_bytes())

    def test_prepare_uninstall_does_not_delete_a_last_moment_replacement(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            module.install(project)
            target = project / ".agents/skills/chaos-engine/SKILL.md"
            original = Path.replace

            def interleave(path, destination, *args, **kwargs):
                if path == target:
                    target.write_bytes(b"concurrent\n")
                return original(path, destination, *args, **kwargs)

            with mock.patch.object(Path, "replace", interleave):
                with self.assertRaises(Exception):
                    module.prepare_uninstall(project)

            self.assertEqual(b"concurrent\n", target.read_bytes())

    def test_unsupported_hard_links_fail_before_user_files_move(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            target = project / "AGENTS.md"
            target.write_bytes(b"user\n")

            with mock.patch.object(os, "link", side_effect=OSError(errno.ENOTSUP, "unsupported")):
                with self.assertRaisesRegex(ValueError, "hard links"):
                    module.install(project)

            self.assertEqual(b"user\n", target.read_bytes())

    def test_write_and_remove_scratch_states_are_recovered_on_retry(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            module.host_anchor(project, create=True)
            target = project / "AGENTS.md"
            target.write_bytes(b"before\n")
            old = target.with_name(f".{target.name}.chaos-engine-old")
            write_payload = (
                b"AGENTS.md\0"
                + module.sha256_bytes(b"before\n").encode()
                + b"\0"
                + module.sha256_bytes(b"after\n").encode()
            )
            write_claim = module.ensure_claim(
                project,
                target.with_name(f".{target.name}.chaos-engine-write-claim"),
                "host-write",
                write_payload,
            )
            self.assertEqual(b"", write_claim.read_bytes())
            self.assertFalse(
                target.with_name(f".{target.name}.chaos-engine-write-claim").exists()
            )
            target.replace(old)

            module.atomic_write(project, target, b"after\n", b"before\n")
            self.assertEqual(b"after\n", target.read_bytes())
            self.assertFalse(old.exists())

            removed = target.with_name(f".{target.name}.chaos-engine-removed")
            remove_payload = (
                b"AGENTS.md\0" + module.sha256_bytes(b"after\n").encode()
            )
            remove_claim = module.ensure_claim(
                project,
                target.with_name(f".{target.name}.chaos-engine-remove-claim"),
                "host-remove",
                remove_payload,
            )
            self.assertEqual(b"", remove_claim.read_bytes())
            self.assertFalse(
                target.with_name(f".{target.name}.chaos-engine-remove-claim").exists()
            )
            target.replace(removed)
            module.atomic_remove(project, target, b"after\n")
            self.assertFalse(target.exists())
            self.assertFalse(removed.exists())

    def test_completed_write_recovery_clears_claim_for_the_next_generation(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            module.host_anchor(project, create=True)
            target = project / "AGENTS.md"
            target.write_bytes(b"new\n")
            old = target.with_name(f".{target.name}.chaos-engine-old")
            old.write_bytes(b"old\n")
            payload = (
                b"AGENTS.md\0"
                + module.sha256_bytes(b"old\n").encode()
                + b"\0"
                + module.sha256_bytes(b"new\n").encode()
            )
            claim = module.ensure_claim(
                project,
                target.with_name(f".{target.name}.chaos-engine-write-claim"),
                "host-write",
                payload,
            )

            module.atomic_write(project, target, b"new\n", b"old\n")
            self.assertFalse(old.exists())
            self.assertFalse(claim.exists())

            module.atomic_write(project, target, b"later\n", b"new\n")
            self.assertEqual(b"later\n", target.read_bytes())

    def test_foreign_prefix_publication_scratch_is_never_claimed_or_deleted(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            module.host_anchor(project, create=True)
            target = project / "AGENTS.md"
            target.write_bytes(b"before\n")
            scratch = target.with_name(f".{target.name}.chaos-engine-new")
            scratch.write_bytes(b"a")

            with self.assertRaisesRegex(ValueError, "publication scratch collision"):
                module.atomic_write(project, target, b"after\n", b"before\n")

            self.assertEqual(b"before\n", target.read_bytes())
            self.assertEqual(b"a", scratch.read_bytes())
            self.assertFalse(
                target.with_name(f".{target.name}.chaos-engine-write-claim").exists()
            )

    def test_partial_fixed_transaction_claim_never_bricks_install_retry(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            project.joinpath(".chaos-engine/manifest.json").write_text(
                json.dumps({"hostToken": "b" * 64}), encoding="utf-8"
            )
            partial = project / f".{module.RECEIPT_NAME}.chaos-engine-write-claim"
            partial.write_bytes(b"partial")

            with self.assertRaisesRegex(ValueError, "claim collision"):
                module.install(project)
            self.assertEqual(b"partial", partial.read_bytes())

            partial.unlink()
            module.install(project)
            self.assertEqual("healthy", module.verify(project)["status"])

    def test_preexisting_legacy_anchor_is_preserved_and_rejected(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            anchor = project / module.ANCHOR_NAME
            anchor.write_bytes(b"u" * 32)

            with self.assertRaisesRegex(ValueError, "anchor collision"):
                module.install(project)

            self.assertEqual(b"u" * 32, anchor.read_bytes())
            self.assertFalse(project.joinpath(module.RECEIPT_NAME).exists())

    def test_preexisting_patterned_anchor_is_preserved_and_rejected(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            anchor = project / f"{module.ACTIVE_ANCHOR_PREFIX}{'a' * 64}"
            anchor.write_bytes(b"")

            with self.assertRaisesRegex(ValueError, "anchor collision"):
                module.install(project)

            self.assertTrue(anchor.exists())
            self.assertFalse(project.joinpath(module.RECEIPT_NAME).exists())

    def test_anchor_creation_is_complete_and_recoverable_from_its_name(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            project.joinpath(".chaos-engine/manifest.json").write_text(
                json.dumps({"hostToken": "b" * 64}), encoding="utf-8"
            )
            anchor = module.host_anchor_path(project, create=True)

            self.assertEqual(b"", anchor.read_bytes())
            self.assertEqual(32, len(module.host_anchor(project)))
            module.install(project)
            self.assertEqual("healthy", module.verify(project)["status"])

    def test_uninstall_retries_after_receipt_was_deleted_before_anchor_cleanup(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            module.install(project)
            module.prepare_uninstall(project)
            original = Path.unlink

            def interrupt(path, *args, **kwargs):
                if path.name.startswith(module.REMOVING_ANCHOR_PREFIX):
                    raise OSError("anchor cleanup failed")
                return original(path, *args, **kwargs)

            with mock.patch.object(Path, "unlink", interrupt):
                with self.assertRaisesRegex(OSError, "anchor cleanup failed"):
                    module.finalize_uninstall(project)

            self.assertFalse(project.joinpath(module.RECEIPT_NAME).exists())
            module.finalize_uninstall(project)
            self.assertFalse(module.host_anchor_paths(project))

    def test_receipt_cannot_claim_an_unrelated_empty_directory(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            module.install(project)
            victim = project / "valuable-empty"
            victim.mkdir()
            receipt_path = project / module.RECEIPT_NAME
            receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
            receipt["createdDirectories"].append("valuable-empty")
            receipt.pop("integritySha256")
            receipt_path.write_bytes(module.receipt_bytes(receipt))

            with self.assertRaisesRegex(ValueError, "integrity drift"):
                module.prepare_uninstall(project)

            self.assertTrue(victim.is_dir())

    def test_installing_receipt_retry_finishes_every_directory_marker(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            original = module.prepare_created_directories

            def interrupt(root, receipt):
                first = receipt["createdDirectories"][0]
                partial = dict(receipt)
                partial["createdDirectories"] = [first]
                original(root, partial)
                raise SystemExit("crash")

            with mock.patch.object(module, "prepare_created_directories", side_effect=interrupt):
                with self.assertRaises(SystemExit):
                    module.install(project)

            module.install(project)

            self.assertEqual("healthy", module.verify(project)["status"])

    def test_concurrent_empty_directory_is_never_claimed(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            original = Path.mkdir
            raced = project / ".agents"

            def interleave(path, *args, **kwargs):
                if path == raced and not path.exists():
                    original(path)
                return original(path, *args, **kwargs)

            with mock.patch.object(Path, "mkdir", interleave):
                with self.assertRaisesRegex(ValueError, "directory claim collision"):
                    module.install(project)

            self.assertTrue(raced.is_dir())

    def test_directory_removal_retries_after_marker_was_deleted(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            module.install(project)
            receipt = module.prepare_uninstall(project)
            owned = project / receipt["createdDirectories"][-1]
            marker = owned / module.DIRECTORY_MARKER
            module.write_directory_claim(
                project, receipt, receipt["createdDirectories"][-1]
            )
            marker.unlink()

            module.finalize_uninstall(project)

            self.assertFalse(owned.exists())

    def test_linked_host_config_is_rejected_without_touching_external_content(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            external = root / "external.json"
            external.write_text('{"outside": true}\n', encoding="utf-8")
            try:
                project.joinpath(".mcp.json").symlink_to(external)
            except OSError as error:
                self.skipTest(f"symlink creation unavailable: {error}")

            with self.assertRaisesRegex(ValueError, "link or reparse"):
                module.install(project)

            self.assertEqual('{"outside": true}\n', external.read_text(encoding="utf-8"))

    def test_uninstall_restores_exact_before_images(self):
        module = load(HOSTS, "chaos_engine_hosts")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.joinpath(".chaos-engine/skills/chaos-engine").mkdir(parents=True)
            project.joinpath(".chaos-engine/skills/chaos-engine/SKILL.md").write_text("# C\n")
            project.joinpath("AGENTS.md").write_bytes(b"user instructions\r\n")
            project.joinpath(".mcp.json").write_text(
                json.dumps({"mcpServers": {"other": {"command": "other"}}}) + "\n",
                encoding="utf-8",
            )
            before_agents = project.joinpath("AGENTS.md").read_bytes()
            before_mcp = project.joinpath(".mcp.json").read_bytes()
            before_directories = {
                path.relative_to(project).as_posix()
                for path in project.rglob("*")
                if path.is_dir()
            }

            module.install(project)
            module.uninstall(project)

            self.assertEqual(before_agents, project.joinpath("AGENTS.md").read_bytes())
            self.assertEqual(before_mcp, project.joinpath(".mcp.json").read_bytes())
            self.assertFalse(project.joinpath(module.RECEIPT_NAME).exists())
            self.assertFalse(project.joinpath(".agents/skills/chaos-engine/SKILL.md").exists())
            self.assertEqual(
                before_directories,
                {
                    path.relative_to(project).as_posix()
                    for path in project.rglob("*")
                    if path.is_dir()
                },
            )

    def test_tool_launcher_resolves_runtime_after_project_move(self):
        module = load(TOOL, "chaos_engine_tool")
        with tempfile.TemporaryDirectory() as temporary:
            first = Path(temporary) / "first"
            second = Path(temporary) / "moved project"
            core = first / ".chaos-engine"
            runtime = first / ".chaos-engine-runtime"
            core.mkdir(parents=True)
            runtime.joinpath("bin").mkdir(parents=True)
            command = runtime / "bin" / ("graphify.exe" if os.name == "nt" else "graphify")
            command.write_text("tool\n", encoding="utf-8")
            first.replace(second)

            resolved = module.resolve_command(second / ".chaos-engine", "graphify")

            self.assertEqual(second / ".chaos-engine-runtime/bin" / command.name, resolved)

    def test_host_tests_are_reached_by_pull_request_gate(self):
        budget = json.loads(
            (ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
        )
        self.assertIn(
            "tests/scripts/test_chaos_engine_hosts.py",
            budget["harness_reachability"]["element_globs"],
        )
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertIn("tests.scripts.test_chaos_engine_hosts", workflow)


if __name__ == "__main__":
    unittest.main()
