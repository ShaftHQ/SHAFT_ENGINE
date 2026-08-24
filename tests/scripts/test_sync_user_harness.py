"""Deploy/drift behavior of scripts/agents/sync_user_harness.py."""

import json
import io
import os
import subprocess  # nosec B404 - tests drive the sync script with isolated temporary directories.
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from unittest import mock
from pathlib import Path

from scripts.agents import sync_user_harness as sync

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/agents/sync_user_harness.py"
MANIFEST = ("CLAUDE.md", "settings.json")
HISTORICAL_HARNESS_REVISION = "3993405e097d5d310c1d8a79d5c1974758064a85"
HISTORICAL_MANIFEST = ROOT / "scripts/agents/user_harness_retired_manifest.json"
HISTORICAL_CLAUDE_SKILL = b"""---
name: act-as-mohab
description: Use at every task start; redirects to the canonical portable entrypoint.
---

# Act as Mohab adapter

Follow the [canonical entrypoint](../../../.agents/skills/act-as-mohab/SKILL.md).
Do not restate policy here.
"""
HISTORICAL_CODEX_CODER = (
    b'name = "coder"\n'
    b'description = "Bounded implementer; defaults to middle capability and may assign only mechanical slices downward."\n'
    b'developer_instructions = """\n'
    b'Load [act-as-mohab](../../.agents/skills/act-as-mohab/SKILL.md), then follow\\r\n'
    b'the [implementer role](../../.agents/skills/act-as-mohab/references/roles.md#implementer)."""\n'
)


class SyncUserHarnessTest(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.root = Path(self._tmp.name)
        self.target = self.root / ".claude"
        self.codex_target = self.root / ".codex"
        self.agents_target = self.root / ".agents"
        self.target.mkdir()
        self.addCleanup(self._tmp.cleanup)

    def run_sync(self, *args: str) -> subprocess.CompletedProcess:
        env = dict(
            os.environ,
            SHAFT_USER_CLAUDE_DIR=str(self.target),
            SHAFT_USER_CODEX_DIR=str(self.codex_target),
            SHAFT_USER_AGENTS_DIR=str(self.agents_target),
        )
        return subprocess.run(
            [sys.executable, str(SCRIPT), *args],
            cwd=ROOT,
            env=env,
            capture_output=True,
            text=True,
            timeout=60,
        )

    def test_user_harness_defers_to_repository_and_syncs_no_skills(self):
        guidance = (ROOT / ".claude/user-harness/CLAUDE.md").read_text(encoding="utf-8")
        readme = " ".join(
            (ROOT / ".claude/user-harness/README.md").read_text(encoding="utf-8").split()
        )

        self.assertIn("repository's source-controlled `AGENTS.md`", guidance)
        self.assertNotIn("../.agents/skills", guidance)
        self.assertIn("does not deploy skills", readme)

    def test_check_reports_only_missing_generic_manifest(self):
        completed = self.run_sync()
        self.assertEqual(completed.returncode, 1)
        for name in MANIFEST:
            self.assertIn(f"MISSING  {name}", completed.stdout)
        self.assertNotIn("MISSING  agents/", completed.stdout)
        self.assertNotIn("MISSING  ../.codex/", completed.stdout)
        self.assertNotIn("/.agents/skills/", completed.stdout.replace("\\", "/"))
        self.assertNotIn("skills/act-as-mohab/SKILL.md", completed.stdout)

    def test_apply_deploys_only_generic_claude_harness_then_check_is_clean(self):
        self.assertEqual(self.run_sync("--apply").returncode, 0)
        for name in MANIFEST:
            self.assertTrue((self.target / name).is_file())
        self.assertFalse((self.target / "agents").exists())
        self.assertFalse(self.codex_target.exists())
        self.assertFalse((self.agents_target / "skills").exists())
        self.assertFalse((self.target / "skills").exists())
        for startup in self.target.rglob("*"):
            if startup.is_file():
                text = startup.read_text(encoding="utf-8")
                self.assertNotIn("../.agents/skills", text)
                self.assertNotIn("../../.agents/skills", text)
        self.assertEqual(self.run_sync().returncode, 0)

    def test_apply_retires_historical_user_skill_without_redeploying_it(self):
        legacy_sources = ((self.target, "act-as-mohab", HISTORICAL_CLAUDE_SKILL),)
        for base, name, historical in legacy_sources:
            target = base / "skills" / name / "SKILL.md"
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(historical)
        personal = self.agents_target / "skills/personal/SKILL.md"
        personal.parent.mkdir(parents=True)
        personal.write_text("---\nname: personal\n---\n\n# Personal\n", encoding="utf-8")

        completed = self.run_sync("--apply")

        self.assertEqual(completed.returncode, 0, completed.stdout)
        for base, name, historical in legacy_sources:
            target = base / "skills" / name / "SKILL.md"
            self.assertFalse(target.exists(), name)
            backup = target.with_name("SKILL.md.bak")
            self.assertTrue(backup.is_file(), name)
            self.assertEqual(backup.read_bytes(), historical, name)
        self.assertTrue(personal.is_file())
        self.assertEqual(self.run_sync().returncode, 0)

    def test_every_historical_retired_target_is_hashed_retired_and_recoverable(self):
        self.assertEqual(len(sync.RETIRED_TARGETS), 56)
        for label, policy in sync.RETIRED_TARGETS.items():
            with self.subTest(label=label):
                source = policy.get("source")
                self.assertIsInstance(source, str)
                historical = subprocess.check_output(
                    [
                        "git",
                        "show",
                        f"{HISTORICAL_HARNESS_REVISION}:{source}",
                    ],
                    cwd=ROOT,
                )
                self.assertIn(sync.content_hash(historical), policy.get("hashes", set()))
                if label.startswith("../.agents/"):
                    target = self.agents_target / label.removeprefix("../.agents/")
                elif label.startswith("../.codex/"):
                    target = self.codex_target / label.removeprefix("../.codex/")
                else:
                    target = self.target / label
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(historical)

                completed = self.run_sync("--apply")

                self.assertEqual(completed.returncode, 0, completed.stdout)
                self.assertFalse(target.exists())
                self.assertEqual(
                    target.with_name(target.name + ".bak").read_bytes(), historical
                )

    def test_full_pinned_historical_inventory_is_migrated_and_recoverable(self):
        self.assertTrue(HISTORICAL_MANIFEST.is_file())
        manifest = json.loads(HISTORICAL_MANIFEST.read_text(encoding="utf-8"))
        self.assertEqual(manifest["base_revision"], HISTORICAL_HARNESS_REVISION)
        entries = manifest["entries"]
        self.assertEqual(len(entries), 56)
        self.assertTrue(all("marker" not in entry for entry in entries))
        categories = {}
        for entry in entries:
            categories[entry["category"]] = categories.get(entry["category"], 0) + 1
        self.assertEqual(categories, {
            "canonical-skill-file": 42,
            "claude-skill-adapter": 3,
            "claude-role-adapter": 5,
            "codex-host-adapter": 1,
            "codex-role-adapter": 5,
        })

        canonical = subprocess.check_output(
            ["git", "ls-tree", "-r", "--name-only", HISTORICAL_HARNESS_REVISION, "--", ".agents/skills"],
            cwd=ROOT,
            text=True,
        ).splitlines()
        expected_sources = {path for path in canonical if path != ".agents/skills/README.md"}
        for prefix in (".claude/skills", ".claude/agents", ".codex/agents"):
            expected_sources.update(subprocess.check_output(
                ["git", "ls-tree", "-r", "--name-only", HISTORICAL_HARNESS_REVISION, "--", prefix],
                cwd=ROOT,
                text=True,
            ).splitlines())
        expected_sources.add(".claude/user-harness/CLAUDE.md")
        self.assertEqual({entry["source"] for entry in entries}, expected_sources)

        migrated = []
        for entry in entries:
            historical = subprocess.check_output(
                ["git", "show", f"{HISTORICAL_HARNESS_REVISION}:{entry['source']}"],
                cwd=ROOT,
            )
            self.assertIn(sync.content_hash(historical), entry["hashes"])
            label = entry["label"]
            if label.startswith("../.agents/"):
                target = self.agents_target / label.removeprefix("../.agents/")
            elif label.startswith("../.codex/"):
                target = self.codex_target / label.removeprefix("../.codex/")
            else:
                target = self.target / label
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(historical)
            migrated.append((target, historical))

        completed = self.run_sync("--apply")

        self.assertEqual(completed.returncode, 0, completed.stdout)
        for target, historical in migrated:
            self.assertFalse(target.exists(), target)
            self.assertEqual(target.with_name(target.name + ".bak").read_bytes(), historical)

    def test_apply_preserves_unknown_user_skill_collision(self):
        target = self.agents_target / "skills/act-as-mohab/SKILL.md"
        target.parent.mkdir(parents=True)
        custom = b"---\nname: chaos-engine\n---\n\n# My custom skill\n"
        target.write_bytes(custom)

        completed = self.run_sync("--apply")

        self.assertEqual(completed.returncode, 2, completed.stdout)
        self.assertIn("CONFLICT", completed.stdout)
        self.assertEqual(target.read_bytes(), custom)
        self.assertFalse(target.with_name("SKILL.md.bak").exists())

    def test_public_managed_marker_is_never_retirement_ownership_evidence(self):
        target = self.codex_target / "agents/coder.toml"
        target.parent.mkdir(parents=True)
        custom = (
            b'# Managed by the SHAFT user harness\n'
            b'name = "coder"\n'
            b'developer_instructions = "my private instructions"\n'
        )
        target.write_bytes(custom)
        before = {
            path.relative_to(self.root): path.read_bytes()
            for path in self.root.rglob("*") if path.is_file()
        }

        completed = self.run_sync("--apply")

        after = {
            path.relative_to(self.root): path.read_bytes()
            for path in self.root.rglob("*") if path.is_file()
        }
        self.assertEqual(completed.returncode, 2, completed.stdout)
        self.assertEqual(after, before)
        self.assertFalse((self.target / "CLAUDE.md").exists())
        self.assertFalse((self.target / "settings.json").exists())

    def test_current_repo_skill_bytes_are_not_dynamic_ownership_proof(self):
        target = self.agents_target / "skills/act-as-mohab/SKILL.md"
        target.parent.mkdir(parents=True)
        current = (ROOT / ".agents/skills/chaos-engine/SKILL.md").read_bytes()
        target.write_bytes(current)

        completed = self.run_sync("--apply")

        self.assertEqual(completed.returncode, 2, completed.stdout)
        self.assertEqual(target.read_bytes(), current)
        self.assertFalse(target.with_name("SKILL.md.bak").exists())

    def test_unknown_collision_aborts_before_any_retirement_or_deployment(self):
        owned = self.target / "skills/act-as-mohab/SKILL.md"
        owned.parent.mkdir(parents=True)
        owned.write_bytes(HISTORICAL_CLAUDE_SKILL)
        unknown = self.codex_target / "agents/coder.toml"
        unknown.parent.mkdir(parents=True)
        unknown.write_bytes(b"personal adapter\n")
        before = {
            path.relative_to(self.root): path.read_bytes()
            for path in self.root.rglob("*") if path.is_file()
        }

        completed = self.run_sync("--apply")

        after = {
            path.relative_to(self.root): path.read_bytes()
            for path in self.root.rglob("*") if path.is_file()
        }
        self.assertEqual(completed.returncode, 2, completed.stdout)
        self.assertEqual(after, before)

    def test_atomic_backup_retries_exclusive_name_and_unlinks_after_success(self):
        self.assertTrue(hasattr(sync, "backup_and_retire"))
        target = self.root / "owned.txt"
        target.write_bytes(b"owned")
        target.with_name("owned.txt.bak").write_bytes(b"older")

        backup = sync.backup_and_retire(target)

        self.assertEqual(backup.name, "owned.txt.bak.1")
        self.assertEqual(backup.read_bytes(), b"owned")
        self.assertFalse(target.exists())

    def test_atomic_backup_handles_a_racing_creator(self):
        self.assertTrue(hasattr(sync, "backup_and_retire"))
        target = self.root / "owned.txt"
        target.write_bytes(b"owned")
        original_move = sync.atomic_move_noreplace
        raced = False

        def racing_move(source, destination):
            nonlocal raced
            if not raced and destination.name == "owned.txt.bak":
                raced = True
                destination.write_bytes(b"racer")
                raise FileExistsError(destination)
            return original_move(source, destination)

        with mock.patch.object(sync, "atomic_move_noreplace", racing_move):
            backup = sync.backup_and_retire(target)

        self.assertEqual(backup.name, "owned.txt.bak.1")
        self.assertEqual(backup.read_bytes(), b"owned")
        self.assertEqual((self.root / "owned.txt.bak").read_bytes(), b"racer")
        self.assertFalse(target.exists())

    def test_atomic_move_never_overwrites_a_recreated_backup_candidate(self):
        self.assertTrue(hasattr(sync, "atomic_move_noreplace"))
        target = self.root / "owned.txt"
        candidate = self.root / "owned.txt.bak"
        target.write_bytes(b"owned")
        candidate.write_bytes(b"reservation")
        candidate.unlink()
        candidate.write_bytes(b"unknown raced bytes")

        with self.assertRaises(FileExistsError):
            sync.atomic_move_noreplace(target, candidate)

        self.assertEqual(target.read_bytes(), b"owned")
        self.assertEqual(candidate.read_bytes(), b"unknown raced bytes")

    def test_backup_failure_never_unlinks_source(self):
        self.assertTrue(hasattr(sync, "backup_and_retire"))
        target = self.root / "owned.txt"
        target.write_bytes(b"owned")
        with mock.patch.object(sync, "atomic_move_noreplace", side_effect=PermissionError("denied")):
            with self.assertRaises(PermissionError):
                sync.backup_and_retire(target)
        self.assertEqual(target.read_bytes(), b"owned")

    def test_raced_unknown_replacement_is_restored_instead_of_retired(self):
        self.assertTrue(hasattr(sync, "RetirementConflict"))
        target = self.root / "owned.txt"
        target.write_bytes(b"owned")
        original_move = sync.atomic_move_noreplace

        def replace_after_race(source, destination):
            Path(source).write_bytes(b"unknown replacement")
            return original_move(source, destination)

        with mock.patch(
            "scripts.agents.sync_user_harness.atomic_move_noreplace",
            side_effect=replace_after_race,
        ):
            with self.assertRaises(sync.RetirementConflict) as raised:
                sync.backup_and_retire(target, lambda data: data == b"owned")

        self.assertEqual(target.read_bytes(), b"unknown replacement")
        self.assertEqual(raised.exception.recovery.read_bytes(), b"unknown replacement")

    def test_conflict_preserves_candidate_when_restored_target_is_repopulated(self):
        target = self.root / "owned.txt"
        target.write_bytes(b"first unknown")
        original_link = os.link
        original_replace = os.replace

        def link_then_repopulate(source, destination):
            original_link(source, destination)
            replacement = self.root / "second-unknown.tmp"
            replacement.write_bytes(b"second unknown")
            original_replace(replacement, destination)

        with mock.patch(
            "scripts.agents.sync_user_harness.os.link",
            side_effect=link_then_repopulate,
        ):
            with self.assertRaises(sync.RetirementConflict) as raised:
                sync.backup_and_retire(target, lambda data: data == b"owned")

        self.assertEqual(target.read_bytes(), b"second unknown")
        self.assertIsNotNone(raised.exception.recovery)
        self.assertEqual(raised.exception.recovery.read_bytes(), b"first unknown")

    def test_owned_backup_conflicts_when_target_reappears_after_atomic_move(self):
        target = self.root / "owned.txt"
        target.write_bytes(b"owned")
        original_move = sync.atomic_move_noreplace

        def move_then_recreate(source, destination):
            original_move(source, destination)
            Path(source).write_bytes(b"unknown live replacement")

        with mock.patch.object(
            sync, "atomic_move_noreplace", side_effect=move_then_recreate
        ):
            with self.assertRaises(sync.RetirementConflict) as raised:
                sync.backup_and_retire(target, lambda data: data == b"owned")

        self.assertEqual(target.read_bytes(), b"unknown live replacement")
        self.assertIsNotNone(raised.exception.recovery)
        self.assertEqual(raised.exception.recovery.read_bytes(), b"owned")

    def test_post_move_conflict_never_reports_retired_success(self):
        target = self.root / "owned.txt"
        recovery = self.root / "owned.txt.bak"
        target.write_bytes(b"owned")
        recovery.write_bytes(b"owned")
        output = io.StringIO()
        with (
            mock.patch.object(sync, "sources", return_value={}),
            mock.patch.object(
                sync,
                "retired_targets",
                return_value=[("owned", ROOT / "historical", target, {sync.content_hash(b"owned")})],
            ),
            mock.patch.object(
                sync,
                "backup_and_retire",
                side_effect=sync.RetirementConflict(target, recovery),
            ),
            mock.patch.object(sys, "argv", [str(SCRIPT), "--apply"]),
            redirect_stdout(output),
        ):
            exit_code = sync.main()

        self.assertEqual(exit_code, 2)
        self.assertIn("CONFLICT", output.getvalue())
        self.assertNotIn("RETIRED", output.getvalue())
        self.assertIn(str(recovery), output.getvalue())

    def test_dangling_symlink_recreation_is_a_conflict_and_recovery_survives(self):
        target = self.root / "owned.txt"
        target.write_bytes(b"owned")
        missing = self.root / "missing-target"
        original_move = sync.atomic_move_noreplace

        def move_then_link(source, destination):
            original_move(source, destination)
            Path(source).symlink_to(missing)

        try:
            with mock.patch.object(
                sync, "atomic_move_noreplace", side_effect=move_then_link
            ):
                with self.assertRaises(sync.RetirementConflict) as raised:
                    sync.backup_and_retire(target, lambda data: data == b"owned")
        except OSError as error:
            self.skipTest(f"symlinks unavailable: {error}")

        self.assertTrue(os.path.lexists(target))
        self.assertTrue(target.is_symlink())
        self.assertFalse(target.exists())
        self.assertEqual(raised.exception.recovery.read_bytes(), b"owned")

    def test_drifted_generic_guidance_is_reported_backed_up_and_redeployed(self):
        self.run_sync("--apply")
        drifted = self.target / "CLAUDE.md"
        drifted.write_text("local drift\n", encoding="utf-8")
        completed = self.run_sync()
        self.assertEqual(completed.returncode, 1)
        self.assertIn("DRIFTED  CLAUDE.md", completed.stdout)
        self.assertEqual(self.run_sync("--apply").returncode, 0)
        self.assertEqual(
            drifted.with_name(drifted.name + ".bak").read_text(encoding="utf-8"),
            "local drift\n",
        )
        self.assertEqual(self.run_sync().returncode, 0)

    def test_codex_retirement_preserves_unowned_config(self):
        self.codex_target.mkdir(parents=True)
        personal_marker = "personal-codex-value-must-survive"
        config = self.codex_target / "config.toml"
        config.write_text(f'personal_key = "{personal_marker}"\n', encoding="utf-8")
        completed = self.run_sync("--apply")

        self.assertEqual(completed.returncode, 0)
        self.assertNotIn(personal_marker, completed.stdout + completed.stderr)
        self.assertEqual(config.read_text(encoding="utf-8"), f'personal_key = "{personal_marker}"\n')
        self.assertFalse((self.codex_target / "AGENTS.md").exists())
        self.assertEqual(self.run_sync().returncode, 0)

    def test_codex_custom_agent_collision_is_not_overwritten(self):
        personal_marker = "personal-agent-must-survive"
        target = self.codex_target / "agents" / "coder.toml"
        target.parent.mkdir(parents=True)
        target.write_text(
            f'name = "coder"\ndeveloper_instructions = "Use act-as-mohab; {personal_marker}"\n',
            encoding="utf-8",
        )

        completed = self.run_sync("--apply")

        self.assertEqual(completed.returncode, 2)
        self.assertIn("CONFLICT  ../.codex/agents/coder.toml", completed.stdout)
        self.assertNotIn(personal_marker, completed.stdout + completed.stderr)
        self.assertIn(personal_marker, target.read_text(encoding="utf-8"))
        self.assertFalse(target.with_name("coder.toml.bak").exists())

    def test_json_reports_a_hard_conflict_without_exposing_target_contents(self):
        personal_marker = "personal-agent-must-survive"
        target = self.codex_target / "agents" / "coder.toml"
        target.parent.mkdir(parents=True)
        target.write_text(personal_marker, encoding="utf-8")

        completed = self.run_sync("--json", "--apply")

        self.assertEqual(completed.returncode, 2)
        report = json.loads(completed.stdout)
        conflict = next(
            entry for entry in report["entries"]
            if entry["label"] == "../.codex/agents/coder.toml"
        )
        self.assertEqual(conflict["state"], "CONFLICT")
        self.assertEqual(conflict["target"], str(target))
        self.assertNotIn(personal_marker, completed.stdout + completed.stderr)

    def test_legacy_codex_harness_agent_migrates_safely(self):
        target = self.codex_target / "agents" / "coder.toml"
        target.parent.mkdir(parents=True)
        target.write_bytes(HISTORICAL_CODEX_CODER)

        completed = self.run_sync("--apply")

        self.assertEqual(completed.returncode, 0)
        self.assertEqual(target.with_name("coder.toml.bak").read_bytes(), HISTORICAL_CODEX_CODER)
        self.assertFalse(target.exists())

    def test_settings_merge_preserves_unowned_keys_and_secret_values(self):
        personal_marker = "personal-value-must-survive"
        existing = {
            "env": {"PERSONAL_API_KEY": personal_marker, "ENABLE_TOOL_SEARCH": "old"},
            "enabledPlugins": {"personal@local": True, "mempalace@mempalace": True},
            "personalSetting": {"nested": 7},
        }
        (self.target / "settings.json").write_text(json.dumps(existing), encoding="utf-8")

        completed = self.run_sync("--apply")

        self.assertEqual(completed.returncode, 0)
        self.assertNotIn(personal_marker, completed.stdout)
        deployed = json.loads((self.target / "settings.json").read_text(encoding="utf-8"))
        self.assertEqual(deployed["env"]["PERSONAL_API_KEY"], personal_marker)
        self.assertEqual(deployed["personalSetting"], {"nested": 7})
        self.assertIs(deployed["enabledPlugins"]["personal@local"], True)
        self.assertIs(deployed["enabledPlugins"]["mempalace@mempalace"], False)
        self.assertEqual(deployed["env"]["ENABLE_TOOL_SEARCH"], "true")
        self.assertEqual(self.run_sync().returncode, 0)

    def test_settings_merge_removes_retired_owned_keys_without_exposing_personal_data(self):
        personal_marker = "retired-migration-value"
        existing = {
            "model": "retired-model",
            "effortLevel": "retired-effort",
            "statusLine": {"type": "command", "command": "retired-statusline"},
            "permissions": {"defaultMode": "acceptEdits"},
            "extraKnownMarketplaces": {"mempalace": {"source": "retired-source"}},
            "env": {
                "MEMPALACE_EMBEDDING_MODEL": "retired-embedder",
                "PERSONAL_API_KEY": personal_marker,
            },
            "enabledPlugins": {"personal@local": True},
            "theme": "dark",
        }
        (self.target / "settings.json").write_text(json.dumps(existing), encoding="utf-8")

        completed = self.run_sync("--apply")

        self.assertEqual(completed.returncode, 0)
        self.assertNotIn(personal_marker, completed.stdout + completed.stderr)
        deployed = json.loads((self.target / "settings.json").read_text(encoding="utf-8"))
        for retired in ("model", "effortLevel", "statusLine", "permissions", "extraKnownMarketplaces"):
            self.assertNotIn(retired, deployed)
        self.assertNotIn("MEMPALACE_EMBEDDING_MODEL", deployed["env"])
        self.assertEqual(deployed["env"]["PERSONAL_API_KEY"], personal_marker)
        self.assertIs(deployed["enabledPlugins"]["personal@local"], True)
        self.assertEqual(deployed["theme"], "dark")
        self.assertEqual(self.run_sync().returncode, 0)


if __name__ == "__main__":
    unittest.main()
