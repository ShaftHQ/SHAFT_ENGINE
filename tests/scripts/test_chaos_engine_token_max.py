"""Token-max / no-proxy harness: MCP uniqueness, origin-sync retrieve, overlay hash."""

from __future__ import annotations

import importlib.util
import json
import sys
import tempfile
from pathlib import Path
from unittest import TestCase, mock

ROOT = Path(__file__).resolve().parents[2]

RUNTIME_FILES = (
    "hooks/kernel.py",
    "hooks/launch.js",
    "hooks/lifecycle.py",
    "hooks/matchers.json",
)

DISTRIBUTIONS = {
    "schemaVersion": 1,
    "default": "portable",
    "distributions": {
        "portable": {
            "profile": "portable",
            "forbiddenTokens": ["shaft"],
            "runtimeFiles": list(RUNTIME_FILES),
            "components": {
                "core": {
                    "owner": "installer",
                    "scope": "project",
                    "lifecycle": "receipt-owned",
                    "taskImpact": "required",
                }
            },
        },
        "repository": {
            "profile": "shaft",
            "forbiddenTokens": [],
            "runtimeFiles": list(RUNTIME_FILES),
            "components": {
                "core": {
                    "owner": "installer",
                    "scope": "project",
                    "lifecycle": "receipt-owned",
                    "taskImpact": "required",
                }
            },
        },
    },
}


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def _write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def seed_repository_project(
    root: Path,
    *,
    mutate: str | None = None,
    omit: str | None = None,
    extra: str | None = None,
) -> Path:
    """Build a temp origin-shaped tree; overlay follows owned source_files()."""
    source = root / "chaos-engine"
    _write(source / "skills/chaos-engine/SKILL.md", "# skill\n")
    _write(source / "LICENSE", "license\n")
    _write(source / "README.md", "origin-only readme\n")
    _write(source / "profiles/shaft/entrypoint.md", "shaft profile\n")
    _write(source / "profiles/shaft/profile.json", '{"schemaVersion":1,"name":"shaft"}\n')
    _write(source / "profiles/portable/entrypoint.md", "portable only\n")
    _write(
        source / "profiles/portable/profile.json",
        '{"schemaVersion":1,"name":"portable"}\n',
    )
    for relative in RUNTIME_FILES:
        _write(source / relative, f"runtime:{relative}\n")
    _write(source / "distributions.json", json.dumps(DISTRIBUTIONS))

    install = load(ROOT / "chaos-engine/install.py", "ce_install_seed_5689")
    overlay = root / ".chaos-engine"
    for path in install.source_files(source, "repository"):
        relative = path.relative_to(source).as_posix()
        if omit is not None and relative == omit:
            continue
        text = path.read_text(encoding="utf-8")
        if mutate is not None and relative == mutate:
            text = f"{text}mutated\n"
        _write(overlay / relative, text)
    if extra is not None:
        _write(overlay / extra, "unowned extra\n")
    return root


class TokenMaxTests(TestCase):
    @classmethod
    def setUpClass(cls):
        cls.policy = load(ROOT / "chaos-engine/mcp_policy.py", "ce_mcp_policy_5689")
        cls.overlay = load(ROOT / "chaos-engine/overlay_match.py", "ce_overlay_match_5689")
        cls.retrieve = load(ROOT / "chaos-engine/retrieve.py", "ce_retrieve_5689")
        cls.hosts = load(ROOT / "chaos-engine/hosts.py", "ce_hosts_5689")

    def test_github_alias_pair_is_left_in_place(self):
        self.assertIsNone(self.policy.uniqueness_error(["github", "github-gh"]))

    def test_graphify_mcp_conflicts_with_owned_cli_github_does_not(self):
        self.assertIsNone(self.policy.cli_owned_conflict_error(["github"]))
        error = self.policy.cli_owned_conflict_error(["graphify"])
        self.assertIsNotNone(error)
        self.assertIn("Prefer gh for GitHub", error)

    def test_user_and_project_ids_share_one_heal_prompt(self):
        text = (ROOT / "chaos-engine/hosts.py").read_text(encoding="utf-8")
        self.assertIn("CLI over MCP when both exist", text)
        self.assertIn(".chaos-engine/", self.hosts.instruction_block("chaos-engine"))
        self.assertIn(".chaos-engine/", self.hosts.instruction_block(".chaos-engine"))
        self.assertEqual(
            ".chaos-engine",
            self.hosts.guidance_tree(ROOT),
        )
        self.assertIn("when gh auth status succeeds", self.policy.HEAL_PROMPT)
        self.assertIn("CLI over MCP when both exist", self.policy.HEAL_PROMPT)
        block = self.hosts.instruction_block(".chaos-engine")
        self.assertIn("Prefer gh for GitHub when gh exists and is configured", block)
        self.assertIn("CLI over MCP when both exist", block)

    def test_overlay_match_adopter_is_true(self):
        with tempfile.TemporaryDirectory() as temporary:
            adopter = Path(temporary) / "adopter"
            adopter.mkdir()
            _write(adopter / ".chaos-engine/hooks/kernel.py", "overlay only\n")
            matched = self.overlay.core_matches_source(adopter)
        self.assertTrue(matched["coreMatchesSource"])
        self.assertEqual("adopter", matched["scope"])

    def test_overlay_match_owned_bytes_and_missing_file(self):
        with tempfile.TemporaryDirectory() as temporary:
            matched = seed_repository_project(Path(temporary) / "ok")
            self.assertTrue(
                self.overlay.core_matches_source(matched)["coreMatchesSource"]
            )

        with tempfile.TemporaryDirectory() as temporary:
            drifted = seed_repository_project(
                Path(temporary) / "drift", mutate="LICENSE"
            )
            result = self.overlay.core_matches_source(drifted)
            self.assertFalse(result["coreMatchesSource"])
            self.assertIn("LICENSE", result["mismatches"])

        with tempfile.TemporaryDirectory() as temporary:
            missing = seed_repository_project(
                Path(temporary) / "missing", omit="LICENSE"
            )
            result = self.overlay.core_matches_source(missing)
            self.assertFalse(result["coreMatchesSource"])
            self.assertIn("LICENSE", result["mismatches"])

        with tempfile.TemporaryDirectory() as temporary:
            extra = seed_repository_project(
                Path(temporary) / "extra", extra="local-only.txt"
            )
            self.assertTrue(self.overlay.core_matches_source(extra)["coreMatchesSource"])
            # Origin-only README and non-selected portable profile are not owned.
            self.assertFalse((extra / ".chaos-engine/README.md").is_file())
            self.assertFalse(
                (extra / ".chaos-engine/profiles/portable/entrypoint.md").is_file()
            )

    def test_doctor_flips_status_only_on_owned_mismatch(self):
        with tempfile.TemporaryDirectory() as temporary:
            matched = seed_repository_project(Path(temporary) / "ok")
            healthy = {
                "status": "healthy",
                "components": {"core": {"status": "healthy"}},
            }
            self.overlay.apply_doctor_overlay_match(healthy, matched)
            self.assertEqual("healthy", healthy["status"])
            self.assertEqual("healthy", healthy["components"]["core"]["status"])
            self.assertTrue(healthy["components"]["core"]["coreMatchesSource"])

        with tempfile.TemporaryDirectory() as temporary:
            # Doctor one-shot heal restores owned bytes from SOURCE (#5794).
            drifted = seed_repository_project(
                Path(temporary) / "drift", omit="hooks/kernel.py"
            )
            self.assertFalse(
                self.overlay.core_matches_source(drifted)["coreMatchesSource"]
            )
            result = {
                "status": "healthy",
                "components": {"core": {"status": "healthy"}},
            }
            self.overlay.apply_doctor_overlay_match(result, drifted)
            self.assertEqual("healthy", result["status"])
            self.assertEqual("healthy", result["components"]["core"]["status"])
            self.assertTrue(result["components"]["core"]["coreMatchesSource"])
            self.assertTrue(
                self.overlay.core_matches_source(drifted)["coreMatchesSource"]
            )
            self.assertFalse(
                (drifted / ".chaos-engine-state/overlay-handoff.md").is_file()
            )

    def test_sync_overlay_from_source_heals_schema_and_playbook_drift(self):
        """Regression #5794: memory-v5 / shaft playbook style owned drift heals."""
        with tempfile.TemporaryDirectory() as temporary:
            root = seed_repository_project(Path(temporary) / "repo")
            # Seed the same class of owned paths observed on ROG after curl install.
            for relative in (
                "assets/memory-v5/SCHEMAS.md",
                "assets/memory-v5/config.schema.json",
                "assets/memory-v5/event.schema.json",
                "profiles/shaft/references/playbooks/allure-extent-report-operator.md",
                "profiles/shaft/references/playbooks/ci-failure-investigator.md",
            ):
                source_path = root / "chaos-engine" / relative
                source_path.parent.mkdir(parents=True, exist_ok=True)
                source_path.write_text(f"source:{relative}\n", encoding="utf-8")
                overlay_path = root / ".chaos-engine" / relative
                overlay_path.parent.mkdir(parents=True, exist_ok=True)
                overlay_path.write_text(f"remote-payload:{relative}\n", encoding="utf-8")
            # Minimal manifest so sync can rewrite files digests.
            install = load(ROOT / "chaos-engine/install.py", "ce_install_seed_5794")
            files = {
                path.relative_to(root / ".chaos-engine").as_posix(): install.file_sha256(path)
                for path in sorted((root / ".chaos-engine").rglob("*"))
                if path.is_file()
            }
            (root / ".chaos-engine/manifest.json").write_text(
                __import__("json").dumps(
                    {
                        "schemaVersion": 1,
                        "distribution": {"id": "repository", "policySha256": "0" * 64},
                        "source": {"commit": "a" * 40, "kind": "local"},
                        "files": files,
                        "hostToken": "b" * 64,
                    },
                    indent=2,
                    sort_keys=True,
                )
                + "\n",
                encoding="utf-8",
            )
            before = self.overlay.core_matches_source(root)
            self.assertFalse(before["coreMatchesSource"])
            self.assertTrue(
                any("memory-v5" in item or "playbooks" in item for item in before["mismatches"])
            )
            synced = self.overlay.sync_overlay_from_source(root)
            self.assertTrue(synced["synced"])
            self.assertGreater(synced["copiedCount"], 0)
            after = self.overlay.core_matches_source(root)
            self.assertTrue(after["coreMatchesSource"])
            # verify_install must remain green after digest rewrite.
            install.verify_install(root / ".chaos-engine")
            doctor = {
                "status": "healthy",
                "components": {"core": {"status": "healthy"}},
            }
            self.overlay.apply_doctor_overlay_match(doctor, root)
            self.assertEqual("healthy", doctor["status"])
            self.assertTrue(doctor["components"]["core"]["coreMatchesSource"])

    def test_doctor_overlay_handoff_when_heal_impossible(self):
        with tempfile.TemporaryDirectory() as temporary:
            drifted = seed_repository_project(
                Path(temporary) / "drift", mutate="LICENSE"
            )
            # Remove SOURCE skill marker after seed so sync sees repository… wait,
            # that would flip scope to adopter. Instead make overlay file immutable
            # by replacing a drifted owned path with a directory of the same name.
            target = drifted / ".chaos-engine" / "LICENSE"
            target.unlink()
            target.mkdir()
            (target / "blocked").write_text("nope\n", encoding="utf-8")
            result = {
                "status": "healthy",
                "components": {"core": {"status": "healthy"}},
            }
            self.overlay.apply_doctor_overlay_match(result, drifted)
            self.assertEqual("recovery-required", result["status"])
            self.assertEqual(
                "overlay-source-mismatch", result["components"]["core"]["detail"]
            )
            self.assertIn(
                "overlay-handoff.md", result["components"]["core"]["fixNext"]
            )
            self.assertNotIn("Reinstall", result["components"]["core"]["fixNext"])
            handoff = drifted / ".chaos-engine-state/overlay-handoff.md"
            self.assertTrue(handoff.is_file())
            body = handoff.read_text(encoding="utf-8")
            self.assertIn("LICENSE", body)
            self.assertIn("Agent prompt:", body)
            self.assertIn("agentPrompt", result["components"]["core"])
            self.assertIn(
                "overlay-handoff.md", result["components"]["core"]["agentPrompt"]
            )

    def test_adopter_sync_is_noop(self):
        with tempfile.TemporaryDirectory() as temporary:
            adopter = Path(temporary) / "adopter"
            adopter.mkdir()
            _write(adopter / ".chaos-engine/hooks/kernel.py", "overlay only\n")
            synced = self.overlay.sync_overlay_from_source(adopter)
            self.assertFalse(synced["synced"])
            self.assertEqual("adopter", synced["scope"])

    def test_retrieve_origin_sync_is_not_store_degraded(self):
        project = ROOT
        message = (
            "primary checkout HEAD (aaa) != origin/main (bbb) "
            "(not synchronized with origin/main). "
            "fix-next: git fetch origin main && git merge --ff-only origin/main\n"
        )
        completed = mock.Mock(returncode=1, stdout="", stderr=message)
        with mock.patch.object(self.retrieve.subprocess, "run", return_value=completed):
            receipt = self.retrieve._run_store(project, "memory", "probe")
        self.assertEqual("skipped", receipt["status"])
        self.assertEqual("origin-sync", receipt["reason"])
        self.assertEqual("advisory", receipt["originSync"])
        self.assertNotEqual("degraded", receipt["status"])

    def test_matrix_closes_grok_caveman_without_duplicating_bodies(self):
        matrix = (
            ROOT / "chaos-engine/references/host-parity-matrix.md"
        ).read_text(encoding="utf-8")
        self.assertIn("GAP-GROK-CAVEMAN", matrix)
        agents = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
        self.assertNotIn("caveman=ultra", agents)

    def test_no_proxy_rule_forbids_wrap_as_health(self):
        rule = (ROOT / "chaos-engine/references/no-proxy.md").read_text(encoding="utf-8")
        self.assertIn("Never `ft launch`", rule)
        self.assertIn("Do not treat wrap/proxy as install health", rule)
