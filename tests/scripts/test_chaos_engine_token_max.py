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

    def test_github_alias_pair_is_duplicate(self):
        error = self.policy.uniqueness_error(["github", "github-gh"])
        self.assertIsNotNone(error)
        self.assertIn("disable extras", error)

    def test_single_github_or_graphify_mcp_conflicts_with_owned_cli(self):
        for server in ("github", "graphify"):
            error = self.policy.cli_owned_conflict_error([server])
            self.assertIsNotNone(error)
            self.assertIn("No duplicate GitHub MCP", error)

    def test_user_and_project_ids_share_one_heal_prompt(self):
        text = (ROOT / "chaos-engine/hosts.py").read_text(encoding="utf-8")
        self.assertIn("disable extras in host MCP config", text)
        self.assertIn(".chaos-engine/", self.hosts.instruction_block("chaos-engine"))
        self.assertIn(".chaos-engine/", self.hosts.instruction_block(".chaos-engine"))
        self.assertEqual(
            ".chaos-engine",
            self.hosts.guidance_tree(ROOT),
        )
        self.assertEqual(
            self.policy.HEAL_PROMPT,
            "No duplicate GitHub MCP. Repair: disable extras in host MCP config.",
        )
        self.assertIn(self.policy.HEAL_PROMPT, self.hosts.instruction_block(".chaos-engine"))

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
            drifted = seed_repository_project(
                Path(temporary) / "drift", omit="hooks/kernel.py"
            )
            result = {
                "status": "healthy",
                "components": {"core": {"status": "healthy"}},
            }
            self.overlay.apply_doctor_overlay_match(result, drifted)
            self.assertEqual("recovery-required", result["status"])
            self.assertEqual(
                "recovery-required", result["components"]["core"]["status"]
            )
            self.assertFalse(result["components"]["core"]["coreMatchesSource"])
            self.assertEqual(
                "overlay-source-mismatch", result["components"]["core"]["detail"]
            )
            self.assertIn("Reinstall", result["components"]["core"]["fixNext"])

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
