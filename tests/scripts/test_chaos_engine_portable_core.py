"""Portable ChaosEngine core and project-profile contract tests (#4792)."""

import json
import re
import unittest
import xml.etree.ElementTree as ET  # nosec B405
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
CORE = ROOT / "chaos-engine"
CANONICAL_SKILL = CORE / "skills/chaos-engine/SKILL.md"
CLEANUP_SCOPES = CORE / "references/cleanup-scopes.md"
TASK_ISOLATION = CORE / "references/task-isolation.md"
REPOSITORY_ADAPTER = ROOT / ".agents/skills/chaos-engine/SKILL.md"
COMPATIBILITY_ALIAS = ROOT / ".agents/skills/act-as-mohab/SKILL.md"
SHAFT_PROFILE = CORE / "profiles/shaft/profile.json"
PORTABLE_README = CORE / "README.md"
BRAND_ASSETS = CORE / "assets/brand"
POSIX_ABSOLUTE_PATH = re.compile(
    r"(?:^|[\s`\"'(=])(/[A-Za-z0-9._~-]+(?:/[A-Za-z0-9._~*{}-]+)+)",
    re.MULTILINE,
)


class ChaosEnginePortableCoreTest(unittest.TestCase):
    def test_portable_graphify_retrieval_is_read_only_and_ordered(self):
        guidance = (CORE / "references/graphify.md").read_text(encoding="utf-8")
        retrieval = guidance.split("## Refresh", 1)[0]
        query = retrieval.find('graphify query "<bounded structural question>"')
        diagnostic = retrieval.find("graphify diagnose multigraph")
        degraded = retrieval.find("If either read-only command fails")

        self.assertEqual(
            {
                "query_exists": True,
                "diagnostic_follows_query": True,
                "degraded_follows_diagnostic": True,
                "contains_mutation": False,
            },
            {
                "query_exists": query >= 0,
                "diagnostic_follows_query": diagnostic > query,
                "degraded_follows_diagnostic": degraded > diagnostic,
                "contains_mutation": any(
                    token in retrieval
                    for token in ("graphify update", " refresh --", "--force")
                ),
            },
        )

    def test_portable_profile_owns_a_real_routing_surface(self):
        profile = ROOT / "chaos-engine/profiles/portable/entrypoint.md"
        routing = ROOT / "chaos-engine/profiles/portable/references/routing.md"

        self.assertTrue(routing.is_file())
        self.assertIn("references/routing.md", profile.read_text(encoding="utf-8"))

    def test_portable_readme_uses_existing_contained_targets(self):
        readme = PORTABLE_README.read_text(encoding="utf-8")
        targets = re.findall(r'\[[^]]+\]\(([^)]+)\)', readme)
        targets.extend(re.findall(r"(?m)^\s{0,3}\[[^]]+\]:\s*(\S+)", readme))
        targets.extend(re.findall(r'(?:href|src|srcset)="([^"]+)"', readme))

        self.assertIn("assets/brand/lockup-light.svg", targets)
        self.assertIn("assets/brand/lockup-dark.svg", targets)
        for target in targets:
            if re.match(r"^[a-z][a-z0-9+.-]*:", target, re.IGNORECASE) or target.startswith("#"):
                continue
            resolved = (CORE / target.split("#", 1)[0]).resolve()
            with self.subTest(target=target):
                self.assertTrue(resolved.is_relative_to(CORE.resolve()))
                self.assertTrue(resolved.exists())

    def test_brand_assets_are_deterministic_accessible_vector_masters(self):
        expected = {
            "favicon-16-dark.svg",
            "favicon-16.svg",
            "favicon-dark.svg",
            "favicon.svg",
            "lockup-dark.svg",
            "lockup-light.svg",
            "symbol-dark.svg",
            "symbol-light.svg",
            "symbol-monochrome-black.svg",
            "symbol-monochrome-white.svg",
            "symbol-primary.svg",
            "specimen.svg",
        }
        self.assertEqual(expected, {path.name for path in BRAND_ASSETS.glob("*.svg")})

        forbidden_tags = {
            "filter", "image", "linearGradient", "radialGradient", "script", "style", "text"
        }
        for path in sorted(BRAND_ASSETS.glob("*.svg")):
            root = ET.parse(path).getroot()  # nosec B314
            tags = {element.tag.rsplit("}", 1)[-1] for element in root.iter()}
            labelled_by = root.attrib.get("aria-labelledby", "").split()
            element_ids = {element.attrib["id"] for element in root.iter() if "id" in element.attrib}
            forbidden_attributes = []
            for element in root.iter():
                for name, value in element.attrib.items():
                    local_name = name.rsplit("}", 1)[-1]
                    if (
                        local_name in {"filter", "href"}
                        or local_name.startswith("on")
                    ):
                        forbidden_attributes.append((local_name, value))
            with self.subTest(path=path.name):
                self.assertEqual("svg", root.tag.rsplit("}", 1)[-1])
                self.assertEqual("img", root.attrib.get("role"))
                self.assertEqual(["title", "desc"], labelled_by)
                self.assertTrue(set(labelled_by).issubset(element_ids))
                self.assertTrue({"title", "desc"}.issubset(tags))
                self.assertFalse(forbidden_tags & tags)
                self.assertEqual([], forbidden_attributes)
                if "monochrome" in path.name:
                    description = next(
                        element.text or ""
                        for element in root.iter()
                        if element.tag.rsplit("}", 1)[-1] == "desc"
                    )
                    self.assertNotIn("amber", description.lower())

    def test_brand_v4_uses_the_polished_quantum_mandate_geometry_and_palette(self):
        brand = (BRAND_ASSETS / "BRAND.md").read_text(encoding="utf-8")
        palette = json.loads((BRAND_ASSETS / "palette.json").read_text(encoding="utf-8"))
        expected_palette = {
            "void": "#06080D",
            "carbon": "#10141C",
            "opticalWhite": "#F2F7FF",
            "ionBlue": "#2F7DFF",
            "electricCyan": "#61D4FF",
            "cyberneticRed": "#FF3B4D",
        }
        self.assertEqual(3, palette["schemaVersion"])
        self.assertEqual(expected_palette, palette["colors"])
        self.assertIn("Quantum Mandate", brand)
        self.assertIn("Chinese seal", brand)
        self.assertIn("abstract dark-authority", brand)

        readme = PORTABLE_README.read_text(encoding="utf-8")
        self.assertIn("Quantum Mandate", readme)
        self.assertNotIn("amber core", readme)

        allowed_colors = {
            "symbol-primary.svg": {"#10141C", "#FF3B4D"},
            "symbol-light.svg": {"#F2F7FF", "#10141C", "#FF3B4D"},
            "symbol-dark.svg": {"#06080D", "#F2F7FF", "#FF3B4D"},
            "lockup-light.svg": {"#F2F7FF", "#10141C", "#FF3B4D"},
            "lockup-dark.svg": {"#06080D", "#F2F7FF", "#FF3B4D"},
            "favicon.svg": {"#10141C", "#FF3B4D"},
            "favicon-dark.svg": {"#F2F7FF", "#FF3B4D"},
            "favicon-16.svg": {"#10141C", "#FF3B4D"},
            "favicon-16-dark.svg": {"#F2F7FF", "#FF3B4D"},
            "symbol-monochrome-black.svg": {"#10141C"},
            "symbol-monochrome-white.svg": {"#F2F7FF"},
            "specimen.svg": {"#06080D", "#10141C", "#F2F7FF", "#FF3B4D"},
        }

        def groups_with_direct_children(
            root, child_count, geometry_version="quantum-mandate-v4"
        ):
            groups = []
            for element in root.iter():
                if element.attrib.get("data-geometry") != geometry_version:
                    continue
                children = list(element)
                if len(children) == child_count:
                    groups.append(children)
            return groups

        def geometry_signature(children):
            signature = []
            for element in children:
                tag = element.tag.rsplit("}", 1)[-1]
                attributes = tuple(
                    sorted(
                        (name.rsplit("}", 1)[-1], value)
                        for name, value in element.attrib.items()
                        if name.rsplit("}", 1)[-1] not in {"fill", "stroke"}
                    )
                )
                signature.append((tag, attributes))
            return tuple(signature)

        canonical_root = ET.parse(BRAND_ASSETS / "symbol-primary.svg").getroot()  # nosec B314
        canonical_groups = groups_with_direct_children(canonical_root, 4)
        self.assertTrue(canonical_groups, "canonical Quantum Mandate v4 geometry is missing")
        canonical_children = canonical_groups[0]
        canonical_geometry = geometry_signature(canonical_children)
        canonical_paths = [element.attrib["d"] for element in canonical_children]
        self.assertEqual(
            "M332 116H600V260H410L282 388V636L410 764H600V908H332L126 702V322Z",
            canonical_paths[0],
        )
        self.assertIn("H760L800 714H690Z", canonical_paths[1])
        self.assertEqual(
            "M478 456 518 416H578L618 456V516L578 556H518L478 516Z",
            canonical_paths[2],
        )
        micro_root = ET.parse(BRAND_ASSETS / "favicon-16.svg").getroot()  # nosec B314
        micro_groups = groups_with_direct_children(
            micro_root, 4, geometry_version="quantum-mandate-v3"
        )
        self.assertTrue(micro_groups, "dedicated Quantum Mandate v3 micro geometry is missing")
        micro_geometry = geometry_signature(micro_groups[0])
        full_size_assets = {
            "symbol-primary.svg", "symbol-light.svg", "symbol-dark.svg",
            "lockup-light.svg", "lockup-dark.svg", "favicon.svg",
            "favicon-dark.svg", "symbol-monochrome-black.svg",
            "symbol-monochrome-white.svg", "specimen.svg",
        }

        for path in sorted(BRAND_ASSETS.glob("*.svg")):
            content = path.read_text(encoding="utf-8")
            root = ET.fromstring(content)  # nosec B314
            colors = {
                value
                for element in root.iter()
                for name, value in element.attrib.items()
                if name.rsplit("}", 1)[-1] in {"fill", "stroke"} and value != "none"
            }
            with self.subTest(path=path.name):
                self.assertNotIn("17d4c5c2b655f876", content)
                expected_geometry_version = (
                    "quantum-mandate-v3"
                    if path.name in {"favicon-16.svg", "favicon-16-dark.svg"}
                    else "quantum-mandate-v4"
                )
                self.assertIn(expected_geometry_version, content)
                self.assertEqual(allowed_colors[path.name], colors)
                groups = groups_with_direct_children(
                    root, 4, geometry_version=expected_geometry_version
                )
                if path.name in full_size_assets:
                    self.assertIn(canonical_geometry, map(geometry_signature, groups))
                if path.name in {"favicon-16.svg", "favicon-16-dark.svg"}:
                    self.assertEqual([micro_geometry], list(map(geometry_signature, groups)))
                    self.assertEqual("crispEdges", root.attrib.get("shape-rendering"))

        specimen_root = ET.parse(BRAND_ASSETS / "specimen.svg").getroot()  # nosec B314
        specimen_groups = groups_with_direct_children(specimen_root, 4)
        specimen_groups.extend(
            groups_with_direct_children(
                specimen_root, 4, geometry_version="quantum-mandate-v3"
            )
        )
        specimen_signatures = list(map(geometry_signature, specimen_groups))
        self.assertEqual(5, len(specimen_signatures))
        self.assertEqual(3, specimen_signatures.count(canonical_geometry))
        self.assertEqual(2, specimen_signatures.count(micro_geometry))
        self.assertEqual({canonical_geometry, micro_geometry}, set(specimen_signatures))
        micro_specimens = [
            element
            for element in specimen_root.iter()
            if element.attrib.get("data-geometry") == "quantum-mandate-v3"
            and element.attrib.get("shape-rendering") == "crispEdges"
        ]
        self.assertEqual(2, len(micro_specimens))

        for name in ("lockup-light.svg", "lockup-dark.svg"):
            content = (BRAND_ASSETS / name).read_text(encoding="utf-8")
            self.assertIn('d="M1610 0V130M1610 0H1680', content)
            self.assertIn('d="M2220 0V130M2220 0H2290', content)

        self.assertIn("interface and data-visualization use only", brand)

    def test_chaos_engine_is_the_canonical_skill_and_act_as_mohab_is_only_an_alias(self):
        canonical = CANONICAL_SKILL.read_text(encoding="utf-8")
        repository_adapter = REPOSITORY_ADAPTER.read_text(encoding="utf-8")
        alias = COMPATIBILITY_ALIAS.read_text(encoding="utf-8")

        self.assertRegex(canonical, r"(?m)^name: chaos-engine$")
        self.assertIn("../../../chaos-engine/skills/chaos-engine/SKILL.md", repository_adapter)
        self.assertIn("../../../chaos-engine/skills/chaos-engine/SKILL.md", alias)
        self.assertLessEqual(len(alias.splitlines()), 12, "compatibility alias must not duplicate policy")
        self.assertNotIn("## Iron laws", alias)

    def test_generic_core_has_no_shaft_or_machine_specific_paths(self):
        forbidden = {
            "ShaftHQ": re.compile(r"ShaftHQ", re.IGNORECASE),
            "SHAFT_ENGINE": re.compile(r"SHAFT_ENGINE", re.IGNORECASE),
            "user guide": re.compile(r"shafthq\.github\.io", re.IGNORECASE),
            "Windows absolute path": re.compile(r"(?<![A-Za-z0-9+.-])[A-Za-z]:[\\/]"),
            "POSIX absolute path": POSIX_ABSOLUTE_PATH,
            "hard-coded default branch": re.compile(r"origin/main", re.IGNORECASE),
        }
        sources = sorted(
            path
            for path in CORE.rglob("*")
            if path.is_file()
            and "profiles" not in path.relative_to(CORE).parts
            and "__pycache__" not in path.relative_to(CORE).parts
            and path.suffix != ".pyc"
        )
        self.assertTrue(sources, "portable core sources must exist")
        for path in sources:
            text = path.read_text(encoding="utf-8")
            for label, pattern in forbidden.items():
                with self.subTest(path=path.relative_to(ROOT), forbidden=label):
                    self.assertIsNone(pattern.search(text))

    def test_canonical_cleanup_policy_is_portable_and_has_three_scopes(self):
        canonical = CANONICAL_SKILL.read_text(encoding="utf-8")
        task_isolation_router = canonical.split("## Task isolation", 1)[1].split(
            "## Operating contract", 1
        )[0]
        cleanup_scopes = CLEANUP_SCOPES.read_text(encoding="utf-8")
        self.assertIn("../../references/cleanup-scopes.md", task_isolation_router)

        for heading in (
            "### Task scope (default)",
            "### Repository scope (explicit)",
            "### Machine scope (approval-gated)",
        ):
            self.assertIn(heading, cleanup_scopes)
        self.assertIn("Memory", cleanup_scopes)
        self.assertIn("Graphify", cleanup_scopes)
        self.assertIn("MemPalace", cleanup_scopes)
        self.assertIn("exact validated manifest", cleanup_scopes)

        task_preservation = (
            "A pre-existing artifact stays outside deletion scope even if the task "
            "touches it."
        )
        repository_preservation = (
            "Preserve and halt on pre-existing unknown, dirty, locked, or concurrently "
            "owned state unless its discard is separately authorized."
        )
        store_refresh = (
            "Refresh and validate all three knowledge stores: native Memory, Graphify, "
            "and MemPalace."
        )

        def assert_cleanup_contract(content: str) -> None:
            task_scope = content.split("### Task scope (default)", 1)[1].split(
                "### Repository scope (explicit)", 1
            )[0]
            repository_scope = content.split(
                "### Repository scope (explicit)", 1
            )[1].split("### Machine scope (approval-gated)", 1)[0]
            machine_scope = content.split(
                "### Machine scope (approval-gated)", 1
            )[1].split("## Verification helper", 1)[0]
            normalized_task = " ".join(task_scope.split())
            normalized_repository = " ".join(repository_scope.split())
            self.assertIn("append-only ownership manifest", normalized_task)
            self.assertIn("ownership record is immutable", normalized_task)
            self.assertIn(task_preservation, normalized_task)
            self.assertIn(repository_preservation, normalized_repository)
            self.assertNotRegex(
                normalized_task,
                r"(?i)(?:unless|except) (?:the task touches|normalization requires)",
            )
            self.assertNotRegex(
                normalized_repository,
                r"unless normalization requires|without separate authorization",
            )
            for scope in (repository_scope, machine_scope):
                normalized_scope = " ".join(scope.split())
                self.assertEqual(1, normalized_scope.count(store_refresh))
                self.assertNotRegex(
                    normalized_scope,
                    r"(?i)(?:are )?(?:not|never) refreshed|refresh(?:ing)? (?:is )?optional",
                )

        assert_cleanup_contract(cleanup_scopes)

        weakening_mutations = (
            cleanup_scopes.replace(
                "even if the task touches it.",
                "even if the task touches it. Unless normalization requires otherwise.",
            ),
            cleanup_scopes.replace(
                "separately authorized.",
                "normalization requires otherwise.",
                1,
            ),
            cleanup_scopes.replace(
                "Graphify, and MemPalace.",
                "Graphify, and MemPalace. Graphify and MemPalace are not refreshed.",
                1,
            ),
        )
        for mutation in weakening_mutations:
            self.assertNotEqual(cleanup_scopes, mutation)
            with self.subTest(mutation=mutation[:80]), self.assertRaises(AssertionError):
                assert_cleanup_contract(mutation)

        forbidden = {
            "repository identity": re.compile(r"ShaftHQ|SHAFT_ENGINE", re.IGNORECASE),
            "user identity": re.compile(r"Mohab", re.IGNORECASE),
            "agent/provider identity": re.compile(
                r"Codex|Claude|Gemini|Grok|OpenAI|Anthropic", re.IGNORECASE
            ),
            "fixed default branch": re.compile(r"origin/(?:main|master|trunk)", re.IGNORECASE),
            "Windows absolute path": re.compile(r"(?<![A-Za-z0-9+.-])[A-Za-z]:[\\/]"),
            "POSIX absolute path": POSIX_ABSOLUTE_PATH,
        }
        for label, pattern in forbidden.items():
            with self.subTest(forbidden=label):
                self.assertIsNone(pattern.search(task_isolation_router + cleanup_scopes))

    def test_task_isolation_gates_planning_on_a_refreshed_primary_checkout(self):
        canonical = CANONICAL_SKILL.read_text(encoding="utf-8")
        task_isolation_router = canonical.split("## Task isolation", 1)[1].split(
            "## Operating contract", 1
        )[0]
        self.assertIn("../../references/task-isolation.md", task_isolation_router)
        task_isolation = TASK_ISOLATION.read_text(encoding="utf-8")
        normalized = " ".join(task_isolation.split())

        required = (
            "Before task-specific planning or discovery",
            "verified primary checkout",
            "clean, unlocked, exclusive state",
            "fetch and prune the configured upstream",
            "local default branch",
            "immutable upstream tip",
            "Refresh and validate native Memory, MemPalace, and Graphify from that exact revision",
            "Only after this gate",
            "dedicated `ChaosEngine/*` branch and linked worktree",
            "perform planning, discovery, and implementation there",
            "explicit continuation",
            "local or remote task branch",
            "never silently restart it from the default branch",
            "process working directory",
        )
        for phrase in required:
            with self.subTest(phrase=phrase):
                self.assertIn(phrase, normalized)

        self.assertRegex(
            normalized,
            r"fetch and prune the configured upstream.*"
            r"Refresh and validate native Memory, MemPalace, and Graphify.*"
            r"Only after this gate",
        )

    def test_posix_absolute_path_guard_is_non_vacuous(self):
        self.assertRegex("cache at /opt/private/agent-cache", POSIX_ABSOLUTE_PATH)
        self.assertRegex("config at /root/.config/private-agent", POSIX_ABSOLUTE_PATH)
        self.assertIsNone(POSIX_ABSOLUTE_PATH.search("https://example.com/opt/tool"))
        self.assertIsNone(POSIX_ABSOLUTE_PATH.search("[role](../../references/roles.md)"))
        self.assertIsNone(POSIX_ABSOLUTE_PATH.search("run ./bin/act-as-mohab.pyz"))
        self.assertIsNone(POSIX_ABSOLUTE_PATH.search("/caveman full"))
        windows_absolute = re.compile(r"(?<![A-Za-z0-9+.-])[A-Za-z]:[\\/]")
        self.assertRegex(r"cache at C:\\private\\agent-cache", windows_absolute)
        self.assertIsNone(windows_absolute.search("https://example.com/tool"))

    def test_shaft_behavior_is_selected_by_a_project_profile(self):
        profile = json.loads(SHAFT_PROFILE.read_text(encoding="utf-8"))

        self.assertEqual(1, profile["schemaVersion"])
        self.assertEqual("shaft", profile["name"])
        self.assertEqual("ChaosEngine/", profile["taskBranchPrefix"])
        self.assertEqual("ShaftHQ/SHAFT_ENGINE", profile["repository"])
        self.assertEqual("ShaftHQ/shafthq.github.io", profile["companionRepositories"][0]["repository"])
        self.assertNotIn("localRoot", profile["companionRepositories"][0])

    def test_every_compatibility_alias_selects_the_repository_profile(self):
        for alias in (
            ROOT / ".agents/skills/act-as-mohab/SKILL.md",
            ROOT / ".claude/skills/act-as-mohab/SKILL.md",
        ):
            with self.subTest(alias=alias.relative_to(ROOT)):
                content = alias.read_text(encoding="utf-8")
                links = re.findall(r"\[[^]]+\]\(([^)]+)\)", content)
                self.assertIn(REPOSITORY_ADAPTER.resolve(), {(alias.parent / link).resolve() for link in links})

    def test_portable_contract_is_scanned_and_run_by_pull_request_ci(self):
        budget = json.loads(
            (ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
        )
        for key in ("active_guidance_globs", "total_guidance_globs", "reference_scan_globs"):
            self.assertIn("chaos-engine/references/*.md", budget[key], key)
        self.assertIn(
            "tests/scripts/test_chaos_engine_portable_core.py",
            budget["harness_reachability"]["element_globs"],
        )
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertIn("tests.scripts.test_chaos_engine_portable_core", workflow)


if __name__ == "__main__":
    unittest.main()
