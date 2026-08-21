"""Portable ChaosEngine core and project-profile contract tests (#4792)."""

import importlib.util
import json
import re
import unittest
import xml.etree.ElementTree as ET  # nosec B405
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
CORE = ROOT / "chaos-engine"
INSTALLER_SPEC = importlib.util.spec_from_file_location(
    "chaos_engine_installer_portable_core", CORE / "install.py"
)
if INSTALLER_SPEC is None or INSTALLER_SPEC.loader is None:
    raise RuntimeError("portable core tests could not load install.py")
INSTALLER = importlib.util.module_from_spec(INSTALLER_SPEC)
INSTALLER_SPEC.loader.exec_module(INSTALLER)
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
    def test_validation_scope_recommends_balanced_and_ci_reruns_only_changed_tests(self):
        guidance = CANONICAL_SKILL.read_text(encoding="utf-8")

        self.assertIn("balanced default", guidance)
        self.assertIn("only tests created or edited", guidance)
        self.assertIn("Do not rerun an entire test suite merely because CI failed", guidance)
        self.assertNotIn("shaft", guidance.casefold())
        self.assertIn("license: MIT", guidance)
        self.assertNotIn("load both vendor skills", guidance.casefold())
        self.assertLessEqual(len(guidance.splitlines()), 500)
        for relative in (
            "references/context-economy.md",
            "references/script-first.md",
            "LICENSE",
            "THIRD_PARTY_NOTICES.md",
        ):
            self.assertIn(relative, guidance.replace("\\", "/"))
            self.assertTrue((CORE / relative).is_file(), relative)

    def test_portable_graphify_retrieval_is_read_only_and_ordered(self):
        guidance = (CORE / "references/graphify.md").read_text(encoding="utf-8")
        retrieval = guidance.split("## Refresh", 1)[0]
        query = retrieval.find('graphify query "<bounded structural question>"')
        diagnostic = retrieval.find("graphify diagnose multigraph")
        degraded = retrieval.find("If the cache is stale")

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
        self.assertTrue((CORE / "STANDALONE.md").is_file())
        self.assertTrue((CORE / "RESEARCH.md").is_file())
        self.assertTrue(INSTALLER.is_origin_only(Path("STANDALONE.md")))
        sources = sorted(
            path
            for path in CORE.rglob("*")
            if path.is_file()
            and "profiles" not in path.relative_to(CORE).parts
            and "vendor" not in path.relative_to(CORE).parts
            and "__pycache__" not in path.relative_to(CORE).parts
            and path.suffix != ".pyc"
            and not INSTALLER.is_origin_only(path.relative_to(CORE))
        )
        self.assertTrue(sources, "portable core sources must exist")
        for path in sources:
            text = path.read_text(encoding="utf-8")
            for label, pattern in forbidden.items():
                with self.subTest(path=path.relative_to(ROOT), forbidden=label):
                    self.assertIsNone(pattern.search(text))

    def test_vendor_companions_are_pinned_trees_outside_owned_core_policy(self):
        for name in ("caveman", "ponytail"):
            vendor = CORE / "vendor" / name
            pin = json.loads((vendor / "PIN.json").read_text(encoding="utf-8"))
            self.assertTrue((vendor / "skills" / name / "SKILL.md").is_file())
            self.assertGreaterEqual(len(pin["files"]), 3)
            for relative in pin["files"]:
                self.assertTrue((vendor / relative).is_file(), relative)

    def test_companions_are_always_on_ultra_and_beat_host_prose(self):
        skill = CANONICAL_SKILL.read_text(encoding="utf-8")
        hooks = (CORE / "references/lifecycle-hooks.md").read_text(encoding="utf-8")
        agents = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
        lowered = skill.casefold()

        self.assertIn("Load both companion skills at the start of every task", skill)
        self.assertIn("selects **ultra**", lowered)
        self.assertNotIn("Default intensity remains each companion's own", skill)
        self.assertIn("yield to the companions", lowered)
        compact_hooks = " ".join(hooks.casefold().split())
        self.assertIn(
            "still apply companions through the entrypoint load", compact_hooks
        )
        self.assertNotIn("load on invoke", lowered)
        self.assertNotIn("Keep prose natural", agents)
        self.assertIn("Caveman", agents)

    def test_harness_changes_require_five_host_compatibility(self):
        skill = CANONICAL_SKILL.read_text(encoding="utf-8")
        lowered = skill.casefold()
        self.assertIn("harness change", lowered)
        self.assertIn("provider-agnostic", lowered)
        self.assertIn("every supported host adapter", lowered)
        self.assertIn("silently no-ops the others", lowered)

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

    def test_task_isolation_gates_planning_on_a_clean_primary_checkout(self):
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
            "After this gate",
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
            r"fetch and prune the configured upstream.*After this gate.*"
            r"Ordinary tasks launch no background store processes",
        )

    def test_ordinary_tasks_do_not_maintain_or_wait_for_knowledge_stores(self):
        canonical = CANONICAL_SKILL.read_text(encoding="utf-8")
        task_isolation = TASK_ISOLATION.read_text(encoding="utf-8")
        retrieval = (CORE / "references/retrieve-first.md").read_text(encoding="utf-8")
        graphify = (CORE / "references/graphify.md").read_text(encoding="utf-8")
        planning = (CORE / "references/work-github-planning.md").read_text(encoding="utf-8")
        combined = " ".join((canonical, task_isolation, retrieval, graphify, planning)).lower()

        self.assertNotIn(
            "refresh and validate native memory, mempalace, and graphify from that exact revision",
            task_isolation.lower(),
        )
        for phrase in (
            "advisory for ordinary tasks",
            "one attempt",
            "no retries",
            "no background store processes",
            "materially new sanitized evidence",
            "failure to reach github is non-blocking",
        ):
            self.assertIn(phrase, combined)
        self.assertIn("status", retrieval.lower())
        self.assertIn("doctor", retrieval.lower())
        self.assertIn("strict", retrieval.lower())
        receipt = (CORE / "references/research-receipt.md").read_text(encoding="utf-8").lower()
        self.assertIn("after a plan is approved", receipt)
        self.assertIn("first implementation mutation", receipt)
        self.assertIn("after a plan is approved", retrieval.lower())
        self.assertNotIn("graphify refresh", planning.lower())
        self.assertIn("maintenance owner", planning.lower())

    def test_planning_grounds_issue_state_before_first_commit(self):
        planning = (CORE / "references/work-github-planning.md").read_text(encoding="utf-8")

        for phrase in (
            "First grounding table",
            "issue state",
            "closing PRs",
            "live-file contradiction",
            "remaining-acceptance",
            "next RED",
            "kill-after-merge",
            "stream count",
            "host-local",
            "plan.md",
            "not enough",
        ):
            with self.subTest(phrase=phrase):
                self.assertIn(phrase, planning)

    def test_project_scoped_memory_objects_do_not_set_branch_or_task(self):
        gotcha = (
            ROOT
            / ".memory/memory/gotchas"
            / "grok-lifecycle-hooks-must-merge-not-overwrite-foreign-handlers.json"
        )
        payload = json.loads(gotcha.read_text(encoding="utf-8"))
        self.assertEqual("project", payload["scope"]["kind"])
        self.assertIsNone(payload["scope"]["task"])
        self.assertIsNone(payload["scope"]["branch"])

        offenders = []
        for path in (ROOT / ".memory/memory").rglob("*.json"):
            scope = json.loads(path.read_text(encoding="utf-8")).get("scope") or {}
            if scope.get("kind") != "project":
                continue
            if scope.get("branch") is not None or scope.get("task") is not None:
                offenders.append(str(path.relative_to(ROOT)))
        self.assertEqual([], offenders)

    def test_graphify_cache_is_only_an_untrusted_positive_lead(self):
        graphify = (CORE / "references/graphify.md").read_text(encoding="utf-8").lower()
        for phrase in (
            "any available cache",
            "untrusted lead",
            "verify every returned path",
            "targeted `rg`",
            "never infer completeness",
            "no callers",
        ):
            self.assertIn(phrase, graphify)

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
        profile_text = SHAFT_PROFILE.read_text(encoding="utf-8")
        profile = json.loads(profile_text)

        self.assertEqual(1, profile["schemaVersion"])
        self.assertEqual("shaft", profile["name"])
        self.assertEqual("ChaosEngine/", profile["taskBranchPrefix"])
        self.assertEqual("ShaftHQ/SHAFT_ENGINE", profile["repository"])
        self.assertEqual("ShaftHQ/shafthq.github.io", profile["companionRepositories"][0]["repository"])
        self.assertNotIn("localRoot", profile_text)
        self.assertNotIn("../shafthq.github.io", profile_text)

    def test_shaft_user_facing_changes_require_companion_docs_prs(self):
        entry = (CORE / "profiles/shaft/entrypoint.md").read_text(encoding="utf-8")
        playbook = (
            CORE
            / "profiles/shaft/references/playbooks/public-behavior-docs-synchronizer.md"
        ).read_text(encoding="utf-8")
        agents = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
        profile_text = SHAFT_PROFILE.read_text(encoding="utf-8")
        profile = json.loads(profile_text)
        windows_absolute = re.compile(r"(?<![A-Za-z0-9+.-])[A-Za-z]:[\\/]")

        for phrase in (
            "companion PR",
            "same delivery",
            "description of the change",
            "screenshots where a human sees UI",
            "human-facing instructions",
            "replay-proven snippets",
            "locator policy",
            "AI-supported details",
            "properties",
            "exact commands",
            "never a fixed sibling path",
        ):
            with self.subTest(surface="entrypoint", phrase=phrase):
                self.assertIn(phrase, entry)
        for phrase in (
            "companion PR",
            "same delivery",
            "description of the change",
            "screenshots where a human sees UI",
            "human-facing instructions",
            "replay-proven snippets",
            "locator policy",
            "author-written id",
            "ARIA role",
            "native relative xpath",
            "discover",
            "never a fixed sibling path",
            "AI-supported details",
            "properties",
            "exact commands",
        ):
            with self.subTest(surface="playbook", phrase=phrase):
                self.assertIn(phrase, playbook)

        def assert_playbook_first_occurrence_nouns(content: str) -> None:
            self.assertEqual(2, content.count("AI-supported details"))
            self.assertEqual(2, content.count("properties"))
            self.assertEqual(2, content.count("exact commands"))
            self.assertEqual(2, content.count("never a fixed sibling path"))
            self.assertEqual(2, content.count("description of the change"))
            self.assertEqual(2, content.count("locator policy"))

        assert_playbook_first_occurrence_nouns(playbook)
        for noun, weakened_noun in (
            ("AI-supported details", "model-supported details"),
            ("properties", "settings"),
            ("exact commands", "exact invocations"),
            ("never a fixed sibling path", "never a hardcoded sibling path"),
            ("description of the change", "description of the edit"),
            ("locator policy", "selector policy"),
        ):
            weakened = playbook.replace(noun, weakened_noun, 1)
            self.assertNotEqual(playbook, weakened)
            with self.subTest(first_occurrence=noun), self.assertRaises(AssertionError):
                assert_playbook_first_occurrence_nouns(weakened)

        self.assertEqual(
            "ShaftHQ/shafthq.github.io",
            profile["companionRepositories"][0]["repository"],
        )
        self.assertNotIn("localRoot", profile_text)
        self.assertNotIn("../shafthq.github.io", profile_text)
        self.assertNotIn("../shafthq.github.io", entry)
        self.assertNotIn("../shafthq.github.io", playbook)
        self.assertIn("[ChaosEngine](.agents/skills/chaos-engine/SKILL.md)", agents)
        self.assertIn("the only router and\nworking-policy owner", agents)
        self.assertNotIn("../shafthq.github.io", agents)
        self.assertIsNone(windows_absolute.search(entry))
        self.assertIsNone(windows_absolute.search(playbook))
        self.assertNotIn("C:\\Users\\Mohab", entry)
        self.assertNotIn("C:\\Users\\Mohab", playbook)

    def test_every_compatibility_alias_selects_the_repository_profile(self):
        for alias in (
            ROOT / ".agents/skills/act-as-mohab/SKILL.md",
            ROOT / ".claude/skills/act-as-mohab/SKILL.md",
        ):
            with self.subTest(alias=alias.relative_to(ROOT)):
                content = alias.read_text(encoding="utf-8")
                links = re.findall(r"\[[^]]+\]\(([^)]+)\)", content)
                self.assertIn(REPOSITORY_ADAPTER.resolve(), {(alias.parent / link).resolve() for link in links})

    def test_portable_local_coding_delegate_is_optional_and_routed(self):
        skill = CORE / "skills/local-coding-delegate/SKILL.md"
        probe = CORE / "skills/local-coding-delegate/scripts/probe_hardware.py"
        portable_entry = CORE / "profiles/portable/entrypoint.md"
        portable_routing = CORE / "profiles/portable/references/routing.md"
        budget = json.loads(
            (ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
        )

        self.assertTrue(skill.is_file())
        self.assertTrue(probe.is_file())
        self.assertIn("local-coding-delegate/SKILL.md", portable_entry.read_text(encoding="utf-8"))
        self.assertIn("local-coding-delegate/SKILL.md", portable_routing.read_text(encoding="utf-8"))
        self.assertIn(
            "Close that writer after its PR exists.",
            skill.read_text(encoding="utf-8"),
        )
        self.assertIn(
            "local-coding-delegate",
            budget["expected_skill_names"]["chaos-engine/skills"],
        )
        self.assertIn(
            "chaos-engine/skills/local-coding-delegate/SKILL.md",
            budget["total_guidance_globs"],
        )
        self.assertIn(
            "chaos-engine/skills/local-coding-delegate/SKILL.md",
            budget["reference_scan_globs"],
        )
        self.assertNotIn(
            "chaos-engine/skills/local-coding-delegate/SKILL.md",
            budget["active_guidance_globs"],
        )
        for files in budget["host_contexts"].values():
            self.assertNotIn(
                "chaos-engine/skills/local-coding-delegate/SKILL.md", files
            )

    def test_portable_local_coding_delegate_and_routing_do_not_leak_shaft_facts(self):
        skill_root = CORE / "skills/local-coding-delegate"
        portable_routing = CORE / "profiles/portable/references/routing.md"
        leaks = (
            r"D:\AI",
            "shaft-java-agent",
            "qwen",
            "Ollama",
            "Aider",
            "SHAFT_LOCAL_AI",
        )

        self.assertTrue(skill_root.is_dir(), "optional portable skill must exist to be scanned")
        texts = [portable_routing.read_text(encoding="utf-8")]
        for path in skill_root.rglob("*"):
            if path.is_file() and "__pycache__" not in path.parts:
                texts.append(path.read_text(encoding="utf-8", errors="ignore"))
        combined = "\n".join(texts)
        for leak in leaks:
            with self.subTest(leak=leak):
                self.assertNotIn(leak, combined)

    def test_shaft_workstation_playbook_is_routed_and_names_the_loop(self):
        playbook = CORE / "profiles/shaft/references/playbooks/workstation-local-coding-agent.md"
        shaft_entry = CORE / "profiles/shaft/entrypoint.md"
        shaft_routing = CORE / "profiles/shaft/references/routing.md"
        required = (
            "scripts/local-coding-agent/shaft-java-agent.ps1",
            "scripts/local-coding-agent/shaft-architect.ps1",
            "scripts/local-coding-agent/shaft-local-ai-stop.ps1",
            "scripts/agents/knowledge_stores.py",
            "The host session orchestrates.",
            "One implementer per batch.",
            "The writer stops when the PR exists.",
            "Owner-gated train, export, and deploy stay out of tree only.",
            "Never auto-train from harvest.",
            "Never overwrite qwen2.5-coder:7b.",
            "Deploy a new Ollama name.",
        )

        self.assertTrue(playbook.is_file())
        body = playbook.read_text(encoding="utf-8")
        for token in required:
            with self.subTest(token=token):
                self.assertIn(token, body)
        self.assertIn("playbooks/workstation-local-coding-agent.md", shaft_routing.read_text(encoding="utf-8"))
        self.assertIn("playbooks/workstation-local-coding-agent.md", shaft_entry.read_text(encoding="utf-8"))

    def test_local_coding_probe_is_stdlib_and_classifies_or_refuses(self):
        import ast
        import importlib.util

        probe = CORE / "skills/local-coding-delegate/scripts/probe_hardware.py"
        self.assertTrue(probe.is_file())
        tree = ast.parse(probe.read_text(encoding="utf-8"))
        imported = {
            alias.name.split(".", 1)[0]
            for node in tree.body
            if isinstance(node, (ast.Import, ast.ImportFrom))
            for alias in (node.names if isinstance(node, ast.Import) else [ast.alias(node.module or "", None)])
        }
        allowed = {
            "__future__",
            "argparse",
            "json",
            "os",
            "platform",
            "ctypes",
            "subprocess",
            "sys",
            "pathlib",
        }
        self.assertTrue(imported <= allowed, imported - allowed)

        spec = importlib.util.spec_from_file_location("probe_hardware", probe)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        gib = 1024 ** 3
        self.assertEqual(
            "refuse",
            module.classify(ram_bytes=4 * gib, gpu_bytes=None, os_name="Windows")[
                "recommendation"
            ],
        )
        self.assertEqual(
            "small",
            module.classify(ram_bytes=12 * gib, gpu_bytes=None, os_name="Linux")[
                "recommendation"
            ],
        )
        self.assertEqual(
            "medium",
            module.classify(ram_bytes=24 * gib, gpu_bytes=6 * gib, os_name="Darwin")[
                "recommendation"
            ],
        )
        self.assertEqual(
            "large",
            module.classify(ram_bytes=64 * gib, gpu_bytes=16 * gib, os_name="Linux")[
                "recommendation"
            ],
        )

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

    def test_planning_keeps_asking_follow_ups_then_runs_unattended(self):
        """#5248: ask-once-then-unattended cannot silently return."""
        planning = (CORE / "references/work-github-planning.md").read_text(encoding="utf-8")
        receipt = (CORE / "references/research-receipt.md").read_text(encoding="utf-8")
        consult = (CORE / "references/consult-first.md").read_text(encoding="utf-8")
        skill = CANONICAL_SKILL.read_text(encoding="utf-8")
        forbidden = "Ask once, at the start, then go unattended"
        for text, name in (
            (planning, "work-github-planning.md"),
            (receipt, "research-receipt.md"),
            (consult, "consult-first.md"),
            (skill, "SKILL.md"),
        ):
            with self.subTest(file=name):
                self.assertNotIn(forbidden, text)
        compact_planning = re.sub(r"\s+", " ", planning)
        compact_receipt = re.sub(r"\s+", " ", receipt)
        compact_consult = re.sub(r"\s+", " ", consult)
        compact_skill = re.sub(r"\s+", " ", skill)
        self.assertIn("keep asking follow-ups until the plan is decision-ready", compact_planning)
        self.assertIn("go completely unattended", compact_planning)
        self.assertIn("consultant agent", compact_planning)
        self.assertIn("keep asking follow-ups until the plan is decision-ready", compact_receipt)
        self.assertIn("consultant agent", compact_receipt)
        self.assertIn("consultant agent", compact_consult)
        self.assertIn("keep asking follow-ups until the plan is decision-ready", compact_skill)
        self.assertIn("consultant agent", compact_skill)


class ChaosEngineOrchestratorModeTest(unittest.TestCase):
    """Portable orchestrator-mode pins (#5210, #5246, #5249)."""

    def _skill(self) -> str:
        return re.sub(r"\s+", " ", CANONICAL_SKILL.read_text(encoding="utf-8"))

    def _delegation(self) -> str:
        return re.sub(
            r"\s+",
            " ",
            (CORE / "references/delegation.md").read_text(encoding="utf-8"),
        )

    def _combined(self) -> str:
        texts = [
            CANONICAL_SKILL.read_text(encoding="utf-8"),
            (CORE / "references/delegation.md").read_text(encoding="utf-8"),
            (CORE / "references/roles.md").read_text(encoding="utf-8"),
            (CORE / "references/orchestrator-bootstrap.md").read_text(encoding="utf-8"),
        ]
        return re.sub(r"\s+", " ", "\n".join(texts))

    def test_entrypoint_auto_switches_and_forbids_self_work_with_serial_default(self):
        skill = self._skill()
        self.assertIn('Do not wait for the owner to say "orchestrate"', skill)
        self.assertIn("do no task work yourself", skill.lower())
        self.assertIn("one writer at a time", skill.lower())
        self.assertRegex(skill, r"1[–-]4")
        self.assertIn("still shows the live status table", skill)
        self.assertIn("never start an edit in the same breath as adopting", skill.lower())

    def test_delegation_pins_status_table_serial_cap_and_learning_session_before_kill(self):
        delegation = self._delegation()
        for column in (
            "ID / work item",
            "Mode stream",
            "Status",
            "Owner / agent",
            "Dependency",
            "Last update",
            "Details / evidence",
            "Next action",
        ):
            with self.subTest(column=column):
                self.assertIn(column, delegation)
        for status in (
            "planned",
            "in progress",
            "blocked",
            "review",
            "completed",
            "out of scope",
        ):
            with self.subTest(status=status):
                self.assertIn(status, delegation)
        self.assertIn("Learning Session before kill", delegation)
        self.assertIn("Refuse a requested cap above 4", delegation)
        self.assertRegex(delegation, r"1[–-]4")
        self.assertIn("File-overlapping writers never run in parallel", delegation)
        self.assertIn("one writer at a time", delegation.lower())

    def test_portable_orchestrator_groups_fewest_prs_and_keeps_working(self):
        delegation = self._delegation()
        for phrase in (
            "stay available",
            "live status table",
            "fewest PRs that still keep one problem per issue",
            "keep working until every in-scope ticket is delivered",
            "Do not treat planning, a status table, or one PR as session complete",
        ):
            with self.subTest(phrase=phrase):
                self.assertIn(phrase, delegation)
        combined = self._combined()
        self.assertIn("stay available", combined)
        self.assertIn("live status table", combined)


if __name__ == "__main__":
    unittest.main()
