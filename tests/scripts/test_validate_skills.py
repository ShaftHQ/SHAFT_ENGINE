"""Skill-hygiene validator tests, plus the plugin marketplace manifest check.

`.claude-plugin/marketplace.json` lists every distributable skill directory by
hand. It is read by the Claude plugin marketplace route only -- the `npx skills
add` command in `README.md:75` passes a **tree URL**
(`.../tree/main/shaft-skills`), which enumerates the directory and never reads
this manifest. So a stale or misspelled entry breaks the plugin route silently
while every in-repo path, and the tree-URL install, keep working.

The marketplace check lives here rather than in `scripts/ci/validate_skills.py`
because that validator is *shipped*: `scripts/mcp/install_shaft_mcp.py` lists it
in `AGENT_VALIDATION_SCRIPT_FILES` and downloads it into any user project that
has an `AGENTS.md` scaffold. No user project has a
`.claude-plugin/marketplace.json` -- the installer never creates one -- so a
manifest check inside the shipped validator makes
`validate_agent_setup.py`, the aggregate validator two playbooks tell agents to
run, fail for every onboarded user. Tests are not shipped;
`tests/scripts/test_validate_skills.py` is already in pr-gate's
`agent_guidance` path filter, and the `agent-guidance` leg already runs this
module, so the check keeps the same trigger with no shipped blast radius.

This assertion is DUPLICATED, not moved:
`test_install_shaft_mcp.py::test_marketplace_lists_every_canonical_skill_directory`
asserts the same invariant and is untouched. Two implementations are justified
here only because they have different triggers -- that one runs in the
`installer-verify` leg, gated by the `intellij` filter, which does not list the
manifest, so a manifest-only edit ran no check at all (#4478). They order
differently (that one sorts `Path` objects, which is case-insensitive on
Windows and case-sensitive on POSIX; this one sorts strings, which is neither),
latent while every skill name is lowercase. Tracked separately rather than
unified here.

Both used to derive their unbacked/unlisted/order diff from `*/SKILL.md`, so a
`shaft-skills/` directory with no `SKILL.md` never entered that set and could
not be reported as unlisted by either. `plugin_errors` below enumerates
directories first and requires the file (#4501), catching it before the diff
even runs, and the duplicate in `test_install_shaft_mcp.py` took the same
inversion in #4511 -- so neither leg is blind to it now.
"""

import json
import tempfile
import unittest
from pathlib import Path, PurePosixPath, PureWindowsPath

from scripts.ci.validate_agent_guidance import expand_reported_globs, require_glob_list
from scripts.ci.validate_skills import validate_repository, validate_skill

MARKETPLACE_PATH = ".claude-plugin/marketplace.json"


def source_shape_escapes(source: str) -> bool:
    """True when `source` is rooted, drive-anchored, or traverses upward.

    Decided under BOTH spellings, never under whichever one `Path` happens to
    be. `marketplace.json` is a single cross-platform artifact, so a source
    must mean the same thing to every host that reads it -- and the flavours
    disagree about exactly the shapes that matter (measured, see
    `SourceShapeTest`): `/etc` is absolute to POSIX but driveless-and-rooted to
    Windows, `C:/Windows` is a drive to Windows and an ordinary relative
    directory name to POSIX, and both backslash spellings are structure to
    Windows and one long filename to POSIX.

    `.root` rather than `.is_absolute()`: `PureWindowsPath('/etc')` is rooted
    but not absolute (it has no drive), which is precisely the case that let a
    POSIX-absolute source through on a Windows host.
    """
    return any(
        flavour.root or flavour.drive or ".." in flavour.parts
        for flavour in (PurePosixPath(source), PureWindowsPath(source))
    )


def source_escapes_root(root: Path, source: str) -> bool:
    """True when `source` does not stay inside `root`.

    Two halves. The shape half above rejects a source that is written as an
    escape. This half asks the question that actually matters -- does the join
    land inside the repository? -- from RESOLVED paths, because a path can be
    spelled two ways and `relative_to` compares spellings rather than locations
    (#4497). The root itself is inside the repository, so it is not an escape;
    an empty source directory is reported as `marketplace-source-empty`.
    """
    if source_shape_escapes(source):
        return True
    resolved_root = root.resolve()
    candidate = (root / source).resolve()
    return candidate != resolved_root and resolved_root not in candidate.parents


def issue(code: str, message: str) -> dict[str, str]:
    """Create a manifest issue against the marketplace path."""
    return {"code": code, "path": MARKETPLACE_PATH, "message": message}


def directories_missing_skill_md(root: Path, source: str) -> list[str]:
    """Names of directories directly under `source`, excluding `references`,
    that hold no `SKILL.md` (#4501).

    Every check below derives the on-disk skill set FROM `*/SKILL.md`, so a
    directory that never got one -- half-authored, or one whose `SKILL.md`
    was deleted or renamed -- never enters that set and cannot be reported by
    an unbacked/unlisted/order diff built from it. This enumerates
    directories first and requires the file the other way round, the same
    order the orphaned `validate-shaft-skills.yml` workflow used.
    """
    return sorted(
        entry.name
        for entry in (root / source).iterdir()
        if entry.is_dir() and entry.name != "references" and not (entry / "SKILL.md").is_file()
    )


def plugin_errors(root: Path, plugin: dict, index: int) -> list[dict[str, str]]:
    """Check one marketplace plugin's skills[] against its source directory."""
    label = plugin.get("name") if isinstance(plugin.get("name"), str) else f"plugins[{index}]"
    source = plugin.get("source")
    if not isinstance(source, str) or not source:
        return [issue("marketplace-plugin-shape", f"plugin '{label}' must declare a string source")]

    # `root / source` silently DISCARDS root when source is absolute, and
    # `Path.glob` rejects an absolute pattern outright, so both are refused
    # before either is reached.
    if source_escapes_root(root, source):
        return [
            issue(
                "marketplace-source-escapes-root",
                f"plugin '{label}' source must be a relative path inside the repository: {source}",
            )
        ]

    if not (root / source).is_dir():
        return [
            issue("marketplace-source-missing", f"plugin '{label}' source directory does not exist: {source}")
        ]

    # Claude Code plugin roots auto-discover `skills/*/SKILL.md`; legacy skill
    # collections keep their explicit marketplace `skills` list.
    listed, list_errors = require_glob_list(plugin, "skills")
    skill_source = source
    if list_errors:
        if (root / source / ".claude-plugin" / "plugin.json").is_file():
            listed = None
            skill_source = f"{source}/skills"
        else:
            return [
                issue(
                    "marketplace-empty-skills",
                    f"plugin '{label}' skills must be a non-empty list",
                )
            ]

    if not (root / skill_source).is_dir():
        return [
            issue(
                "marketplace-source-empty",
                f"plugin '{label}' source '{skill_source}' contains no <skill>/SKILL.md directories",
            )
        ]

    missing_skill_md = [
        issue(
            "marketplace-skill-missing-skillmd",
            f"plugin '{label}' source directory '{source}/{name}' has no SKILL.md",
        )
        for name in directories_missing_skill_md(root, skill_source)
    ]

    # A source directory holding no `*/SKILL.md` reports nothing when it
    # matches nothing, so route the disk side through the reporting expander
    # too: an empty match is an `empty-glob`, never a silent pass.
    pattern = f"{skill_source}/*/SKILL.md"
    found, glob_errors = expand_reported_globs(root, [pattern], f"plugin '{label}' source")
    if glob_errors:
        return missing_skill_md + [
            issue(
                "marketplace-source-empty",
                f"plugin '{label}' source '{skill_source}' contains no <skill>/SKILL.md directories",
            )
        ]

    # The vercel-labs skills CLI resolves each entry's PARENT as the scan
    # container, so entries must name individual skill directories -- never a
    # container -- for both it and Claude Code to discover the same set.
    on_disk = sorted(f"./{path.parent.name}" for path in found)
    entries = on_disk if listed is None else [str(entry) for entry in listed]
    errors = [
        issue(
            "marketplace-entry-unbacked",
            f"plugin '{label}' lists '{entry}' but "
            f"{source}/{entry.removeprefix('./')}/SKILL.md does not exist",
        )
        for entry in sorted(set(entries) - set(on_disk))
    ]
    errors.extend(
        issue(
            "marketplace-skill-unlisted",
            f"plugin '{label}' does not list '{entry}', which exists under {source}",
        )
        for entry in sorted(set(on_disk) - set(entries))
    )
    if not errors and entries != on_disk:
        errors.append(
            issue(
                "marketplace-entry-order",
                f"plugin '{label}' skills must be sorted and free of duplicates: {on_disk}",
            )
        )
    return missing_skill_md + errors


def marketplace_errors(root: Path) -> list[dict[str, str]]:
    """Check the plugin marketplace manifest against the skill dirs on disk."""
    manifest_path = root / MARKETPLACE_PATH
    if not manifest_path.is_file():
        return [issue("marketplace-missing", "the plugin marketplace manifest is missing")]
    # One bad byte must not raise out of here: an uncaught UnicodeDecodeError
    # or AttributeError would abort every remaining check in whatever aggregate
    # runs this, converting one malformed file into a total blackout.
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        return [issue("marketplace-malformed", f"cannot be read as JSON: {error}")]
    if not isinstance(manifest, dict):
        return [issue("marketplace-malformed", f"must be a JSON object, got {type(manifest).__name__}")]

    plugins = manifest.get("plugins")
    if not isinstance(plugins, list) or not plugins:
        return [issue("marketplace-malformed", "plugins must be a non-empty list")]

    errors: list[dict[str, str]] = []
    for index, plugin in enumerate(plugins):
        if not isinstance(plugin, dict):
            errors.append(issue("marketplace-plugin-shape", f"plugins[{index}] must be an object"))
            continue
        errors.extend(plugin_errors(root, plugin, index))
    return errors


class ValidateSkillsTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)
        self.budget_path = self.root / "scripts/ci/agent_guidance_budget.json"
        self.budget = {"skill_budgets": {".agents/skills": {"max_skill_md_bytes": 400}}}
        self.write_budget()
        self.write(
            ".agents/skills/example/SKILL.md",
            "---\n"
            "name: example\n"
            "description: >-\n"
            "  Do the example thing well. Use when the task looks like an\n"
            "  example so future readers know when to reach for it.\n"
            "---\n\n"
            "# Example\n\nBody content that is non-empty.\n"
            "See `references/detail.md` for more.\n",
        )
        self.write(".agents/skills/example/references/detail.md", "# Detail\n")

    def tearDown(self):
        self.temporary_directory.cleanup()

    def write(self, relative_path, content):
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def write_budget(self):
        self.budget_path.parent.mkdir(parents=True, exist_ok=True)
        self.budget_path.write_text(json.dumps(self.budget), encoding="utf-8")

    def codes(self):
        return {error["code"] for error in validate_repository(self.root, self.budget_path)}

    def test_valid_skill_passes(self):
        self.assertEqual(validate_repository(self.root, self.budget_path), [])

    def test_folded_block_scalar_description_is_read_in_full(self):
        self.write(
            ".agents/skills/example/SKILL.md",
            "---\nname: example\ndescription: >-\n  x\n---\n\n# Example\n\nBody.\n",
        )
        self.assertIn("description-too-thin", self.codes())

    def test_rejects_missing_skill_md(self):
        (self.root / ".agents/skills/empty").mkdir(parents=True)
        self.assertIn("skill-missing", self.codes())

    def test_rejects_malformed_frontmatter(self):
        self.write(".agents/skills/example/SKILL.md", "# No frontmatter at all\n")
        self.assertIn("frontmatter-malformed", self.codes())

    def test_rejects_name_mismatch(self):
        self.write(
            ".agents/skills/example/SKILL.md",
            "---\nname: not-example\ndescription: Use when testing name mismatches.\n---\n\nBody.\n",
        )
        self.assertIn("frontmatter-name", self.codes())

    def test_rejects_missing_description(self):
        self.write(
            ".agents/skills/example/SKILL.md",
            "---\nname: example\n---\n\nBody.\n",
        )
        self.assertIn("frontmatter-description", self.codes())

    def test_rejects_description_missing_trigger(self):
        self.write(
            ".agents/skills/example/SKILL.md",
            "---\nname: example\ndescription: A skill that does example things nicely.\n---\n\nBody.\n",
        )
        self.assertIn("description-missing-trigger", self.codes())

    def test_rejects_empty_body(self):
        self.write(
            ".agents/skills/example/SKILL.md",
            "---\nname: example\ndescription: Use when the body is empty.\n---\n\n   \n",
        )
        self.assertIn("body-empty", self.codes())

    def test_rejects_dead_backtick_reference(self):
        self.write(
            ".agents/skills/example/SKILL.md",
            "---\nname: example\ndescription: Use when checking dead references.\n---\n\n"
            "Body. See `references/missing.md`.\n",
        )
        self.assertIn("dead-reference", self.codes())

    def test_rejects_dead_markdown_link_reference(self):
        self.write(
            ".agents/skills/example/SKILL.md",
            "---\nname: example\ndescription: Use when checking dead references.\n---\n\n"
            "Body. See [detail](references/missing.md) for more.\n",
        )
        self.assertIn("dead-reference", self.codes())

    def test_rejects_oversized_skill_md(self):
        self.budget["skill_budgets"][".agents/skills"]["max_skill_md_bytes"] = 10
        self.write_budget()
        self.assertIn("byte-budget", self.codes())

    def test_reports_missing_budget_config(self):
        self.budget_path.write_text(json.dumps({}), encoding="utf-8")
        self.assertIn("budget-config", self.codes())

    def test_current_repository_skills_are_valid(self):
        repository_root = Path(__file__).resolve().parents[2]
        self.assertEqual(validate_repository(repository_root), [])

    def test_shipped_validator_ignores_a_project_without_a_marketplace_manifest(self):
        """The shipped validator must stay clean in a user project (#4478 F1).

        `scripts/mcp/install_shaft_mcp.py` downloads `validate_skills.py` into
        any project with an `AGENTS.md` scaffold, and no such project has a
        `.claude-plugin/marketplace.json`. This fixture root is exactly that
        shape, so a manifest check leaking back into the shipped validator
        fails here.
        """
        self.assertFalse((self.root / MARKETPLACE_PATH).exists())
        self.assertEqual(validate_repository(self.root, self.budget_path), [])


class MarketplaceManifestTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)
        self.write("shaft-skills/example-pack/SKILL.md", "# Example pack\n")
        self.marketplace = {
            "name": "fixture",
            "plugins": [
                {
                    "name": "fixture-pack",
                    "source": "./shaft-skills",
                    "skills": ["./example-pack"],
                }
            ],
        }
        self.write_marketplace()

    def tearDown(self):
        self.temporary_directory.cleanup()

    def write(self, relative_path, content):
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def write_marketplace(self):
        self.write(MARKETPLACE_PATH, json.dumps(self.marketplace))

    def codes(self):
        return {error["code"] for error in marketplace_errors(self.root)}

    def second_plugin(self, **overrides):
        plugin = {"name": "second-pack", "source": "./shaft-skills", "skills": ["./example-pack"]}
        plugin.update(overrides)
        self.marketplace["plugins"].append(plugin)
        self.write_marketplace()

    def test_matching_manifest_passes(self):
        self.assertEqual(marketplace_errors(self.root), [])

    def test_rejects_marketplace_entry_without_skill_directory(self):
        self.marketplace["plugins"][0]["skills"] = ["./example-pack", "./ghost-pack"]
        self.write_marketplace()
        self.assertIn("marketplace-entry-unbacked", self.codes())

    def test_rejects_skill_directory_missing_from_marketplace(self):
        self.write("shaft-skills/unlisted-pack/SKILL.md", "# Unlisted\n")
        self.assertIn("marketplace-skill-unlisted", self.codes())

    def test_rejects_source_directory_without_skill_md(self):
        """A directory with no SKILL.md never enters the glob-derived set below,
        so it was invisible to every other check here (#4501): it can be
        neither unbacked nor unlisted, because it never enters the set being
        compared. Enumerated by directory, not by file, so it is caught even
        though nothing else lists or expects it.
        """
        (self.root / "shaft-skills/no-skill-md").mkdir()
        self.assertIn("marketplace-skill-missing-skillmd", self.codes())

    def test_rejects_manifest_entry_whose_skill_md_was_deleted(self):
        """The other #4501 mutation: delete a real SKILL.md, keep the entry
        listed. The directory still exists and is still listed, so nothing
        about the entry/unbacked/unlisted diff below would ever notice.
        """
        (self.root / "shaft-skills/example-pack/SKILL.md").unlink()
        self.assertIn("marketplace-skill-missing-skillmd", self.codes())

    def test_does_not_flag_the_references_support_directory(self):
        """`references/` is a legitimate non-skill support directory (#4501
        suggested fix); it must not be required to hold a SKILL.md.
        """
        (self.root / "shaft-skills/references").mkdir()
        self.assertNotIn("marketplace-skill-missing-skillmd", self.codes())

    def test_rejects_marketplace_entry_order_drift(self):
        self.write("shaft-skills/second-pack/SKILL.md", "# Second\n")
        self.marketplace["plugins"][0]["skills"] = ["./second-pack", "./example-pack"]
        self.write_marketplace()
        self.assertIn("marketplace-entry-order", self.codes())

    def test_rejects_missing_marketplace_manifest(self):
        (self.root / MARKETPLACE_PATH).unlink()
        self.assertIn("marketplace-missing", self.codes())

    def test_rejects_missing_plugin_source_directory(self):
        self.marketplace["plugins"][0]["source"] = "./no-such-pack-root"
        self.write_marketplace()
        self.assertIn("marketplace-source-missing", self.codes())

    def test_rejects_plugin_without_a_source(self):
        del self.marketplace["plugins"][0]["source"]
        self.write_marketplace()
        self.assertIn("marketplace-plugin-shape", self.codes())

    def test_rejects_malformed_marketplace_manifest(self):
        self.write(MARKETPLACE_PATH, "{not json")
        self.assertIn("marketplace-malformed", self.codes())

    # --- fail-open shapes: every one of these passed before #4478 round 2 ---

    def test_rejects_plugin_without_a_skills_list(self):
        del self.marketplace["plugins"][0]["skills"]
        self.write_marketplace()
        self.assertIn("marketplace-empty-skills", self.codes())

    def test_rejects_empty_skills_list(self):
        self.marketplace["plugins"][0]["skills"] = []
        self.write_marketplace()
        self.assertIn("marketplace-empty-skills", self.codes())

    def test_rejects_empty_skills_list_on_a_later_plugin(self):
        """Plugin 1 correct must not launder plugin 2 installing zero skills."""
        self.second_plugin(skills=[])
        self.assertIn("marketplace-empty-skills", self.codes())

    def test_rejects_source_directory_holding_no_skills(self):
        (self.root / "empty-pack-root").mkdir()
        self.marketplace["plugins"][0]["source"] = "./empty-pack-root"
        self.write_marketplace()
        self.assertIn("marketplace-source-empty", self.codes())

    def test_rejects_source_pointing_at_the_repository_root(self):
        self.marketplace["plugins"][0]["source"] = "."
        self.write_marketplace()
        self.assertIn("marketplace-source-empty", self.codes())

    def test_rejects_source_pointing_at_a_skill_directory_not_the_container(self):
        self.marketplace["plugins"][0]["source"] = "./shaft-skills/example-pack"
        self.write_marketplace()
        self.assertIn("marketplace-source-empty", self.codes())

    def test_rejects_absolute_source_path(self):
        """`root / absolute` discards root, so an absolute source escapes it.

        The absolute path is built at runtime rather than written as a
        drive-letter literal: `C:/Windows` is not absolute on POSIX at all, so
        a hardcoded one asserts a Windows-specific consequence of a
        Windows-specific input and fails on the ubuntu runner.
        """
        absolute_source = Path(tempfile.gettempdir()).resolve()
        self.assertTrue(absolute_source.is_absolute())
        self.marketplace["plugins"][0]["source"] = absolute_source.as_posix()
        self.write_marketplace()
        self.assertIn("marketplace-source-escapes-root", self.codes())

    def test_rejects_parent_traversal_source_path(self):
        self.marketplace["plugins"][0]["source"] = "../elsewhere"
        self.write_marketplace()
        self.assertIn("marketplace-source-escapes-root", self.codes())

    def test_rejects_a_source_resolving_outside_the_root_through_a_symlink(self):
        """The location half: a spelling with no `..` that still lands outside.

        `relative_to` compares spellings, not locations (#4497), so the
        decision is made from resolved paths.
        """
        outside = self.root.parent / f"outside-{self.root.name}"
        outside.mkdir()
        try:
            (self.root / "escape-link").symlink_to(outside, target_is_directory=True)
        except OSError as error:  # unprivileged Windows runner
            self.skipTest(f"symlinks unavailable: {error}")
        self.marketplace["plugins"][0]["source"] = "./escape-link"
        self.write_marketplace()
        self.assertIn("marketplace-source-escapes-root", self.codes())

    def test_rejects_manifest_that_is_a_list_without_raising(self):
        self.write(MARKETPLACE_PATH, "[]")
        self.assertIn("marketplace-malformed", self.codes())

    def test_rejects_manifest_that_is_null_without_raising(self):
        self.write(MARKETPLACE_PATH, "null")
        self.assertIn("marketplace-malformed", self.codes())

    def test_rejects_undecodable_manifest_without_raising(self):
        (self.root / MARKETPLACE_PATH).write_bytes(b"\xff\xfe\x00invalid utf-8")
        self.assertIn("marketplace-malformed", self.codes())

    def test_current_repository_marketplace_matches_the_skill_directories(self):
        repository_root = Path(__file__).resolve().parents[2]
        self.assertEqual(marketplace_errors(repository_root), [])

    def test_optional_portable_local_coding_delegate_passes_hygiene(self):
        repository_root = Path(__file__).resolve().parents[2]
        skill_dir = repository_root / "chaos-engine/skills/local-coding-delegate"
        self.assertTrue((skill_dir / "SKILL.md").is_file())
        self.assertEqual(validate_skill(repository_root, skill_dir, 20000), [])


class SourceShapeTest(unittest.TestCase):
    """The escape guard's *shape* half, evaluated for both spellings at once.

    `Path` is `WindowsPath` on a Windows runner and `PosixPath` on ubuntu, so a
    guard written against `Path` alone reaches a different verdict per platform
    for exactly the shapes that matter -- measured before this class existed:

        source              POSIX   Windows
        '../elsewhere'      True    True
        '..\\elsewhere'     False   True     <- disagreed
        '/etc'              True    False    <- disagreed
        'C:/Windows'        False   True     <- disagreed
        '//server/share'    True    True
        '\\\\server\\share' False   True     <- disagreed

    A manifest is one cross-platform artifact read by every host, so it must
    get one verdict. `source_shape_escapes` decides under BOTH spellings, which
    also makes the whole table assertable from either platform.
    """

    INSIDE = ("./shaft-skills", "shaft-skills", ".", "shaft-skills/nested")
    OUTSIDE = (
        "../elsewhere",
        "..\\elsewhere",
        "/etc",
        "C:/Windows",
        "//server/share",
        "\\\\server\\share",
    )

    def test_relative_sources_are_accepted(self):
        for source in self.INSIDE:
            with self.subTest(source=source):
                self.assertFalse(source_shape_escapes(source))

    def test_posix_absolute_source_is_rejected(self):
        self.assertTrue(source_shape_escapes("/etc"))

    def test_windows_drive_source_is_rejected(self):
        """Rejected on POSIX too, where it is merely a relative directory name.

        Reporting it as `marketplace-source-missing` there would invite someone
        to satisfy the check by creating a literal `C:` directory.
        """
        self.assertTrue(source_shape_escapes("C:/Windows"))

    def test_forward_slash_unc_source_is_rejected(self):
        self.assertTrue(source_shape_escapes("//server/share"))

    def test_backslash_unc_source_is_rejected(self):
        self.assertTrue(source_shape_escapes("\\\\server\\share"))

    def test_backslash_parent_traversal_is_rejected(self):
        self.assertTrue(source_shape_escapes("..\\elsewhere"))

    @staticmethod
    def single_flavour_verdict(flavour):
        """The pre-fix predicate, as it saw whichever flavour `Path` was."""
        return bool(flavour.is_absolute() or flavour.drive or ".." in flavour.parts)

    def test_shapes_the_flavours_disagree_about_are_still_rejected(self):
        """The regression guard for the ubuntu-only failure this class fixes.

        Each source here is one the two flavours reach OPPOSITE verdicts on, so
        a `Path`-based guard rejects it on one platform and lets it through as
        merely "missing" on the other. Pinning the disagreement keeps the list
        honest: if a shape stops disagreeing, this test says so rather than
        quietly guarding nothing.
        """
        for source in ("..\\elsewhere", "/etc", "C:/Windows", "\\\\server\\share"):
            with self.subTest(source=source):
                self.assertNotEqual(
                    self.single_flavour_verdict(PurePosixPath(source)),
                    self.single_flavour_verdict(PureWindowsPath(source)),
                    f"{source!r} is listed here because the flavours disagree about it",
                )
                self.assertTrue(
                    source_shape_escapes(source),
                    f"{source!r} must be rejected on every platform, not just one",
                )


if __name__ == "__main__":
    unittest.main()
