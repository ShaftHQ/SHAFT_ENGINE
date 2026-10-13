"""Repository pins compared with the latest stable upstream."""

from __future__ import annotations

import io
import json
import re
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

from scripts.ci.validate_quality_configuration import validate_workflow_readme_inventory
from scripts.ci.watch_upstream_pins import (
    CAVEMAN_OVERLAY,
    Pin,
    Stable,
    compare_and_suggest,
    compare_pin,
    load_pins,
    main,
    newest_stable_version,
)


ROOT = Path(__file__).resolve().parents[2]
GRAPHIFY_URL = "https://pypi.org/pypi/graphifyy/json"
MEMPALACE_URL = "https://pypi.org/pypi/mempalace/json"


class MemoryIssues:
    def __init__(self) -> None:
        self.created: list[tuple[int, str, str]] = []
        self.updated: list[tuple[int, str]] = []
        self._open: dict[str, int] = {}
        self._next = 1

    def open_by_title(self, title: str) -> int | None:
        return self._open.get(title)

    def create(self, title: str, body: str) -> int:
        number = self._next
        self._next += 1
        self._open[title] = number
        self.created.append((number, title, body))
        return number

    def update(self, number: int, body: str) -> None:
        self.updated.append((number, body))


class RefusingStore:
    def open_by_title(self, title: str) -> int | None:
        raise AssertionError(title)

    def create(self, title: str, body: str) -> int:
        raise AssertionError(title)

    def update(self, number: int, body: str) -> None:
        raise AssertionError(number)


class WatchUpstreamPinsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)

    def write_repo(self, *, graphify: str = "0.9.43", mempalace: str = "3.10.0", receipt: str = "0.9.88") -> Path:
        root = Path(self.temporary.name)
        chaos = root / "chaos-engine"
        chaos.mkdir()
        spec = {
            "dependencies": {
                "mempalace": {"stableChannel": MEMPALACE_URL},
                "graphify": {"stableChannel": GRAPHIFY_URL},
                "memory": {"stableChannel": "https://registry.npmjs.org/@aictx%2fmemory/latest"},
            },
            "tools": {
                "mempalace": {"package": f"mempalace=={mempalace}"},
                "graphify": {"package": f"graphifyy=={graphify}"},
                "memory": {"package": "@aictx/memory@0.2.1"},
                "uv": {"package": "uv==0.11.29"},
            },
        }
        (chaos / "dependencies.json").write_text(json.dumps(spec), encoding="utf-8")
        (root / ".chaos-engine-dependencies.json").write_text(
            json.dumps({"components": {"graphify": {"installedVersion": receipt, "version": receipt}}}),
            encoding="utf-8",
        )
        vendors = (
            ("caveman", "a" * 40, "JuliusBrussee/caveman"),
            ("icm-architect", "b" * 40, "RinDig/icm-architect"),
            ("ponytail", "c" * 40, "DietrichGebert/ponytail"),
        )
        for name, commit, repository in vendors:
            pin_dir = chaos / "vendor" / name
            pin_dir.mkdir(parents=True)
            (pin_dir / "PIN.json").write_text(
                json.dumps({"repository": repository, "commit": commit}),
                encoding="utf-8",
            )
        return root

    def fetch_graphify_behind(self, pin: Pin) -> Stable:
        if pin.name == "graphify":
            return Stable(version="0.9.88")
        if pin.kind == "pypi":
            return Stable(version=pin.current)
        return Stable(release_commit=pin.current)

    def test_behind_pin_suggests_once_with_current_latest_and_url(self) -> None:
        root = self.write_repo()
        store = MemoryIssues()
        buffer = io.StringIO()
        with redirect_stdout(buffer):
            code = main(
                ["--root", str(root), "--write"],
                fetch=self.fetch_graphify_behind,
                store_factory=lambda _repo: store,
            )

        self.assertEqual(0, code)
        self.assertEqual(1, len(store.created))
        number, title, body = store.created[0]
        self.assertEqual("Upstream pin drift", title)
        self.assertIn("0.9.43", body)
        self.assertIn("0.9.88", body)
        self.assertIn(GRAPHIFY_URL, body)
        report = json.loads(buffer.getvalue())
        graphify = next(row for row in report if row["name"] == "graphify")
        self.assertEqual("0.9.43", graphify["current"])
        self.assertEqual("0.9.88", graphify["latest"])
        self.assertTrue(graphify["behind"])
        self.assertEqual(number, store.open_by_title(title))

    def test_second_run_does_not_open_another_suggestion(self) -> None:
        root = self.write_repo()
        store = MemoryIssues()
        args = ["--root", str(root), "--write"]
        with redirect_stdout(io.StringIO()):
            first = main(args, fetch=self.fetch_graphify_behind, store_factory=lambda _repo: store)
            second = main(args, fetch=self.fetch_graphify_behind, store_factory=lambda _repo: store)

        self.assertEqual(0, first)
        self.assertEqual(0, second)
        self.assertEqual(1, len(store.created))
        self.assertEqual(1, len(store.updated))
        self.assertEqual(store.created[0][0], store.updated[0][0])

    def test_matching_pins_suggest_nothing(self) -> None:
        root = self.write_repo()

        def fetch(pin: Pin) -> Stable:
            if pin.kind == "pypi":
                return Stable(version=pin.current)
            return Stable(release_commit=pin.current)

        buffer = io.StringIO()
        with redirect_stdout(buffer):
            code = main(
                ["--root", str(root), "--write"],
                fetch=fetch,
                store_factory=lambda _repo: RefusingStore(),
            )

        self.assertEqual(0, code)
        self.assertTrue(all(not row["behind"] for row in json.loads(buffer.getvalue())))

    def test_report_only_ignores_the_machine_receipt(self) -> None:
        root = self.write_repo(receipt="9.9.9")

        def fetch(pin: Pin) -> Stable:
            if pin.name == "graphify":
                return Stable(version="9.9.9", unreleased_version="9.9.10")
            if pin.kind == "pypi":
                return Stable(version=pin.current)
            return Stable(release_commit=pin.current)

        buffer = io.StringIO()
        with redirect_stdout(buffer):
            code = main(["--root", str(root)], fetch=fetch, store_factory=RefusingStore)

        self.assertEqual(0, code)
        graphify = next(row for row in json.loads(buffer.getvalue()) if row["name"] == "graphify")
        self.assertEqual("0.9.43", graphify["current"])
        self.assertEqual("9.9.9", graphify["latest"])
        self.assertNotEqual("9.9.9", graphify["current"])
        self.assertNotIn("memory", {row["name"] for row in json.loads(buffer.getvalue())})

    def test_mempalace_published_release_is_current_when_head_is_newer(self) -> None:
        pin = Pin("mempalace", "pypi", "3.10.0", MEMPALACE_URL)
        stable = Stable(
            version="3.10.0",
            release_commit="e" * 40,
            head_commit="f" * 40,
            unreleased_version="3.11.0",
        )

        verdict = compare_pin(pin, stable)

        self.assertFalse(verdict.behind)
        self.assertEqual("3.10.0", verdict.latest)
        self.assertNotEqual(stable.head_commit, verdict.latest)
        self.assertNotEqual(stable.unreleased_version, verdict.latest)

    def test_pypi_stable_channel_skips_prereleases_and_yanked_files(self) -> None:
        payload = {
            "info": {"version": "3.11.0rc1"},
            "releases": {
                "3.11.0": [{"yanked": True}],
                "3.11.0rc1": [{"yanked": False}],
                "3.10.0": [{"yanked": False}],
            },
        }

        latest = newest_stable_version(payload)
        verdict = compare_pin(
            Pin("mempalace", "pypi", "3.10.0", MEMPALACE_URL),
            Stable(version=latest, head_commit="f" * 40, unreleased_version="3.11.0"),
        )

        self.assertEqual("3.10.0", latest)
        self.assertFalse(verdict.behind)

    def test_git_latest_is_the_release_commit_not_a_newer_head(self) -> None:
        old = "1" * 40
        release = "2" * 40
        head = "3" * 40
        url = "https://github.com/JuliusBrussee/caveman"
        behind = compare_pin(
            Pin("caveman", "git", old, url, repository="JuliusBrussee/caveman", overlay=CAVEMAN_OVERLAY),
            Stable(release_commit=release, head_commit=head),
        )
        current = compare_pin(
            Pin("caveman", "git", release, url, repository="JuliusBrussee/caveman"),
            Stable(release_commit=release, head_commit=head),
        )

        self.assertTrue(behind.behind)
        self.assertEqual(release, behind.latest)
        self.assertNotEqual(head, behind.latest)
        self.assertFalse(current.behind)

    def test_repo_without_releases_uses_default_branch_head(self) -> None:
        head = "4" * 40
        old = "5" * 40
        url = "https://github.com/RinDig/icm-architect"
        current = compare_pin(
            Pin("icm-architect", "git", head, url, repository="RinDig/icm-architect"),
            Stable(head_commit=head),
        )
        behind = compare_pin(
            Pin("icm-architect", "git", old, url, repository="RinDig/icm-architect"),
            Stable(head_commit=head),
        )

        self.assertFalse(current.behind)
        self.assertTrue(behind.behind)
        self.assertEqual(head, behind.latest)

    def test_caveman_issue_requires_reapplying_the_overlay(self) -> None:
        root = self.write_repo()
        pins = load_pins(root)
        upstream = {}
        for pin in pins:
            if pin.kind == "pypi":
                upstream[pin.name] = Stable(version=pin.current)
            elif pin.name == "caveman":
                upstream[pin.name] = Stable(release_commit="d" * 40, head_commit="e" * 40)
            else:
                upstream[pin.name] = Stable(release_commit=pin.current)
        store = MemoryIssues()

        _verdicts, action = compare_and_suggest(pins, upstream, store)

        self.assertEqual("created", action)
        self.assertEqual(1, len(store.created))
        body = store.created[0][2]
        self.assertIn("chaos-engine/vendor/caveman/INVENTORY.md", body)
        self.assertIn("re-apply", body.lower())
        self.assertIn("a" * 40, body)
        self.assertIn("d" * 40, body)
        self.assertIn("https://github.com/JuliusBrussee/caveman", body)

    def test_several_drifts_stay_one_suggestion(self) -> None:
        pins = (
            Pin("graphify", "pypi", "0.9.43", GRAPHIFY_URL),
            Pin("ponytail", "git", "c" * 40, "https://github.com/DietrichGebert/ponytail"),
        )
        upstream = {
            "graphify": Stable(version="0.9.88"),
            "ponytail": Stable(release_commit="9" * 40, head_commit="8" * 40),
        }
        store = MemoryIssues()

        _verdicts, action = compare_and_suggest(pins, upstream, store)

        self.assertEqual("created", action)
        self.assertEqual(1, len(store.created))
        body = store.created[0][2]
        self.assertIn("0.9.43", body)
        self.assertIn("0.9.88", body)
        self.assertIn(GRAPHIFY_URL, body)
        self.assertIn("c" * 40, body)
        self.assertIn("9" * 40, body)
        self.assertNotIn("8" * 40, body)

    def test_every_vendor_pin_is_watched_and_other_tools_are_not(self) -> None:
        root = self.write_repo()
        extra = root / "chaos-engine" / "vendor" / "extra"
        extra.mkdir()
        (extra / "PIN.json").write_text(
            json.dumps({"repository": "acme/extra", "commit": "6" * 40}),
            encoding="utf-8",
        )

        pins = {pin.name: pin for pin in load_pins(root)}

        self.assertEqual(
            {"mempalace", "graphify", "caveman", "icm-architect", "ponytail", "extra"},
            set(pins),
        )
        self.assertEqual(CAVEMAN_OVERLAY, pins["caveman"].overlay)
        self.assertEqual("", pins["ponytail"].overlay)
        self.assertEqual("", pins["extra"].overlay)

    def test_repository_pins_follow_the_committed_files(self) -> None:
        pins = {pin.name: pin for pin in load_pins(ROOT)}
        spec = json.loads((ROOT / "chaos-engine" / "dependencies.json").read_text(encoding="utf-8"))
        vendors = sorted(path.parent.name for path in (ROOT / "chaos-engine" / "vendor").glob("*/PIN.json"))

        self.assertEqual(spec["tools"]["mempalace"]["package"].split("==", 1)[1], pins["mempalace"].current)
        self.assertEqual(spec["dependencies"]["mempalace"]["stableChannel"], pins["mempalace"].upstream_url)
        self.assertEqual(spec["tools"]["graphify"]["package"].split("==", 1)[1], pins["graphify"].current)
        self.assertEqual(spec["dependencies"]["graphify"]["stableChannel"], pins["graphify"].upstream_url)
        self.assertEqual(vendors, sorted(name for name, pin in pins.items() if pin.kind == "git"))
        for name in vendors:
            payload = json.loads((ROOT / "chaos-engine" / "vendor" / name / "PIN.json").read_text(encoding="utf-8"))
            self.assertEqual(payload["commit"], pins[name].current)
            self.assertEqual(f"https://github.com/{payload['repository']}", pins[name].upstream_url)

    def test_workflow_schedules_the_same_entry_and_is_inventoried(self) -> None:
        text = (ROOT / ".github" / "workflows" / "upstream-watch.yml").read_text(encoding="utf-8")
        cron = re.search(r"cron:\s*'([^']+)'", text)
        self.assertIsNotNone(cron)
        fields = cron.group(1).split()
        minute, hour, day_of_month, month, day_of_week = fields
        self.assertEqual(5, len(fields))
        self.assertTrue(day_of_week != "*" or day_of_month == "*", cron.group(1))
        self.assertIn("workflow_dispatch:", text)
        self.assertIn("python3 scripts/ci/watch_upstream_pins.py --write", text)
        self.assertIn("issues: write", text)
        self.assertNotEqual("", minute + hour + month)
        errors = validate_workflow_readme_inventory(ROOT)
        self.assertEqual([], [error for error in errors if "upstream-watch.yml" in error])

    def test_pr_gate_selects_this_module_for_pins_and_the_workflow(self) -> None:
        from scripts.ci.harness_pr_gate import classify_paths

        module = "tests.scripts.test_watch_upstream_pins"
        for path in (
            "chaos-engine/dependencies.json",
            "chaos-engine/vendor/caveman/PIN.json",
            "chaos-engine/vendor/ponytail/PIN.json",
            "chaos-engine/vendor/icm-architect/PIN.json",
        ):
            plan = classify_paths([path])
            self.assertIn("installer", plan.surfaces, path)
            self.assertIn(module, plan.test_modules, path)
        workflow = classify_paths([".github/workflows/upstream-watch.yml"])
        self.assertIn(module, workflow.test_modules)
        self.assertNotIn("installer", workflow.surfaces)
        script = classify_paths(["scripts/ci/watch_upstream_pins.py"])
        self.assertIn(module, script.test_modules)
        self.assertNotIn("installer", script.surfaces)
        guidance = (ROOT / ".github" / "workflows" / "pr-gate.yml").read_text(encoding="utf-8")
        self.assertIn("'scripts/ci/watch_upstream_pins.py'", guidance)
        self.assertIn("'tests/scripts/test_watch_upstream_pins.py'", guidance)
        self.assertIn("'.github/workflows/upstream-watch.yml'", guidance)


if __name__ == "__main__":
    unittest.main()
