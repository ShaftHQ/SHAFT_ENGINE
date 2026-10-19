"""Shipped harness follow-ups: install, rg, retrieve, host prune, PR sections."""

from __future__ import annotations

import importlib.util
import json
import os
import shutil
import subprocess  # nosec B404 - fixed git argv in one test, no shell.
import tempfile
import threading
import time
import unittest
import unittest.mock
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "chaos-engine"
COMMIT = "1" * 40


def load(name: str):
    path = SOURCE / f"{name}.py" if not name.startswith("scripts/") else ROOT / name
    if name == "validate_pr_closing_keywords":
        path = ROOT / "scripts/ci/validate_pr_closing_keywords.py"
    elif name == "hooks/retrieve_justification":
        path = SOURCE / "hooks" / "retrieve_justification.py"
    spec = importlib.util.spec_from_file_location(f"followup_{name}", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class DisjointInstallTest(unittest.TestCase):
    def test_outside_source_installs_and_nested_source_is_rejected(self):
        install = load("install")
        mindset = load("harness_mindset")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            installed = install.install(project, SOURCE, COMMIT)
            self.assertTrue((installed / "install.py").is_file())
            nested = root / "nested-source"
            shutil.copytree(SOURCE, nested, ignore=shutil.ignore_patterns("__pycache__", "*.pyc"))
            project_inside = nested / "consumer"
            project_inside.mkdir()
            with self.assertRaisesRegex(
                ValueError, "ChaosEngine source and project trees must be disjoint"
            ):
                install.install(project_inside, nested, COMMIT)
            inside_temp = project / "tmp"
            inside_temp.mkdir()
            with unittest.mock.patch.object(mindset, "tempfile_dir", return_value=str(inside_temp)):
                parent = mindset.disjoint_download_parent(project)
            self.assertFalse(mindset.path_is_inside(parent, project))
            bootstrap = load("bootstrap")
            with unittest.mock.patch.object(bootstrap.tempfile, "gettempdir", return_value=str(inside_temp)):
                downloaded = bootstrap.disjoint_download_parent(project)
            self.assertFalse(mindset.path_is_inside(downloaded, project))


class RgStdinTest(unittest.TestCase):
    def test_pathless_rg_returns_without_reading_stdin_and_pathed_rg_matches(self):
        mindset = load("harness_mindset")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            here = root / "here"
            there = root / "there"
            here.mkdir()
            there.mkdir()
            (here / "note.txt").write_text("PATHLESS_TOKEN\n", encoding="utf-8")
            (there / "note.txt").write_text("PATHED_TOKEN\n", encoding="utf-8")
            if shutil.which("rg") is None:
                self.skipTest("rg is not installed")
            read_fd, write_fd = os.pipe()
            holder: dict[str, object] = {}

            def invoke() -> None:
                try:
                    holder["result"] = mindset.run_rg(["rg", "PATHLESS_TOKEN"], cwd=here, stdin=read_fd)
                except OSError as error:
                    holder["error"] = error

            worker = threading.Thread(target=invoke)
            worker.start()
            worker.join(5)
            blocked = worker.is_alive()
            if blocked:
                os.close(write_fd)
                worker.join(2)
            os.close(read_fd)
            if not blocked:
                os.close(write_fd)
            self.assertFalse(blocked, "pathless rg waited on stdin")
            if "error" in holder:
                raise holder["error"]
            result = holder["result"]
            self.assertEqual(0, result.returncode, result.stderr)
            self.assertIn("PATHLESS_TOKEN", result.stdout)
            pathed = mindset.run_rg(["rg", "PATHED_TOKEN", str(there)], cwd=here)
            self.assertEqual(0, pathed.returncode, pathed.stderr)
            self.assertIn("PATHED_TOKEN", pathed.stdout)
            self.assertNotIn(str(here), pathed.stdout)


class GraphifySectionRelevanceTest(unittest.TestCase):
    """A Graphify section hit is judged on its label and text, not only its file path."""

    PAGES = {
        "teaching/course-updates/freshness-audit.md": (
            "# Automatest course material: freshness audit (Oct 2026)\n\nIntro.\n\n"
            "## Top 10 fixes (in order)\n\n4. **Environment Setup** still Eclipse + JDK 17"
            " + JavaSE-1.8 screenshots.\n"
        ),
        "teaching/trainee-numbers.md": (
            "# Trainee numbers (verified vs estimated)\n\n## Verified from attendance sheets\n\n"
            "### Cohort profile (rounds 31-34 forms, aggregate)\n\nMostly juniors.\n"
        ),
        "identity/profiles.md": "# Identity and public profiles\n\nLinks.\n",
        "index.md": "# Home\n\nStart here.\n",
    }

    def _receipt(self, query: str, labels: list[tuple[str, str]]) -> dict:
        retrieve = load("retrieve")
        stores = load("stores")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            for relative, text in self.PAGES.items():
                (project / relative).parent.mkdir(parents=True, exist_ok=True)
                (project / relative).write_text(text, encoding="utf-8")
            out = project / "graphify-out"
            indexed = stores.index_documentation(project)
            stores.write_documentation_filing(out, indexed)
            graph = stores.merge_documentation_graph(out / "graph.json", indexed)
            known = {(node["path"], node["label"]) for node in graph["nodes"]}
            for item in labels:
                self.assertIn(item, known)
            body = "Graph: graphify-out/graph.json (9 nodes) | Start: [] | 9 nodes found\n\n" + "".join(
                f"NODE {label} [src={path} loc= community=]\n" for path, label in labels
            ) + "NODE ../index.md [src= loc= community=]\n"
            completed = subprocess.CompletedProcess([], 0, stdout=body, stderr="")
            env = {key: value for key, value in os.environ.items() if not key.endswith("GRAPHIFY_OUT")}
            with (
                unittest.mock.patch.dict(os.environ, env, clear=True),
                unittest.mock.patch.object(retrieve, "_record_store_outcome"),
            ):
                return retrieve._store_hit_receipt(project, "graphify", query, completed, False)

    def test_section_text_match_keeps_a_hit_whose_path_shares_no_query_word(self):
        audit = "teaching/course-updates/freshness-audit.md"
        receipt = self._receipt("Eclipse JDK 17 screenshots", [
            (audit, "Automatest course material: freshness audit (Oct 2026) › Top 10 fixes (in order)"),
            (audit, "Automatest course material: freshness audit (Oct 2026)"),
        ])
        self.assertEqual("used", receipt["status"], receipt)
        first = receipt["hits"][0]
        self.assertEqual(audit, first["path"])
        self.assertIn("Top 10 fixes", first["label"])

    def test_label_match_ranks_above_a_path_only_match(self):
        receipt = self._receipt("Cohort profile", [
            ("teaching/trainee-numbers.md", "Trainee numbers (verified vs estimated) › "
             "Verified from attendance sheets › Cohort profile (rounds 31-34 forms, aggregate)"),
            ("identity/profiles.md", "Identity and public profiles"),
            ("index.md", "Home"),
        ])
        paths = [hit["path"] for hit in receipt["hits"]]
        self.assertEqual("teaching/trainee-numbers.md", paths[0], receipt)
        self.assertNotIn("index.md", paths)

    def test_path_only_hits_still_pass_the_floor(self):
        mindset = load("harness_mindset")
        hits = [{"path": "chaos-engine/retrieve.py"}, {"path": "shaft-intellij/Plugin.java"}]
        kept = mindset.relevance_floor(hits, "retrieve budget")
        self.assertEqual([{"path": "chaos-engine/retrieve.py"}], kept)


class GraphifyFileCollapseTest(unittest.TestCase):
    """#7179: one Graphify hit per file; extra matching sections become labels, freed slots go to other files."""

    PAGES = GraphifySectionRelevanceTest.PAGES
    _receipt = GraphifySectionRelevanceTest._receipt

    def test_sections_of_one_file_collapse_into_one_hit_with_ranked_labels(self):
        audit = "teaching/course-updates/freshness-audit.md"
        top = "Automatest course material: freshness audit (Oct 2026) › Top 10 fixes (in order)"
        page = "Automatest course material: freshness audit (Oct 2026)"
        receipt = self._receipt("freshness audit Eclipse screenshots", [(audit, page), (audit, top)])
        self.assertEqual("used", receipt["status"], receipt)
        self.assertEqual([audit], [hit["path"] for hit in receipt["hits"]], receipt)
        hit = receipt["hits"][0]
        self.assertEqual(top, hit["label"])
        self.assertEqual([top, page], hit["labels"])

    def test_single_section_hits_carry_no_labels_list(self):
        receipt = self._receipt("Cohort profile", [
            ("teaching/trainee-numbers.md", "Trainee numbers (verified vs estimated) › "
             "Verified from attendance sheets › Cohort profile (rounds 31-34 forms, aggregate)"),
        ])
        self.assertNotIn("labels", receipt["hits"][0])

    def test_freed_slots_go_to_more_distinct_files(self):
        retrieve = load("retrieve")
        nodes = [("chaos-engine/retrieve.py", f"retrieve section {index}") for index in range(10)]
        nodes += [(f"chaos-engine/retrieve_{index}.py", "retrieve helper") for index in range(6)]
        body = "".join(f"NODE {label} [src={path} loc=L{line} community=]\n"
                       for line, (path, label) in enumerate(nodes, start=1))
        completed = subprocess.CompletedProcess([], 0, stdout=body, stderr="")
        with tempfile.TemporaryDirectory() as temporary, \
                unittest.mock.patch.object(retrieve, "_record_store_outcome"):
            receipt = retrieve._store_hit_receipt(Path(temporary), "graphify", "retrieve", completed, False)
        paths = [hit["path"] for hit in receipt["hits"]]
        self.assertEqual(len(paths), len(set(paths)), receipt)
        self.assertEqual(7, len(paths), receipt)
        first = receipt["hits"][0]
        self.assertEqual("chaos-engine/retrieve.py", first["path"])
        self.assertEqual(1, first["line"])
        self.assertEqual(3, len(first["labels"]))


class RetrieveBudgetTest(unittest.TestCase):
    def test_graphify_drops_irrelevant_hits_and_stays_in_budget(self):
        retrieve = load("retrieve")
        mindset = load("harness_mindset")
        body = (
            "src=shaft-intellij/src/ShaftPluginExecutor.java L12\n"
            "src=chaos-engine/retrieve.py L40\n" + ("x" * 5000)
        )
        completed = unittest.mock.Mock(returncode=0, stdout=body, stderr="")
        with (
            unittest.mock.patch.object(retrieve.subprocess, "run", return_value=completed),
            unittest.mock.patch.object(retrieve, "_tool_py", return_value=SOURCE / "tool.py"),
            unittest.mock.patch.object(retrieve, "_record_store_outcome"),
        ):
            receipt = retrieve._run_store(ROOT, "graphify", "retrieve budget token")
        paths = [hit["path"] for hit in receipt["hits"]]
        self.assertIn("chaos-engine/retrieve.py", paths)
        self.assertNotIn("shaft-intellij/src/ShaftPluginExecutor.java", paths)
        self.assertLessEqual(receipt["bytes"], mindset.store_budget_bytes("graphify"))
        self.assertLessEqual(len(receipt["excerpt"].encode("utf-8")), mindset.store_budget_bytes("graphify"))

    def test_memory_no_hit_receipt_is_small(self):
        retrieve = load("retrieve")
        body = "memory excerpt\n" + ("unrelated prose " * 200)
        completed = unittest.mock.Mock(returncode=0, stdout=body, stderr="")
        with (
            unittest.mock.patch.object(retrieve.subprocess, "run", return_value=completed),
            unittest.mock.patch.object(retrieve, "_tool_py", return_value=SOURCE / "tool.py"),
            unittest.mock.patch.object(retrieve, "_record_store_outcome"),
        ):
            receipt = retrieve._run_store(ROOT, "memory", "nothing matches here")
        encoded = json.dumps(receipt).encode("utf-8")
        self.assertEqual("skipped", receipt["status"])
        self.assertLess(len(encoded), 300)
        self.assertNotIn("unrelated prose", encoded.decode())


class HostPruneAndUsageTest(unittest.TestCase):
    def test_live_entries_stay_orphans_prune_and_dry_run_does_not_delete(self):
        mindset = load("harness_mindset")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine-hosts.json").write_text("{}\n", encoding="utf-8")
            live = project / (".chaos-engine-hosts.active-" + ("ab" * 32))
            live.write_bytes(b"")
            state = project / ".chaos-engine-state"
            state.mkdir()
            orphan = state / "orphaned-.chaos-engine-hosts.active-old"
            orphan.write_bytes(b"stale")
            old = time.time() - (8 * 24 * 60 * 60)
            os.utime(orphan, (old, old))
            preview = mindset.prune_orphaned_host_entries(project, dry_run=True)
            self.assertIn(orphan.name, preview["removed"])
            self.assertTrue(orphan.is_file())
            self.assertTrue(live.is_file())
            doctor = mindset.doctor_followups(project)
            self.assertTrue(doctor["hostPrune"]["dryRun"])
            self.assertTrue(orphan.is_file())
            done = mindset.prune_orphaned_host_entries(project, dry_run=False)
            self.assertIn(orphan.name, done["removed"])
            self.assertFalse(orphan.exists())
            self.assertTrue(live.is_file())
            self.assertTrue((project / ".chaos-engine-hosts.json").is_file())

    def test_retrieve_record_has_no_content_and_stays_bounded(self):
        mindset = load("harness_mindset")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            marker = "notes/kept-out-of-the-record.md"
            mindset.record_retrieve_usage(
                project,
                store="graphify",
                hit_count=2,
                byte_size=40,
                paths=["chaos-engine/retrieve.py", marker],
            )
            raw = (project / ".chaos-engine-state" / "retrieve-usage.json").read_text(encoding="utf-8")
            self.assertNotIn(marker, raw)
            self.assertNotIn("retrieve.py", raw)
            payload = json.loads(raw)
            record = payload["records"][0]
            self.assertEqual("graphify", record["store"])
            self.assertEqual(2, record["hitCount"])
            self.assertEqual(40, record["bytes"])
            self.assertFalse(record["cited"])
            self.assertTrue(mindset.note_path_read(project, "chaos-engine/retrieve.py"))
            rate = mindset.usage_rate(project)
            self.assertEqual(1, rate["cited"])
            self.assertEqual(1, rate["records"])
            for _ in range(120):
                mindset.record_retrieve_usage(
                    project, store="memory", hit_count=0, byte_size=1, paths=[]
                )
            stored = (project / ".chaos-engine-state" / "retrieve-usage.json").read_bytes()
            self.assertLessEqual(len(stored), 65536)
            self.assertLessEqual(len(json.loads(stored)["records"]), 100)

    def test_later_citation_flips_cited_through_the_store_ledger(self):
        mindset = load("harness_mindset")
        gate = load("hooks/retrieve_justification")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            relative = "chaos-engine/retrieve.py"
            hit = project / relative
            hit.parent.mkdir(parents=True)
            hit.write_text("retrieve\n", encoding="utf-8")
            mindset.record_retrieve_usage(
                project,
                store="graphify",
                hit_count=1,
                byte_size=12,
                paths=[relative],
            )
            raw = json.loads(
                (project / ".chaos-engine-state" / "retrieve-usage.json").read_text(encoding="utf-8")
            )
            self.assertFalse(raw["records"][0]["cited"])
            cited = gate.record_citations(project, "graphify", f"src={relative} L40\n")
            self.assertIn(relative, cited)
            rate = mindset.usage_rate(project)
            self.assertEqual(1, rate["cited"])
            self.assertEqual(1, rate["records"])
            self.assertEqual(1, mindset.doctor_followups(project)["retrieveUsage"]["cited"])
            self.assertEqual("retrieve hit use: 1/1", mindset.usage_line(project))

    def test_later_read_flips_cited_through_the_read_tool(self):
        mindset = load("harness_mindset")
        gate = load("hooks/retrieve_justification")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            relative = "chaos-engine/retrieve.py"
            hit = project / relative
            hit.parent.mkdir(parents=True)
            hit.write_text("retrieve\n", encoding="utf-8")
            mindset.record_retrieve_usage(
                project,
                store="mempalace",
                hit_count=1,
                byte_size=8,
                paths=[relative],
            )
            reason = gate.file_read_block_reason(
                project=project,
                event_name="PreToolUse",
                tool_name="Read",
                tool_input={"file_path": str(hit.resolve())},
                commands=(),
            )
            self.assertIsNone(reason)
            self.assertEqual(1, mindset.usage_rate(project)["cited"])

    def test_retrieve_attaches_the_same_call_citation_after_the_usage_row(self):
        retrieve = load("retrieve")
        mindset = load("harness_mindset")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            tool = project / "chaos-engine"
            tool.mkdir()
            (tool / "tool.py").write_text("print('tool')\n", encoding="utf-8")
            (tool / "retrieve.py").write_text("retrieve\n", encoding="utf-8")
            body = "src=chaos-engine/retrieve.py L40\n"
            completed = unittest.mock.Mock(returncode=0, stdout=body, stderr="")
            with unittest.mock.patch.object(retrieve.subprocess, "run", return_value=completed):
                receipt = retrieve.retrieve("retrieve budget token", store="graphify", project=project)
            self.assertEqual("used", receipt["status"])
            self.assertEqual(["chaos-engine/retrieve.py"], [hit["path"] for hit in receipt["hits"]])
            rate = mindset.usage_rate(project)
            self.assertEqual(1, rate["records"])
            self.assertEqual(1, rate["cited"])

    def test_mempalace_source_basename_cites_the_resolved_repo_path(self):
        retrieve = load("retrieve")
        mindset = load("harness_mindset")
        gate = load("hooks/retrieve_justification")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            subprocess.run(["git", "init", "-q"], cwd=project, check=True)  # nosec B603 B607
            for relative in ("a/Unique.java", "a/Dup.java", "b/Dup.java"):
                path = project / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("class X {}\n", encoding="utf-8")
            tool = project / "chaos-engine" / "tool.py"
            tool.parent.mkdir(parents=True)
            tool.write_text('print("Source: Unique.java")\n', encoding="utf-8")
            subprocess.run(["git", "add", "-A"], cwd=project, check=True)  # nosec B603 B607
            receipt = retrieve.retrieve("Unique history", store="mempalace", project=project)
            self.assertEqual("used", receipt["status"])
            self.assertEqual(["a/Unique.java"], [hit["path"] for hit in receipt["hits"]])
            rate = mindset.usage_rate(project)
            self.assertEqual(1, rate["records"])
            self.assertEqual(1, rate["cited"])
            self.assertEqual("retrieve hit use: 1/1", mindset.usage_line(project))
            self.assertEqual(1, mindset.doctor_followups(project)["retrieveUsage"]["cited"])
            self.assertEqual(
                ["a/Unique.java"],
                gate.record_citations(project, "mempalace", "Source: Unique.java\n"),
            )
            self.assertEqual(
                [],
                gate.record_citations(project, "mempalace", "Source: Dup.java\n"),
            )


class PrSectionTest(unittest.TestCase):
    def test_missing_assumption_is_rejected_and_explicit_na_is_accepted(self):
        mindset = load("harness_mindset")
        gate = load("validate_pr_closing_keywords")
        missing = gate.mindset_body_errors("Closes #1\n")
        self.assertTrue(any(item["code"] == "riskiest-assumption" for item in missing))
        accepted = mindset.assumption_section_errors("## Riskiest assumption\n\nN/A\n")
        self.assertEqual([], accepted)
        linked = mindset.assumption_section_errors(
            "## Riskiest assumption\n\nTargeted test: `FlutterEnabledFalseTest`.\n"
        )
        self.assertEqual([], linked)
        self.assertEqual([], mindset.assumption_section_errors("no section", author="dependabot[bot]"))

    def test_lessons_pr_without_an_outcome_is_rejected(self):
        mindset = load("harness_mindset")
        gate = load("validate_pr_closing_keywords")
        with unittest.mock.patch.dict(os.environ, {"PR_TITLE": "Lesson: restated a gate", "PR_BODY": ""}):
            errors = gate.mindset_body_errors("## Riskiest assumption\n\nN/A\n")
        self.assertTrue(any(item["code"] == "lesson-outcome" for item in errors))
        self.assertEqual(
            [],
            mindset.lesson_outcome_errors(
                "## Outcome changed\n\nWould have avoided the Flutter nightly failure.\n",
                title="Lesson: flutter flag",
            ),
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            note = mindset.record_lesson_note(project, "no outcome, kept as a note")
            self.assertIn("no outcome", note.read_text(encoding="utf-8"))

    def test_zero_trigger_report_matches_the_retirement_document(self):
        mindset = load("harness_mindset")
        text = (SOURCE / "references" / "rule-retirement.md").read_text(encoding="utf-8")
        self.assertIn("retire or justify", text)
        with tempfile.TemporaryDirectory() as temporary:
            report = mindset.zero_trigger_report(Path(temporary))
        self.assertTrue(report["baseline"])
        for rule_id in report["zeroTriggers"]:
            self.assertIn(rule_id, text)
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            mindset.record_rule_trigger(project, "rg-explicit-path")
            report = mindset.zero_trigger_report(project)
        self.assertNotIn("rg-explicit-path", report["zeroTriggers"])

    def test_shipped_checks_record_their_rule_triggers(self):
        mindset = load("harness_mindset")
        gate = load("validate_pr_closing_keywords")
        retrieve = load("retrieve")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            tool = project / "chaos-engine"
            tool.mkdir()
            (tool / "tool.py").write_text("print('tool')\n", encoding="utf-8")
            env = {
                "CHAOS_ENGINE_PROJECT": str(project),
                "PR_AUTHOR": "mohab",
                "PR_TITLE": "Lesson: restated a gate",
                "PR_BODY": "",
            }
            body = (
                "## Plan\n\nspec-x.md\n\nretrieve: mempalace used\n\n"
                "## Riskiest assumption\n\nN/A\n"
                "## Outcome changed\n\nWould have avoided the Flutter nightly failure.\n"
            )
            with unittest.mock.patch.dict(os.environ, env, clear=False):
                findings = gate.mindset_body_errors(body)
            self.assertEqual([], findings)
            completed = unittest.mock.Mock(
                returncode=0, stdout="src=chaos-engine/retrieve.py L40\n", stderr=""
            )
            with unittest.mock.patch.object(retrieve.subprocess, "run", return_value=completed):
                retrieve._run_store(project, "graphify", "retrieve budget token")
            if shutil.which("rg") is None:
                mindset.prepare_rg(["rg", "token"], project=project)
            else:
                mindset.run_rg(["rg", "token"], cwd=project)
            mindset.prune_orphaned_host_entries(project, dry_run=True)
            report = mindset.zero_trigger_report(project)
        for rule_id in (
            "riskiest-assumption",
            "plan-receipts",
            "lesson-outcome",
            "relevance-floor",
            "rg-explicit-path",
            "host-prune",
        ):
            self.assertNotIn(rule_id, report["zeroTriggers"], rule_id)

    def test_non_lesson_title_leaves_the_lesson_counter_at_zero(self):
        mindset = load("harness_mindset")
        gate = load("validate_pr_closing_keywords")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            mindset.lesson_outcome_errors(
                "## Outcome changed\n\nWould have avoided the Flutter nightly failure.\n",
                title="Fix the button",
                project=project,
            )
            report = mindset.zero_trigger_report(project)
            self.assertIn("lesson-outcome", report["zeroTriggers"])
            stored = project / ".chaos-engine-state" / "rule-triggers.json"
            if stored.is_file():
                rules = json.loads(stored.read_text(encoding="utf-8")).get("rules", {})
                entry = rules.get("lesson-outcome")
                count = int(entry.get("count") or 0) if isinstance(entry, dict) else 0
                self.assertEqual(0, count)
            env = {
                "CHAOS_ENGINE_PROJECT": str(project),
                "PR_AUTHOR": "mohab",
                "PR_TITLE": "Fix the button",
            }
            with unittest.mock.patch.dict(os.environ, env, clear=False):
                gate.mindset_body_errors("## Riskiest assumption\n\nN/A\n")
            report = mindset.zero_trigger_report(project)
            self.assertIn("lesson-outcome", report["zeroTriggers"])


class MemPalacePinTest(unittest.TestCase):
    def test_pinned_version_is_one_the_shim_still_accepts(self):
        shim = load("mempalace_fast_mine")
        pin = json.loads((SOURCE / "dependencies.json").read_text(encoding="utf-8"))
        version = pin["tools"]["mempalace"]["package"].split("==", 1)[1]
        self.assertIn(version, shim.SUPPORTED_VERSIONS)

    def test_mempalace_pin_bump_keeps_search_results_identical_to_the_recorded_baseline(self):
        """36-document FTS rows stay identical when the shipped rowid map replaces the full-scan delete."""
        import sqlite3

        shim = load("mempalace_fast_mine")
        pin = json.loads((SOURCE / "dependencies.json").read_text(encoding="utf-8"))
        version = pin["tools"]["mempalace"]["package"].split("==", 1)[1]
        self.assertIn(version, shim.SUPPORTED_VERSIONS)

        def fresh():
            db = sqlite3.connect(":memory:")
            db.execute(
                "CREATE TABLE documents(collection_id INTEGER, id TEXT, metadata_json TEXT)"
            )
            db.execute(
                "CREATE VIRTUAL TABLE docs_fts USING fts5("
                "collection_id UNINDEXED, doc_id UNINDEXED, document)"
            )
            for index in range(36):
                db.execute(
                    "INSERT INTO docs_fts(collection_id, doc_id, document) VALUES (1, ?, ?)",
                    (f"d{index}", f"baseline sample-{index:02d}"),
                )
            return db

        def original(cur, doc_id, document):
            cur.execute(
                "DELETE FROM docs_fts WHERE collection_id = ? AND doc_id = ?",
                (1, doc_id),
            )
            cur.execute(
                "INSERT INTO docs_fts(collection_id, doc_id, document) VALUES (?, ?, ?)",
                (1, doc_id, document),
            )

        baseline = fresh()
        patched = fresh()
        replace = shim.replace_fts(None)
        collection = type("Collection", (), {"_fts_available": lambda self, cur: True})()
        for index in range(36):
            document = f"rewritten sample-{index:02d} sharedtoken"
            original(baseline.cursor(), f"d{index}", document)
            replace(collection, patched.cursor(), 1, f"d{index}", document)
        self.assertEqual(
            baseline.execute("SELECT doc_id, document FROM docs_fts ORDER BY doc_id").fetchall(),
            patched.execute("SELECT doc_id, document FROM docs_fts ORDER BY doc_id").fetchall(),
        )


if __name__ == "__main__":
    unittest.main()
