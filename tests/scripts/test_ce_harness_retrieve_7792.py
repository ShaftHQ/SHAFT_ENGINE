"""#7792: harness retrieval stays on the shared store, split from the Java corpus."""

from __future__ import annotations

import importlib.util
import json
import os
import sqlite3
import sys
import tempfile
import types
import unittest
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402 - self-locating under python3 -I (#7197)

ROOT = Path(__file__).resolve().parents[2]


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise AssertionError(f"missing {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _retrieve():
    return _load("ce_retrieve_7792", ROOT / "chaos-engine/retrieve.py")


def _split():
    return _load("ce_corpus_split_7792", ROOT / "chaos-engine/corpus_split.py")


def _paths(receipt: dict) -> list[str]:
    return [str(hit.get("path")) for hit in receipt.get("hits") or [] if isinstance(hit, dict)]


class HarnessRetrieveTest(unittest.TestCase):
    def test_harness_pr_gate_failure_tail_hits_the_gate_not_a_java_harness(self) -> None:
        receipt = _retrieve().retrieve("harness_pr_gate failure tail", project=ROOT)
        paths = _paths(receipt)
        self.assertTrue(paths, receipt)
        self.assertEqual("scripts/ci/harness_pr_gate.py", paths[0])
        for path in paths:
            self.assertFalse(path.startswith("shaft-capture/"), path)
            self.assertFalse(path.startswith("shaft-engine/"), path)
            self.assertFalse(path.startswith("shaft-"), path)

    def test_awaiting_checks_pre_push_hits_the_queue_card_or_lesson(self) -> None:
        receipt = _retrieve().retrieve("AWAITING_CHECKS pre-push", project=ROOT)
        paths = _paths(receipt)
        self.assertTrue(paths, receipt)
        self.assertIn(
            paths[0],
            {
                "chaos-engine/references/ci-status-economy.md",
                "tests/scripts/test_ce_bugfix_2026_10_30_2.py",
            },
        )
        self.assertNotEqual(
            "chaos-engine/references/design-review-video-checklist.md", paths[0]
        )

    def test_query_without_a_rare_token_is_skipped(self) -> None:
        receipt = _retrieve().retrieve("failure tail", project=ROOT)
        self.assertEqual("skipped", receipt["status"])
        self.assertEqual("no-rare-token", receipt["reason"])
        self.assertEqual([], receipt.get("hits") or [])

    def test_caller_query_resolves_the_gate_without_a_bare_seed(self) -> None:
        retrieve = _retrieve()
        seeds: list[str] = []
        real = retrieve.subprocess.run

        def spy(args, *pos, **kwargs):
            argv = list(args) if isinstance(args, (list, tuple)) else []
            index = 0
            while index < len(argv):
                if argv[index] == "--seed" and index + 1 < len(argv):
                    seeds.append(argv[index + 1])
                    index += 2
                    continue
                index += 1
            return real(args, *pos, **kwargs)

        with patch.object(retrieve.subprocess, "run", spy):
            receipt = retrieve.retrieve("what calls harness_pr_gate", project=ROOT)
        self.assertIn("scripts/ci/harness_pr_gate.py", _paths(receipt))
        self.assertNotIn("harness_pr_gate", seeds)

    def test_mempalace_drops_a_semantic_hit_without_the_rare_token(self) -> None:
        receipt = _retrieve().retrieve("AWAITING_CHECKS pre-push", store="mempalace", project=ROOT)
        hits = [hit for hit in receipt.get("hits") or [] if isinstance(hit, dict)]
        if not hits:
            self.assertNotEqual("used", receipt.get("status"))
            return
        blob = f"{hits[0].get('path') or ''} {hits[0].get('section') or ''}"
        self.assertIn("AWAITING_CHECKS", blob)


class CorpusSplitTest(unittest.TestCase):
    def test_generic_java_type_names_are_walk_seeds_only_as_the_whole_query(self) -> None:
        split = _split()
        self.assertNotIn("Harness", split.walk_seeds("failure tail Harness"))
        self.assertEqual(["Harness"], split.walk_seeds("Harness"))

    def test_both_corpora_when_the_query_names_a_java_type_and_a_harness_file(self) -> None:
        receipt = _retrieve().retrieve("LocalAgentMode harness_pr_gate", project=ROOT)
        paths = _paths(receipt)
        self.assertTrue(any(path.endswith("LocalAgentMode.java") for path in paths), paths)
        self.assertIn("scripts/ci/harness_pr_gate.py", paths)

    def test_semantic_hit_without_the_rare_token_is_dropped(self) -> None:
        split = _split()
        kept = split.drop_semantic_misses(
            "AWAITING_CHECKS pre-push",
            [
                {
                    "path": "chaos-engine/references/design-review-video-checklist.md",
                    "section": "design review video",
                }
            ],
        )
        self.assertEqual([], kept)

    def test_always_loaded_guidance_is_not_a_top_hit(self) -> None:
        split = _split()
        kept = split.drop_always_loaded(
            "AWAITING_CHECKS pre-push",
            [
                {
                    "path": "chaos-engine/references/bot-entry.md",
                    "section": "AWAITING_CHECKS is mentioned in passing",
                },
                {
                    "path": "chaos-engine/references/ci-status-economy.md",
                    "section": "`AWAITING_CHECKS` on mergeQueueEntry",
                },
            ],
        )
        self.assertTrue(kept)
        self.assertNotEqual("chaos-engine/references/bot-entry.md", kept[0]["path"])
        self.assertTrue(all(hit["path"] != "chaos-engine/references/bot-entry.md" for hit in kept))

    def test_graphify_path_predicate_keeps_the_harness_file(self) -> None:
        split = _split()
        kept = split.apply_path_predicate(
            "harness_pr_gate failure tail",
            [
                {
                    "path": "shaft-capture/src/test/java/com/shaft/capture/runtime/CaptureEventPipelineDuplicateStepsTest.java",
                    "label": "Harness",
                    "section": "Harness",
                },
                {
                    "path": "scripts/ci/harness_pr_gate.py",
                    "label": "harness_pr_gate.py",
                    "section": "failure tail",
                },
            ],
        )
        self.assertEqual(["scripts/ci/harness_pr_gate.py"], [hit["path"] for hit in kept])

    def test_harness_paths_use_a_separate_collection(self) -> None:
        split = _split()
        self.assertEqual("chaos-engine-harness", split.collection_for_path("chaos-engine/retrieve.py"))
        self.assertEqual(
            "chaos-engine-harness", split.collection_for_path("scripts/ci/harness_pr_gate.py")
        )
        self.assertEqual(
            "chaos-engine-harness",
            split.collection_for_path("tests/scripts/test_ce_bugfix_2026_10_30_2.py"),
        )
        self.assertEqual(
            "mempalace_drawers",
            split.collection_for_path("shaft-engine/src/main/java/com/shaft/driver/SHAFT.java"),
        )
        self.assertEqual(
            "mempalace_drawers", split.collection_for_path("scripts/ci/overlay_pre_push.py")
        )
        groups = split.mempalace_mine_groups(
            [
                "chaos-engine/retrieve.py",
                "shaft-engine/src/main/java/com/shaft/driver/SHAFT.java",
                "scripts/ci/harness_pr_gate.py",
            ]
        )
        self.assertIn(("chaos-engine-harness", ["chaos-engine/retrieve.py", "scripts/ci/harness_pr_gate.py"]), groups)
        self.assertIn(
            (None, ["shaft-engine/src/main/java/com/shaft/driver/SHAFT.java"]),
            groups,
        )

    def test_harness_query_reads_the_harness_collection(self) -> None:
        split = _split()
        with tempfile.TemporaryDirectory() as temporary:
            database = Path(temporary) / "sqlite_exact.sqlite3"
            connection = sqlite3.connect(database)
            connection.execute("CREATE TABLE collections (id INTEGER PRIMARY KEY, name TEXT)")
            connection.execute(
                "CREATE TABLE documents (id INTEGER PRIMARY KEY, collection_id INTEGER, document TEXT, metadata_json TEXT)"
            )
            connection.execute("INSERT INTO collections VALUES (1, 'mempalace_drawers')")
            connection.execute("INSERT INTO collections VALUES (2, 'chaos-engine-harness')")
            connection.execute(
                "INSERT INTO documents VALUES (1, 1, 'drawers copy', ?)",
                (json.dumps({"source_file": "notes/harness_pr_gate_copy.py"}),),
            )
            connection.execute(
                "INSERT INTO documents VALUES (2, 2, 'harness copy', ?)",
                (json.dumps({"source_file": "scripts/ci/harness_pr_gate.py"}),),
            )
            connection.commit()
            connection.close()
            with patch.object(split, "_palace_sqlite", return_value=database):
                hits = split.resolve_hits(ROOT, "harness_pr_gate failure tail")
        self.assertTrue(hits)
        self.assertEqual(["scripts/ci/harness_pr_gate.py"], [hit["path"] for hit in hits])
        self.assertEqual("chaos-engine-harness", hits[0].get("collection"))

    def test_full_mine_records_harness_paths_in_the_harness_collection(self) -> None:
        stores = _load("ce_stores_full_mine_7792", ROOT / "chaos-engine/stores.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            snapshot = root / "snapshot"
            (snapshot / "chaos-engine").mkdir(parents=True)
            (snapshot / "chaos-engine" / "retrieve.py").write_text("print('harness')\n", encoding="utf-8")
            (snapshot / "scripts" / "ci").mkdir(parents=True)
            (snapshot / "scripts" / "ci" / "harness_pr_gate.py").write_text("print('gate')\n", encoding="utf-8")
            (snapshot / "product").mkdir()
            (snapshot / "product" / "App.java").write_text("class App {}\n", encoding="utf-8")
            seen: list[str | None] = []

            def invoke(command, cwd):
                seen.append(os.environ.get("CHAOS_ENGINE_MEMPALACE_COLLECTION"))
                return 0

            with (
                patch.object(stores.shutil, "which", return_value="/tools/mempalace"),
                patch.object(stores, "resolve_palace", return_value=root / "palace"),
                patch.object(stores, "incremental_mine_paths", return_value=None),
                patch.object(stores, "read_wing", return_value=""),
                patch.object(stores, "write_palace_marker"),
                patch.object(stores, "_apply_documentation_index"),
            ):
                stores._refresh_mempalace(
                    root,
                    snapshot=snapshot,
                    primary=root,
                    revision="a" * 40,
                    invoke=invoke,
                )
        self.assertIn("chaos-engine-harness", seen)

    def test_fast_mine_routes_the_harness_collection(self) -> None:
        fast = _load("ce_fast_mine_7792", ROOT / "chaos-engine/mempalace_fast_mine.py")
        calls: list[tuple] = []

        def get_collection(palace_path, collection_name=None, **kwargs):
            calls.append((collection_name, kwargs))
            return "collection"

        collection = types.ModuleType("mempalace.palace.collection")
        collection.get_collection = get_collection
        palace = types.ModuleType("mempalace.palace")
        palace.get_collection = get_collection
        previous = {
            key: sys.modules.get(key)
            for key in ("mempalace", "mempalace.palace", "mempalace.palace.collection")
        }
        sys.modules["mempalace"] = types.ModuleType("mempalace")
        sys.modules["mempalace.palace"] = palace
        sys.modules["mempalace.palace.collection"] = collection
        os.environ["CHAOS_ENGINE_MEMPALACE_COLLECTION"] = "chaos-engine-harness"
        try:
            fast.install_collection_route()
            sys.modules["mempalace.palace.collection"].get_collection("palace.sqlite")
        finally:
            os.environ.pop("CHAOS_ENGINE_MEMPALACE_COLLECTION", None)
            for key, module in previous.items():
                if module is None:
                    sys.modules.pop(key, None)
                else:
                    sys.modules[key] = module
        self.assertEqual("chaos-engine-harness", calls[0][0])
        self.assertTrue(calls[0][1].get("_skip_name_check"))


if __name__ == "__main__":
    unittest.main()
