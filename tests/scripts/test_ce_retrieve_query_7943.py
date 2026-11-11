"""#7943 retrieve query alias and leading store. #7945 short classified ids are rare."""

from __future__ import annotations

import contextlib
import importlib.util
import io
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402 - self-locating under python3 -I (#7197)

ROOT = Path(__file__).resolve().parents[2]
WORKING_FORM = 'tool.py retrieve --store graphify "<query>"'


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise AssertionError(f"missing {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _retrieve():
    return _load("ce_retrieve_7943", ROOT / "chaos-engine/retrieve.py")


def _split():
    return _load("ce_corpus_split_7943", ROOT / "chaos-engine/corpus_split.py")


class RetrieveQueryCliTest(unittest.TestCase):
    def _main(self, module, argv: list[str]) -> tuple[int, str, str, list[tuple]]:
        calls: list[tuple] = []

        def fake(query, **kwargs):
            calls.append((query, kwargs.get("store")))
            return {"status": "skipped", "reason": "dry-run", "query": query}

        stdout = io.StringIO()
        stderr = io.StringIO()
        with (
            patch.object(module, "retrieve", fake),
            contextlib.redirect_stdout(stdout),
            contextlib.redirect_stderr(stderr),
        ):
            code = module.main(argv)
        return code, stdout.getvalue(), stderr.getvalue(), calls

    def test_query_flag_matches_positional_and_store_flag(self) -> None:
        module = _retrieve()
        flag = self._main(module, ["--query", "foo"])
        positional = self._main(module, ["foo"])
        stored = self._main(module, ["--store", "graphify", "foo"])
        stored_flag = self._main(module, ["--store", "graphify", "--query", "foo"])
        self.assertEqual(0, flag[0])
        self.assertEqual(0, positional[0])
        self.assertEqual(0, stored[0])
        self.assertEqual(0, stored_flag[0])
        self.assertEqual([("foo", None)], flag[3])
        self.assertEqual(flag[3], positional[3])
        self.assertEqual([("foo", "graphify")], stored[3])
        self.assertEqual(stored[3], stored_flag[3])

    def test_leading_store_name_plus_query_reaches_retrieve(self) -> None:
        module = _retrieve()
        for store in ("memory", "mempalace", "graphify", "deja"):
            with self.subTest(store=store):
                code, _stdout, stderr, calls = self._main(
                    module, [store, "ISSUES blockers retest"]
                )
                self.assertEqual(0, code, stderr)
                self.assertEqual([("ISSUES blockers retest", store)], calls)
                self.assertNotIn(WORKING_FORM, stderr)

    def test_lone_graphify_query_is_not_stolen(self) -> None:
        module = _retrieve()
        lone = self._main(module, ["graphify"])
        flagged = self._main(module, ["--query", "graphify"])
        explicit = self._main(module, ["--store", "graphify", "--query", "graphify"])
        self.assertEqual([("graphify", None)], lone[3])
        self.assertEqual(lone[3], flagged[3])
        self.assertEqual([("graphify", "graphify")], explicit[3])
        self.assertEqual(0, lone[0])
        self.assertEqual(0, explicit[0])

    def test_disagreeing_positional_and_query_flag_exits_2(self) -> None:
        module = _retrieve()
        code, stdout, stderr, calls = self._main(module, ["alpha", "--query", "beta"])
        self.assertEqual(2, code)
        self.assertIn(WORKING_FORM, stderr)
        self.assertEqual("", stdout)
        self.assertEqual([], calls)

    def test_matching_positional_and_query_flag_is_one_query(self) -> None:
        module = _retrieve()
        code, _stdout, stderr, calls = self._main(module, ["foo", "--query", "foo"])
        self.assertEqual(0, code, stderr)
        self.assertEqual([("foo", None)], calls)


class RareClassifiedIdTest(unittest.TestCase):
    def test_short_classified_ids_are_rare_tokens(self) -> None:
        split = _split()
        rare = (
            "J06",
            "J04",
            "G40",
            "FUNC-J01-BUG-03",
            "func-j01",
            "Func-1",
            "DEF-42",
            "def-9",
            "SHAFT#5812",
            "shaft#1",
        )
        for token in rare:
            with self.subTest(token=token):
                self.assertTrue(split.is_rare(token), token)
                self.assertEqual([token], split.rare_tokens(f"failure tail {token}"))
        self.assertEqual([], split.rare_tokens("failure tail"))
        self.assertFalse(split.is_rare("failure"))
        self.assertFalse(split.is_rare("tail"))
        self.assertFalse(split.is_rare("j06"))
        self.assertFalse(split.is_rare("g40"))
        self.assertFalse(split.is_rare("J6"))
        self.assertFalse(split.is_rare("G4"))
        self.assertFalse(split.is_rare("J0A"))
        self.assertFalse(split.is_rare("xfunc-1"))

    def test_classified_id_is_not_skipped_as_no_rare_token(self) -> None:
        module = _retrieve()
        queries = (
            "failure tail J06",
            "failure tail J04",
            "failure tail G40",
            "failure tail func-j01",
            "failure tail DEF-42",
            "failure tail shaft#1",
        )
        for query in queries:
            with self.subTest(query=query):
                self.assertFalse(module._skip_no_rare_before_store("graphify", query))
                kept = module._keep_store_path_or_skip_no_rare(
                    "graphify",
                    query,
                    {
                        "status": "used",
                        "reason": "hits",
                        "hits": [],
                        "query": query,
                        "excerpt": "",
                        "bytes": 0,
                    },
                )
                self.assertNotEqual("no-rare-token", kept.get("reason"))
                with (
                    patch.object(module, "_graphify_direct", return_value=None),
                    patch.object(
                        module,
                        "_run_store",
                        return_value={
                            "status": "used",
                            "reason": "hits",
                            "hits": [],
                            "query": query,
                            "excerpt": "",
                            "bytes": 0,
                        },
                    ),
                    patch.object(module, "_commit_retrieve_receipt"),
                    patch.object(module, "_mindset", return_value=None),
                    patch.object(module, "graph_freshness_field", return_value={"status": "fresh"}),
                ):
                    receipt = module.retrieve(query, project=ROOT, store="graphify")
                self.assertNotEqual("no-rare-token", receipt.get("reason"))
        self.assertTrue(module._skip_no_rare_before_store("graphify", "failure tail"))

    def test_short_classified_id_is_a_checkout_text_hit(self) -> None:
        split = _split()
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _git(root, "init", "-q")
            _git(root, "config", "user.email", "t@t")
            _git(root, "config", "user.name", "t")
            (root / "j06.txt").write_text("see J06 here\n", encoding="utf-8")
            (root / "func.txt").write_text("see func-j01 here\n", encoding="utf-8")
            _git(root, "add", "j06.txt", "func.txt")
            _git(root, "commit", "-qm", "ids")
            j06 = [hit["path"] for hit in split.resolve_hits(root, "J06")]
            func = [hit["path"] for hit in split.resolve_hits(root, "func-j01")]
            self.assertEqual(["j06.txt"], j06)
            self.assertEqual(["func.txt"], func)
            outcome = split.direct_graphify_outcome(root, "J06")
        self.assertIsNotNone(outcome)
        self.assertEqual(["j06.txt"], [hit["path"] for hit in outcome["hits"]])


def _git(root: Path, *args: str) -> None:
    subprocess.run(  # nosec B603 - fixed git argv in a temp repo, no shell.
        ["git", "-c", "core.hooksPath=", *args],
        cwd=root,
        check=True,
        capture_output=True,
        text=True,
    )


if __name__ == "__main__":
    unittest.main()
