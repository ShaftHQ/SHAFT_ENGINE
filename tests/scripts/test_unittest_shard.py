"""Tests for the deterministic unittest shard runner (#6207)."""

from __future__ import annotations

import argparse
import io
import unittest
from contextlib import redirect_stderr
from unittest.mock import patch

from scripts.ci import unittest_shard as shard


class _Sample(unittest.TestCase):
    def test_a(self) -> None:
        pass

    def test_b(self) -> None:
        pass

    def test_c(self) -> None:
        pass


def sample_tests() -> list[unittest.TestCase]:
    return list(shard.iter_tests(unittest.defaultTestLoader.loadTestsFromTestCase(_Sample)))


class ShardSelectionTest(unittest.TestCase):
    def test_shards_partition_the_module_exactly(self) -> None:
        tests = sample_tests()
        for total in (1, 2, 3, 4):
            with self.subTest(total=total):
                ids = [t.id() for i in range(1, total + 1) for t in shard.select(tests, i, total)]
                self.assertCountEqual([t.id() for t in tests], ids)
                self.assertEqual(len(ids), len(set(ids)))

    def test_selection_is_deterministic_by_id(self) -> None:
        tests = sample_tests()
        self.assertEqual(
            [t.id() for t in shard.select(tests, 1, 2)],
            [t.id() for t in shard.select(list(reversed(tests)), 1, 2)],
        )

    def test_parse_shard_rejects_out_of_range(self) -> None:
        self.assertEqual((2, 4), shard.parse_shard("2/4"))
        for bad in ("0/2", "3/2", "x/2", "1/0", "1"):
            with self.subTest(bad=bad), self.assertRaises(argparse.ArgumentTypeError):
                shard.parse_shard(bad)

    def test_empty_shard_fails_instead_of_passing_silently(self) -> None:
        with redirect_stderr(io.StringIO()) as stderr:
            code = shard.main([f"{__name__}._Sample", "--shard", "4/4"])
        self.assertEqual(1, code)
        self.assertIn("selected no tests", stderr.getvalue())

    def test_main_runs_the_selected_shard(self) -> None:
        with redirect_stderr(io.StringIO()):
            self.assertEqual(0, shard.main([f"{__name__}._Sample", "--shard", "1/2"]))
        failing = unittest.TestResult()
        failing.failures.append((None, "boom"))
        with patch.object(unittest.TextTestRunner, "run", return_value=failing), redirect_stderr(io.StringIO()):
            self.assertEqual(1, shard.main([f"{__name__}._Sample", "--shard", "1/2"]))


if __name__ == "__main__":
    unittest.main()
