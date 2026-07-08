import unittest
from unittest import mock

from scripts.ci.run_sharded_and_merge import main


class RunShardedAndMergeTest(unittest.TestCase):
    def test_runs_tests_assembly_and_merge_for_every_shard(self):
        with mock.patch("scripts.ci.run_sharded_and_merge.subprocess.run") as run:
            run.return_value = mock.Mock(returncode=0)

            exit_code = main(["--shards", "2"])

        self.assertEqual(exit_code, 0)
        # 2 shards x (mvn test + assemble) + 1 merge == 5 subprocess calls.
        self.assertEqual(run.call_count, 5)
        commands = [call.args[0] for call in run.call_args_list]

        self.assertIn("-Dshaft.shard=1/2", commands[0])
        self.assertIn("-DheadlessExecution=true", commands[0])
        self.assertTrue(any("assemble_shard_blob.py" in part for part in commands[1]))
        self.assertIn("--shard", commands[1])
        self.assertIn("1", commands[1])

        self.assertIn("-Dshaft.shard=2/2", commands[2])
        self.assertIn("2", commands[3])

        merge_command = commands[4]
        joined = " ".join(merge_command)
        self.assertIn("ShardMergeCli", joined)
        self.assertIn("target/shard-blobs/shard-1", joined)
        self.assertIn("target/shard-blobs/shard-2", joined)

    def test_skip_tests_only_runs_assembly_and_merge(self):
        with mock.patch("scripts.ci.run_sharded_and_merge.subprocess.run") as run:
            run.return_value = mock.Mock(returncode=0)

            exit_code = main(["--shards", "2", "--skip-tests"])

        self.assertEqual(exit_code, 0)
        # 2 shards x assemble only + 1 merge == 3 subprocess calls.
        self.assertEqual(run.call_count, 3)

    def test_failing_step_raises_with_a_labeled_message(self):
        with mock.patch("scripts.ci.run_sharded_and_merge.subprocess.run") as run:
            run.return_value = mock.Mock(returncode=1)

            with self.assertRaisesRegex(SystemExit, "Shard 1 Maven test failed"):
                main(["--shards", "1"])

    def test_zero_shards_is_rejected(self):
        with self.assertRaises(SystemExit):
            main(["--shards", "0"])

    def test_custom_merge_command_template_is_rendered(self):
        with mock.patch("scripts.ci.run_sharded_and_merge.subprocess.run") as run:
            run.return_value = mock.Mock(returncode=0)

            exit_code = main([
                "--shards", "1", "--skip-tests",
                "--merge-command", "java -cp report-aggregate.jar com.shaft.reportaggregate.ShardMergeCli --output {output} {blobs}",
                "--output", "custom-merged",
            ])

        self.assertEqual(exit_code, 0)
        merge_command = run.call_args_list[-1].args[0]
        self.assertIn("--output", merge_command)
        self.assertIn("custom-merged", merge_command)
        self.assertIn("target/shard-blobs/shard-1", merge_command)


if __name__ == "__main__":
    unittest.main()
