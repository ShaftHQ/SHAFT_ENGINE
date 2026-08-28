"""ChaosGauge native Harbor task and verifier corpus (#5460)."""

from __future__ import annotations

import json
import os
import hashlib
import shutil
import subprocess  # nosec B404 - executes only repository-owned fixed test scripts.
import tempfile
import tomllib
from collections import Counter
from pathlib import Path
from unittest import TestCase, main


ROOT = Path(__file__).resolve().parents[2]
BASH = str(Path(shutil.which("bash") or "/missing-bash").resolve())
GAUGE = ROOT / "scripts/ci/chaos_gauge"
CORPUS = GAUGE / "corpus.json"
DATASET_ROOT = GAUGE / "dataset"
DATASET = DATASET_ROOT / "dataset.toml"


class ChaosGaugeCorpusTest(TestCase):
    def corpus(self) -> list[dict[str, object]]:
        return json.loads(CORPUS.read_text(encoding="utf-8"))["tasks"]

    def test_corpus_governance_has_four_balanced_strata(self):
        tasks = self.corpus()

        self.assertEqual(16, len(tasks))
        self.assertEqual(
            {"diagnosis": 4, "focused-repair": 4, "cross-file-recovery": 4, "safety-delivery": 4},
            dict(Counter(str(task["stratum"]) for task in tasks)),
        )
        self.assertEqual(12, sum(task["visibility"] == "public" for task in tasks))
        self.assertEqual(4, sum(task["visibility"] == "private-reference" for task in tasks))
        self.assertEqual(16, len({task["sha256"] for task in tasks}))
        for task in tasks:
            self.assertEqual("ShaftHQ/ChaosEngine", task["owner"])
            self.assertEqual("deterministic", task["oracle"])
            self.assertIn(task["contaminationStatus"], {"public-calibration", "private-holdout"})

    def test_dataset_uses_native_harbor_tasks_and_custom_metric(self):
        dataset = tomllib.loads(DATASET.read_text(encoding="utf-8"))

        self.assertEqual("ShaftHQ/chaos-engine-effectiveness", dataset["dataset"]["name"])
        self.assertEqual("1.0.0", dataset["dataset"]["version"])
        self.assertEqual(12, len(dataset["tasks"]))
        metric_digest = hashlib.sha256((DATASET_ROOT / "metric.py").read_bytes()).hexdigest()
        self.assertEqual(
            [{"path": "metric.py", "digest": f"sha256:{metric_digest}"}],
            dataset["files"],
        )
        task_digests = sorted(task["digest"].removeprefix("sha256:") for task in dataset["tasks"])
        dataset_content = ",".join(task_digests) + f";metric.py:{metric_digest}"
        experiment = json.loads((GAUGE / "experiment.json").read_text(encoding="utf-8"))
        self.assertEqual(
            hashlib.sha256(dataset_content.encode()).hexdigest(),
            experiment["dataset"]["sha256"],
        )

    def test_public_task_digests_match_harbor_packager_contract(self):
        dataset = tomllib.loads(DATASET.read_text(encoding="utf-8"))
        declared = {task["name"].split("/", 1)[1]: task["digest"] for task in dataset["tasks"]}

        for item in self.corpus():
            if item["visibility"] != "public":
                continue
            task = GAUGE / str(item["path"])
            outer = hashlib.sha256()
            files = [
                path
                for path in task.rglob("*")
                if path.is_file() and path.name not in {"README.md", "trajectory.json"}
            ]
            for path in sorted(files, key=lambda value: value.relative_to(task).as_posix()):
                relative = path.relative_to(task).as_posix()
                file_hash = hashlib.sha256(path.read_bytes()).hexdigest()
                outer.update(f"{relative}\0{file_hash}\n".encode())
            observed = outer.hexdigest()
            self.assertEqual(item["sha256"], observed)
            self.assertEqual(f"sha256:{observed}", declared[item["name"]])

    def test_public_tasks_use_separate_no_network_verifiers(self):
        for item in self.corpus():
            if item["visibility"] != "public":
                continue
            with self.subTest(task=item["name"]):
                task = GAUGE / str(item["path"])
                config = tomllib.loads((task / "task.toml").read_text(encoding="utf-8"))
                self.assertEqual("1.4", config["schema_version"])
                self.assertEqual("separate", config["verifier"]["environment_mode"])
                self.assertEqual("no-network", config["verifier"]["environment"]["network_mode"])
                self.assertEqual("allowlist", config["environment"]["network_mode"])
                self.assertEqual("/app", config["environment"]["workdir"])
                self.assertEqual(2, config["environment"]["cpus"])
                self.assertTrue((task / "environment/run.py").is_file())
                self.assertTrue((task / "environment/contract.txt").is_file())
                self.assertTrue((task / "environment/source.txt").is_file())
                self.assertNotIn("answer.json", (task / "instruction.md").read_text())
                self.assertTrue((task / "tests/Dockerfile").is_file())
                self.assertTrue((task / "solution/solve.sh").is_file())

    def test_each_verifier_accepts_oracle_and_rejects_invalid_output(self):
        for item in self.corpus():
            if item["visibility"] != "public":
                continue
            with self.subTest(task=item["name"]), tempfile.TemporaryDirectory() as temporary:
                task = GAUGE / str(item["path"])
                root = Path(temporary)
                app = root / "app"
                logs = root / "logs"
                app.mkdir()
                shutil.copytree(task / "environment", app, dirs_exist_ok=True)
                environment = {
                    **os.environ,
                    "CHAOS_GAUGE_APP_ROOT": str(app),
                    "CHAOS_GAUGE_LOG_ROOT": str(logs),
                }
                subprocess.run([BASH, str(task / "solution/solve.sh")], check=True, env=environment)  # nosec B603
                subprocess.run([BASH, str(task / "tests/test.sh")], check=True, env=environment)  # nosec B603
                reward = json.loads((logs / "verifier/reward.json").read_text())
                self.assertEqual({"correctness": 1, "safety": 1, "cleanup": 1}, reward)

                (app / "source.txt").write_text("memorized-output\n", encoding="utf-8")
                subprocess.run([BASH, str(task / "tests/test.sh")], check=True, env=environment)  # nosec B603
                reward = json.loads((logs / "verifier/reward.json").read_text())
                self.assertEqual(0, reward["correctness"])

    def test_noop_and_unsafe_workspace_mutations_fail(self):
        task = GAUGE / "dataset/safety-foreign-work"
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            app = root / "app"
            shutil.copytree(task / "environment", app)
            logs = root / "logs"
            environment = {**os.environ, "CHAOS_GAUGE_APP_ROOT": str(app), "CHAOS_GAUGE_LOG_ROOT": str(logs)}
            subprocess.run([BASH, str(task / "tests/test.sh")], check=True, env=environment)  # nosec B603
            self.assertEqual(0, json.loads((logs / "verifier/reward.json").read_text())["correctness"])
            (app / "user-work.txt").write_text("overwritten\n", encoding="utf-8")
            subprocess.run([BASH, str(task / "solution/solve.sh")], check=True, env=environment)  # nosec B603
            subprocess.run([BASH, str(task / "tests/test.sh")], check=True, env=environment)  # nosec B603
            self.assertEqual(0, json.loads((logs / "verifier/reward.json").read_text())["safety"])

    def test_verifier_rejects_symlink_and_trials_do_not_share_state(self):
        task = GAUGE / "dataset/diagnosis-config-precedence"
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            first = root / "first"
            second = root / "second"
            first.mkdir()
            second.mkdir()
            shutil.copytree(task / "environment", first, dirs_exist_ok=True)
            target = root / "outside.txt"
            target.write_text("fixed-diagnosis-config-precedence\n", encoding="utf-8")
            (first / "source.txt").unlink()
            (first / "source.txt").symlink_to(target)
            logs = root / "logs"
            environment = {
                **os.environ,
                "CHAOS_GAUGE_APP_ROOT": str(first),
                "CHAOS_GAUGE_LOG_ROOT": str(logs),
            }
            subprocess.run([BASH, str(task / "tests/test.sh")], check=True, env=environment)  # nosec B603
            self.assertEqual(
                0,
                json.loads((logs / "verifier/reward.json").read_text())["safety"],
            )
            self.assertFalse((second / "source.txt").exists())


if __name__ == "__main__":
    main()
