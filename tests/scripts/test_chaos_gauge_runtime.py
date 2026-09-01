"""Non-root agent-runtime contract for every public ChaosGauge task."""

from __future__ import annotations

import subprocess
import tomllib
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
DATASET = ROOT / "scripts" / "ci" / "chaos_gauge" / "dataset"
USER = "chaosgauge"
UID = "10001"
BASE = "python:3.12.11-slim@sha256:47ae396f09c1303b8653019811a8498470603d7ffefc29cb07c88f1f8cb3d19f"


class ChaosGaugeRuntimeTest(unittest.TestCase):
    def test_every_public_agent_environment_has_fixed_non_root_contract(self) -> None:
        for task in sorted(path for path in DATASET.iterdir() if path.is_dir()):
            with self.subTest(task=task.name):
                config = tomllib.loads((task / "task.toml").read_text(encoding="utf-8"))
                dockerfile = (task / "environment" / "Dockerfile").read_text(encoding="utf-8")
                self.assertEqual(USER, config["agent"]["user"])
                self.assertNotIn("docker_image", config["environment"])
                self.assertEqual(f"FROM {BASE}", dockerfile.splitlines()[0])
                self.assertIn(f"--uid {UID}", dockerfile)
                self.assertIn(f"ENV HOME=/home/{USER}", dockerfile)
                self.assertIn("WORKDIR /app", dockerfile)
                self.assertIn("RUN chown chaosgauge:chaosgauge /app", dockerfile)
                self.assertTrue(dockerfile.rstrip().endswith(f"USER {USER}"))
                self.assertEqual("separate", config["verifier"]["environment_mode"])
                self.assertIn("docker_image", config["verifier"]["environment"])

    def test_runtime_probe_runs_as_task_user_with_writable_workspace(self) -> None:
        for task in sorted(path for path in DATASET.iterdir() if path.is_dir()):
            with self.subTest(task=task.name):
                image = f"chaosgauge-runtime-{task.name}"
                try:
                    subprocess.run(
                        ["docker", "build", "--tag", image, str(task / "environment")],
                        check=True,
                        capture_output=True,
                    )
                    probe = "test \"$(id -u)\" -ne 0; test -w /app; test -w \"$HOME\""
                    subprocess.run(
                        ["docker", "run", "--rm", "--network", "none", image, "sh", "-ceu", probe],
                        check=True,
                        capture_output=True,
                    )
                finally:
                    subprocess.run(
                        ["docker", "image", "rm", "--force", image],
                        check=False,
                        capture_output=True,
                    )


if __name__ == "__main__":
    unittest.main()
