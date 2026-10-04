"""Durable jobs survive agent-session kills without duplicate workers (#6524-#6529)."""

from __future__ import annotations

import importlib.util
import json
import os
import signal
import subprocess  # nosec B404 - runs the jobs CLI under test with fixed argv.
import sys
import tempfile
import time
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
JOBS = CE / "jobs.py"
POSIX = os.name == "posix"
LINUX = sys.platform.startswith("linux")


def load():
    spec = importlib.util.spec_from_file_location("ce_jobs_under_test", JOBS)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def wait_for(predicate, timeout=20.0, interval=0.05):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        value = predicate()
        if value:
            return value
        time.sleep(interval)
    raise AssertionError("condition not reached in time")


WORKER = r'''
import os, subprocess, sys, time
from pathlib import Path
jobs, out = sys.argv[1], Path(sys.argv[2])
out.mkdir(exist_ok=True)
def done(step):
    return subprocess.call([sys.executable, jobs, "checkpoint", "w", step, "--check"]) == 0
for step in ("one", "two", "three"):
    if done(step):
        continue
    with open(out / "ran.log", "a") as log:
        log.write(step + "\n")
    part = out / (step + ".txt.part")
    part.write_text("partial")
    if step == "two":
        # An orphan grandchild that leaves the job's process group (cast_render).
        subprocess.Popen([sys.executable, "-c", "import time; time.sleep(300)"], start_new_session=True)
        (out / "orphan.started").write_text("")
        while not (out / "go").exists():
            time.sleep(0.05)
    part.write_text(step + " complete")
    os.replace(part, out / (step + ".txt"))
    subprocess.check_call([sys.executable, jobs, "checkpoint", "w", step])
'''


@unittest.skipUnless(POSIX, "process groups and signals are POSIX")
class DurableJobTests(unittest.TestCase):
    def setUp(self):
        self.jobs = load()
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.base = Path(self.temporary.name)
        self.root = self.base / "state"
        self.env = dict(os.environ, CHAOS_ENGINE_JOBS_DIR=str(self.root))
        self.env.pop("CHAOS_ENGINE_JOB_RUN", None)

    def cli(self, *arguments):
        return subprocess.run(  # nosec B603 - fixed argv under test.
            [sys.executable, str(JOBS), *arguments],
            cwd=self.base, env=self.env, capture_output=True, text=True, check=False, timeout=60,
        )

    def lease(self, name):
        return self.jobs.read_lease(self.root / name) or {}

    def stop_after(self, name):
        self.addCleanup(lambda: self.cli("stop", name))

    def test_second_start_is_refused_while_the_lease_is_live(self):
        self.stop_after("busy")
        first = self.cli("start", "busy", "--heartbeat", "0.5", "--", sys.executable, "-c", "import time; time.sleep(60)")
        self.assertEqual(0, first.returncode, first.stderr)
        run_id = self.lease("busy")["run_id"]

        second = self.cli("start", "busy", "--", sys.executable, "-c", "print('duplicate')")
        resumed = self.cli("resume", "busy")
        status = self.cli("status", "busy")

        self.assertEqual(3, second.returncode)
        self.assertIn("refused", second.stderr)
        self.assertEqual(0, resumed.returncode)
        self.assertIn("leave alone", resumed.stdout)
        self.assertEqual(run_id, self.lease("busy")["run_id"], "a refused start must not replace the lease")
        self.assertEqual(0, status.returncode)
        self.assertIn("live", status.stdout)
        if LINUX:
            self.assertEqual(2, len(self.jobs.run_members(run_id)), "one supervisor and one worker only")

    def test_a_dead_pid_makes_the_lease_stale_and_start_takes_it_over(self):
        dead = subprocess.Popen([sys.executable, "-c", "pass"])  # nosec B603
        dead.wait()
        directory = self.root / "orphaned"
        now = time.time()
        self.jobs.write_json(directory / "lease.json", {
            "job": "orphaned", "run_id": "old", "state": "running", "pid": dead.pid, "pgid": None,
            "command": ["true"], "cwd": str(self.base), "heartbeat_at": now, "stale_s": 600, "runs": 1,
        })

        self.assertEqual("stale", self.jobs.classify(self.lease("orphaned")))
        self.assertEqual(3, self.cli("status", "orphaned").returncode)
        started = self.cli("start", "orphaned", "--", sys.executable, "-c", "pass")

        self.assertEqual(0, started.returncode, started.stderr)
        self.assertIn("took over stale run", started.stdout)
        wait_for(lambda: self.jobs.classify(self.lease("orphaned")) == "done")
        self.assertNotEqual("old", self.lease("orphaned")["run_id"])

    def test_an_expired_heartbeat_is_stale_even_with_a_live_pid_and_takeover_kills_it(self):
        hung = subprocess.Popen(  # nosec B603 - a stand-in for a hung worker.
            [sys.executable, "-c", "import time; time.sleep(300)"],
            env=dict(self.env, CHAOS_ENGINE_JOB_RUN="hung-run"), start_new_session=True,
        )
        self.addCleanup(lambda: (hung.kill(), hung.wait()))
        directory = self.root / "hung"
        self.jobs.write_json(directory / "lease.json", {
            "job": "hung", "run_id": "hung-run", "state": "running", "pid": hung.pid, "pgid": hung.pid,
            "child_pid": hung.pid, "command": [sys.executable, "-c", "pass"], "cwd": str(self.base),
            "heartbeat_at": time.time() - 100, "heartbeat_s": 1, "stale_s": 10, "runs": 1, "max_runs": 3,
        })

        self.assertTrue(self.jobs.pid_alive(hung.pid))
        self.assertEqual("stale", self.jobs.classify(self.lease("hung")))
        resumed = self.cli("resume", "hung")

        self.assertEqual(0, resumed.returncode, resumed.stderr)
        self.assertIn("took over stale run", resumed.stdout)
        self.assertIsNotNone(wait_for(lambda: hung.poll() is not None or None), "takeover kills the hung worker")
        wait_for(lambda: self.jobs.classify(self.lease("hung")) == "done")
        self.assertEqual(2, self.lease("hung")["runs"])

    def test_takeover_removes_part_orphans_and_a_refused_start_keeps_live_parts(self):
        cache = self.base / "cache"
        cache.mkdir()
        (cache / "nested").mkdir()
        (cache / "scene.mp4.part").write_text("half")
        (cache / "nested" / "clip.png.part").write_text("half")
        (cache / "done.mp4").write_text("finished")
        self.jobs.write_json(self.root / "render" / "lease.json", {
            "job": "render", "run_id": "gone", "state": "failed", "pid": None, "command": ["true"],
            "cwd": str(self.base), "part_dirs": ["cache"], "heartbeat_at": 0, "runs": 1,
        })

        self.stop_after("render")
        started = self.cli("start", "render", "--part-dir", "cache", "--heartbeat", "0.5", "--",
                           sys.executable, "-c", "import time; time.sleep(60)")
        self.assertEqual(0, started.returncode, started.stderr)
        self.assertEqual([], sorted(cache.rglob("*.part")))
        self.assertEqual("finished", (cache / "done.mp4").read_text())

        (cache / "live.mp4.part").write_text("in flight")
        refused = self.cli("start", "render", "--part-dir", "cache", "--", sys.executable, "-c", "pass")
        self.assertEqual(3, refused.returncode)
        self.assertTrue((cache / "live.mp4.part").exists(), "a live job's .part files are never touched")

    def test_atomic_output_never_leaves_a_partial_file(self):
        target = self.base / "out.txt"
        target.write_text("old")
        with self.assertRaises(RuntimeError):
            with self.jobs.atomic_output(target) as part:
                part.write_text("half")
                raise RuntimeError("killed mid-write")
        self.assertEqual("old", target.read_text())
        self.assertFalse((self.base / "out.txt.part").exists())
        with self.jobs.atomic_output(target) as part:
            part.write_text("new")
        self.assertEqual("new", target.read_text())

    @unittest.skipUnless(LINUX, "orphan detection reads /proc")
    def test_kill_mid_step_then_resume_finishes_without_redoing_or_duplicating(self):
        out = self.base / "out"
        worker = self.base / "worker.py"
        worker.write_text(WORKER, encoding="utf-8")
        started = self.cli("start", "w", "--heartbeat", "0.5", "--part-dir", "out", "--",
                           sys.executable, str(worker), str(JOBS), str(out))
        self.assertEqual(0, started.returncode, started.stderr)
        wait_for(lambda: (out / "orphan.started").exists() and (out / "two.txt.part").exists())
        lease = self.lease("w")
        run_id = lease["run_id"]
        members = wait_for(lambda: len(self.jobs.run_members(run_id)) >= 3 and self.jobs.run_members(run_id))
        orphans = [pid for pid in members if pid not in (lease["pid"], lease["child_pid"])]

        # The machine kills the worker mid-step (session death plus a crash).
        os.kill(lease["pid"], signal.SIGKILL)
        os.killpg(lease["pgid"], signal.SIGKILL)
        wait_for(lambda: self.jobs.classify(self.lease("w")) == "stale")
        self.assertTrue(any(self.jobs.pid_alive(pid) for pid in orphans), "the orphan outlived its parent")

        (out / "go").write_text("")
        resumed = self.cli("resume", "w")
        self.assertEqual(0, resumed.returncode, resumed.stderr)
        self.assertIn("removed 1 .part orphan", resumed.stdout)
        wait_for(lambda: self.jobs.classify(self.lease("w")) == "done")
        again = self.cli("resume", "w")

        self.assertEqual(["one", "two", "two", "three"], (out / "ran.log").read_text().split())
        self.assertEqual("two complete", (out / "two.txt").read_text())
        self.assertEqual([], sorted(out.rglob("*.part")))
        self.assertFalse(any(self.jobs.pid_alive(pid) for pid in orphans), "takeover kills children of dead parents")
        self.assertIn("leave alone", again.stdout)
        steps = [json.loads(line)["step"] for line in (self.root / "w" / "checkpoint.jsonl").read_text().splitlines()]
        self.assertEqual(["one", "two", "three"], steps)

    def test_stop_is_final_for_the_watchdog(self):
        started = self.cli("start", "halt", "--heartbeat", "0.5", "--", sys.executable, "-c", "import time; time.sleep(60)")
        self.assertEqual(0, started.returncode, started.stderr)
        self.assertEqual(0, self.cli("stop", "halt").returncode)
        resumed = self.cli("resume", "halt")
        self.assertEqual(5, resumed.returncode)
        self.assertEqual(5, self.cli("status", "halt").returncode)


class DurableJobContractTests(unittest.TestCase):
    def test_tool_dispatches_job_and_lists_it_in_help(self):
        with tempfile.TemporaryDirectory() as temporary:
            env = dict(os.environ, CHAOS_ENGINE_JOBS_DIR=temporary)
            status = subprocess.run(  # nosec B603 - fixed argv under test.
                [sys.executable, str(CE / "tool.py"), "job", "status", "nothing"],
                env=env, capture_output=True, text=True, check=False, timeout=60,
            )
            help_text = subprocess.run(  # nosec B603 - fixed argv under test.
                [sys.executable, str(CE / "tool.py"), "--help"],
                capture_output=True, text=True, check=False, timeout=60,
            ).stdout
        self.assertEqual(6, status.returncode, status.stderr)
        self.assertIn("job nothing: absent", status.stdout)
        self.assertIn("tool.py job start NAME", help_text)

    def test_session_budget_rules_are_routed_from_the_core_card(self):
        card = (CE / "skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        reference = (CE / "references/durable-jobs.md").read_text(encoding="utf-8")
        index = json.loads((CE / "harness-index.json").read_text(encoding="utf-8"))
        self.assertIn("references/durable-jobs.md", card)
        self.assertIn("references/durable-jobs.md", {entry.get("path") for entry in index["entries"]})
        for rule in ("45-50 min", "checkpoint before every long step", "never hold work only in the session",
                     "tool.py job status", "tool.py job resume", "never a liveness signal"):
            self.assertIn(rule, reference.casefold() if rule.islower() else reference)

    def test_design_runbook_launches_builds_through_the_job_runner(self):
        runbook = (CE / "addons/design-skills/references/pipelines/runbook.md").read_text(encoding="utf-8")
        self.assertIn("tool.py job start", runbook)
        self.assertIn("tool.py job resume", runbook)
        self.assertNotIn("setsid nohup", runbook)


if __name__ == "__main__":
    unittest.main()
