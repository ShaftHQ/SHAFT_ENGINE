"""#7220: host prerequisite detection, skip-when-latest, Windows branches, upgrade prompt."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]


def load():
    spec = importlib.util.spec_from_file_location("ce_prereq_7220", ROOT / "chaos-engine" / "prerequisites.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


INSTALL = {"CHAOS_ENGINE_PREREQUISITES": "install"}
SHAFT_POM = (
    "<project><dependencies><dependency><groupId>io.github.shafthq</groupId>"
    "<artifactId>shaft-engine</artifactId><version>{}</version></dependency></dependencies></project>"
)


class Prerequisites7220Test(unittest.TestCase):
    def setUp(self):
        self.m = load()
        self.tmp = tempfile.TemporaryDirectory()
        self.project = Path(self.tmp.name)

    def tearDown(self):
        self.tmp.cleanup()

    def test_detection_per_project_type(self):
        self.assertEqual(["node"], self.m.required(self.project, windows=False))
        (self.project / ".git").mkdir()
        self.assertEqual(["git", "node"], self.m.required(self.project, windows=False))
        (self.project / "pom.xml").write_text("<project/>", encoding="utf-8")
        (self.project / ".git" / "config").write_text('[remote "origin"]\n url = https://github.com/a/b\n', encoding="utf-8")
        self.assertEqual(["git", "mvn", "gh", "node"], self.m.required(self.project, windows=False))
        self.assertEqual(["winget", "pwsh", "git", "mvn", "gh", "node"], self.m.required(self.project, windows=True))

    def test_skip_when_latest_and_upgrade_when_older(self):
        (self.project / ".git").mkdir()
        cache = self.project / "cache.json"
        with mock.patch.object(self.m.shutil, "which", return_value="/bin/x"):
            rows = self.m.plan(self.project, windows=False, current_mode="install", cache_file=cache,
                               version_of=lambda exe: (2, 50, 0),
                               latest_of=lambda tool, _c: (2, 50, 0) if tool == "git" else ())
            self.assertEqual({"git": "skip", "node": "skip"}, {r["tool"]: r["action"] for r in rows})
            rows = self.m.plan(self.project, windows=False, current_mode="install", cache_file=cache,
                               version_of=lambda exe: (2, 40, 0), latest_of=lambda tool, _c: (2, 50, 0))
            self.assertEqual("upgrade", rows[0]["action"])

    def test_latest_lookup_is_cached_and_offline_safe(self):
        cache = self.project / "c.json"
        calls = []

        def fetch(url):
            calls.append(url)
            return {"tag_name": "v2.80.0"}

        self.assertEqual((2, 80, 0), self.m.latest_version("gh", cache, fetch=fetch, now=1000))
        self.assertEqual((2, 80, 0), self.m.latest_version("gh", cache, fetch=fetch, now=2000))
        self.assertEqual(1, len(calls))

        def offline(url):
            raise OSError("offline")

        self.assertEqual((), self.m.latest_version("pwsh", cache, fetch=offline, now=1000))

    def test_ci_and_no_tty_only_report(self):
        self.assertEqual("report", self.m.mode({"CI": "true", "CHAOS_ENGINE_PREREQUISITES": "install"}))
        self.assertEqual("off", self.m.mode({"CHAOS_ENGINE_PREREQUISITES": "off"}))
        runner = mock.Mock()
        with mock.patch.object(self.m.shutil, "which", return_value=None):
            report = self.m.provision(self.project, env={"CI": "1"}, runner=runner, windows=True,
                                      version_of=lambda exe: ())
        runner.assert_not_called()
        self.assertTrue(all(r["status"] == "reported" for r in report["tools"]))

    def test_windows_branches_winget_pwsh_then_choco_fallback(self):
        runner = mock.Mock(return_value=SimpleNamespace(returncode=1))
        with mock.patch.object(self.m.shutil, "which", return_value=None), \
                mock.patch.object(self.m, "is_admin", return_value=False):
            report = self.m.provision(self.project, env=INSTALL, runner=runner, windows=True,
                                      version_of=lambda exe: ())
        tools = {r["tool"]: r for r in report["tools"]}
        self.assertEqual(self.m.WINGET_BOOTSTRAP, runner.call_args_list[0].args[0])
        self.assertEqual("failed", tools["winget"]["status"])
        self.assertEqual("reported", tools["choco"]["status"])  # winget missing, non-admin: report only.
        self.assertIn("needs admin", tools["choco"]["command"])

        runner = mock.Mock(return_value=SimpleNamespace(returncode=0))
        with mock.patch.object(self.m.shutil, "which", return_value=None):
            report = self.m.provision(self.project, env=INSTALL, runner=runner, windows=True,
                                      version_of=lambda exe: ())
        argvs = [c.args[0] for c in runner.call_args_list]
        self.assertIn("Microsoft.PowerShell", argvs[1])
        self.assertNotIn("choco", [r["tool"] for r in report["tools"]])  # winget works: no Chocolatey.

    def test_posix_never_runs_on_linux(self):
        runner = mock.Mock()
        with mock.patch.object(self.m.shutil, "which", return_value=None), \
                mock.patch.object(self.m.sys, "platform", "linux"):
            report = self.m.provision(self.project, env=INSTALL, runner=runner, windows=False,
                                      version_of=lambda exe: ())
        runner.assert_not_called()
        self.assertEqual("reported", report["tools"][0]["status"])

    def test_outdated_compare_counts_newer_dev_as_current(self):
        self.assertTrue(self.m.is_outdated("9.4.20250101", "10.4.20261020"))
        self.assertFalse(self.m.is_outdated("10.4.20261021-SNAPSHOT", "10.4.20261020"))
        self.assertFalse(self.m.is_outdated("10.4.20261020", "10.4.20261020"))

    def test_prompt_default_no_and_timeout(self):
        self.assertFalse(self.m.ask_yes_no("q", reader=lambda t: None))
        self.assertFalse(self.m.ask_yes_no("q", reader=lambda t: "\n"))
        self.assertTrue(self.m.ask_yes_no("q", reader=lambda t: "y\n"))
        seen = []
        self.m.ask_yes_no("q", reader=lambda t: seen.append(t))
        self.assertEqual([10], seen)
        self.assertFalse(self.m.ask_yes_no("q", env={"CI": "1"}))  # No TTY / CI: never asks.

    def hook(self):
        return json.loads((ROOT / "shaft-skills/ce-addons/shaft-engine-users/post-install.json").read_text(encoding="utf-8"))

    def test_framework_upgrade_only_after_success_and_when_outdated(self):
        (self.project / "pom.xml").write_text(SHAFT_POM.format("9.0.0"), encoding="utf-8")
        hook = self.hook()
        runner = mock.Mock(return_value=SimpleNamespace(returncode=0))
        result = self.m.framework_steps(self.project, hook, install_ok=False, env=INSTALL, runner=runner,
                                        latest_tag=lambda: "10.4.20261020", ask=lambda *a, **k: True)
        self.assertEqual("not-offered", result["upgrade"])
        self.assertEqual("installed", result["agenticInstall"])
        result = self.m.framework_steps(self.project, hook, install_ok=True, env=INSTALL, runner=runner,
                                        latest_tag=lambda: "10.4.20261020", ask=lambda *a, **k: False)
        self.assertEqual("declined", result["upgrade"])
        (self.project / "pom.xml").write_text(SHAFT_POM.format("10.5.0"), encoding="utf-8")
        result = self.m.framework_steps(self.project, hook, install_ok=True, env=INSTALL, runner=runner,
                                        latest_tag=lambda: "10.4.20261020", ask=lambda *a, **k: True)
        self.assertEqual("current", result["upgrade"])
        report = self.m.framework_steps(self.project, hook, install_ok=True, env={"CI": "1"}, runner=runner)
        self.assertEqual("reported", report["agenticInstall"])

    def test_hooks_load_from_installed_addons_only(self):
        self.assertEqual([], self.m.framework_hooks(self.project))
        target = self.project / ".chaos-engine/addons/x"
        target.mkdir(parents=True)
        (target / "post-install.json").write_text(json.dumps(self.hook()), encoding="utf-8")
        self.assertEqual(1, len(self.m.framework_hooks(self.project)))

    def test_not_detected_and_never_raises(self):
        self.assertFalse(self.m.framework_steps(self.project, self.hook(), install_ok=True, env=INSTALL)["detected"])
        with mock.patch.object(self.m, "provision", side_effect=RuntimeError("x")):
            self.assertEqual("error", self.m.run(self.project, install_ok=True)["mode"])


if __name__ == "__main__":
    unittest.main()
