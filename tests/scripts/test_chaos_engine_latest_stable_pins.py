"""Companion intensity and vendored pin commits follow the canonical sources."""

from __future__ import annotations

import importlib.util
import json
import os
import shutil
import subprocess  # nosec B404 - fixed node argv for the shipped resolver, never a shell.
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "chaos-engine"


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class VendorPinCommitTest(unittest.TestCase):
    def test_published_upstream_commit_is_the_pin_file(self):
        hosts = _load("hosts_latest_stable_pins", SOURCE / "hosts.py")
        images = hosts.companion_plugin_images()
        for name, attribute in (
            ("caveman", "CAVEMAN_UPSTREAM_COMMIT"),
            ("ponytail", "PONYTAIL_UPSTREAM_COMMIT"),
            ("icm-architect", "ICM_ARCHITECT_UPSTREAM_COMMIT"),
        ):
            pin = json.loads((SOURCE / "vendor" / name / "PIN.json").read_text(encoding="utf-8"))
            with self.subTest(vendor=name):
                self.assertEqual(pin["commit"], getattr(hosts, attribute))
                upstream = images[f"plugins/{name}/UPSTREAM.md"].decode("utf-8")
                self.assertIn(f"`{pin['commit']}`", upstream)

    def test_pin_loader_follows_a_mutated_copy(self):
        hosts = _load("hosts_latest_stable_pins_copy", SOURCE / "hosts.py")
        original = json.loads(
            (SOURCE / "vendor/caveman/PIN.json").read_text(encoding="utf-8")
        )
        mutated = dict(original)
        replacement = "b" if not str(original["commit"]).startswith("b") else "c"
        mutated["commit"] = replacement + str(original["commit"])[1:]
        self.assertNotEqual(original["commit"], mutated["commit"])
        with tempfile.TemporaryDirectory() as temporary:
            vendor = Path(temporary) / "vendor" / "caveman"
            vendor.mkdir(parents=True)
            (vendor / "PIN.json").write_text(json.dumps(mutated), encoding="utf-8")
            self.assertEqual(
                mutated["commit"], hosts._vendor_pin_commit("caveman", root=Path(temporary))
            )


@unittest.skipIf(shutil.which("node") is None, "node is required for companion mode resolvers")
class CompanionProjectModeTest(unittest.TestCase):
    def test_project_mode_beats_user_default_and_stop_turns_off(self):
        node = shutil.which("node")
        script = r"""
const fs = require('fs');
const path = require('path');
const project = process.argv[1];
const home = process.argv[2];
const empty = process.argv[3];
delete process.env.CAVEMAN_DEFAULT_MODE;
delete process.env.PONYTAIL_DEFAULT_MODE;
const caveman = require(process.argv[4]);
const parse = require(process.argv[5]);
const ponytail = require(process.argv[6]);

function writeConfig(dir, mode) {
  const folder = path.join(home, dir);
  fs.mkdirSync(folder, {recursive: true});
  fs.writeFileSync(path.join(folder, 'config.json'), JSON.stringify({defaultMode: mode}));
}

function resolve(at) {
  process.chdir(at);
  return {
    caveman: caveman.getDefaultMode(at),
    ponytail: ponytail.getDefaultMode(),
  };
}

const lines = [];
for (const label of ['off-run-1', 'off-run-2']) {
  writeConfig('caveman', 'off');
  writeConfig('ponytail', 'off');
  const mode = resolve(project);
  lines.push(label + ' caveman=' + mode.caveman + ' ponytail=' + mode.ponytail);
}
writeConfig('caveman', 'caveman');
writeConfig('ponytail', 'lite');
const lower = resolve(project);
lines.push('lower-run caveman=' + lower.caveman + ' ponytail=' + lower.ponytail);
const stop = parse.parseModeChange('stop caveman', {
  getDefaultMode: () => caveman.getDefaultMode(project),
});
const normal = parse.parseModeChange('normal mode', {
  getDefaultMode: () => caveman.getDefaultMode(project),
});
lines.push(
  'stop caveman=' + (stop && stop.action) + ' ponytail=' +
  (ponytail.isDeactivationCommand('stop ponytail') ? 'off' : 'on')
);
lines.push(
  'normal caveman=' + (normal && normal.action) + ' ponytail=' +
  (ponytail.isDeactivationCommand('normal mode') ? 'off' : 'on')
);
writeConfig('caveman', 'off');
writeConfig('ponytail', 'off');
const outside = resolve(empty);
lines.push('non-project caveman=' + outside.caveman + ' ponytail=' + outside.ponytail);
process.stdout.write(lines.join('\n') + '\n');
"""
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "project"
            empty = root / "empty"
            home = root / "home"
            marker = project / "chaos-engine/skills/chaos-engine/SKILL.md"
            marker.parent.mkdir(parents=True)
            marker.write_text("# ChaosEngine\n", encoding="utf-8")
            empty.mkdir()
            home.mkdir()
            env = os.environ.copy()
            env.pop("CAVEMAN_DEFAULT_MODE", None)
            env.pop("PONYTAIL_DEFAULT_MODE", None)
            env["HOME"] = str(home)
            env["USERPROFILE"] = str(home)
            env["XDG_CONFIG_HOME"] = str(home)
            completed = subprocess.run(  # nosec B603 - resolved node path and a fixed script.
                [
                    node,
                    "-e",
                    script,
                    str(project),
                    str(home),
                    str(empty),
                    str(SOURCE / "vendor/caveman/src/hooks/caveman-config.js"),
                    str(SOURCE / "vendor/caveman/src/hooks/caveman-parse.js"),
                    str(SOURCE / "vendor/ponytail/hooks/ponytail-config.js"),
                ],
                cwd=project,
                env=env,
                capture_output=True,
                text=True,
                timeout=30,
                check=False,
            )
        self.assertEqual(0, completed.returncode, completed.stderr)
        print(completed.stdout, end="")
        lines = completed.stdout.splitlines()
        self.assertEqual(
            [
                "off-run-1 caveman=ultracave ponytail=ultra",
                "off-run-2 caveman=ultracave ponytail=ultra",
                "lower-run caveman=ultracave ponytail=ultra",
                "stop caveman=clear ponytail=off",
                "normal caveman=clear ponytail=off",
                "non-project caveman=off ponytail=off",
            ],
            lines,
        )
        self.assertEqual(lines[0].split(" ", 1)[1], lines[1].split(" ", 1)[1])


if __name__ == "__main__":
    unittest.main()
