"""#6974/#6973: a selected project pack's postInstall (SHAFT agentic installer) runs automatically."""

from __future__ import annotations

import importlib.util
import json
import subprocess  # nosec B404 - CompletedProcess type only.
import sys
import tempfile
import types
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _bootstrap():
    spec = importlib.util.spec_from_file_location("ce_bootstrap_6974", ROOT / "chaos-engine/bootstrap.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class _Response:
    def __init__(self, body: bytes) -> None:
        self.body, self.headers, self.status = body, {}, 200

    def read(self, *_args):
        body, self.body = self.body, b""
        return body

    def __enter__(self):
        return self

    def __exit__(self, *_args):
        return False


def _project_with_pack(temporary: str, post_install) -> Path:
    project = Path(temporary)
    pack = project / ".chaos-engine/packs/shaft"
    pack.mkdir(parents=True)
    profile = json.loads((ROOT / "shaft-skills/ce-pack/profile.json").read_text(encoding="utf-8"))
    if post_install is None:
        profile.pop("postInstall", None)
    else:
        profile["postInstall"] = post_install
    (pack / "profile.json").write_text(json.dumps(profile), encoding="utf-8")
    return project


class PackPostInstallTest(unittest.TestCase):
    def test_shaft_pack_declares_the_agentic_installer_with_skills_and_cli(self) -> None:
        profile = json.loads((ROOT / "shaft-skills/ce-pack/profile.json").read_text(encoding="utf-8"))
        self.assertEqual("scripts/mcp/install_shaft_agentic_tools.py", profile["postInstall"]["script"])
        self.assertEqual(["--install-shaft-skills", "--install-shaft-cli"], profile["postInstall"]["args"])
        self.assertTrue((ROOT / profile["postInstall"]["script"]).is_file())

    def test_selected_pack_runs_automatically_from_the_pinned_commit(self) -> None:
        bootstrap = _bootstrap()
        urls, calls = [], []

        def opener(request, *_args, **_kwargs):
            urls.append(getattr(request, "full_url", request))
            return _Response(b"print('installer')\n")

        def runner(argv, **kwargs):
            calls.append((argv, kwargs))
            return subprocess.CompletedProcess(argv, 0)

        with tempfile.TemporaryDirectory() as temporary:
            project = _project_with_pack(temporary, {"script": "scripts/mcp/install_shaft_agentic_tools.py",
                                                     "args": ["--install-shaft-skills", "--install-shaft-cli"]})
            entries = bootstrap.pack_post_installs(project)
            self.assertEqual(1, len(entries))
            code = bootstrap.run_pack_post_install(project, "ShaftHQ/SHAFT_ENGINE", "abc123", entries[0],
                                                   opener=opener, runner=runner)
        self.assertEqual(0, code)
        self.assertTrue(urls[0].endswith("/ShaftHQ/SHAFT_ENGINE/abc123/scripts/mcp/install_shaft_agentic_tools.py"))
        self.assertEqual(sys.executable, calls[0][0][0])
        self.assertEqual(["--install-shaft-skills", "--install-shaft-cli"], calls[0][0][2:])

    def test_unsafe_or_absent_post_install_is_ignored_and_skip_flag_skips(self) -> None:
        bootstrap = _bootstrap()
        for bad in (None, {"script": "../evil.py", "args": []}, {"script": "x.py", "args": ["rm"]},
                    {"script": "x.sh", "args": []}):
            with tempfile.TemporaryDirectory() as temporary:
                self.assertEqual([], bootstrap.pack_post_installs(_project_with_pack(temporary, bad)), bad)
        with tempfile.TemporaryDirectory() as temporary:
            project = _project_with_pack(temporary, {"script": "a/b.py", "args": []})
            args = types.SimpleNamespace(project=str(project), branch=None, repository="o/r",
                                         skip_pack_post_install=True)
            result: dict = {"commit": "c"}
            self.assertEqual(0, bootstrap.post_install_packs(args, result))
            self.assertNotIn("packPostInstall", result)

    def test_addons_doc_keeps_add_ons_opt_in_and_documents_the_pack_exception(self) -> None:
        text = (ROOT / "chaos-engine/references/addons.md").read_text(encoding="utf-8")
        self.assertIn("never installed by default and never auto-selected", text)
        self.assertIn("## Project-pack exception", text)
        self.assertIn("--skip-pack-post-install", text)


if __name__ == "__main__":
    unittest.main()
