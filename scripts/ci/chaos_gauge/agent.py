"""Harbor Codex treatment that installs the complete pinned ChaosEngine source."""

from __future__ import annotations

import hashlib
import re
import shlex
from pathlib import Path
from typing import Any

from harbor.agents.installed.codex import Codex


_SHA256 = re.compile(r"[0-9a-f]{64}")
_COMMIT = re.compile(r"[0-9a-f]{40}")


def _tree_sha256(root: Path) -> str:
    digest = hashlib.sha256()
    for path in sorted(item for item in root.rglob("*") if item.is_file()):
        relative = path.relative_to(root).as_posix()
        if "__pycache__" in path.parts or path.suffix == ".pyc":
            continue
        digest.update(f"{relative}\0{hashlib.sha256(path.read_bytes()).hexdigest()}\n".encode())
    return digest.hexdigest()


class ChaosEngineCodex(Codex):
    """Native Harbor Codex plus full ChaosEngine installer and Codex activation."""

    def __init__(
        self,
        *args: Any,
        harness_source: str,
        harness_commit: str,
        harness_sha256: str,
        adapter_sha256: str,
        **kwargs: Any,
    ) -> None:
        """Bind this treatment to exact local adapter and harness bytes."""
        super().__init__(*args, **kwargs)
        source = Path(harness_source).resolve()
        expected = Path(__file__).resolve().parents[3] / "chaos-engine"
        if source != expected or not (source / "install.py").is_file():
            raise ValueError("harness_source must be this repository's canonical chaos-engine directory")
        if _COMMIT.fullmatch(harness_commit) is None:
            raise ValueError("harness_commit must be an immutable 40-hex revision")
        if _SHA256.fullmatch(harness_sha256) is None or _tree_sha256(source) != harness_sha256:
            raise ValueError("canonical ChaosEngine source digest does not match treatment")
        adapter_digest = hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
        if _SHA256.fullmatch(adapter_sha256) is None or adapter_digest != adapter_sha256:
            raise ValueError("ChaosGauge adapter digest does not match treatment")
        self._harness_source = source
        self._harness_commit = harness_commit

    async def install(self, environment) -> None:
        await super().install(environment)
        await self.ensure_system_dependencies(environment, ("git", "python3"))
        remote_source = "/installed-agent/chaos-engine-source"
        await environment.upload_dir(self._harness_source, remote_source)
        script = f"""
import hashlib, json, runpy
from pathlib import Path
project = Path('/app')
source = Path({remote_source!r})
commit = {self._harness_commit!r}
installer = runpy.run_path(str(source / 'install.py'))
target = installer['install_with_dependencies'](
    project, source, commit,
    distribution='repository',
)
manifest = installer['verify_install'](target)
hosts = installer['load_installed_controller'](target, 'hosts')
hosts.activate_detected_plugins(project)
host_status = hosts.verify(project, core_commit=commit)
dependency_status = installer['doctor_with_dependencies'](project, verify_clients=False)
required = [
    project / 'AGENTS.md',
    project / '.codex/hooks.json',
    project / '.agents/skills/chaos-engine/SKILL.md',
    project / '.chaos-engine-hosts.json',
]
receipt = json.loads((project / '.chaos-engine-hosts.json').read_text())
if manifest['source']['commit'] != commit or receipt.get('coreCommit') != commit:
    raise RuntimeError('ChaosEngine receipt does not match treatment commit')
if dependency_status.get('status') != 'healthy':
    raise RuntimeError('ChaosEngine dependency provisioning is incomplete')
if host_status.get('status') != 'healthy' or not all(path.is_file() for path in required):
    raise RuntimeError('ChaosEngine Codex host activation is incomplete')
print(json.dumps({{'status': 'installed', 'coreSha256': hashlib.sha256((target / 'manifest.json').read_bytes()).hexdigest()}}, sort_keys=True))
"""
        await self.exec_as_agent(
            environment,
            command=f"python3 -c {shlex.quote(script)}",
        )
