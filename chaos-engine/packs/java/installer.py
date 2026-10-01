"""Java pack installer hooks: POM artifact detection and Maven Tools provisioning.

Bound into ``install.py`` by ``pack_binding.bind_pack``.
"""

from __future__ import annotations

import json
import os
import re
import shutil
import stat
import subprocess  # nosec B404 - fixed list-form Maven build commands.
import sys
import tempfile
import time
from pathlib import Path

# Core helpers resolved through the install controller namespace after binding.
_doctor_python_cli = file_sha256 = load_dependency_controller = None
load_installed_controller = None

__all__ = (
    "MAVEN_TOOLS_CACHE_BUSY_WAIT_SECONDS",
    "MAVEN_TOOLS_CACHE_BUSY_POLL_SECONDS",
    "_MAX_POM_BYTES",
    "_MAVEN_ID_PARENTS",
    "_XML_COMMENT",
    "_XML_TAG",
    "java_pack_enabled",
    "maven_coordinate_ids",
    "project_maven_ids",
    "ensure_maven_tools",
    "maven_tools_repair_fix_next",
    "repair_maven_tools",
)


MAVEN_TOOLS_CACHE_BUSY_WAIT_SECONDS = 30.0


MAVEN_TOOLS_CACHE_BUSY_POLL_SECONDS = 0.2


_XML_COMMENT = re.compile(r"<!--.*?-->", re.DOTALL)
_XML_TAG = re.compile(
    r"<(/?)(?:[\w.-]+:)?([A-Za-z_][\w.-]*)(?:\s[^>]*)?(/?)>",
    re.DOTALL,
)


_MAX_POM_BYTES = 2_000_000


_MAVEN_ID_PARENTS = {
    ("project", "artifactId"),
    ("modules", "module"),
    ("dependency", "artifactId"),
}


def java_pack_enabled(project: Path) -> bool:
    """The java pack enables its tools for a project with a root build file."""
    return (project / "pom.xml").is_file()


def maven_coordinate_ids(pom: Path) -> set[str]:
    """Return project, module, and dependency artifact ids from one POM."""
    try:
        raw = pom.read_bytes()
    except OSError:
        return set()
    if len(raw) > _MAX_POM_BYTES:
        return set()
    try:
        text = _XML_COMMENT.sub("", raw.decode("utf-8"))
    except UnicodeDecodeError:
        return set()
    ids: set[str] = set()
    stack: list[str] = []
    pending_parent: str | None = None
    last_end = 0
    for match in _XML_TAG.finditer(text):
        if pending_parent is not None:
            value = text[last_end:match.start()].strip()
            if value:
                ids.add(value)
            pending_parent = None
        closing, name, self_close = match.group(1), match.group(2), match.group(3)
        last_end = match.end()
        if closing:
            if stack and stack[-1] == name:
                stack.pop()
            continue
        if self_close:
            continue
        stack.append(name)
        if len(stack) >= 2 and (stack[-2], stack[-1]) in _MAVEN_ID_PARENTS:
            pending_parent = stack[-2]
    return ids


def project_maven_ids(project: Path) -> set[str]:
    pom = project / "pom.xml"
    if not pom.is_file():
        return set()
    return maven_coordinate_ids(pom)


def ensure_maven_tools(  # noqa: MC0001 - cross-resource provisioning is one transaction.
    target: Path, specification: dict[str, object], *, runner=subprocess.run,
    reporter=None, confirmer=None, opener=None, mode: str = "native",
) -> tuple[Path, Path] | dict[str, str]:
    hosts = load_installed_controller(target, "hosts")
    dependencies = load_dependency_controller(target)
    contract = specification["dependencies"]["maven-tools-mcp"]
    resolver_options = {} if opener is None else {"opener": opener}
    version = dependencies.resolve_stable_version(
        "maven-tools-mcp", contract, **resolver_options
    )
    if mode == "docker":
        docker = shutil.which("docker")
        if not docker:
            raise ValueError("explicit Maven Tools Docker mode requires healthy Docker")
        probe = runner(
            [docker, "version", "--format", "{{.Server.Version}}"],
            capture_output=True, text=True, check=False, timeout=30,
        )
        if probe.returncode != 0:
            raise ValueError("explicit Maven Tools Docker mode requires healthy Docker")
        return {
            "mode": "docker",
            "command": str(Path(docker).resolve()),
            "image": f"arvindand/maven-tools-mcp:{version}",
        }
    if mode != "native":
        raise ValueError("unsupported Maven Tools mode")
    tag = f"v{version}"
    cache_deadline = time.monotonic() + MAVEN_TOOLS_CACHE_BUSY_WAIT_SECONDS
    busy_announced = False
    while True:
        cache_status = hosts.maven_tools_cache_status(version)
        status = cache_status.get("status")
        if status == "healthy":
            existing = hosts.discover_maven_tools_runtime()
            if existing is not None:
                # Healthy shared cache is reuse (action:reused); probe flakes must
                # not force a rebuild that then hits "cache version already exists".
                return existing
            break
        if status == "busy":
            remaining = cache_deadline - time.monotonic()
            if remaining <= 0:
                raise RuntimeError("Maven Tools MCP cache is busy")
            if not busy_announced:
                if reporter is not None and hasattr(reporter, "detail"):
                    try:
                        reporter.detail(
                            "Waiting for Maven Tools MCP cache lock "
                            f"(up to {MAVEN_TOOLS_CACHE_BUSY_WAIT_SECONDS:g}s)…"
                        )
                    except Exception:  # noqa: BLE001 - reporter is best-effort
                        pass
                print(
                    "ChaosEngine: waiting for Maven Tools MCP cache "
                    f"(up to {MAVEN_TOOLS_CACHE_BUSY_WAIT_SECONDS:g}s)…",
                    file=sys.stderr,
                )
                busy_announced = True
            time.sleep(min(MAVEN_TOOLS_CACHE_BUSY_POLL_SECONDS, remaining))
            continue
        if status != "absent":
            discard = getattr(hosts, "discard_invalid_maven_tools_cache", None)
            if not callable(discard):
                raise ValueError("Maven Tools MCP cache is invalid")
            discard(version)
        break
    java_minimum = "25.0.0"
    java_contract = specification.get("dependencies", {}).get("java") if isinstance(
        specification.get("dependencies"), dict
    ) else None
    if isinstance(java_contract, dict) and isinstance(java_contract.get("minimumVersion"), str):
        java_minimum = str(java_contract["minimumVersion"])
    try:
        java_minimum_major = int(str(java_minimum).split(".", 1)[0])
    except ValueError:
        java_minimum_major = 25
    java_candidates = []
    configured = os.environ.get("CHAOSENGINE_JAVA")
    java_home = os.environ.get("JAVA_HOME")
    path_java = shutil.which("java")
    if configured:
        java_candidates.append(Path(configured).expanduser())
    if java_home:
        java_candidates.append(Path(java_home) / "bin" / ("java.exe" if os.name == "nt" else "java"))
    if path_java:
        java_candidates.append(Path(path_java))
    java = next(
        (
            item.resolve()
            for item in java_candidates
            if item.is_file()
            and (hosts.java_major(item.resolve()) or 0) >= java_minimum_major
        ),
        None,
    )
    compiler_present = getattr(hosts, "java_compiler_present", None)
    if java is not None and callable(compiler_present) and not compiler_present(java):
        # JRE-only Java cannot compile Maven Tools; prefer managed Temurin JDK.
        java = None
    if java is None:
        provision = getattr(hosts, "ensure_managed_temurin_jdk", None)
        if callable(provision):
            managed = provision(
                specification, opener=opener, reporter=reporter, confirmer=confirmer,
            )
            if managed is not None:
                java = managed
    if java is None:
        raise ValueError(
            "Temurin JDK 25 with javac is required for Maven Tools MCP "
            "(JRE-only Java is not enough); install Temurin 25 JDK or set CHAOSENGINE_JAVA"
        )
    # Ensure a Maven CLI exists for operators when ambient mvn is missing/too old.
    # Upstream build still prefers mvnw below; managed Maven is provisioned into the
    # CE tools cache during this dependencies phase rather than after a failure.
    ensure_maven = getattr(hosts, "ensure_managed_maven", None)
    if callable(ensure_maven):
        ensure_maven(
            specification, opener=opener, reporter=reporter, confirmer=confirmer,
        )
    cache_root = hosts.maven_tools_cache_root()
    cache_root.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix=".maven-tools-source-") as source_name:
        source = Path(source_name) / "source"
        git = shutil.which("git")
        if not git:
            raise ValueError("git is required to install Maven Tools MCP")
        if confirmer is not None:
            confirmer(f"Download Maven Tools {tag} source with git")
        if reporter is not None:
            reporter.start("Install Maven Tools", detail=tag)
        runner([
            git, "clone", "--branch", tag, "--depth", "1",
            "https://github.com/arvindand/maven-tools-mcp.git", str(source),
        ], check=True, timeout=300)
        revision = runner(
            [git, "-C", str(source), "rev-parse", "HEAD"], check=True,
            capture_output=True, text=True, timeout=30,
        ).stdout.strip().casefold()
        if re.fullmatch(r"[0-9a-f]{40}", revision) is None:
            raise ValueError("Maven Tools stable tag did not resolve to an immutable commit")
        wrapper = source / ("mvnw.cmd" if os.name == "nt" else "mvnw")
        if not wrapper.is_file():
            raise ValueError("Maven Tools upstream wrapper is missing")
        if os.name != "nt":
            wrapper.chmod(wrapper.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
        environment = os.environ.copy()
        environment["JAVA_HOME"] = str(java.parent.parent)
        if confirmer is not None:
            confirmer("Build and install Maven Tools")
        # Upstream tests are skipped: they are upstream CI's job, and a crashing
        # upstream test (#6339) must not block installing a tagged release.
        runner([str(wrapper), "-B", "clean", "package", "-Pci", "-DskipTests"], cwd=source, env=environment, check=True, timeout=900)
        built = source / f"target/maven-tools-mcp-{version}.jar"
        if not built.is_file() or built.stat().st_size == 0:
            raise ValueError("Maven Tools build did not produce the pinned JAR")
        with tempfile.TemporaryDirectory(prefix=".publishing-", dir=cache_root) as staging_name:
            staging = Path(staging_name)
            jar = staging / built.name
            shutil.copyfile(built, jar)
            receipt = {"version": version, "commit": revision, "jar": jar.name, "sha256": file_sha256(jar)}
            (staging / hosts.MAVEN_TOOLS_MCP_RECEIPT).write_text(json.dumps(receipt, sort_keys=True) + "\n", encoding="utf-8")
            hosts.publish_maven_tools_cache(staging)
    runtime = hosts.discover_maven_tools_runtime()
    if runtime is None or not hosts.probe_maven_tools_runtime(*runtime):
        raise ValueError("Maven Tools MCP publication probe failed")
    return runtime


def maven_tools_repair_fix_next() -> str:
    """Return the targeted repair for a corrupt or missing Maven Tools JAR (#6337)."""
    cli = _doctor_python_cli()
    return (
        f"Run `{cli} .chaos-engine/install.py repair --project . --component maven-tools-mcp` "
        "(discards a corrupt cached JAR, then reuses a healthy cached version or "
        f"reinstalls), then `{cli} .chaos-engine/install.py doctor --project .`."
    )


def repair_maven_tools(
    project: Path,
    target: Path,
    host_controller,
    *,
    runner=None,
    rebind=None,
) -> dict[str, object]:
    """Discard corrupt Maven Tools caches, then reuse a healthy version or reinstall (#6337)."""
    discarded: list[str] = []
    for version in host_controller.maven_tools_cached_versions():
        if host_controller.maven_tools_cache_status(version).get("status") == "invalid":
            host_controller.discard_invalid_maven_tools_cache(version)
            discarded.append(version)
    selected = host_controller.selected_maven_tools_cache_status()
    action = "reused"
    if selected.get("status") != "healthy":
        controller = load_dependency_controller(target)
        specification = controller.load_specification(target / "dependencies.json")
        ensure_maven_tools(target, specification, runner=runner or subprocess.run)
        selected = host_controller.selected_maven_tools_cache_status()
        action = "reinstalled"
    healthy = selected.get("status") == "healthy"
    if healthy and rebind is not None:
        rebind()
    return {
        "status": "repaired" if healthy else "recovery-required",
        "component": "maven-tools-mcp",
        "action": action,
        "discarded": discarded,
        "version": selected.get("version"),
        "cacheStatus": selected.get("status"),
    }
