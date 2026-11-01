"""
Java pack installer hooks: POM artifact detection and Maven Tools provisioning.

Bound into ``install.py`` by ``pack_binding.bind_pack``.
"""

from __future__ import annotations

import contextlib
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


def _core_helper(name: str):
    """Stand-in for a core helper; binding replaces it with the controller's own."""

    def unbound(*_args, **_kwargs):
        raise RuntimeError(f"java pack helper {name} is resolved by the controller binding")

    unbound.__name__ = name
    return unbound


_doctor_python_cli = _core_helper("_doctor_python_cli")
file_sha256 = _core_helper("file_sha256")
load_dependency_controller = _core_helper("load_dependency_controller")
load_installed_controller = _core_helper("load_installed_controller")

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
    "MAVEN_TOOLS_REPOSITORY",
    "MAVEN_TOOLS_CLONE_ATTEMPTS",
    "clone_maven_tools_source",
    "_maven_tools_docker_runtime",
    "_await_maven_tools_cache",
    "_maven_tools_build_java",
    "_maven_tools_required_major",
    "_pom_java_release_major",
    "_suggest_jdk_homes",
    "_jdk_homes_under_root",
    "_posix_jdk_search_roots",
    "_windows_jdk_search_roots",
    "maven_tools_java_home_fix_next",
    "maven_tools_wrapper_fix_next",
    "_resolve_java_path",
    "_jdk_home_from_standard_layout",
    "_jdk_home_from_host_probes",
    "_validate_standard_jdk_home",
    "preflight_maven_tools_jdk",
    "_build_and_publish_maven_tools",
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


MAVEN_TOOLS_REPOSITORY = "https://github.com/arvindand/maven-tools-mcp.git"
MAVEN_TOOLS_CLONE_ATTEMPTS = 3


def clone_maven_tools_source(runner, git: str, tag: str, source: Path, *, sleep=time.sleep) -> None:
    """
    Shallow-clone the pinned Maven Tools tag into ``source`` (#7170).

    ``core.longpaths`` is set per command because Windows Git defaults it off
    (CI only passed because the workflow sets it globally). Transient failures
    are retried on a clean directory, and the final error carries git's stderr
    so CE-INSTALL-FAILED reports say why git exited 128.
    """
    command = [
        git, "-c", "core.longpaths=true", "clone", "--branch", tag, "--depth", "1",
        MAVEN_TOOLS_REPOSITORY, str(source),
    ]
    last_error = ""
    for attempt in range(1, MAVEN_TOOLS_CLONE_ATTEMPTS + 1):
        shutil.rmtree(source, ignore_errors=True)
        try:
            runner(command, check=True, timeout=300, capture_output=True, text=True)
            return
        except subprocess.CalledProcessError as error:
            detail = (error.stderr or error.stdout or "").strip().splitlines()
            last_error = f"exit {error.returncode}: {' | '.join(detail[-3:]) or 'no git output'}"
        except subprocess.TimeoutExpired:
            last_error = "timed out after 300s"
        if attempt < MAVEN_TOOLS_CLONE_ATTEMPTS:
            sleep(2 ** attempt)
    raise ValueError(
        f"git clone of Maven Tools {tag} failed after {MAVEN_TOOLS_CLONE_ATTEMPTS} attempts ({last_error})"
    )


def _maven_tools_docker_runtime(runner, version: str) -> dict[str, str]:
    """Explicit Docker mode: verify a healthy daemon and name the pinned image."""
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


def _await_maven_tools_cache(hosts, version: str, reporter):
    """Reuse a healthy cache, wait out a busy one, discard an invalid one; None means build."""
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
            return None
        if status == "busy":
            remaining = cache_deadline - time.monotonic()
            if remaining <= 0:
                raise RuntimeError("Maven Tools MCP cache is busy")
            if not busy_announced:
                if reporter is not None and hasattr(reporter, "detail"):
                    with contextlib.suppress(Exception):  # reporter is best-effort
                        reporter.detail(
                            "Waiting for Maven Tools MCP cache lock "
                            f"(up to {MAVEN_TOOLS_CACHE_BUSY_WAIT_SECONDS:g}s)…"
                        )
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
        return None



def _maven_tools_required_major(specification: dict[str, object] | None, source: Path | None = None) -> int:
    """Return the JDK major required to build Maven Tools (pom release, else contract)."""
    major = None
    if source is not None:
        major = _pom_java_release_major(source / "pom.xml")
    if major is None and isinstance(specification, dict):
        dependencies = specification.get("dependencies")
        java_contract = dependencies.get("java") if isinstance(dependencies, dict) else None
        if isinstance(java_contract, dict) and isinstance(java_contract.get("minimumVersion"), str):
            try:
                major = int(str(java_contract["minimumVersion"]).split(".", 1)[0])
            except ValueError:
                major = None
    return major if isinstance(major, int) and major > 0 else 25


def _pom_java_release_major(pom: Path) -> int | None:
    """Best-effort JDK major from a POM release / maven.compiler.release / jdk.version."""
    try:
        if not pom.is_file() or pom.stat().st_size > _MAX_POM_BYTES:
            return None
        text = _XML_COMMENT.sub("", pom.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError):
        return None
    patterns = (
        r"<maven\.compiler\.release>\s*(\d+)\s*</maven\.compiler\.release>",
        r"<release>\s*(\d+)\s*</release>",
        r"<jdk\.version>\s*(\d+)\s*</jdk\.version>",
        r"<java\.version>\s*(\d+)\s*</java\.version>",
        r"<maven\.compiler\.source>\s*(\d+)\s*</maven\.compiler\.source>",
    )
    for pattern in patterns:
        match = re.search(pattern, text)
        if match:
            try:
                return int(match.group(1))
            except ValueError:
                continue
    return None


def _windows_jdk_search_roots() -> list[Path]:
    """Common Windows vendor roots under ProgramFiles / LocalAppData."""
    roots: list[Path] = []
    for base in (
        os.environ.get("ProgramFiles"),
        os.environ.get("ProgramFiles(x86)"),
        os.environ.get("LOCALAPPDATA"),
    ):
        if not base:
            continue
        root = Path(base)
        roots.extend(
            (
                root / "Eclipse Adoptium",
                root / "Java",
                root / "Microsoft",
                root / "Amazon Corretto",
                root / "Programs" / "Eclipse Adoptium",
            )
        )
    return roots


def _posix_jdk_search_roots() -> list[Path]:
    """Common POSIX JDK roots without absolute-path string literals."""
    return [
        Path(os.sep).joinpath("usr", "lib", "jvm"),
        Path(os.sep).joinpath("Library", "Java", "JavaVirtualMachines"),
        Path.home() / ".local/share/chaos-engine/java",
        Path.home() / ".sdkman/candidates/java",
    ]


def _jdk_homes_under_root(
    root: Path, *, java_name: str, javac_name: str, seen: set[str], found: list[str], limit: int,
) -> bool:
    """Append usable JDK homes under root. Return True when found reached limit."""
    try:
        if not root.is_dir():
            return False
        candidates = [root]
        try:
            candidates.extend(sorted(root.iterdir()))
        except OSError:
            # Directory vanished mid-scan; keep the root candidate only.
            pass
        for candidate in candidates:
            homes = [candidate]
            mac = candidate / "Contents" / "Home"
            if mac.is_dir():
                homes.append(mac)
            for home in homes:
                java = home / "bin" / java_name
                javac = home / "bin" / javac_name
                try:
                    if not java.is_file() or not javac.is_file():
                        continue
                    resolved = str(home.resolve())
                except OSError:
                    continue
                if resolved in seen:
                    continue
                seen.add(resolved)
                found.append(resolved)
                if len(found) >= limit:
                    return True
    except OSError:
        return False
    return False


def _suggest_jdk_homes() -> list[str]:
    """Return existing candidate JDK homes from common install locations."""
    java_name = "java.exe" if os.name == "nt" else "java"
    javac_name = "javac.exe" if os.name == "nt" else "javac"
    roots = _windows_jdk_search_roots() if os.name == "nt" else _posix_jdk_search_roots()
    found: list[str] = []
    seen: set[str] = set()
    for root in roots:
        if _jdk_homes_under_root(
            root, java_name=java_name, javac_name=javac_name, seen=seen, found=found, limit=5,
        ):
            return found
    return found


def maven_tools_java_home_fix_next(*, required_major: int, suggested: list[str] | None = None) -> str:
    """OS-specific commands to set JAVA_HOME/PATH, then reopen the shell and rerun install."""
    suggestions = suggested if suggested is not None else _suggest_jdk_homes()
    example = suggestions[0] if suggestions else "<jdk-home>"
    extra = ""
    if suggestions:
        listed = "; ".join(suggestions[:3])
        extra = f" Detected JDK candidate(s): {listed}."
    if os.name == "nt":
        # Set User JAVA_HOME, refresh $env:JAVA_HOME, then mutate Path with the
        # literal $jdk value (User-scope set does not update $env:JAVA_HOME).
        return (
            "fix-next: set a JDK "
            f"{required_major}+ home with bin\\java.exe and bin\\javac.exe, then reopen PowerShell "
            "and rerun the ChaosEngine install. PowerShell (User scope): "
            f"$jdk='{example}'; "
            "[Environment]::SetEnvironmentVariable('JAVA_HOME',$jdk,'User'); "
            "$env:JAVA_HOME=$jdk; "
            "$p=[Environment]::GetEnvironmentVariable('Path','User'); "
            "if ($p -notlike ('*'+$jdk+'\\bin*')) { "
            "[Environment]::SetEnvironmentVariable('Path', ($p.TrimEnd(';')+';'+$jdk+'\\bin'),'User') }; "
            "then open a new PowerShell, verify with echo $env:JAVA_HOME; java -version; javac -version."
            + extra
        )
    return (
        "fix-next: export JAVA_HOME to a JDK "
        f"{required_major}+ home that contains bin/java and bin/javac "
        f"(example: export JAVA_HOME='{example}'; export PATH=\"$JAVA_HOME/bin:$PATH\"), "
        "persist it in your shell profile, open a new shell, verify with "
        'echo "$JAVA_HOME"; java -version; javac -version, then rerun the ChaosEngine install.'
        + extra
    )


def maven_tools_wrapper_fix_next() -> str:
    """Fix-next when the upstream Maven Tools mvnw/mvnw.cmd is missing after clone."""
    wrapper = "mvnw.cmd" if os.name == "nt" else "mvnw"
    return (
        "fix-next: the Maven Tools upstream wrapper "
        f"({wrapper}) is missing from the cloned source (mvn not resolvable). "
        "Re-run the ChaosEngine install so the pinned tag is re-cloned; confirm git/network "
        f"succeed and the tag contains {wrapper}; then rerun install. "
        "Do not change JDK settings for this failure — repair the wrapper/clone first."
    )


def _resolve_java_path(java: Path) -> Path:
    try:
        return Path(java).resolve()
    except OSError:
        return Path(java)


def _jdk_home_from_standard_layout(resolved_java: Path, java_name: str) -> str | None:
    for candidate in (resolved_java.parent.parent, resolved_java.parent.parent.parent):
        if not (candidate / "bin" / java_name).is_file():
            continue
        try:
            return str(candidate.resolve())
        except OSError:
            return str(candidate)
    return None


def _jdk_home_from_host_probes(hosts, resolved_java: Path, required_major: int) -> str | None:
    """Non-standard layouts: trust hosts major+javac; JAVA_HOME=parent.parent."""
    if not resolved_java.is_file():
        return None
    major_fn = getattr(hosts, "java_major", None)
    compiler_present = getattr(hosts, "java_compiler_present", None)
    major = major_fn(resolved_java) if callable(major_fn) else None
    has_javac = compiler_present(resolved_java) if callable(compiler_present) else False
    if not (isinstance(major, int) and major >= required_major and has_javac):
        return None
    try:
        return str(resolved_java.parent.parent.resolve())
    except OSError:
        return str(resolved_java.parent.parent)


def _validate_standard_jdk_home(hosts, home: str, required_major: int, fix) -> None:
    java_name = "java.exe" if os.name == "nt" else "java"
    javac_name = "javac.exe" if os.name == "nt" else "javac"
    home_path = Path(home)
    java_bin = home_path / "bin" / java_name
    javac_bin = home_path / "bin" / javac_name
    if not java_bin.is_file():
        raise ValueError("JAVA_HOME does not point at a JDK with java. " + fix())
    compiler_present = getattr(hosts, "java_compiler_present", None)
    has_javac = (
        compiler_present(java_bin) if callable(compiler_present) else javac_bin.is_file()
    )
    if not has_javac:
        raise ValueError("JAVA_HOME does not point at a JDK with javac. " + fix())
    major_fn = getattr(hosts, "java_major", None)
    major = major_fn(java_bin) if callable(major_fn) else None
    if not isinstance(major, int) or major < required_major:
        observed = major if isinstance(major, int) else "unknown"
        raise ValueError(
            f"JAVA_HOME java major {observed} is below required {required_major}. "
            + fix()
        )


def preflight_maven_tools_jdk(
    hosts,
    java: Path,
    *,
    required_major: int,
    wrapper: Path | None = None,
) -> str:
    """Validate the JDK home used for Maven Tools before `mvn package`.

    Returns the usable JAVA_HOME string. Raises ValueError with an OS-specific
    fix-next when JAVA_HOME/javac/java major/wrapper checks fail.
    """
    fix = lambda: maven_tools_java_home_fix_next(required_major=required_major)
    if wrapper is not None and not wrapper.is_file():
        raise ValueError(
            "Maven Tools upstream wrapper is missing (mvn not resolvable). "
            + maven_tools_wrapper_fix_next()
        )
    java_name = "java.exe" if os.name == "nt" else "java"
    resolve_home = getattr(hosts, "_jdk_home_from_java_binary", None)
    home: str | None = None
    if callable(resolve_home):
        try:
            home = resolve_home(Path(java))
        except (OSError, TypeError, ValueError):
            # Host helper rejected the binary; fall through to layout probes.
            home = None
    resolved_java = _resolve_java_path(java)
    if home is None:
        home = _jdk_home_from_standard_layout(resolved_java, java_name)
    nonstandard = False
    if home is None:
        home = _jdk_home_from_host_probes(hosts, resolved_java, required_major)
        nonstandard = home is not None
    if home is None:
        raise ValueError(
            "JAVA_HOME is unset or unusable for Maven Tools "
            f"(no JDK home with bin/java from {java}). "
            + fix()
        )
    if not nonstandard:
        _validate_standard_jdk_home(hosts, home, required_major, fix)
    return home

def _maven_tools_build_java(hosts, specification, *, opener, reporter, confirmer) -> Path:
    """Return a JDK (with javac) new enough to build Maven Tools, provisioning Temurin if needed."""
    java_minimum_major = _maven_tools_required_major(specification)
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
            "Temurin JDK "
            f"{java_minimum_major} with javac is required for Maven Tools MCP "
            "(JRE-only Java is not enough); install a matching JDK or set CHAOSENGINE_JAVA. "
            + maven_tools_java_home_fix_next(required_major=java_minimum_major)
        )
    return java


def _build_and_publish_maven_tools(
    hosts, runner, java: Path, version: str, tag: str, *, confirmer, reporter,
    specification: dict[str, object] | None = None,
) -> None:
    """Clone the pinned tag, build it with the upstream wrapper and publish the JAR to the cache."""
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
        clone_maven_tools_source(runner, git, tag, source)
        revision = runner(
            [git, "-C", str(source), "rev-parse", "HEAD"], check=True,
            capture_output=True, text=True, timeout=30,
        ).stdout.strip().casefold()
        if re.fullmatch(r"[0-9a-f]{40}", revision) is None:
            raise ValueError("Maven Tools stable tag did not resolve to an immutable commit")
        wrapper = source / ("mvnw.cmd" if os.name == "nt" else "mvnw")
        if not wrapper.is_file():
            raise ValueError(
                "Maven Tools upstream wrapper is missing. "
                + maven_tools_wrapper_fix_next()
            )
        if os.name != "nt":
            wrapper.chmod(wrapper.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
        required_major = _maven_tools_required_major(specification, source)
        # Preflight before mvn package: fail closed with OS-specific fix-next.
        java_home = preflight_maven_tools_jdk(
            hosts, java, required_major=required_major, wrapper=wrapper,
        )
        environment = os.environ.copy()
        environment["JAVA_HOME"] = java_home
        if confirmer is not None:
            confirmer("Build and install Maven Tools")
        # Upstream tests are skipped: they are upstream CI's job, and a crashing
        # upstream test (#6339) must not block installing a tagged release.
        try:
            runner(
                [str(wrapper), "-B", "clean", "package", "-Pci", "-DskipTests"],
                cwd=source,
                env=environment,
                check=True,
                timeout=900,
            )
        except subprocess.CalledProcessError as error:
            detail = ((error.stderr or error.stdout or "") + " " + str(error)).casefold()
            if "environment variable" in detail or "java_home" in detail:
                raise ValueError(
                    "Maven Tools build failed because JAVA_HOME is missing or unusable. "
                    + maven_tools_java_home_fix_next(required_major=required_major)
                ) from error
            raise
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


def ensure_maven_tools(
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
        return _maven_tools_docker_runtime(runner, version)
    if mode != "native":
        raise ValueError("unsupported Maven Tools mode")
    tag = f"v{version}"
    existing = _await_maven_tools_cache(hosts, version, reporter)
    if existing is not None:
        return existing
    java = _maven_tools_build_java(
        hosts, specification, opener=opener, reporter=reporter, confirmer=confirmer,
    )
    # Ensure a Maven CLI exists for operators when ambient mvn is missing/too old.
    # Upstream build still prefers mvnw below; managed Maven is provisioned into the
    # CE tools cache during this dependencies phase rather than after a failure.
    ensure_maven = getattr(hosts, "ensure_managed_maven", None)
    if callable(ensure_maven):
        ensure_maven(
            specification, opener=opener, reporter=reporter, confirmer=confirmer,
        )
    _build_and_publish_maven_tools(
        hosts, runner, java, version, tag, confirmer=confirmer, reporter=reporter,
        specification=specification,
    )
    runtime = hosts.discover_maven_tools_runtime()
    probe = hosts.probe_maven_tools_runtime
    if runtime is None or not probe(*runtime):
        detail = "" if runtime is None else str(getattr(probe, "last_failure", "") or "")
        reason = "Maven Tools MCP publication probe failed"
        if runtime is None:
            reason += ": no verified Java 17+ runtime and jar"
        elif detail:
            reason += ": " + detail
        raise ValueError(reason)
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
