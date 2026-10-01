"""Java pack: managed Temurin JDK, managed Maven, and the Maven Tools MCP runtime.

Bound into ``hosts.py`` by ``pack_binding.bind_pack``; functions resolve core
helpers through the host controller namespace.
"""

from __future__ import annotations

from contextlib import contextmanager
import errno
import hashlib
import json
import os
import urllib.request
import queue
import re
import secrets
import shutil
import stat
import subprocess  # nosec B404 - probes a resolved local Java executable.
import sys
import threading
import time
from pathlib import Path

# Core helpers resolved through the hosts controller namespace after binding.
_cache_anchor = _discard_tree_nofollow = _load_dependencies_controller = None
_path_is_under = _rename_no_replace = _rmdir_stable_cache_directory = None
_runtime_contract = _tools_host_platform = _unlink_stable_cache_file = None
_validate_cache_path = is_link_or_reparse = portable_python_server = None

__all__ = (
    "MAVEN_TOOLS_JAR_SUFFIX",
    "MAVEN_TOOLS_MCP_VERSION",
    "MAVEN_TOOLS_MCP_COMMIT",
    "MAVEN_TOOLS_MCP_RECEIPT",
    "MAVEN_TOOLS_CACHE_LOCK",
    "MAVEN_TOOLS_CACHE_LOCK_MAGIC",
    "MAVEN_TOOLS_CACHE_LOCK_INIT_GRACE_SECONDS",
    "MAVEN_TOOLS_CACHE_LOCK_INIT_POLL_SECONDS",
    "TEMURIN_RECEIPT",
    "LEGACY_MAVEN_TOOLS_SERVER",
    "java_major",
    "java_compiler_present",
    "managed_temurin_root",
    "managed_maven_root",
    "maven_version",
    "verified_managed_maven",
    "ensure_managed_temurin_jdk",
    "ensure_managed_maven",
    "verified_maven_tools_jar",
    "maven_tools_data_root",
    "maven_tools_cache_root",
    "_maven_tools_version_directory",
    "_read_maven_tools_cache_lock",
    "maven_tools_cache_lock",
    "_maven_tools_cache_status_unlocked",
    "maven_tools_cache_status",
    "purge_maven_tools_cache",
    "discard_invalid_maven_tools_cache",
    "_reuse_healthy_maven_tools_cache",
    "publish_maven_tools_cache",
    "maven_tools_cached_versions",
    "_configured_maven_tools_jar",
    "selected_maven_tools_cache_status",
    "discover_maven_tools_runtime",
    "verified_managed_temurin",
    "probe_maven_tools_runtime",
    "_add_maven_server",
    "exact_legacy_native_maven_server",
    "_LEGACY_NATIVE_MAVEN_CODEX_BLOCK",
    "remove_exact_legacy_native_maven_codex_block",
)


# #6216: branch-agnostic fallback; the tool's own fix-next names the resolved branch.
# Account-relative jar location (the account root varies per machine).
MAVEN_TOOLS_JAR_SUFFIX = "ChaosEngine/tools/maven-tools-mcp/3.2.0/maven-tools-mcp-3.2.0.jar"


MAVEN_TOOLS_MCP_VERSION = "3.2.0"


MAVEN_TOOLS_MCP_COMMIT = "4475ff6c61f23ea9a93cb6d5665a63235ef2ef36"


MAVEN_TOOLS_MCP_RECEIPT = "install-receipt.json"


MAVEN_TOOLS_CACHE_LOCK = ".cache.lock"


MAVEN_TOOLS_CACHE_LOCK_MAGIC = b"chaos-engine-maven-tools-cache-lock-v1\n"


# The creator writes the magic right after O_EXCL; an opener in that window sees
# empty or partial magic and waits this long before calling it a collision (#6333).
MAVEN_TOOLS_CACHE_LOCK_INIT_GRACE_SECONDS = 1.0


MAVEN_TOOLS_CACHE_LOCK_INIT_POLL_SECONDS = 0.02


TEMURIN_RECEIPT = "runtime-receipt.json"


LEGACY_MAVEN_TOOLS_SERVER = {
    "command": "docker",
    "args": ["run", "-i", "--rm", "arvindand/maven-tools-mcp:3.2.0"],
}


def java_major(java: Path) -> int | None:
    try:
        result = subprocess.run(  # nosec B603 - executable is resolved before use.
            [str(java), "-version"],
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return None
    match = re.search(r'version "(?P<major>\d+)', result.stderr + result.stdout)
    return int(match.group("major")) if match else None


def java_compiler_present(java: Path) -> bool:
    """True when the Java home that owns `java` also ships `javac` (JDK, not JRE)."""
    try:
        resolved = java.resolve(strict=True)
    except OSError:
        return False
    javac = resolved.with_name("javac.exe" if os.name == "nt" else "javac")
    return javac.is_file() and not is_link_or_reparse(javac)


def managed_temurin_root(version: str = "25.0.4+7") -> Path:
    system, architecture, _host = _tools_host_platform()
    return (
        maven_tools_cache_root().parent
        / "temurin"
        / version
        / f"{system}-{architecture}"
    )


def managed_maven_root(version: str = "3.9.12") -> Path:
    system, architecture, _host = _tools_host_platform()
    return (
        maven_tools_cache_root().parent
        / "maven"
        / version
        / f"{system}-{architecture}"
    )


def maven_version(mvn: Path) -> str | None:
    """Parse a stable Apache Maven version from `mvn -v` output."""
    try:
        result = subprocess.run(  # nosec B603 - executable is resolved before use.
            [str(mvn), "-v"],
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return None
    match = re.search(
        r"Apache Maven (?P<version>\d+(?:\.\d+){1,3})",
        result.stdout + result.stderr,
    )
    return match.group("version") if match else None


def verified_managed_maven(candidate: Path, host_platform: str, *, version: str) -> Path | None:
    if not candidate.is_file() or is_link_or_reparse(candidate):
        return None
    receipt_path = candidate.parents[1] / TEMURIN_RECEIPT
    if not receipt_path.is_file() or is_link_or_reparse(receipt_path):
        return None
    try:
        receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
        digest = hashlib.sha256(candidate.read_bytes()).hexdigest()
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return None
    expected_architecture = (
        "x64" if host_platform == "windows-arm64" else host_platform.split("-", 1)[1]
    )
    expected = {
        "schemaVersion": 1,
        "runtime": "maven",
        "version": version,
        "hostPlatform": host_platform,
        "artifactArchitecture": expected_architecture,
        "emulated": host_platform == "windows-arm64",
        "mvn": candidate.relative_to(receipt_path.parent).as_posix(),
        "mvnSha256": digest,
    }
    if receipt != expected:
        return None
    observed = maven_version(candidate)
    if observed is None:
        return None
    module = _load_dependencies_controller()
    if module is None:
        return None
    try:
        if not module.version_at_least(observed, version):
            return None
    except ValueError:
        return None
    return candidate.resolve()


def ensure_managed_temurin_jdk(
    specification: dict[str, object] | None = None,
    *,
    opener=None,
    reporter=None,
    confirmer=None,
) -> Path | None:
    """Provision checksum-verified Temurin JDK into the CE tools cache when needed (#5630)."""
    system, architecture, host_platform = _tools_host_platform()
    temurin = _runtime_contract(specification, "temurin")
    version = (
        str(temurin["version"])
        if isinstance(temurin, dict) and isinstance(temurin.get("version"), str)
        else "25.0.4+7"
    )
    root = managed_temurin_root(version)
    java = root / (
        "bin/java.exe" if os.name == "nt" else
        "Contents/Home/bin/java" if sys.platform == "darwin" else "bin/java"
    )
    verified = verified_managed_temurin(java, host_platform, version=version)
    if verified is not None and java_compiler_present(verified):
        return verified
    if specification is None or temurin is None:
        return None
    artifacts = temurin.get("artifacts")
    artifact = artifacts.get(host_platform) if isinstance(artifacts, dict) else None
    if not isinstance(artifact, dict):
        return None
    url, digest = artifact.get("url"), artifact.get("sha256")
    if not isinstance(url, str) or not isinstance(digest, str):
        return None
    module = _load_dependencies_controller()
    if module is None:
        return None
    open_url = opener or urllib.request.urlopen
    parent = root.parent
    parent.mkdir(parents=True, exist_ok=True)
    if confirmer is not None:
        confirmer(f"Download Temurin JDK {version} from {url}")
    if reporter is not None:
        reporter.trace(f"provision managed Temurin JDK {version}")
    suffix = ".zip" if str(url).endswith(".zip") else ".tar.gz"
    transaction = parent / f".{version}-{architecture}.{secrets.token_hex(8)}.building"
    archive = transaction.with_suffix(suffix)
    try:
        if root.exists() or is_link_or_reparse(root):
            # Incomplete prior attempt — refuse to clobber without a clean tree.
            if verified_managed_temurin(java, host_platform, version=version) is None:
                raise ValueError("existing managed Temurin JDK tree is invalid")
            return java.resolve()
        module._download_artifact(str(url), archive, str(digest), open_url, reporter=reporter)
        module._extract_runtime_archive(archive, transaction)
        # Write runtime receipt expected by verified_managed_temurin.
        relative_java = (
            "bin/java.exe" if os.name == "nt" else
            "Contents/Home/bin/java" if sys.platform == "darwin" else "bin/java"
        )
        installed_java = transaction / relative_java
        if not installed_java.is_file():
            raise ValueError("Temurin JDK archive did not contain java")
        javac = installed_java.with_name("javac.exe" if os.name == "nt" else "javac")
        if not javac.is_file():
            raise ValueError("Temurin JDK archive did not contain javac")
        expected_architecture = (
            "x64" if host_platform == "windows-arm64" else host_platform.split("-", 1)[1]
        )
        receipt = {
            "schemaVersion": 1,
            "runtime": "temurin",
            "version": version,
            "hostPlatform": host_platform,
            "artifactArchitecture": expected_architecture,
            "emulated": host_platform == "windows-arm64",
            "java": relative_java,
            "javaSha256": hashlib.sha256(installed_java.read_bytes()).hexdigest(),
        }
        (transaction / TEMURIN_RECEIPT).write_text(
            json.dumps(receipt, sort_keys=True) + "\n", encoding="utf-8"
        )
        transaction.rename(root)
    except BaseException:
        archive.unlink(missing_ok=True)
        if transaction.exists() and not is_link_or_reparse(transaction):
            shutil.rmtree(transaction)
        raise
    finally:
        archive.unlink(missing_ok=True)
    verified = verified_managed_temurin(java, host_platform, version=version)
    if verified is None or not java_compiler_present(verified):
        raise ValueError("managed Temurin JDK provision did not produce a usable javac")
    return verified


def ensure_managed_maven(
    specification: dict[str, object] | None = None,
    *,
    opener=None,
    reporter=None,
    confirmer=None,
    which=shutil.which,
) -> Path | None:
    """Provision checksum-verified Apache Maven into the CE tools cache when needed."""
    module = _load_dependencies_controller()
    maven = _runtime_contract(specification, "maven")
    minimum = (
        str(maven["minimumVersion"])
        if isinstance(maven, dict) and isinstance(maven.get("minimumVersion"), str)
        else "3.9.0"
    )
    ambient = which("mvn")
    if ambient:
        observed = maven_version(Path(ambient))
        if (
            observed is not None
            and module is not None
            and module.version_at_least(observed, minimum)
        ):
            return Path(ambient).resolve()
    if specification is None or maven is None or module is None:
        return None
    version = str(maven.get("version") or "")
    if not version:
        return None
    system, architecture, host_platform = _tools_host_platform()
    root = managed_maven_root(version)
    mvn = root / ("bin/mvn.cmd" if os.name == "nt" else "bin/mvn")
    verified = verified_managed_maven(mvn, host_platform, version=version)
    if verified is not None:
        return verified
    artifacts = maven.get("artifacts")
    artifact = artifacts.get(host_platform) if isinstance(artifacts, dict) else None
    if not isinstance(artifact, dict):
        return None
    url, digest = artifact.get("url"), artifact.get("sha256")
    if not isinstance(url, str) or not isinstance(digest, str):
        return None
    open_url = opener or urllib.request.urlopen
    parent = root.parent
    parent.mkdir(parents=True, exist_ok=True)
    if confirmer is not None:
        confirmer(f"Download Apache Maven {version} from {url}")
    if reporter is not None:
        reporter.trace(f"provision managed Apache Maven {version}")
    suffix = ".zip" if str(url).endswith(".zip") else ".tar.gz"
    transaction = parent / f".maven-{version}-{architecture}.{secrets.token_hex(8)}.building"
    archive = transaction.with_suffix(suffix)
    try:
        if root.exists() or is_link_or_reparse(root):
            if verified_managed_maven(mvn, host_platform, version=version) is None:
                raise ValueError("existing managed Maven tree is invalid")
            return mvn.resolve()
        module._download_artifact(str(url), archive, str(digest), open_url, reporter=reporter)
        module._extract_runtime_archive(archive, transaction)
        relative_mvn = "bin/mvn.cmd" if os.name == "nt" else "bin/mvn"
        installed = transaction / relative_mvn
        if not installed.is_file():
            raise ValueError("Maven archive did not contain mvn")
        if os.name != "nt":
            installed.chmod(installed.stat().st_mode | stat.S_IXUSR)
        expected_architecture = (
            "x64" if host_platform == "windows-arm64" else host_platform.split("-", 1)[1]
        )
        receipt = {
            "schemaVersion": 1,
            "runtime": "maven",
            "version": version,
            "hostPlatform": host_platform,
            "artifactArchitecture": expected_architecture,
            "emulated": host_platform == "windows-arm64",
            "mvn": relative_mvn,
            "mvnSha256": hashlib.sha256(installed.read_bytes()).hexdigest(),
        }
        (transaction / TEMURIN_RECEIPT).write_text(
            json.dumps(receipt, sort_keys=True) + "\n", encoding="utf-8"
        )
        transaction.rename(root)
    except BaseException:
        archive.unlink(missing_ok=True)
        if transaction.exists() and not is_link_or_reparse(transaction):
            shutil.rmtree(transaction)
        raise
    finally:
        archive.unlink(missing_ok=True)
    verified = verified_managed_maven(mvn, host_platform, version=version)
    if verified is None:
        raise ValueError("managed Maven provision did not produce a usable mvn")
    return verified


def verified_maven_tools_jar(candidate: Path) -> Path | None:
    if not candidate.is_file() or is_link_or_reparse(candidate):
        return None
    jar = candidate.resolve()
    receipt_path = jar.parent / MAVEN_TOOLS_MCP_RECEIPT
    if not receipt_path.is_file() or is_link_or_reparse(receipt_path):
        return None
    try:
        if os.stat(jar, follow_symlinks=False).st_nlink != 1 or os.stat(receipt_path, follow_symlinks=False).st_nlink != 1:
            return None
    except OSError:
        return None
    try:
        receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return None
    try:
        digest = hashlib.sha256(jar.read_bytes()).hexdigest()
    except OSError:
        return None
    version = receipt.get("version") if isinstance(receipt, dict) else None
    commit = receipt.get("commit") if isinstance(receipt, dict) else None
    expected = {
        "version": version,
        "commit": commit,
        "jar": f"maven-tools-mcp-{version}.jar",
        "sha256": digest,
    }
    return jar if (
        isinstance(version, str)
        and re.fullmatch(r"\d+(?:\.\d+){1,3}", version)
        and isinstance(commit, str)
        and re.fullmatch(r"[0-9a-f]{40}", commit)
        and receipt == expected
        and jar.name == expected["jar"]
    ) else None


def maven_tools_data_root() -> Path:
    configured = os.environ.get("LOCALAPPDATA" if os.name == "nt" else "XDG_DATA_HOME", "")
    return Path(configured or Path.home() / ".local/share").absolute()


def maven_tools_cache_root() -> Path:
    return maven_tools_data_root() / "ChaosEngine/tools/maven-tools-mcp"


def _maven_tools_version_directory(root: Path, version: str) -> Path:
    if re.fullmatch(r"\d+(?:\.\d+){1,3}", version) is None:
        raise ValueError(f"unsupported Maven Tools MCP cache version: {version}")
    return root.absolute() / version


def _read_maven_tools_cache_lock(stream) -> bytes:
    """Read the cache lock, waiting out a creator that has not finished writing the magic.

    Empty or partial magic is re-read for up to ``MAVEN_TOOLS_CACHE_LOCK_INIT_GRACE_SECONDS``
    (#6333, same rule as the #6328 project lock). Foreign bytes return at once so the
    caller still reports a collision without delay; an abandoned partial lock is
    returned after the grace and also reported.
    """
    started = time.monotonic()
    while True:
        stream.seek(0)
        contents = stream.read()
        initializing = contents != MAVEN_TOOLS_CACHE_LOCK_MAGIC and MAVEN_TOOLS_CACHE_LOCK_MAGIC.startswith(
            contents
        )
        if not initializing or time.monotonic() - started >= MAVEN_TOOLS_CACHE_LOCK_INIT_GRACE_SECONDS:
            return contents
        time.sleep(MAVEN_TOOLS_CACHE_LOCK_INIT_POLL_SECONDS)


@contextmanager
def maven_tools_cache_lock(root: Path | None = None, *, anchor: Path | None = None):
    root = (root or maven_tools_cache_root()).absolute()
    anchor = anchor or _cache_anchor(root)
    _validate_cache_path(root, anchor)
    root.mkdir(parents=True, exist_ok=True)
    _validate_cache_path(root, anchor)
    lock_path = root / MAVEN_TOOLS_CACHE_LOCK
    flags = os.O_RDWR | getattr(os, "O_BINARY", 0)
    created = False
    try:
        descriptor = os.open(lock_path, flags | os.O_CREAT | os.O_EXCL, 0o600)
        created = True
    except FileExistsError:
        if is_link_or_reparse(lock_path):
            raise ValueError(f"Maven Tools MCP cache lock is linked: {lock_path}")
        descriptor = os.open(lock_path, flags)
    try:
        stream = os.fdopen(descriptor, "r+b", closefd=True)
    except BaseException:
        os.close(descriptor)
        raise
    try:
        opened = os.fstat(stream.fileno())
        named = os.stat(lock_path, follow_symlinks=False)
        if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino) or named.st_nlink != 1:
            raise ValueError(f"Maven Tools MCP cache lock collision: {lock_path}")
        if created:
            stream.write(MAVEN_TOOLS_CACHE_LOCK_MAGIC)
            stream.flush()
            os.fsync(stream.fileno())
        elif _read_maven_tools_cache_lock(stream) != MAVEN_TOOLS_CACHE_LOCK_MAGIC:
            raise ValueError(f"Maven Tools MCP cache lock collision: {lock_path}")
        stream.seek(0)
        if os.name == "nt":
            import msvcrt  # pylint: disable=import-outside-toplevel

            msvcrt.locking(stream.fileno(), msvcrt.LK_NBLCK, 1)
        else:
            import fcntl  # pylint: disable=import-outside-toplevel,import-error

            fcntl.flock(stream.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError as error:
        stream.close()
        raise RuntimeError("another Maven Tools MCP cache operation is already running") from error
    except BaseException:
        stream.close()
        raise
    try:
        yield
    finally:
        try:
            stream.seek(0)
            if os.name == "nt":
                msvcrt.locking(stream.fileno(), msvcrt.LK_UNLCK, 1)
            else:
                fcntl.flock(stream.fileno(), fcntl.LOCK_UN)
        finally:
            stream.close()


def _maven_tools_cache_status_unlocked(
    root: Path, version: str, *, anchor: Path
) -> dict[str, str]:
    version_root = _maven_tools_version_directory(root, version)
    result = {"component": "maven-tools-mcp", "version": version, "path": str(version_root)}
    _validate_cache_path(version_root, anchor)
    tombstone = root / f".purging-{version}"
    purge_claims = tuple(root.glob(f".purged-{version}-*")) if root.is_dir() else ()
    if tombstone.exists() or is_link_or_reparse(tombstone) or purge_claims:
        return {**result, "status": "invalid", "reason": "cache purge recovery is required"}
    if not version_root.exists() and not is_link_or_reparse(version_root):
        return {**result, "status": "absent"}
    if is_link_or_reparse(root) or is_link_or_reparse(version_root) or not version_root.is_dir():
        return {**result, "status": "invalid", "reason": "cache path is linked or invalid"}
    expected_names = {
        f"maven-tools-mcp-{version}.jar",
        MAVEN_TOOLS_MCP_RECEIPT,
    }
    try:
        names = {path.name for path in version_root.iterdir()}
    except OSError:
        return {**result, "status": "invalid", "reason": "cache directory is inaccessible"}
    if names != expected_names:
        return {**result, "status": "invalid", "reason": "cache contains unknown or missing files"}
    jar = version_root / f"maven-tools-mcp-{version}.jar"
    if verified_maven_tools_jar(jar) is None:
        return {**result, "status": "invalid", "reason": "JAR receipt validation failed"}
    receipt = json.loads((version_root / MAVEN_TOOLS_MCP_RECEIPT).read_text(encoding="utf-8"))
    return {**result, "status": "healthy", "commit": str(receipt["commit"])}


def maven_tools_cache_status(
    version: str = MAVEN_TOOLS_MCP_VERSION, *, root: Path | None = None
) -> dict[str, str]:
    cache_root = (root or maven_tools_cache_root()).absolute()
    anchor = _cache_anchor(cache_root)
    version_root = _maven_tools_version_directory(cache_root, version)
    try:
        _validate_cache_path(version_root, anchor)
        if not cache_root.exists() and not is_link_or_reparse(cache_root):
            return {"component": "maven-tools-mcp", "version": version, "path": str(version_root), "status": "absent"}
        with maven_tools_cache_lock(cache_root, anchor=anchor):
            return _maven_tools_cache_status_unlocked(cache_root, version, anchor=anchor)
    except RuntimeError:
        return {"component": "maven-tools-mcp", "version": version, "path": str(version_root), "status": "busy"}
    except (OSError, ValueError):
        return {"component": "maven-tools-mcp", "version": version, "path": str(version_root), "status": "invalid", "reason": "cache lock is linked or invalid"}


def purge_maven_tools_cache(
    version: str, *, root: Path | None = None
) -> dict[str, str]:
    cache_root = (root or maven_tools_cache_root()).absolute()
    anchor = _cache_anchor(cache_root)
    version_root = _maven_tools_version_directory(cache_root, version)
    if not cache_root.exists() and not is_link_or_reparse(cache_root):
        return {"component": "maven-tools-mcp", "version": version, "path": str(version_root), "status": "absent"}
    with maven_tools_cache_lock(cache_root, anchor=anchor):
        observed = _maven_tools_cache_status_unlocked(cache_root, version, anchor=anchor)
        if observed["status"] == "absent":
            return observed
        if observed["status"] != "healthy":
            raise ValueError(f"Maven Tools MCP cache purge refused: {observed.get('reason', 'invalid cache')}")
        jar = version_root / f"maven-tools-mcp-{version}.jar"
        receipt = version_root / MAVEN_TOOLS_MCP_RECEIPT
        identities = {
            jar.name: os.stat(jar, follow_symlinks=False),
            receipt.name: os.stat(receipt, follow_symlinks=False),
        }
        directory_identity = os.stat(version_root, follow_symlinks=False)
        tombstone = cache_root / f".purging-{version}"
        if tombstone.exists() or is_link_or_reparse(tombstone):
            raise ValueError("Maven Tools MCP cache purge recovery is required")
        try:
            _rename_no_replace(version_root, tombstone)
        except FileExistsError as error:
            raise ValueError("Maven Tools MCP cache purge recovery is required") from error
        removed_any = False
        try:
            tombstone_jar = tombstone / jar.name
            tombstone_receipt = tombstone / receipt.name
            if (
                verified_maven_tools_jar(tombstone_jar) is None
                or {path.name for path in tombstone.iterdir()} != {jar.name, receipt.name}
            ):
                raise ValueError("Maven Tools MCP cache changed before purge")
            _unlink_stable_cache_file(tombstone_jar, identities[jar.name])
            removed_any = True
            _unlink_stable_cache_file(tombstone_receipt, identities[receipt.name])
            claim = cache_root / f".purged-{version}-{secrets.token_hex(16)}"
            _rename_no_replace(tombstone, claim)
            _rmdir_stable_cache_directory(claim, directory_identity)
        except BaseException:
            if not removed_any and tombstone.exists() and not version_root.exists():
                _rename_no_replace(tombstone, version_root)
            raise
        return {**observed, "status": "purged"}


def discard_invalid_maven_tools_cache(
    version: str, *, root: Path | None = None
) -> dict[str, str]:
    """Discard an invalid Maven Tools version tree so install can rebuild it.

    Healthy trees stay purge-only via purge_maven_tools_cache. Never follows
    links out of the cache root: linked leaves are unlinked in place.
    """
    cache_root = (root or maven_tools_cache_root()).absolute()
    anchor = _cache_anchor(cache_root)
    version_root = _maven_tools_version_directory(cache_root, version)
    result = {
        "component": "maven-tools-mcp",
        "version": version,
        "path": str(version_root),
    }
    if not cache_root.exists() and not is_link_or_reparse(cache_root):
        return {**result, "status": "absent"}
    with maven_tools_cache_lock(cache_root, anchor=anchor):
        _validate_cache_path(cache_root, anchor)
        try:
            observed = _maven_tools_cache_status_unlocked(
                cache_root, version, anchor=anchor
            )
        except ValueError:
            # Linked version leaves fail path validation; still discardable in place.
            if is_link_or_reparse(version_root) and _path_is_under(
                version_root, cache_root
            ):
                observed = {
                    **result,
                    "status": "invalid",
                    "reason": "cache path is linked or invalid",
                }
            else:
                raise
        if observed["status"] == "absent":
            return observed
        if observed["status"] == "healthy":
            raise ValueError(
                "Maven Tools MCP cache discard refused: healthy cache requires purge"
            )
        if observed["status"] != "invalid":
            raise ValueError(
                f"Maven Tools MCP cache discard refused: {observed.get('status', 'unknown')}"
            )
        tombstone = cache_root / f".purging-{version}"
        for marker in (tombstone, *sorted(cache_root.glob(f".purged-{version}-*"))):
            if is_link_or_reparse(marker):
                if not _path_is_under(marker, cache_root):
                    raise ValueError("Maven Tools MCP discard path escapes cache root")
                marker.unlink()
            elif marker.exists():
                _discard_tree_nofollow(marker, cache_root)
        if is_link_or_reparse(version_root):
            if not _path_is_under(version_root, cache_root):
                raise ValueError("Maven Tools MCP discard path escapes cache root")
            version_root.unlink()
        elif version_root.exists():
            claim = cache_root / f".discarding-{version}-{secrets.token_hex(16)}"
            _rename_no_replace(version_root, claim)
            _discard_tree_nofollow(claim, cache_root)
        return {**observed, "status": "discarded"}


def _reuse_healthy_maven_tools_cache(
    cache_root: Path, version: str, *, anchor: Path, target: Path
) -> Path | None:
    """Return target when an existing version tree is healthy; else None."""
    try:
        observed = _maven_tools_cache_status_unlocked(
            cache_root, version, anchor=anchor
        )
    except ValueError:
        return None
    if observed.get("status") == "healthy":
        return target
    return None


def publish_maven_tools_cache(staging: Path, *, root: Path | None = None) -> Path:
    """Publish a staged Maven Tools MCP version into the shared user cache.

    An already-present *healthy* version is reused (idempotent install / dual
    reinstall) instead of raising CE-INSTALL-FAILED. Invalid or colliding trees
    still fail closed.
    """
    staging = staging.absolute()
    cache_root = (root or maven_tools_cache_root()).absolute()
    anchor = _cache_anchor(cache_root)
    try:
        staged_receipt = json.loads(
            (staging / MAVEN_TOOLS_MCP_RECEIPT).read_text(encoding="utf-8")
        )
        version = str(staged_receipt["version"])
    except (OSError, KeyError, json.JSONDecodeError, TypeError) as error:
        raise ValueError("Maven Tools MCP staging receipt is invalid") from error
    common_root = Path(os.path.commonpath((staging, cache_root)))
    _validate_cache_path(staging, common_root)
    _validate_cache_path(cache_root, common_root)
    if is_link_or_reparse(staging) or not staging.is_dir():
        raise ValueError("Maven Tools MCP staging directory is invalid")
    jar = staging / f"maven-tools-mcp-{version}.jar"
    expected_names = {jar.name, MAVEN_TOOLS_MCP_RECEIPT}
    try:
        names = {path.name for path in staging.iterdir()}
    except OSError as error:
        raise ValueError("Maven Tools MCP staging pair is inaccessible") from error
    if names != expected_names or verified_maven_tools_jar(jar) is None:
        raise ValueError("Maven Tools MCP staging pair is invalid")
    cache_root.mkdir(parents=True, exist_ok=True)
    with maven_tools_cache_lock(cache_root, anchor=anchor):
        target = _maven_tools_version_directory(cache_root, version)
        if target.exists() or is_link_or_reparse(target):
            reused = _reuse_healthy_maven_tools_cache(
                cache_root, version, anchor=anchor, target=target
            )
            if reused is not None:
                return reused
            raise ValueError(f"Maven Tools MCP cache version already exists: {target}")
        if os.stat(staging).st_dev != os.stat(cache_root).st_dev:
            raise ValueError("Maven Tools MCP staging directory must use the cache filesystem")
        try:
            _rename_no_replace(staging, target)
        except FileExistsError as error:
            reused = _reuse_healthy_maven_tools_cache(
                cache_root, version, anchor=anchor, target=target
            )
            if reused is not None:
                return reused
            raise ValueError(f"Maven Tools MCP cache version already exists: {target}") from error
        except OSError as error:
            if error.errno == errno.EEXIST:
                reused = _reuse_healthy_maven_tools_cache(
                    cache_root, version, anchor=anchor, target=target
                )
                if reused is not None:
                    return reused
                raise ValueError(f"Maven Tools MCP cache version already exists: {target}") from error
            raise
        return target


def maven_tools_cached_versions(*, root: Path | None = None) -> list[str]:
    """Return numeric cached Maven Tools versions, newest first.

    Runtime discovery and doctor share this order so doctor judges the version
    the installer actually reuses instead of a pinned release (#6336).
    """
    cache = root or maven_tools_cache_root()
    return sorted(
        (
            path.name for path in cache.iterdir()
            if path.is_dir() and re.fullmatch(r"\d+(?:\.\d+){1,3}", path.name)
        ),
        key=lambda value: tuple(int(part) for part in value.split(".")),
        reverse=True,
    ) if cache.is_dir() else []


def _configured_maven_tools_jar() -> Path | None:
    configured_jar = os.environ.get("CHAOSENGINE_MAVEN_TOOLS_MCP_JAR")
    return Path(configured_jar).expanduser() if configured_jar else None


def selected_maven_tools_cache_status(*, root: Path | None = None) -> dict[str, str]:
    """Report the Maven Tools JAR that runtime discovery would select (#6336).

    Order matches ``discover_maven_tools_runtime``: a verified configured JAR,
    then the newest cached version whose receipt verifies. With no healthy
    candidate, report busy, then the newest invalid tree (with its reason),
    then absent.
    """
    cache_root = (root or maven_tools_cache_root()).absolute()
    configured = _configured_maven_tools_jar()
    verified = verified_maven_tools_jar(configured) if configured is not None else None
    if verified is not None:
        receipt = json.loads((verified.parent / MAVEN_TOOLS_MCP_RECEIPT).read_text(encoding="utf-8"))
        return {
            "component": "maven-tools-mcp",
            "version": str(receipt["version"]),
            "path": str(verified.parent),
            "status": "healthy",
            "commit": str(receipt["commit"]),
            "source": "CHAOSENGINE_MAVEN_TOOLS_MCP_JAR",
        }
    observed = [
        maven_tools_cache_status(version, root=cache_root)
        for version in maven_tools_cached_versions(root=cache_root)
    ]
    for wanted in ("healthy", "busy", "invalid"):
        match = next((item for item in observed if item.get("status") == wanted), None)
        if match is not None:
            return match
    return {"component": "maven-tools-mcp", "path": str(cache_root), "status": "absent"}


def discover_maven_tools_runtime() -> tuple[Path, Path] | None:
    cache = maven_tools_cache_root()
    versions = maven_tools_cached_versions(root=cache)
    jar_candidates = [_configured_maven_tools_jar(), *(
        cache / version / f"maven-tools-mcp-{version}.jar" for version in versions
    )]
    jar = next(
        (
            verified
            for candidate in jar_candidates
            if candidate is not None
            for verified in (verified_maven_tools_jar(candidate),)
            if verified is not None
        ),
        None,
    )
    if jar is None:
        return None

    configured_java = os.environ.get("CHAOSENGINE_JAVA")
    java_home = os.environ.get("JAVA_HOME")
    path_java = shutil.which("java")
    _system, _architecture, host_platform = _tools_host_platform()
    managed_java = managed_temurin_root() / (
        "bin/java.exe" if os.name == "nt" else
        "Contents/Home/bin/java" if sys.platform == "darwin" else "bin/java"
    )
    java_candidates = [
        Path(configured_java).expanduser() if configured_java else None,
        Path(java_home) / "bin" / ("java.exe" if os.name == "nt" else "java")
        if java_home
        else None,
        Path(path_java) if path_java else None,
        verified_managed_temurin(managed_java, host_platform),
    ]
    for candidate in java_candidates:
        if candidate is None or not candidate.is_file():
            continue
        try:
            resolved = candidate.resolve(strict=True)
        except OSError:
            continue
        if is_link_or_reparse(resolved):
            continue
        if (java_major(resolved) or 0) >= 17:
            return resolved, jar
    return None


def verified_managed_temurin(
    candidate: Path, host_platform: str, *, version: str = "25.0.4+7"
) -> Path | None:
    if not candidate.is_file() or is_link_or_reparse(candidate):
        return None
    receipt_path = candidate.parents[3 if sys.platform == "darwin" else 1] / TEMURIN_RECEIPT
    if not receipt_path.is_file() or is_link_or_reparse(receipt_path):
        return None
    try:
        receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
        digest = hashlib.sha256(candidate.read_bytes()).hexdigest()
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return None
    expected_architecture = "x64" if host_platform == "windows-arm64" else host_platform.split("-", 1)[1]
    expected = {
        "schemaVersion": 1,
        "runtime": "temurin",
        "version": version,
        "hostPlatform": host_platform,
        "artifactArchitecture": expected_architecture,
        "emulated": host_platform == "windows-arm64",
        "java": candidate.relative_to(receipt_path.parent).as_posix(),
        "javaSha256": digest,
    }
    major = java_major(candidate)
    if receipt != expected or major is None:
        return None
    # Floor only: installed major must meet the Java 25+ contract, not an exact build pin.
    return candidate.resolve() if major >= 25 else None


def probe_maven_tools_runtime(
    java: Path,
    jar: Path,
    *,
    popen=subprocess.Popen,
    timeout: float = 30.0,
) -> bool:
    """Require a real MCP initialize and non-empty tools/list exchange."""
    process = None
    try:
        process = popen(  # nosec B603 - both executables are receipt-verified owned paths.
            [str(java), "-jar", str(jar)],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            encoding="utf-8",
        )
        if process.stdin is None or process.stdout is None:
            return False

        def exchange(requests: list[dict[str, object]]) -> dict[str, object]:
            for request in requests:
                process.stdin.write(json.dumps(request, separators=(",", ":")) + "\n")
            process.stdin.flush()
            received: queue.Queue[str] = queue.Queue(maxsize=1)
            threading.Thread(
                target=lambda: received.put(process.stdout.readline()), daemon=True
            ).start()
            response = json.loads(received.get(timeout=timeout))
            return response if isinstance(response, dict) else {}

        initialized = exchange([{
            "jsonrpc": "2.0", "id": 1, "method": "initialize",
            "params": {"protocolVersion": "2025-11-25", "capabilities": {},
                       "clientInfo": {"name": "chaosengine-installer", "version": "1"}},
        }])
        if initialized.get("id") != 1 or not isinstance(initialized.get("result"), dict):
            return False
        listed = exchange([
            {"jsonrpc": "2.0", "method": "notifications/initialized", "params": {}},
            {"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}},
        ])
        result = listed.get("result")
        return listed.get("id") == 2 and isinstance(result, dict) and bool(result.get("tools"))
    except (OSError, ValueError, json.JSONDecodeError, queue.Empty):
        return False
    finally:
        if process is not None and process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=5)


def _add_maven_server(
    servers: dict[str, dict[str, object]],
    maven_runtime: tuple[Path, Path] | None,
    maven_docker: tuple[str, str] | None,
    managed_python: Path | None,
) -> None:
    if maven_runtime is not None:
        # Portable shared-cache contract: never embed workstation-absolute
        # java/jar paths in git-tracked project overlay (#5782). Discovery of
        # maven_runtime still gates whether the entry is published.
        servers["maven-tools-mcp"] = portable_python_server(
            [".chaos-engine/tool.py", "maven-tools-mcp"],
            managed_python=managed_python,
        )
    elif maven_docker is not None:
        _docker, image = maven_docker
        # Prefer bare `docker` so committed overlay stays machine-portable.
        servers["maven-tools-mcp"] = {
            "command": "docker",
            "args": ["run", "-i", "--rm", image],
        }


def exact_legacy_native_maven_server(server: object) -> bool:
    if not isinstance(server, dict) or set(server) != {"command", "args"}:
        return False
    command = server.get("command")
    args = server.get("args")
    if not isinstance(command, str) or not isinstance(args, list):
        return False
    normalized_command = command.replace("\\", "/")
    absolute_command = normalized_command.startswith("/") or re.match(
        r"^[A-Za-z]:/", normalized_command
    )
    if not absolute_command or normalized_command.rsplit("/", 1)[-1].casefold() not in {
        "java", "java.exe",
    }:
        return False
    if len(args) != 3 or args[0] != "-jar" or args[2] != (
        "--spring.profiles.active=docker,no-context7"
    ):
        return False
    jar = args[1]
    if not isinstance(jar, str):
        return False
    normalized_jar = jar.replace("\\", "/")
    return (
        normalized_jar.startswith("/") or re.match(r"^[A-Za-z]:/", normalized_jar)
    ) and normalized_jar.endswith("/" + MAVEN_TOOLS_JAR_SUFFIX)


_LEGACY_NATIVE_MAVEN_CODEX_BLOCK = re.compile(
    r'\r?\n\[mcp_servers\."maven-tools-mcp"\]\r?\n'
    r'command = (?P<command>"(?:[^"\\]|\\.)*")\r?\n'
    r'args = \["-jar", (?P<jar>"(?:[^"\\]|\\.)*"), '
    r'"--spring\.profiles\.active=docker,no-context7"\]\r?\n'
)


def remove_exact_legacy_native_maven_codex_block(existing: str) -> str:
    match = _LEGACY_NATIVE_MAVEN_CODEX_BLOCK.search(existing)
    if match is None:
        return existing
    try:
        command = json.loads(match.group("command"))
        jar = json.loads(match.group("jar"))
    except json.JSONDecodeError:
        return existing
    server = {
        "command": command,
        "args": [
            "-jar", jar, "--spring.profiles.active=docker,no-context7",
        ],
    }
    if not exact_legacy_native_maven_server(server):
        return existing
    return existing[:match.start()] + existing[match.end():]
