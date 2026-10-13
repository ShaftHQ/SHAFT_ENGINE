#!/usr/bin/env python3
"""
Stage the pinned Allure Cucumber 8 adapter jar, sources, and javadoc.

SHAFT redistributes the allure-java build a1df39ff under its own version so
Maven Central can serve it. The jar bytes are rejected unless the SHA-256
matches the pin passed by the module POM.
"""

from __future__ import annotations

import hashlib
import io
import os
import shutil
import subprocess  # nosec B404 - mvn and javadoc are fixed toolchain commands
import sys
import tarfile
import urllib.request
from pathlib import Path
from urllib.parse import urlparse

JAR_URL = (
    "https://raw.githubusercontent.com/MohabMohie/allure-java/maven-repo/"
    "io/github/shafthq/allure-cucumber8-jvm/a1df39ff/allure-cucumber8-jvm-a1df39ff.jar"
)
SOURCE_COMMIT = "a1df39fffb1389ecbce951d617b5da3c49bb1a3a"
SOURCE_URL = f"https://github.com/MohabMohie/allure-java/archive/{SOURCE_COMMIT}.tar.gz"
SOURCE_PREFIX = f"allure-java-{SOURCE_COMMIT}/allure-cucumber8-jvm/src/main/java/"
USER_AGENT = "shaft-engine-allure-cucumber8-publish"


def toolchain(name: str) -> str:
    if name == "mvn":
        override = os.environ.get("MVN", "")
        if override:
            return override
    java_home = os.environ.get("JAVA_HOME", "")
    if java_home:
        base = Path(java_home) / "bin" / name
        for candidate in (Path(str(base) + ".exe"), base):
            if candidate.is_file():
                return str(candidate)
    names = (f"{name}.cmd", f"{name}.bat", f"{name}.exe", name) if os.name == "nt" else (name,)
    for candidate_name in names:
        found = shutil.which(candidate_name)
        if found:
            return found
    raise RuntimeError(f"{name} is not on PATH")


def run_tool(argv: list[str]) -> None:
    command = argv
    if os.name == "nt" and argv[0].lower().endswith((".cmd", ".bat")):
        command = ["cmd.exe", "/d", "/c", *argv]
    try:
        subprocess.run(command, check=True)  # nosec B603 - the executable is a resolved toolchain and the argument list is fixed
    except FileNotFoundError as exc:
        raise RuntimeError(f"toolchain not found: {argv[0]}") from exc


def download(url: str) -> bytes:
    parsed = urlparse(url)
    if parsed.scheme != "https" or parsed.hostname not in {"raw.githubusercontent.com", "github.com"}:
        raise RuntimeError(f"refusing download URL {url}")
    request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    with urllib.request.urlopen(request, timeout=120) as response:  # nosec B310 - https hosts checked above
        return response.read()


def extract_java_sources(archive: bytes, destination: Path) -> None:
    if destination.exists():
        shutil.rmtree(destination)
    destination.mkdir(parents=True)
    with tarfile.open(fileobj=io.BytesIO(archive), mode="r:gz") as tar:
        for member in tar.getmembers():
            if not member.isfile() or not member.name.startswith(SOURCE_PREFIX):
                continue
            if not member.name.endswith(".java"):
                continue
            relative = member.name[len(SOURCE_PREFIX):]
            if relative == "" or relative.startswith(("/", "\\")) or ".." in Path(relative).parts:
                raise RuntimeError(f"refusing source path {member.name}")
            target = destination / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            extracted = tar.extractfile(member)
            if extracted is None:
                raise RuntimeError(f"missing source {member.name}")
            target.write_bytes(extracted.read())
    if not (destination / "io/qameta/allure/cucumber8jvm/AllureCucumber8Jvm.java").is_file():
        raise RuntimeError("adapter sources did not include AllureCucumber8Jvm.java")


def javadoc_classpath(work: Path) -> str:
    pom = work / "javadoc-classpath-pom.xml"
    pom.write_text(
        """<?xml version="1.0" encoding="UTF-8"?>
<project>
  <modelVersion>4.0.0</modelVersion>
  <groupId>io.github.shafthq</groupId>
  <artifactId>allure-cucumber8-javadoc-classpath</artifactId>
  <version>1</version>
  <dependencyManagement>
    <dependencies>
      <dependency>
        <groupId>io.cucumber</groupId>
        <artifactId>cucumber-bom</artifactId>
        <version>8.0.4</version>
        <type>pom</type>
        <scope>import</scope>
      </dependency>
    </dependencies>
  </dependencyManagement>
  <dependencies>
    <dependency>
      <groupId>io.qameta.allure</groupId>
      <artifactId>allure-java-commons</artifactId>
      <version>3.0.0</version>
    </dependency>
    <dependency>
      <groupId>org.slf4j</groupId>
      <artifactId>slf4j-api</artifactId>
      <version>2.0.20</version>
    </dependency>
    <dependency>
      <groupId>io.cucumber</groupId>
      <artifactId>cucumber-plugin</artifactId>
    </dependency>
    <dependency>
      <groupId>io.cucumber</groupId>
      <artifactId>gherkin</artifactId>
    </dependency>
    <dependency>
      <groupId>io.cucumber</groupId>
      <artifactId>messages</artifactId>
    </dependency>
  </dependencies>
</project>
""",
        encoding="utf-8",
    )
    output = work / "javadoc-classpath.txt"
    run_tool(
        [
            toolchain("mvn"),
            "-f",
            str(pom),
            "-B",
            "dependency:build-classpath",
            f"-Dmdep.outputFile={output}",
            "-DincludeScope=compile",
        ]
    )
    return output.read_text(encoding="utf-8").strip()


def write_javadoc(source_root: Path, work: Path) -> None:
    javadoc_src = work / "javadoc-src"
    if javadoc_src.exists():
        shutil.rmtree(javadoc_src)
    shutil.copytree(source_root / "io", javadoc_src / "io")
    javadoc_out = work / "javadoc-out"
    if javadoc_out.exists():
        shutil.rmtree(javadoc_out)
    javadoc_out.mkdir()
    run_tool(
        [
            toolchain("javadoc"),
            "-quiet",
            "-d",
            str(javadoc_out),
            "-classpath",
            javadoc_classpath(work),
            "-sourcepath",
            str(javadoc_src),
            "-subpackages",
            "io.qameta.allure.cucumber8jvm",
            "-Xdoclint:none",
            "-encoding",
            "UTF-8",
        ]
    )
    if not any(javadoc_out.rglob("*.html")):
        raise RuntimeError("javadoc produced no html")


def main() -> int:
    if len(sys.argv) != 3:
        print("usage: prepare_upstream_adapter.py TARGET_DIR EXPECTED_SHA256", file=sys.stderr)
        return 2
    target = Path(sys.argv[1]).resolve()
    expected = sys.argv[2].strip().lower()
    target.mkdir(parents=True, exist_ok=True)
    jar_bytes = download(JAR_URL)
    digest = hashlib.sha256(jar_bytes).hexdigest()
    if digest != expected:
        print(f"adapter jar sha256 {digest} != {expected}", file=sys.stderr)
        return 1
    (target / "upstream-adapter.jar").write_bytes(jar_bytes)
    source_root = target / "upstream-src"
    extract_java_sources(download(SOURCE_URL), source_root)
    write_javadoc(source_root, target)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f"prepare_upstream_adapter failed: {exc}", file=sys.stderr)
        raise SystemExit(1) from exc
