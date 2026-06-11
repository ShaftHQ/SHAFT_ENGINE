#!/usr/bin/env python3
"""Smoke the packaged SHAFT MCP server over stdio and Streamable HTTP."""

from __future__ import annotations

import argparse
import json
import queue
import socket
import subprocess
import threading
import time
import urllib.error
import urllib.request
import xml.etree.ElementTree as ET
from pathlib import Path
from typing import Any, TextIO

ROOT = Path(__file__).resolve().parents[2]
NAMESPACE = {"m": "http://maven.apache.org/POM/4.0.0"}
PROTOCOL_VERSION = "2025-03-26"
EXPECTED_TOOLS = {"driver_initialize", "browser_navigate", "browser_get_current_url", "element_click"}


def reactor_version() -> str:
    root = ET.parse(ROOT / "pom.xml").getroot()
    version = root.findtext("m:version", namespaces=NAMESPACE)
    if not version:
        raise RuntimeError("Root Maven version is missing")
    return version.strip()


def initialize_request(request_id: int = 1) -> dict[str, Any]:
    return {
        "jsonrpc": "2.0",
        "id": request_id,
        "method": "initialize",
        "params": {
            "protocolVersion": PROTOCOL_VERSION,
            "capabilities": {},
            "clientInfo": {"name": "shaft-mcp-smoke", "version": "1.0"},
        },
    }


def start_reader(stream: TextIO, target: queue.Queue[str] | list[str]) -> threading.Thread:
    def read() -> None:
        for line in iter(stream.readline, ""):
            value = line.rstrip("\r\n")
            if isinstance(target, queue.Queue):
                target.put(value)
            else:
                target.append(value)

    thread = threading.Thread(target=read, daemon=True)
    thread.start()
    return thread


def stop_process(process: subprocess.Popen[str]) -> None:
    if process.poll() is not None:
        return
    process.terminate()
    try:
        process.wait(timeout=10)
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait(timeout=10)


def assert_server(response: dict[str, Any], version: str) -> None:
    server_info = response["result"]["serverInfo"]
    if server_info != {"name": "shaft-mcp", "version": version}:
        raise AssertionError(f"Unexpected MCP server identity: {server_info}")


def assert_tools(response: dict[str, Any]) -> None:
    tools = response["result"]["tools"]
    names = {tool["name"] for tool in tools}
    missing = EXPECTED_TOOLS - names
    if missing:
        raise AssertionError(f"MCP tools are missing: {sorted(missing)}")


def assert_expected_tool_error(response: dict[str, Any]) -> None:
    result = response.get("result", {})
    content = json.dumps(result).lower()
    if not result.get("isError") or "no active browser session" not in content:
        raise AssertionError(f"Tool call did not reach the expected service error: {response}")


def validate_stdio(jar: Path, version: str) -> None:
    process = subprocess.Popen(
        ["java", "-jar", str(jar)],
        cwd=ROOT / "shaft-mcp",
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1,
    )
    assert process.stdin and process.stdout and process.stderr
    stdout: queue.Queue[str] = queue.Queue()
    stderr: list[str] = []
    start_reader(process.stdout, stdout)
    start_reader(process.stderr, stderr)

    def send(message: dict[str, Any]) -> None:
        process.stdin.write(json.dumps(message, separators=(",", ":")) + "\n")
        process.stdin.flush()

    def response(request_id: int, timeout: int = 45) -> dict[str, Any]:
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            try:
                line = stdout.get(timeout=0.25)
            except queue.Empty:
                if process.poll() is not None:
                    raise RuntimeError(f"stdio server exited early ({process.returncode}): {stderr[-20:]}")
                continue
            if not line:
                raise AssertionError("stdio stdout contained a blank non-protocol line")
            try:
                message = json.loads(line)
            except json.JSONDecodeError as error:
                raise AssertionError(f"stdio stdout contained non-JSON output: {line!r}") from error
            if message.get("id") == request_id:
                return message
        raise TimeoutError(f"Timed out waiting for stdio response {request_id}: {stderr[-20:]}")

    try:
        send(initialize_request())
        assert_server(response(1), version)
        send({"jsonrpc": "2.0", "method": "notifications/initialized", "params": {}})
        send({"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}})
        assert_tools(response(2))
        send({
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/call",
            "params": {"name": "browser_get_current_url", "arguments": {}},
        })
        assert_expected_tool_error(response(3))
        if not stdout.empty():
            raise AssertionError(f"Unexpected extra stdio output: {list(stdout.queue)}")
    finally:
        stop_process(process)


def free_port() -> int:
    with socket.socket() as server:
        server.bind(("127.0.0.1", 0))
        return int(server.getsockname()[1])


def parse_http_response(body: str) -> dict[str, Any] | None:
    if not body:
        return None
    for line in body.splitlines():
        if line.startswith("data:"):
            return json.loads(line.removeprefix("data:").strip())
    return json.loads(body)


def validate_http(jar: Path, version: str) -> None:
    port = free_port()
    process = subprocess.Popen(
        [
            "java",
            "-jar",
            str(jar),
            "--spring.profiles.active=http",
            f"--server.port={port}",
        ],
        cwd=ROOT / "shaft-mcp",
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    assert process.stdout and process.stderr
    stdout: list[str] = []
    stderr: list[str] = []
    start_reader(process.stdout, stdout)
    start_reader(process.stderr, stderr)
    endpoint = f"http://127.0.0.1:{port}/mcp"

    def post(message: dict[str, Any], session_id: str | None = None) -> tuple[int, Any, str]:
        headers = {
            "Accept": "application/json, text/event-stream",
            "Content-Type": "application/json",
            "Mcp-Protocol-Version": PROTOCOL_VERSION,
        }
        if session_id:
            headers["Mcp-Session-Id"] = session_id
        request = urllib.request.Request(
            endpoint,
            data=json.dumps(message, separators=(",", ":")).encode(),
            headers=headers,
            method="POST",
        )
        with urllib.request.urlopen(request, timeout=20) as response:
            return response.status, response.headers, response.read().decode()

    try:
        deadline = time.monotonic() + 45
        while True:
            try:
                status, headers, body = post(initialize_request())
                break
            except (urllib.error.URLError, urllib.error.HTTPError):
                if process.poll() is not None:
                    raise RuntimeError(f"HTTP server exited early ({process.returncode}): {stderr[-20:]}")
                if time.monotonic() >= deadline:
                    raise TimeoutError(f"Timed out waiting for {endpoint}: {stderr[-20:]}")
                time.sleep(0.5)
        if status != 200:
            raise AssertionError(f"Initialize returned HTTP {status}")
        initialize = parse_http_response(body)
        assert initialize
        assert_server(initialize, version)
        session_id = headers.get("Mcp-Session-Id")
        if not session_id:
            raise AssertionError("Streamable HTTP initialize did not return Mcp-Session-Id")

        status, _, body = post(
            {"jsonrpc": "2.0", "method": "notifications/initialized", "params": {}},
            session_id,
        )
        if status != 202 or body:
            raise AssertionError(f"Initialized notification returned HTTP {status}: {body!r}")

        status, _, body = post(
            {"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}},
            session_id,
        )
        tools = parse_http_response(body)
        if status != 200 or not tools:
            raise AssertionError(f"tools/list returned HTTP {status}: {body!r}")
        assert_tools(tools)

        status, _, body = post(
            {
                "jsonrpc": "2.0",
                "id": 3,
                "method": "tools/call",
                "params": {"name": "browser_get_current_url", "arguments": {}},
            },
            session_id,
        )
        tool_result = parse_http_response(body)
        if status != 200 or not tool_result:
            raise AssertionError(f"tools/call returned HTTP {status}: {body!r}")
        assert_expected_tool_error(tool_result)
    finally:
        stop_process(process)


def main() -> int:
    version = reactor_version()
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--jar",
        type=Path,
        default=ROOT / "shaft-mcp" / "target" / f"SHAFT_MCP-{version}.jar",
    )
    args = parser.parse_args()
    jar = args.jar.resolve()
    if not jar.is_file():
        raise FileNotFoundError(f"Packaged MCP server is missing: {jar}")
    validate_stdio(jar, version)
    validate_http(jar, version)
    print(f"SHAFT MCP {version} passed packaged stdio and Streamable HTTP smoke tests.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
