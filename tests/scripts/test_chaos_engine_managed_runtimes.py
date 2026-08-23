import importlib.util
import json
import tempfile
import io
import zipfile
import tarfile
from unittest import TestCase, main, mock
from types import SimpleNamespace
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


def load(name: str):
    path = ROOT / "chaos-engine" / f"{name}.py"
    spec = importlib.util.spec_from_file_location(f"chaos_engine_{name}_managed", path)
    module = importlib.util.module_from_spec(spec)
    if spec.loader is None:
        raise RuntimeError(f"no module loader for {path}")
    spec.loader.exec_module(module)
    return module


DEPENDENCIES = load("dependencies")
INSTALLER = load("install")
HOSTS = load("hosts")


class ManagedRuntimeManifestTest(TestCase):
    def setUp(self):
        self.specification = json.loads(
            (ROOT / "chaos-engine/dependencies.json").read_text(encoding="utf-8")
        )

    def test_manifest_pins_all_supported_runtime_artifacts(self):
        self.assertEqual(2, self.specification["schemaVersion"])
        self.assertEqual("0.11.29", self.specification["runtimes"]["uv"]["version"])
        self.assertEqual("3.10", self.specification["runtimes"]["python"]["version"])
        self.assertEqual("24.19.0", self.specification["runtimes"]["node"]["version"])
        self.assertEqual("25.0.4+7", self.specification["runtimes"]["temurin"]["version"])
        for runtime in ("uv", "node"):
            self.assertEqual(
                set(DEPENDENCIES.SUPPORTED_PLATFORMS),
                set(self.specification["runtimes"][runtime]["artifacts"]),
            )
        for runtime in ("uv", "node", "temurin"):
            for artifact in self.specification["runtimes"][runtime]["artifacts"].values():
                self.assertRegex(artifact["sha256"], r"^[0-9a-f]{64}$")
                self.assertTrue(artifact["url"].startswith("https://"))
        emulated = self.specification["runtimes"]["temurin"]["artifacts"]["windows-arm64"]
        self.assertTrue(emulated["emulated"])
        self.assertEqual("x64", emulated["artifactArchitecture"])

    def test_platform_selector_rejects_unsupported_before_returning_artifact(self):
        with self.assertRaisesRegex(ValueError, "unsupported platform"):
            DEPENDENCIES.select_runtime_artifact(
                self.specification, "node", system="freebsd", machine="x86_64"
            )

    def test_node_dispatch_binds_owned_executable_and_digest(self):
        with tempfile.TemporaryDirectory() as temporary:
            generation = Path(temporary)
            node = generation / "node/bin/node"
            script = generation / "npm/node_modules/@aictx/memory/dist/cli/main.js"
            node.parent.mkdir(parents=True)
            script.parent.mkdir(parents=True)
            node.write_bytes(b"owned-node")
            script.write_bytes(b"owned-memory")
            dispatch = DEPENDENCIES.node_dispatch(generation, script)
            self.assertEqual("node", dispatch["kind"])
            self.assertEqual("node/bin/node", dispatch["executable"])
            self.assertEqual(DEPENDENCIES.sha256(node), dispatch["executableSha256"])
            self.assertEqual("npm/node_modules/@aictx/memory/dist/cli/main.js", dispatch["script"])

    def test_safe_extraction_rejects_parent_traversal(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            archive = root / "bad.zip"
            with zipfile.ZipFile(archive, "w") as bundle:
                bundle.writestr("runtime/../../outside", b"bad")
            with self.assertRaisesRegex(ValueError, "unsafe path"):
                DEPENDENCIES._extract_runtime_archive(archive, root / "runtime")
            self.assertFalse((root / "outside").exists())

    def test_safe_extraction_omits_in_archive_relative_symlink(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            archive = root / "node.tar.gz"
            with tarfile.open(archive, "w:gz") as bundle:
                payload = b"npm-cli"
                target = tarfile.TarInfo("node/lib/npm-cli.js")
                target.size = len(payload)
                bundle.addfile(target, io.BytesIO(payload))
                link = tarfile.TarInfo("node/bin/npm")
                link.type = tarfile.SYMTYPE
                link.linkname = "../lib/npm-cli.js"
                bundle.addfile(link)
            DEPENDENCIES._extract_runtime_archive(archive, root / "runtime")
            self.assertEqual(b"npm-cli", (root / "runtime/lib/npm-cli.js").read_bytes())
            self.assertFalse((root / "runtime/bin/npm").exists())

    def test_download_rejects_checksum_and_removes_partial(self):
        class Response(io.BytesIO):
            def __enter__(self): return self
            def __exit__(self, *_args): self.close()
        with tempfile.TemporaryDirectory() as temporary:
            destination = Path(temporary) / "runtime.zip"
            with self.assertRaisesRegex(ValueError, "checksum"):
                DEPENDENCIES._download_artifact(
                    "https://example.invalid/runtime.zip", destination, "0" * 64,
                    opener=lambda *_args, **_kwargs: Response(b"wrong"),
                )
            self.assertFalse(destination.exists())

    def test_download_limit_removes_partial(self):
        class Response(io.BytesIO):
            def __enter__(self): return self
            def __exit__(self, *_args): self.close()
        with tempfile.TemporaryDirectory() as temporary, \
             mock.patch.object(DEPENDENCIES, "MAX_RUNTIME_ARCHIVE_BYTES", 4):
            destination = Path(temporary) / "runtime.zip"
            with self.assertRaisesRegex(ValueError, "download limit"):
                DEPENDENCIES._download_artifact(
                    "https://example.invalid/runtime.zip", destination, "0" * 64,
                    opener=lambda *_args, **_kwargs: Response(b"too-large"),
                )
            self.assertFalse(destination.exists())

    def test_generation_provisions_uv_and_node_from_verified_archives(self):
        def archive(mode, entries):
            output = io.BytesIO()
            with tarfile.open(fileobj=output, mode=mode) as bundle:
                for name, payload in entries.items():
                    member = tarfile.TarInfo(name)
                    member.size = len(payload)
                    member.mode = 0o755
                    bundle.addfile(member, io.BytesIO(payload))
            return output.getvalue()

        uv = archive("w:gz", {"uv-x86_64-unknown-linux-gnu/uv": b"uv"})
        node = archive("w:xz", {
            "node-v24.19.0-linux-x64/bin/node": b"node",
            "node-v24.19.0-linux-x64/bin/npm": b"npm",
        })
        specification = json.loads(json.dumps(self.specification))
        key = DEPENDENCIES.platform_key()
        if key != "linux-x64":
            self.skipTest("fixture targets Linux x64")
        specification["runtimes"]["uv"]["artifacts"][key]["sha256"] = DEPENDENCIES.hashlib.sha256(uv).hexdigest()
        specification["runtimes"]["node"]["artifacts"][key]["sha256"] = DEPENDENCIES.hashlib.sha256(node).hexdigest()

        class Response(io.BytesIO):
            def __enter__(self): return self
            def __exit__(self, *_args): self.close()

        def opener(url, **_kwargs):
            return Response(node if "nodejs.org" in url else uv)

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            generation, transaction = root / "generation", root / "transaction"
            generation.mkdir(); transaction.mkdir()
            DEPENDENCIES.provision_generation_runtimes(
                generation, transaction, specification, opener=opener
            )
            self.assertEqual(b"uv", (generation / "bootstrap/bin/uv").read_bytes())
            self.assertEqual(b"node", (generation / "node/bin/node").read_bytes())
            memory = DEPENDENCIES.generation_install_plan(generation, specification)["memory"][0]
            self.assertEqual(str(generation / "node/bin/node"), memory[0])
            self.assertEqual(str(generation / "node/lib/node_modules/npm/bin/npm-cli.js"), memory[1])


class ManagedRuntimeCliTest(TestCase):
    def test_wrappers_bootstrap_uv_and_forward_maven_tools(self):
        shell = (ROOT / "chaos-engine/install.sh").read_text(encoding="utf-8")
        powershell = (ROOT / "chaos-engine/install.ps1").read_text(encoding="utf-8")
        for document in (shell, powershell):
            self.assertIn("0.11.29", document)
            self.assertIn("3.10", document)
        self.assertIn("--with-maven-tools", shell)
        self.assertIn("WithMavenTools", powershell)

    def test_maven_tools_conflicts_with_skip_tools(self):
        parser = INSTALLER.parser()
        args = parser.parse_args(
            ["install", "--project", ".", "--source", ".", "--commit", "0" * 40,
             "--skip-tools", "--with-maven-tools"]
        )
        with self.assertRaisesRegex(ValueError, "cannot be combined"):
            INSTALLER.validate_install_options(args)

    def test_install_forwards_maven_tools_request(self):
        parser = INSTALLER.parser()
        args = parser.parse_args(
            ["install", "--project", ".", "--source", ".", "--commit", "0" * 40,
             "--with-maven-tools"]
        )
        self.assertTrue(args.with_maven_tools)

    def test_owned_mcp_servers_bind_managed_python(self):
        managed = Path("/owned/python")
        servers = HOSTS.owned_servers(managed_python=managed)
        self.assertEqual(str(managed), servers["chaosengine-memory"]["command"])
        self.assertEqual(str(managed), servers["chaosengine-mempalace"]["command"])

    def test_hook_documents_bind_managed_python_and_node(self):
        python = Path("/owned/python")
        node = Path("/owned/node")
        lifecycle = json.loads(HOSTS.lifecycle_hooks_document("codex", managed_python=python))
        self.assertIn(str(python), lifecycle["hooks"]["SessionStart"][0]["hooks"][0]["command"])
        copilot = json.loads(HOSTS.copilot_hooks_document(node))
        self.assertIn(str(node), copilot["hooks"]["sessionStart"][0]["bash"])

    def test_maven_tools_reuses_verified_existing_runtime(self):
        expected = (Path("/java25"), Path("/maven-tools.jar"))
        hosts = mock.Mock()
        hosts.discover_maven_tools_runtime.return_value = expected
        hosts.probe_maven_tools_runtime.return_value = True
        with mock.patch.object(INSTALLER, "load_installed_controller", return_value=hosts):
            self.assertEqual(expected, INSTALLER.ensure_maven_tools(Path("/core"), {}))
        hosts.discover_maven_tools_runtime.assert_called_once_with()

    def test_maven_tools_discovers_owned_temurin_without_ambient_java(self):
        with tempfile.TemporaryDirectory() as temporary:
            data_root = Path(temporary)
            java = data_root / "ChaosEngine/tools/temurin/25.0.4+7/linux-x64/bin/java"
            java.parent.mkdir(parents=True)
            java.write_bytes(b"java")
            jar = data_root / "ChaosEngine/tools/maven-tools-mcp/3.2.0/maven-tools-mcp-3.2.0.jar"
            jar.parent.mkdir(parents=True)
            jar.write_bytes(b"jar")
            with mock.patch.dict("os.environ", {"XDG_DATA_HOME": str(data_root)}, clear=True), \
                 mock.patch.object(HOSTS.sys, "platform", "linux"), \
                 mock.patch.object(HOSTS, "verified_maven_tools_jar", return_value=jar), \
                 mock.patch.object(HOSTS, "verified_managed_temurin", return_value=java), \
                 mock.patch.object(HOSTS, "java_major", return_value=25), \
                 mock.patch.object(HOSTS.shutil, "which", return_value=None):
                self.assertEqual((java, jar), HOSTS.discover_maven_tools_runtime())

    def test_maven_tools_probe_requires_initialize_and_nonempty_tools_list(self):
        process = mock.Mock()
        process.stdin = io.StringIO()
        process.stdout = io.StringIO(
            '{"jsonrpc":"2.0","id":1,"result":{"serverInfo":{"name":"maven-tools-mcp"}}}\n'
            '{"jsonrpc":"2.0","id":2,"result":{"tools":[{"name":"latest-version"}]}}\n'
        )
        process.stderr = io.StringIO()
        process.poll.return_value = None
        process.wait.return_value = 0
        self.assertTrue(HOSTS.probe_maven_tools_runtime(Path("/java"), Path("/tools.jar"), popen=lambda *_args, **_kwargs: process))
        requests = [json.loads(line) for line in process.stdin.getvalue().splitlines()]
        self.assertEqual(["initialize", "notifications/initialized", "tools/list"], [item["method"] for item in requests])

    def test_maven_tools_probe_rejects_empty_tools(self):
        process = mock.Mock()
        process.stdin = io.StringIO()
        process.stdout = io.StringIO(
            '{"jsonrpc":"2.0","id":1,"result":{"serverInfo":{"name":"maven-tools-mcp"}}}\n'
            '{"jsonrpc":"2.0","id":2,"result":{"tools":[]}}\n'
        )
        process.stderr = io.StringIO()
        process.poll.return_value = None
        process.wait.return_value = 0
        self.assertFalse(HOSTS.probe_maven_tools_runtime(Path("/java"), Path("/tools.jar"), popen=lambda *_args, **_kwargs: process))

    def test_maven_tools_archive_fallback_builds_and_publishes(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            java = root / "java"
            java.write_bytes(b"java")
            cache = root / "cache"
            final = (java, cache / "3.2.0/maven-tools-mcp-3.2.0.jar")
            hosts = SimpleNamespace(
                discover_maven_tools_runtime=mock.Mock(side_effect=[None, final]),
                java_major=lambda _path: 25,
                maven_tools_cache_status=lambda: {"status": "absent"},
                maven_tools_cache_root=lambda: cache,
                MAVEN_TOOLS_MCP_VERSION="3.2.0",
                MAVEN_TOOLS_MCP_COMMIT="4475ff6c61f23ea9a93cb6d5665a63235ef2ef36",
                MAVEN_TOOLS_MCP_RECEIPT="install-receipt.json",
                publish_maven_tools_cache=mock.Mock(),
                probe_maven_tools_runtime=mock.Mock(return_value=True),
            )
            dependencies = SimpleNamespace(
                _download_artifact=lambda _url, path, _sha: path.write_bytes(b"archive"),
                _extract_runtime_archive=lambda _archive, source: (
                    source.mkdir(),
                    (source / "mvnw").write_bytes(b"wrapper"),
                ),
            )
            specification = {"runtimes": {"maven-tools-source": {
                "url": "https://example.invalid/source.zip", "sha256": "0" * 64,
            }}}

            def runner(command, **kwargs):
                if Path(command[0]).name == "mvnw":
                    output = Path(kwargs["cwd"]) / "target/maven-tools-mcp-3.2.0.jar"
                    output.parent.mkdir()
                    output.write_bytes(b"jar")
                return SimpleNamespace(returncode=0)

            with mock.patch.dict("os.environ", {"CHAOSENGINE_JAVA": str(java)}, clear=False), \
                 mock.patch.object(INSTALLER, "load_installed_controller", return_value=hosts), \
                 mock.patch.object(INSTALLER, "load_dependency_controller", return_value=dependencies), \
                 mock.patch.object(INSTALLER.shutil, "which", return_value=None):
                self.assertEqual(final, INSTALLER.ensure_maven_tools(Path("/core"), specification, runner=runner))
            hosts.publish_maven_tools_cache.assert_called_once()


if __name__ == "__main__":
    main()
