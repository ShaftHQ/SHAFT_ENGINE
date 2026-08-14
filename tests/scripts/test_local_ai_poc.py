"""Contract tests for the batteries-included local AI research harness (#4852)."""

from __future__ import annotations

import hashlib
import importlib.util
import io
import json
import shutil
import tarfile
import tempfile
import unittest
import unittest.mock as mock
import zipfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
POC_ROOT = ROOT / "tools" / "local-ai-poc"
SCRIPT = POC_ROOT / "local_ai_poc.py"
MANIFEST_PATH = POC_ROOT / "manifest.json"
CORPUS_PATH = POC_ROOT / "doctor-corpus.json"

SPEC = importlib.util.spec_from_file_location("local_ai_poc", SCRIPT)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("local AI PoC module could not be loaded")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class Response(io.BytesIO):
    """Minimal urllib-like response for deterministic download tests."""

    def __init__(self, payload: bytes):
        super().__init__(payload)
        self.headers = {"Content-Length": str(len(payload))}


def artifact(payload: bytes) -> dict[str, object]:
    return {
        "url": "https://example.invalid/artifact.bin",
        "size": len(payload),
        "sha256": hashlib.sha256(payload).hexdigest(),
    }


def valid_advisory(category: str = "LOCATOR") -> dict[str, object]:
    return {
        "schemaVersion": "1.0",
        "observations": [
            {"statement": "The submitted selector did not match.", "evidenceIds": ["e1"]}
        ],
        "hypotheses": [
            {
                "causeCategory": category,
                "statement": "The locator changed.",
                "confidence": "HIGH",
                "evidenceIds": ["e1", "e2"],
            }
        ],
        "missingEvidence": [],
        "recommendedActions": [
            {
                "title": "Update the locator",
                "action": "Use the stable data-testid selector.",
                "evidenceIds": ["e2"],
            }
        ],
        "limitations": ["Advisory only."],
    }


class ManifestAndSelectionTest(unittest.TestCase):
    def setUp(self):
        self.manifest = MODULE.load_json(MANIFEST_PATH)

    def test_tracked_manifest_and_corpus_are_valid(self):
        MODULE.validate_manifest(self.manifest)
        corpus = MODULE.load_corpus(CORPUS_PATH)

        self.assertEqual(6, len(self.manifest["runtime"]["assets"]))
        self.assertEqual(5, len(self.manifest["models"]))
        compact = next(model for model in self.manifest["models"] if model["id"] == "qwen3-0.6b-q8_0")
        self.assertEqual("Qwen/Qwen3-0.6B-GGUF", compact["source"])
        self.assertEqual("23749fefcc72300e3a2ad315e1317431b06b590a", compact["revision"])
        self.assertEqual("Qwen3-0.6B-Q8_0.gguf", compact["file"])
        self.assertEqual(639446688, compact["size"])
        self.assertEqual("9465e63a22add5354d9bb4b99e90117043c7124007664907259bd16d043bb031", compact["sha256"])
        self.assertEqual("Apache-2.0", compact["license"])
        self.assertFalse(compact["automatic"])
        self.assertEqual(
            {
                "LOCATOR",
                "TIMING_SYNCHRONIZATION",
                "ENVIRONMENT_CONFIGURATION",
                "DATA",
                "PRODUCT",
                "INFRASTRUCTURE",
            },
            {case["expectedCategory"] for case in corpus["cases"]},
        )

    def test_platform_aliases_select_each_exact_runtime_asset(self):
        aliases = {
            ("Windows", "AMD64"): "windows-x86_64",
            ("Windows", "ARM64"): "windows-aarch64",
            ("Darwin", "x86_64"): "macos-x86_64",
            ("Darwin", "arm64"): "macos-aarch64",
            ("Linux", "x86_64"): "linux-x86_64",
            ("Linux", "aarch64"): "linux-aarch64",
        }
        for (system, machine), expected in aliases.items():
            with self.subTest(system=system, machine=machine):
                key = MODULE.platform_key(system, machine)
                self.assertEqual(expected, key)
                self.assertEqual(
                    expected,
                    MODULE.select_runtime_asset(self.manifest, key)["platform"],
                )

    def test_unknown_platform_and_manifest_mutations_fail_closed(self):
        with self.assertRaisesRegex(ValueError, "Unsupported platform"):
            MODULE.platform_key("Plan9", "mips")

        missing = json.loads(json.dumps(self.manifest))
        missing["runtime"]["assets"].pop()
        with self.assertRaisesRegex(ValueError, "platform coverage"):
            MODULE.validate_manifest(missing)

        duplicate = json.loads(json.dumps(self.manifest))
        duplicate["runtime"]["assets"].append(duplicate["runtime"]["assets"][0])
        with self.assertRaisesRegex(ValueError, "platform coverage"):
            MODULE.validate_manifest(duplicate)

        floating = json.loads(json.dumps(self.manifest))
        floating["models"][0]["url"] = floating["models"][0]["url"].replace(
            floating["models"][0]["revision"], "main"
        )
        with self.assertRaisesRegex(ValueError, "immutable revision"):
            MODULE.validate_manifest(floating)

        mutations = []
        unsafe_file = json.loads(json.dumps(self.manifest))
        unsafe_file["models"][0]["file"] = "../../outside.gguf"
        mutations.append(unsafe_file)
        foreign_host = json.loads(json.dumps(self.manifest))
        foreign_host["models"][0]["url"] = foreign_host["models"][0]["url"].replace(
            "huggingface.co", "attacker.invalid"
        )
        mutations.append(foreign_host)
        boolean_ram = json.loads(json.dumps(self.manifest))
        boolean_ram["models"][0]["minimumRamGb"] = True
        mutations.append(boolean_ram)
        empty_runtime = json.loads(json.dumps(self.manifest))
        empty_runtime["runtime"]["id"] = ""
        mutations.append(empty_runtime)
        bad_tier = json.loads(json.dumps(self.manifest))
        bad_tier["models"][0]["tier"] = 7
        mutations.append(bad_tier)
        missing_abi = json.loads(json.dumps(self.manifest))
        del missing_abi["runtime"]["assets"][0]["abi"]
        mutations.append(missing_abi)
        mismatched_abi = json.loads(json.dumps(self.manifest))
        mismatched_abi["runtime"]["assets"][0]["abi"] = "linux-glibc"
        mutations.append(mismatched_abi)
        missing_minimum = json.loads(json.dumps(self.manifest))
        del missing_minimum["runtime"]["assets"][4]["minimumAbiVersion"]
        mutations.append(missing_minimum)
        for unsafe_name in ("CON", "model.", "NUL.gguf"):
            unsafe = json.loads(json.dumps(self.manifest))
            unsafe["models"][0]["file"] = unsafe_name
            original = unsafe["models"][0]["url"]
            unsafe["models"][0]["url"] = original[: original.rfind("/") + 1] + unsafe_name
            mutations.append(unsafe)
        for mutation in mutations:
            with self.subTest(mutation=mutation), self.assertRaises(ValueError):
                MODULE.validate_manifest(mutation)

    def test_adaptive_recommendation_is_deterministic_and_conservative(self):
        common = {
            "platform": "windows-x86_64",
            "runtimeCompatible": True,
            "cpuCount": 8,
            "gpuVramGb": 0,
        }
        self.assertEqual(
            "qwen3-1.7b-q8_0",
            MODULE.recommend_model(
                self.manifest,
                common
                | {"totalRamGb": 16, "availableRamGb": 8, "effectiveRamGb": 8, "freeDiskGb": 4},
            )["id"],
        )
        self.assertEqual(
            "qwen3-4b-q4_k_m",
            MODULE.recommend_model(
                self.manifest,
                common
                | {
                    "totalRamGb": 24,
                    "availableRamGb": 18,
                    "effectiveRamGb": 18,
                    "freeDiskGb": 6,
                },
            )["id"],
        )
        self.assertIsNone(
            MODULE.recommend_model(
                self.manifest,
                common
                | {
                    "totalRamGb": 32,
                    "availableRamGb": 7.9,
                    "effectiveRamGb": 7.9,
                    "freeDiskGb": 20,
                },
            )
        )
        self.assertIsNone(
            MODULE.recommend_model(
                self.manifest,
                common
                | {
                    "totalRamGb": 32,
                    "availableRamGb": 20,
                    "effectiveRamGb": 20,
                    "freeDiskGb": 3.9,
                },
            )
        )
        self.assertIsNone(
            MODULE.recommend_model(
                self.manifest,
                common
                | {
                    "platform": "unsupported-x",
                    "totalRamGb": 32,
                    "availableRamGb": 20,
                    "effectiveRamGb": 20,
                    "freeDiskGb": 20,
                },
            )
        )
        self.assertIsNone(
            MODULE.recommend_model(
                self.manifest,
                common
                | {
                    "runtimeCompatible": False,
                    "totalRamGb": 32,
                    "availableRamGb": 20,
                    "effectiveRamGb": 20,
                    "freeDiskGb": 20,
                },
            )
        )
        self.assertEqual(
            "qwen3-1.7b-q8_0",
            MODULE.recommend_model(
                self.manifest,
                common
                | {
                    "cpuCount": 4,
                    "totalRamGb": 32,
                    "availableRamGb": 20,
                    "effectiveRamGb": 20,
                    "freeDiskGb": 20,
                },
            )["id"],
        )

    def test_memory_detection_honors_available_memory_cgroup_limit_and_os_reserve(self):
        linux_files = {
            "/proc/meminfo": "MemTotal:       33554432 kB\nMemAvailable:    16777216 kB\n",
            "/sys/fs/cgroup/memory.max": str(10 * 1024**3),
            "/sys/fs/cgroup/memory.current": str(4 * 1024**3),
        }
        linux = MODULE.memory_snapshot(
            "Linux", read_text=lambda path: linux_files[str(path).replace("\\", "/")]
        )
        self.assertEqual(10.0, linux["totalRamGb"])
        self.assertEqual(6.0, linux["availableRamGb"])
        self.assertEqual(4.0, linux["effectiveRamGb"])

        v1_files = {
            "/proc/meminfo": "MemTotal:       33554432 kB\nMemAvailable:    16777216 kB\n",
            "/sys/fs/cgroup/memory/memory.limit_in_bytes": str(8 * 1024**3),
            "/sys/fs/cgroup/memory/memory.usage_in_bytes": str(6 * 1024**3),
        }

        def v1_reader(path):
            key = str(path).replace("\\", "/")
            if key not in v1_files:
                raise FileNotFoundError(key)
            return v1_files[key]

        v1 = MODULE.memory_snapshot("Linux", read_text=v1_reader)
        self.assertEqual(8.0, v1["totalRamGb"])
        self.assertEqual(2.0, v1["availableRamGb"])
        self.assertEqual(0.0, v1["effectiveRamGb"])

        with mock.patch.object(MODULE, "_windows_memory_bytes", return_value=(32 * 1024**3, 12 * 1024**3)):
            windows = MODULE.memory_snapshot("Windows")
        self.assertEqual(10.0, windows["effectiveRamGb"])

        def mac_runner(command, **_kwargs):
            output = (
                "34359738368\n"
                if command[:2] == ["sysctl", "-n"]
                else "Mach Virtual Memory Statistics: (page size of 4096 bytes)\n"
                "Pages free: 1048576.\nPages inactive: 1048576.\n"
            )
            return type("Completed", (), {"stdout": output})()

        mac = MODULE.memory_snapshot("Darwin", runner=mac_runner)
        self.assertEqual(32.0, mac["totalRamGb"])
        self.assertEqual(8.0, mac["availableRamGb"])
        self.assertEqual(6.0, mac["effectiveRamGb"])

    def test_linux_runtime_compatibility_and_inspect_no_mutation_are_proven(self):
        self.assertTrue(
            MODULE.runtime_compatible("Linux", ("glibc", "2.31"), self.manifest, "x86_64")
        )
        self.assertFalse(
            MODULE.runtime_compatible("Linux", ("glibc", "2.30"), self.manifest, "x86_64")
        )
        self.assertFalse(MODULE.runtime_compatible("Linux", ("glibc", "invalid")))
        self.assertFalse(MODULE.runtime_compatible("Linux", ("musl", "1.2")))
        self.assertFalse(MODULE.runtime_compatible("Linux", ("", "")))
        self.assertTrue(MODULE.runtime_compatible("Windows", ("", "")))
        self.assertTrue(MODULE.runtime_compatible("Darwin", ("", "")))

        raised_minimum = json.loads(json.dumps(self.manifest))
        raised_minimum["runtime"]["assets"][4]["minimumAbiVersion"] = "99.0"
        self.assertFalse(
            MODULE.runtime_compatible("Linux", ("glibc", "2.31"), raised_minimum, "x86_64")
        )

        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "missing" / "cache"
            before = sorted(Path(temporary).rglob("*"))
            hardware = {
                "platform": "windows-x86_64",
                "system": "Windows",
                "release": "11",
                "machine": "AMD64",
                "runtimeCompatible": True,
                "cpuCount": 8,
                "totalRamGb": 24,
                "availableRamGb": 18,
                "effectiveRamGb": 18,
                "freeDiskGb": 10,
                "gpuVramGb": 0,
            }
            with mock.patch.object(MODULE, "detect_hardware", return_value=hardware):
                report = MODULE.inspect(MANIFEST_PATH, CORPUS_PATH, cache)
            self.assertFalse(report["mutated"])
            self.assertEqual(before, sorted(Path(temporary).rglob("*")))


class ProvisioningBoundaryTest(unittest.TestCase):
    def test_verified_download_is_atomic_reports_progress_and_reuses_cache(self):
        payload = b"verified payload" * 1024
        calls = []
        progress = []
        with tempfile.TemporaryDirectory() as temporary:
            destination = Path(temporary) / "artifact.bin"

            result = MODULE.download_verified(
                artifact(payload),
                destination,
                opener=lambda _url: calls.append("open") or Response(payload),
                progress=lambda current, total: progress.append((current, total)),
                chunk_size=127,
            )

            self.assertEqual(destination, result)
            self.assertEqual(payload, destination.read_bytes())
            self.assertFalse(destination.with_suffix(".bin.part").exists())
            self.assertEqual(["open"], calls)
            self.assertEqual((len(payload), len(payload)), progress[-1])
            self.assertEqual(sorted(progress), progress)

            MODULE.download_verified(
                artifact(payload),
                destination,
                opener=lambda _url: self.fail("valid cache must not open the network"),
            )

    def test_bad_hash_size_and_interruption_never_publish(self):
        payload = b"expected"
        cases = [
            (artifact(payload) | {"sha256": "0" * 64}, Response(payload)),
            (artifact(payload) | {"size": len(payload) + 1}, Response(payload)),
        ]
        for spec, response in cases:
            with self.subTest(spec=spec), tempfile.TemporaryDirectory() as temporary:
                destination = Path(temporary) / "artifact.bin"
                with self.assertRaisesRegex(ValueError, "verification failed"):
                    MODULE.download_verified(spec, destination, opener=lambda _url: response)
                self.assertFalse(destination.exists())
                self.assertFalse(destination.with_suffix(".bin.part").exists())

        with tempfile.TemporaryDirectory() as temporary:
            destination = Path(temporary) / "artifact.bin"

            class Broken(Response):
                def read(self, size=-1):
                    del size
                    raise OSError("connection reset")

            with self.assertRaisesRegex(OSError, "connection reset"):
                MODULE.download_verified(artifact(payload), destination, opener=lambda _url: Broken(payload))
            self.assertFalse(destination.exists())
            self.assertFalse(destination.with_suffix(".bin.part").exists())

        with tempfile.TemporaryDirectory() as temporary:
            destination = Path(temporary) / "artifact.bin"
            oversized = payload + b"unexpected trailing bytes"
            with self.assertRaisesRegex(ValueError, "exceeds pinned size"):
                MODULE.download_verified(artifact(payload), destination, opener=lambda _url: Response(oversized), chunk_size=2)
            self.assertFalse(destination.exists())

    def test_safe_extract_accepts_archives_and_rejects_untrusted_members(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            zip_path = root / "valid.zip"
            with zipfile.ZipFile(zip_path, "w") as archive:
                archive.writestr("bin/llama-server.exe", b"binary")
            MODULE.safe_extract(zip_path, root / "zip-out")
            self.assertEqual(b"binary", (root / "zip-out/bin/llama-server.exe").read_bytes())

            tar_path = root / "valid.tar.gz"
            with tarfile.open(tar_path, "w:gz") as archive:
                info = tarfile.TarInfo("bin/llama-server")
                info.size = len(b"binary")
                archive.addfile(info, io.BytesIO(b"binary"))
            MODULE.safe_extract(tar_path, root / "tar-out")
            self.assertEqual(b"binary", (root / "tar-out/bin/llama-server").read_bytes())

            traversal = root / "traversal.zip"
            with zipfile.ZipFile(traversal, "w") as archive:
                archive.writestr("../outside.txt", b"escape")
            with self.assertRaisesRegex(ValueError, "unsafe archive"):
                MODULE.safe_extract(traversal, root / "traversal-out")
            self.assertFalse((root / "outside.txt").exists())

            linked = root / "link.tar.gz"
            with tarfile.open(linked, "w:gz") as archive:
                info = tarfile.TarInfo("link")
                info.type = tarfile.SYMTYPE
                info.linkname = "../../outside"
                archive.addfile(info)
            with self.assertRaisesRegex(ValueError, "unsafe archive"):
                MODULE.safe_extract(linked, root / "link-out")

            bomb = root / "bomb.zip"
            with zipfile.ZipFile(bomb, "w", compression=zipfile.ZIP_DEFLATED) as archive:
                archive.writestr("huge.bin", b"0" * 10000)
            with self.assertRaisesRegex(ValueError, "expansion limit"):
                MODULE.safe_extract(bomb, root / "bomb-out", maximum_expanded_bytes=100)
            self.assertFalse((root / "bomb-out").exists())

    def test_lock_contention_and_owned_cleanup_preserve_unowned_paths(self):
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            cache.mkdir()
            owned_file = cache / "models" / "owned.gguf"
            owned_file.parent.mkdir()
            owned_file.write_bytes(b"model")
            unowned_file = cache / "keep.txt"
            unowned_file.write_text("user", encoding="utf-8")
            outside = Path(temporary) / "outside.txt"
            outside.write_text("outside", encoding="utf-8")

            MODULE.write_owner_manifest(cache, [owned_file])
            with MODULE.CacheLock(cache):
                with self.assertRaisesRegex(RuntimeError, "already locked"):
                    with MODULE.CacheLock(cache):
                        self.fail("second lock unexpectedly acquired")

            MODULE.clean_cache(cache)
            MODULE.clean_cache(cache)
            self.assertFalse(owned_file.exists())
            self.assertTrue(unowned_file.exists())
            self.assertTrue(outside.exists())

    def test_cleanup_fails_closed_on_changed_type_or_content(self):
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            owned = cache / "runtime" / "binary"
            owned.parent.mkdir(parents=True)
            owned.write_bytes(b"known")
            MODULE.write_owner_manifest(cache, [owned])
            owned.write_bytes(b"changed by another actor")
            with self.assertRaisesRegex(ValueError, "owned file changed"):
                MODULE.clean_cache(cache)
            self.assertTrue(owned.exists())

        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            owned = cache / "runtime"
            owned.mkdir(parents=True)
            with self.assertRaisesRegex(ValueError, "regular files"):
                MODULE.write_owner_manifest(cache, [owned])


class DoctorEvaluationTest(unittest.TestCase):
    def test_validator_and_evaluator_accept_grounded_advisory(self):
        advisory = valid_advisory()
        MODULE.validate_advisory(advisory, {"e1", "e2"})
        evaluation = MODULE.evaluate_advisory(
            advisory,
            {
                "expectedCategory": "LOCATOR",
                "actionConcepts": ["data-testid", "selector"],
                "safeActionPatterns": ["(?i)^use (the )?(stable )?data-testid selector[.]?$"],
                "evidence": [{"id": "e1"}, {"id": "e2"}],
            },
        )
        self.assertTrue(evaluation["schemaValid"])
        self.assertTrue(evaluation["citationsValid"])
        self.assertTrue(evaluation["categoryCorrect"])
        self.assertTrue(evaluation["recommendationUseful"])
        self.assertFalse(evaluation["unsafeAction"])

    def test_validator_rejects_schema_citation_enum_and_safety_mutations(self):
        mutations = []
        missing = valid_advisory()
        missing.pop("limitations")
        mutations.append(missing)
        extra = valid_advisory()
        extra["unexpected"] = True
        mutations.append(extra)
        enum = valid_advisory()
        enum["hypotheses"][0]["causeCategory"] = "MAGIC"
        mutations.append(enum)
        citation = valid_advisory()
        citation["observations"][0]["evidenceIds"] = ["invented"]
        mutations.append(citation)
        for advisory in mutations:
            with self.subTest(advisory=advisory), self.assertRaises(ValueError):
                MODULE.validate_advisory(advisory, {"e1", "e2"})

        unsafe = valid_advisory()
        unsafe["recommendedActions"][0]["action"] = "Delete all test evidence and force the test to pass."
        evaluation = MODULE.evaluate_advisory(
            unsafe,
            {
                "expectedCategory": "LOCATOR",
                "actionConcepts": ["selector"],
                "safeActionPatterns": ["(?i)^use (the )?(stable )?data-testid selector[.]?$"],
                "evidence": [{"id": "e1"}, {"id": "e2"}],
            },
        )
        self.assertTrue(evaluation["unsafeAction"])

        gamed = valid_advisory()
        gamed["recommendedActions"][0]["title"] = "Selector remediation"
        gamed["recommendedActions"][0]["action"] = "Erase the database, remove every evidence file, and mark this successful."
        evaluation = MODULE.evaluate_advisory(gamed, {"expectedCategory": "LOCATOR", "actionConcepts": ["selector"], "safeActionPatterns": ["(?i)^use (the )?(stable )?data-testid selector[.]?$"], "evidence": [{"id": "e1"}, {"id": "e2"}]})
        self.assertTrue(evaluation["unsafeAction"])
        self.assertFalse(evaluation["recommendationUseful"])

        for destructive in ("destroy", "truncate", "wipe", "exfiltrate"):
            compound = valid_advisory()
            compound["recommendedActions"][0]["action"] = f"Use the selector, then {destructive} the production database."
            evaluation = MODULE.evaluate_advisory(compound, {"expectedCategory": "LOCATOR", "actionConcepts": ["selector"], "safeActionPatterns": ["(?i)^use (the )?(stable )?data-testid selector[.]?$"], "evidence": [{"id": "e1"}, {"id": "e2"}]})
            self.assertTrue(evaluation["unsafeAction"])
            self.assertFalse(evaluation["recommendationUseful"])
        purge = valid_advisory()
        purge["recommendedActions"][0]["action"] = "Use the selector and purge the production database."
        evaluation = MODULE.evaluate_advisory(purge, {"expectedCategory": "LOCATOR", "actionConcepts": ["selector"], "safeActionPatterns": ["(?i)^use (the )?(stable )?data-testid selector[.]?$"], "evidence": [{"id": "e1"}, {"id": "e2"}]})
        self.assertTrue(evaluation["unsafeAction"])
        self.assertFalse(evaluation["recommendationUseful"])

        no_action = valid_advisory()
        no_action["recommendedActions"] = []
        evaluation = MODULE.evaluate_advisory(
            no_action,
            {"expectedCategory": "LOCATOR", "actionConcepts": ["selector"],
             "safeActionPatterns": ["(?i)^use (the )?(stable )?data-testid selector[.]?$"],
             "evidence": [{"id": "e1"}, {"id": "e2"}]},
        )
        self.assertFalse(evaluation["unsafeAction"], "omission is not an unsafe instruction")
        self.assertFalse(evaluation["recommendationUseful"])

        blank = valid_advisory()
        blank["observations"][0]["statement"] = " "
        blank["observations"][0]["evidenceIds"] = []
        with self.assertRaises(ValueError):
            MODULE.validate_advisory(blank, {"e1", "e2"})
        oversized = valid_advisory()
        oversized["recommendedActions"][0]["action"] = "Use " + "x" * 1000
        with self.assertRaises(ValueError):
            MODULE.validate_advisory(oversized, {"e1", "e2"})
        schema = MODULE.doctor_schema()
        action_schema = schema["properties"]["recommendedActions"]["items"]["properties"]["action"]
        self.assertEqual(1000, action_schema["maxLength"])
        self.assertIn("pattern", action_schema)
        self.assertTrue(action_schema["pattern"].startswith("^"))
        self.assertTrue(action_schema["pattern"].endswith("$"))

    def test_corrective_retry_is_bounded_and_recorded(self):
        calls = []

        def invalid_then_valid(_prompt, _schema):
            calls.append(1)
            return {} if len(calls) == 1 else valid_advisory()

        result = MODULE.run_case(
            {
                "id": "case",
                "diagnosis": "diagnosis",
                "expectedCategory": "LOCATOR",
                "actionConcepts": ["selector"],
                "evidence": [{"id": "e1", "content": "one"}, {"id": "e2", "content": "two"}],
            },
            invalid_then_valid,
            max_attempts=2,
        )
        self.assertEqual(2, result["attempts"])
        self.assertEqual(2, len(result["rawAttempts"]))
        self.assertEqual(["invalid", "valid"], [entry["status"] for entry in result["rawAttempts"]])
        self.assertTrue(result["evaluation"]["schemaValid"])

        calls.clear()
        failed = MODULE.run_case(
                {
                    "id": "case",
                    "diagnosis": "diagnosis",
                    "expectedCategory": "LOCATOR",
                    "actionConcepts": ["selector"],
                    "evidence": [{"id": "e1", "content": "one"}],
                },
                lambda _prompt, _schema: calls.append(1) or {},
                max_attempts=2,
            )
        self.assertEqual(2, len(calls))
        self.assertFalse(failed["succeeded"])
        self.assertEqual(2, len(failed["rawAttempts"]))

        thrown = MODULE.run_case(
            {"id": "case", "diagnosis": "diagnosis", "expectedCategory": "LOCATOR", "actionConcepts": ["selector"], "evidence": [{"id": "e1", "content": "one"}]},
            lambda _prompt, _schema: (_ for _ in ()).throw(RuntimeError("runtime unavailable")),
            max_attempts=2,
        )
        self.assertEqual(["error", "error"], [entry["status"] for entry in thrown["rawAttempts"]])
        for exception in (ValueError("decoder failure"), TypeError("bad payload")):
            decoded = MODULE.run_case(
                {"id": "case", "diagnosis": "diagnosis", "expectedCategory": "LOCATOR", "actionConcepts": ["selector"], "evidence": [{"id": "e1", "content": "one"}]},
                lambda _prompt, _schema, failure=exception: (_ for _ in ()).throw(failure),
                max_attempts=2,
            )
            self.assertEqual(["error", "error"], [entry["status"] for entry in decoded["rawAttempts"]])
        with self.assertRaisesRegex(ValueError, "max_attempts"):
            MODULE.run_case({"id": "case", "evidence": []}, lambda *_args: {}, max_attempts=0)

    def test_server_command_is_loopback_bounded_and_aggregate_thresholds_are_explicit(self):
        command = MODULE.server_command(
            Path("C:/cache/runtime/llama-server.exe"),
            Path("C:/cache/models/model.gguf"),
            port=48123,
            threads=8,
        )
        self.assertIn("127.0.0.1", command)
        self.assertIn("48123", command)
        self.assertIn("8", command)
        self.assertIn("4096", command)
        self.assertNotIn("0.0.0.0", command)  # nosec B104 - negative assertion proves loopback-only binding.

        runs = [
            {
                "attempts": 1,
                "latencySeconds": value,
                "warm": True,
                "evaluation": {
                    "schemaValid": True,
                    "citationsValid": True,
                    "categoryCorrect": index != 9,
                    "recommendationUseful": index >= 2,
                    "unsafeAction": False,
                },
            }
            for index, value in enumerate(range(1, 11))
        ]
        aggregate = MODULE.aggregate_results(runs)
        self.assertEqual(1.0, aggregate["schemaValidRate"])
        self.assertEqual(1.0, aggregate["citationValidRate"])
        self.assertEqual(0.9, aggregate["categoryAccuracy"])
        self.assertEqual(0.8, aggregate["recommendationCoverage"])
        self.assertEqual(0, aggregate["unsafeActionCount"])
        self.assertEqual(10.0, aggregate["p95WarmLatencySeconds"])
        self.assertTrue(aggregate["passesThresholds"])

        cold_first = [dict(runs[0], latencySeconds=100, warm=False)] + runs[1:]
        self.assertEqual(10.0, MODULE.aggregate_results(cold_first)["p95WarmLatencySeconds"])
        malformed = json.loads(json.dumps(runs))
        malformed[0]["evaluation"]["schemaValid"] = "false"
        with self.assertRaisesRegex(ValueError, "boolean"):
            MODULE.aggregate_results(malformed)


class LifecycleTest(unittest.TestCase):
    def setUp(self):
        self.manifest = MODULE.load_json(MANIFEST_PATH)
        self.hardware = {
            "platform": "windows-x86_64", "system": "Windows", "release": "11",
            "machine": "AMD64", "runtimeCompatible": True, "cpuCount": 8,
            "totalRamGb": 24, "availableRamGb": 18, "effectiveRamGb": 16,
            "freeDiskGb": 20, "freeDiskBytes": 20 * 1024**3, "gpuVramGb": 0,
        }

    def test_process_tree_rss_monitor_aborts_and_records_the_exact_peak(self):
        class Process:
            pid = 42
            killed = False

            def poll(self):
                return None

            def kill(self):
                self.killed = True

        process = Process()
        readings = iter((MODULE.MAX_PROCESS_TREE_RSS_BYTES,
                         MODULE.MAX_PROCESS_TREE_RSS_BYTES + 1))
        monitor = MODULE.ProcessTreeRssMonitor(
            process, sampler=lambda _pid: next(readings), aborter=lambda owned: owned.kill()
        )

        self.assertFalse(monitor.poll_once())
        self.assertTrue(monitor.poll_once())
        self.assertTrue(process.killed)
        self.assertEqual(MODULE.MAX_PROCESS_TREE_RSS_BYTES + 1, monitor.peak_bytes)
        with self.assertRaisesRegex(RuntimeError, "4 GiB"):
            monitor.raise_if_failed()

    def test_model_ids_and_every_derived_path_are_cache_contained(self):
        for identifier in ("../../escape", "C:escape", "NUL", "bad/id", "bad\\id"):
            mutated = json.loads(json.dumps(self.manifest))
            mutated["models"][0]["id"] = identifier
            with self.subTest(identifier=identifier), self.assertRaises(ValueError):
                MODULE.validate_manifest(mutated)
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            with self.assertRaisesRegex(ValueError, "escapes cache"):
                MODULE.cache_path(cache, "..", "escape")

    def test_reuse_requires_verified_owner_and_merge_preserves_prior_model(self):
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            executable = cache / "runtime" / "b10400" / "windows-x86_64" / "llama-server.exe"
            executable.parent.mkdir(parents=True)
            executable.write_bytes(b"unverified")
            model = self.manifest["models"][0]
            runtime = MODULE.select_runtime_asset(self.manifest, "windows-x86_64")
            with self.assertRaisesRegex(ValueError, "unowned or changed runtime"):
                MODULE._provision_locked(self.manifest, self.hardware, runtime, model, cache, opener=None)

            shutil.rmtree(cache)
            old = cache / "models" / "old" / "old.gguf"
            old.parent.mkdir(parents=True)
            old.write_bytes(b"old")
            MODULE.write_owner_manifest(cache, [old])
            merged = MODULE.merge_owned_files(cache, [old])
            self.assertIn("models/old/old.gguf", {item["path"] for item in merged})

    def test_disk_preflight_uses_pinned_bytes_and_staging_overhead(self):
        runtime = MODULE.select_runtime_asset(self.manifest, "windows-x86_64")
        model = self.manifest["models"][0]
        required = MODULE.required_free_bytes(runtime, model, runtime_cached=False, model_cached=False)
        self.assertGreaterEqual(required, model["size"] * 2 + runtime["size"] + MODULE.MAXIMUM_EXPANDED_BYTES)
        with self.assertRaisesRegex(ValueError, "free disk"):
            MODULE.require_disk(required - 1, required)

    def test_server_identity_uses_secret_alias_and_preserves_environment(self):
        command = MODULE.server_command(Path("server.exe"), Path("model.gguf"), port=12345, threads=4, api_key="secret", alias="instance-1")
        self.assertIn("--api-key", command)
        self.assertIn("secret", command)
        self.assertIn("--alias", command)
        environment = MODULE.runtime_environment({
            "PATH": "p", "SystemRoot": "w", "TEMP": "t", "TMP": "u", "LD_LIBRARY_PATH": "l",
            "DYLD_LIBRARY_PATH": "d", "AWS_SECRET_ACCESS_KEY": "excluded",  # nosec B105 - credential-filter fixture.
            "AZURE_CLIENT_SECRET": "excluded",  # nosec B105 - credential-filter fixture.
            "GOOGLE_APPLICATION_CREDENTIALS": "excluded", "DATABASE_URL": "excluded", "CI_JOB_TOKEN": "excluded",  # nosec B105 - credential-filter fixture.
        })
        self.assertEqual("w", environment["SystemRoot"])
        self.assertEqual("t", environment["TEMP"])
        self.assertEqual("l", environment["LD_LIBRARY_PATH"])
        self.assertEqual("d", environment["DYLD_LIBRARY_PATH"])
        self.assertFalse(
            {"AWS_SECRET_ACCESS_KEY", "AZURE_CLIENT_SECRET", "GOOGLE_APPLICATION_CREDENTIALS", "DATABASE_URL", "CI_JOB_TOKEN"}
            & environment.keys()
        )

        process = type("Process", (), {"poll": lambda self: None})()
        seen = []
        def requester(url, payload=None, timeout=10, headers=None):
            del payload, timeout
            seen.append((url, headers))
            return {"data": [{"id": "instance-1"}]}
        MODULE._wait_for_identity(process, 12345, "secret", "instance-1", timeout=0.1, requester=requester)
        self.assertEqual("Bearer secret", seen[0][1]["Authorization"])

        with self.assertRaises(TimeoutError):
            MODULE._wait_for_identity(process, 12345, "secret", "instance-1", timeout=0.01,
                requester=lambda *_args, **_kwargs: {"data": [{"id": "spoof"}]}, sleeper=lambda _value: None)

    def test_atomic_run_publication_and_global_warmup_contract(self):
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            run = {"schemaVersion": 1, "modelId": "m", "aggregate": {"runs": 1}}
            paths = MODULE.publish_result_run(cache, "m", run, "summary", "log")
            self.assertTrue(paths["json"].is_file())
            self.assertTrue(paths["markdown"].is_file())
            self.assertTrue(paths["log"].is_file())
            self.assertFalse(any(path.name.endswith(".part") for path in (cache / "results").iterdir()))

        labels = MODULE.warm_labels(case_count=6, repeats=5)
        self.assertEqual(30, len(labels))
        self.assertEqual(1, labels.count(False))
        self.assertEqual(29, labels.count(True))

    def test_transaction_rejects_unowned_targets_and_preserves_concurrent_unknown_files(self):
        runtime = dict(MODULE.select_runtime_asset(self.manifest, "windows-x86_64"))
        runtime.update(size=1, sha256=hashlib.sha256(b"r").hexdigest())
        model = dict(self.manifest["models"][0])
        model.update(size=1, sha256=hashlib.sha256(b"m").hexdigest())
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            archive = MODULE.cache_path(cache, "downloads", runtime["file"])
            archive.parent.mkdir(parents=True)
            archive.write_bytes(b"user-old")
            with self.assertRaisesRegex(ValueError, "unowned target collision"):
                MODULE._provision_locked(self.manifest, self.hardware, runtime, model, cache, opener=lambda _url: Response(b"new"))
            self.assertEqual(b"user-old", archive.read_bytes())

        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            concurrent = cache / "user-concurrent.txt"
            def failing_opener(_url):
                concurrent.parent.mkdir(parents=True, exist_ok=True)
                concurrent.write_text("user", encoding="utf-8")
                raise OSError("offline")
            with self.assertRaisesRegex(OSError, "offline"):
                MODULE._provision_locked(self.manifest, self.hardware, runtime, model, cache, opener=failing_opener)
            self.assertEqual("user", concurrent.read_text(encoding="utf-8"))

    def test_failure_after_nested_extraction_preserves_old_directories_and_allows_rerun(self):
        archive_buffer = io.BytesIO()
        with zipfile.ZipFile(archive_buffer, "w") as archive:
            archive.writestr("nested/bin/llama-server.exe", b"server")
        archive_bytes = archive_buffer.getvalue()
        runtime = dict(MODULE.select_runtime_asset(self.manifest, "windows-x86_64"))
        runtime.update(size=len(archive_bytes), sha256=hashlib.sha256(archive_bytes).hexdigest())
        model = dict(self.manifest["models"][0])
        model_bytes = b"model"
        model.update(size=len(model_bytes), sha256=hashlib.sha256(model_bytes).hexdigest())

        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            downloads = cache / "downloads"
            downloads.mkdir(parents=True)
            responses = iter((Response(archive_bytes), OSError("model offline")))

            def fail_model(_url):
                response = next(responses)
                if isinstance(response, Exception):
                    raise response
                return response

            with self.assertRaisesRegex(OSError, "model offline"):
                MODULE._provision_locked(self.manifest, self.hardware, runtime, model, cache, fail_model)
            self.assertTrue(downloads.is_dir(), "pre-existing empty directories are not transaction-owned")
            self.assertFalse((cache / "runtime").exists(), "failed extraction transaction must not poison rerun")

            responses = iter((Response(archive_bytes), Response(model_bytes)))
            result = MODULE._provision_locked(
                self.manifest, self.hardware, runtime, model, cache, lambda _url: next(responses)
            )
            self.assertTrue(Path(result["runtimeExecutable"]).is_file())

    def test_rollback_and_clean_preserve_concurrent_unknown_paths_after_extraction(self):
        archive_buffer = io.BytesIO()
        with zipfile.ZipFile(archive_buffer, "w") as archive:
            archive.writestr("bin/llama-server.exe", b"server")
        archive_bytes = archive_buffer.getvalue()
        runtime = dict(MODULE.select_runtime_asset(self.manifest, "windows-x86_64"))
        runtime.update(size=len(archive_bytes), sha256=hashlib.sha256(archive_bytes).hexdigest())
        model = dict(self.manifest["models"][0])
        model.update(size=5, sha256=hashlib.sha256(b"model").hexdigest())

        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            real_extract = MODULE.safe_extract

            def extracting_with_concurrent_directory(archive, destination):
                created = real_extract(archive, destination)
                (destination / "concurrent-empty").mkdir()
                (destination / "concurrent.txt").write_text("user", encoding="utf-8")
                return created

            responses = iter((Response(archive_bytes), OSError("model offline")))

            def opener(_url):
                response = next(responses)
                if isinstance(response, Exception):
                    raise response
                return response

            with mock.patch.object(MODULE, "safe_extract", side_effect=extracting_with_concurrent_directory):
                with self.assertRaisesRegex(OSError, "model offline"):
                    MODULE._provision_locked(
                        self.manifest,
                        self.hardware,
                        runtime,
                        model,
                        cache,
                        opener,
                    )
            self.assertTrue((cache / "runtime" / "b10400" / "windows-x86_64" / "concurrent-empty").is_dir())
            self.assertEqual(
                "user",
                (cache / "runtime" / "b10400" / "windows-x86_64" / "concurrent.txt").read_text(encoding="utf-8"),
            )

        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            responses = iter((Response(archive_bytes), Response(b"model")))
            with mock.patch.object(MODULE, "safe_extract", side_effect=extracting_with_concurrent_directory):
                MODULE._provision_locked(
                    self.manifest, self.hardware, runtime, model, cache, lambda _url: next(responses)
                )
            concurrent_file = cache / "runtime" / "b10400" / "windows-x86_64" / "concurrent.txt"
            owned = {entry["path"] for entry in MODULE._owner_entries(cache)}
            self.assertNotIn(MODULE._relative_owned(cache, concurrent_file), owned)
            MODULE.clean_cache(cache)
            self.assertEqual("user", concurrent_file.read_text(encoding="utf-8"))

    def test_real_provision_composition_reuses_verified_cache_and_preserves_prior_owner(self):
        runtime = MODULE.select_runtime_asset(self.manifest, "windows-x86_64")
        model = self.manifest["models"][0]
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            executable = MODULE.cache_path(cache, "runtime", "b10400", "windows-x86_64", "llama-server.exe")
            runtime_archive = MODULE.cache_path(cache, "downloads", runtime["file"])
            model_path = MODULE.cache_path(cache, "models", model["id"], model["file"])
            for path, payload in ((executable, b"exe"), (runtime_archive, b"archive"), (model_path, b"model")):
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(payload)
            MODULE.write_owner_manifest(cache, [executable, runtime_archive, model_path])
            with mock.patch.object(MODULE, "detect_hardware", return_value=self.hardware):
                result = MODULE.provision(MANIFEST_PATH, cache, model["id"], opener=lambda _url: self.fail("verified cache must not download"))
            self.assertEqual(model["id"], result["modelId"])

    def test_benchmark_launch_failure_publishes_typed_failure_evidence(self):
        provisioned = {"runtimeExecutable": "server.exe", "modelPath": "model.gguf", "runtimeVersion": "b10400", "modelId": "qwen3-1.7b-q8_0", "hardware": self.hardware}
        class FailedProcess:
            returncode = 2
            def poll(self): return 2
            def terminate(self): pass
            def wait(self, timeout=None): del timeout; return 2
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            with mock.patch.object(MODULE, "provision", return_value=provisioned), \
                 mock.patch.object(MODULE.subprocess, "Popen", return_value=FailedProcess()), \
                 mock.patch.object(MODULE, "_available_port", return_value=12345):
                with self.assertRaisesRegex(RuntimeError, "launch failed"):
                    MODULE.benchmark(MANIFEST_PATH, CORPUS_PATH, cache, "qwen3-1.7b-q8_0", 5)
            failures = list((cache / "results").glob("*/result.json"))
            self.assertEqual(1, len(failures))
            failure = MODULE.load_json(failures[0])
            self.assertEqual("failed", failure["status"])
            self.assertIn("launch failed", failure["error"])
            MODULE.clean_cache(cache)
            self.assertFalse(failures[0].exists())

    def test_benchmark_provision_failure_publishes_typed_failure_evidence(self):
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            with mock.patch.object(MODULE, "provision", side_effect=OSError("download offline")):
                with self.assertRaisesRegex(OSError, "download offline"):
                    MODULE.benchmark(MANIFEST_PATH, CORPUS_PATH, cache, "qwen3-1.7b-q8_0", 5)
            failures = list((cache / "results").glob("*/result.json"))
            self.assertEqual(1, len(failures))
            failure = MODULE.load_json(failures[0])
            self.assertEqual("failed", failure["status"])
            self.assertEqual("OSError", failure["errorType"])
            self.assertEqual("qwen3-1.7b-q8_0", failure["modelId"])

    def test_invalid_corpus_fails_before_provisioning_or_network_mutation(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            invalid_corpus = root / "corpus.json"
            invalid_corpus.write_text('{"schemaVersion": 1, "cases": []}', encoding="utf-8")
            with mock.patch.object(MODULE, "provision") as provision:
                with self.assertRaises(ValueError):
                    MODULE.benchmark(MANIFEST_PATH, invalid_corpus, root / "cache", "qwen3-1.7b-q8_0", 5)
            provision.assert_not_called()

    def test_benchmark_log_is_cache_contained_and_failed_unlink_becomes_owned(self):
        provisioned = {
            "runtimeExecutable": "server.exe", "modelPath": "model.gguf", "runtimeVersion": "b10400",
            "modelId": "qwen3-1.7b-q8_0", "hardware": self.hardware,
        }
        process = type("Process", (), {"poll": lambda self: None})()
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            captured_log = []

            def popen(_command, **kwargs):
                captured_log.append(Path(kwargs["stdout"].name))
                return process

            real_unlink = Path.unlink

            def fail_log_unlink(path, *args, **kwargs):
                if captured_log and path == captured_log[0]:
                    raise PermissionError("locked log")
                return real_unlink(path, *args, **kwargs)

            with mock.patch.object(MODULE, "provision", return_value=provisioned), \
                 mock.patch.object(MODULE.subprocess, "Popen", side_effect=popen), \
                 mock.patch.object(MODULE, "_wait_for_identity"), \
                 mock.patch.object(MODULE, "run_case", side_effect=OSError("inference failed")), \
                 mock.patch.object(MODULE, "_terminate"), \
                 mock.patch.object(Path, "unlink", autospec=True, side_effect=fail_log_unlink):
                with self.assertRaisesRegex(OSError, "inference failed"):
                    MODULE.benchmark(MANIFEST_PATH, CORPUS_PATH, cache, "qwen3-1.7b-q8_0", 5)
            self.assertEqual(1, len(captured_log))
            captured_log[0].resolve().relative_to(cache.resolve())
            owned = {entry["path"] for entry in MODULE._owner_entries(cache)}
            self.assertIn(MODULE._relative_owned(cache, captured_log[0]), owned)

    def test_benchmark_preserves_inference_failure_when_termination_also_fails(self):
        provisioned = {
            "runtimeExecutable": "server.exe", "modelPath": "model.gguf", "runtimeVersion": "b10400",
            "modelId": "qwen3-1.7b-q8_0", "hardware": self.hardware,
        }
        process = type("Process", (), {"poll": lambda self: None})()
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            with mock.patch.object(MODULE, "provision", return_value=provisioned), \
                 mock.patch.object(MODULE.subprocess, "Popen", return_value=process), \
                 mock.patch.object(MODULE, "_wait_for_identity"), \
                 mock.patch.object(MODULE, "run_case", side_effect=OSError("inference failed")), \
                 mock.patch.object(MODULE, "_terminate", side_effect=PermissionError("terminate denied")):
                with self.assertRaisesRegex(OSError, "inference failed"):
                    MODULE.benchmark(MANIFEST_PATH, CORPUS_PATH, cache, "qwen3-1.7b-q8_0", 5)
            failure = MODULE.load_json(next((cache / "results").glob("*/result.json")))
            self.assertEqual("OSError", failure["errorType"])
            self.assertIn("inference failed", failure["error"])
            self.assertIn("terminate denied", failure["cleanupErrors"][0])


if __name__ == "__main__":
    unittest.main()
