"""Historical Memory object rewrite (#5845)."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
HOSTS = ROOT / "chaos-engine/hosts.py"


def load():
    spec = importlib.util.spec_from_file_location("hosts_memory_migrate_5845", HOSTS)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _write_minimal_store(project: Path, obj: dict) -> Path:
    memory = project / ".memory"
    (memory / "schema").mkdir(parents=True)
    (memory / "memory").mkdir()
    (memory / "relations").mkdir()
    (memory / "events.jsonl").write_text("", encoding="utf-8")
    (memory / "config.json").write_text(
        json.dumps(
            {
                "version": 5,
                "project": {"id": "project.fixture", "name": "fixture"},
                "memory": {"autoIndex": True, "defaultTokenBudget": 6000},
            },
            indent=2,
        )
        + "\n",
        encoding="utf-8",
    )
    for name in ("config.schema.json", "event.schema.json", "object.schema.json", "patch.schema.json", "relation.schema.json"):
        src = ROOT / "chaos-engine/assets/memory-v5" / name
        (memory / "schema" / name).write_bytes(src.read_bytes())
    path = memory / "memory" / "architecture.json"
    path.write_text(json.dumps(obj, indent=2) + "\n", encoding="utf-8")
    (memory / "memory" / "architecture.md").write_text("# architecture\n", encoding="utf-8")
    return path


class MemoryMigrateTests(unittest.TestCase):
    def test_rewrites_extra_fields_and_types_with_backup(self):
        hosts = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            original = {
                "body_path": "memory/architecture.md",
                "content_hash": "sha256:" + "a" * 64,
                "created_at": "2026-06-15T16:59:56+03:00",
                "evidence": ["pom.xml", {"kind": "file", "id": "AGENTS.md"}],
                "facets": {"category": "architecture"},
                "id": "architecture.current",
                "scope": {"kind": "project", "project": "project.fixture"},
                "source": {"kind": "system"},
                "status": "active",
                "tags": [],
                "title": "Current Architecture",
                "type": "architecture",
                "updated_at": "2026-07-08T16:41:17+03:00",
            }
            path = _write_minimal_store(project, original)
            events = project / ".memory/events.jsonl"
            events.write_text(
                '{"actor":"agent","event":"memory.updated","id":"architecture.current",'
                '"timestamp":"2026-07-08T16:41:17+03:00"}\n',
                encoding="utf-8",
            )
            self.assertTrue(hosts.legacy_memory_v5_objects_compatible(project))
            result = hosts.migrate_legacy_memory_store(project)
            self.assertEqual("migrated", result["status"])
            self.assertEqual(1, result["objects"])
            backup = Path(str(result["backup"]))
            self.assertTrue((backup / "memory" / "architecture.json").is_file())
            migrated = json.loads(path.read_text(encoding="utf-8"))
            self.assertEqual("feature", migrated["type"])
            self.assertEqual("feature.current", migrated["id"])
            self.assertNotIn("scope", migrated)
            self.assertNotIn("facets", migrated)
            self.assertEqual(
                [{"kind": "file", "id": "pom.xml"}, {"kind": "file", "id": "AGENTS.md"}],
                migrated["evidence"],
            )
            expected_hash = hosts._memory_object_content_hash(
                migrated,
                (project / ".memory/memory/architecture.md").read_text(encoding="utf-8"),
            )
            self.assertEqual(expected_hash, migrated["content_hash"])
            self.assertNotEqual(original["content_hash"], migrated["content_hash"])
            self.assertFalse(hosts.legacy_memory_v5_objects_compatible(project))
            self.assertIn('"id":"feature.current"', events.read_text(encoding="utf-8"))
            self.assertNotIn('"id":"architecture.current"', events.read_text(encoding="utf-8"))

    def test_transform_error_restores_originals(self):
        hosts = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            original = {
                "body_path": "memory/architecture.md",
                "content_hash": "sha256:" + "b" * 64,
                "created_at": "2026-06-15T16:59:56+03:00",
                "evidence": [],
                "id": "architecture.current",
                "scope": {"kind": "project"},
                "source": {"kind": "system"},
                "status": "active",
                "tags": [],
                "title": "Current Architecture",
                "type": "architecture",
                "updated_at": "2026-07-08T16:41:17+03:00",
            }
            path = _write_minimal_store(project, original)
            before = path.read_bytes()

            def boom(_value):
                raise ValueError("forced")

            hosts._migrate_one_memory_object = boom
            result = hosts.migrate_legacy_memory_store(project)
            self.assertEqual("failed", result["status"])
            self.assertEqual(before, path.read_bytes())

    def test_id_collision_fails_closed(self):
        hosts = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            first = {
                "body_path": "memory/architecture.md",
                "content_hash": "sha256:" + "c" * 64,
                "created_at": "2026-06-15T16:59:56+03:00",
                "evidence": [],
                "id": "architecture.same",
                "scope": {"kind": "project"},
                "source": {"kind": "system"},
                "status": "active",
                "tags": [],
                "title": "A",
                "type": "architecture",
                "updated_at": "2026-07-08T16:41:17+03:00",
            }
            path = _write_minimal_store(project, first)
            second = dict(first)
            second["id"] = "workflow.same"
            second["type"] = "workflow"
            second["title"] = "B"
            (project / ".memory/memory" / "workflow.json").write_text(
                json.dumps(second, indent=2) + "\n", encoding="utf-8"
            )
            (project / ".memory/memory" / "workflow.md").write_text("# w\n", encoding="utf-8")
            before = path.read_bytes()
            result = hosts.migrate_legacy_memory_store(project)
            self.assertEqual("failed", result["status"])
            self.assertIn("collision", str(result.get("reason", "")).lower())
            self.assertEqual(before, path.read_bytes())

    def test_doctor_probe_does_not_migrate(self):
        hosts = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            original = {
                "body_path": "memory/architecture.md",
                "content_hash": "sha256:" + "d" * 64,
                "created_at": "2026-06-15T16:59:56+03:00",
                "evidence": [],
                "id": "architecture.current",
                "scope": {"kind": "project"},
                "source": {"kind": "system"},
                "status": "active",
                "tags": [],
                "title": "Current Architecture",
                "type": "architecture",
                "updated_at": "2026-07-08T16:41:17+03:00",
            }
            path = _write_minimal_store(project, original)
            before = path.read_bytes()
            (project / ".chaos-engine").mkdir()
            (project / ".chaos-engine/tool.py").write_text("print('noop')\n", encoding="utf-8")
            payload = json.dumps(
                {
                    "ok": True,
                    "data": {"valid": False, "errors": [{"code": "MemorySchemaValidationFailed"}]},
                    "error": {"code": "MemorySchemaValidationFailed"},
                }
            )
            completed = type("R", (), {"returncode": 1, "stdout": payload, "stderr": ""})()
            import unittest.mock as mock
            with mock.patch.object(hosts.subprocess, "run", return_value=completed):
                status = hosts.retrieval_runtime_status(project)
            self.assertEqual("compatible-legacy", status["status"])
            self.assertEqual(before, path.read_bytes())
