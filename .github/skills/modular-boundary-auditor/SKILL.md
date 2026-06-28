---
name: modular-boundary-auditor
description: Use when auditing Maven module boundaries, BOM wiring, legacy coordinates, bundled examples, or clean consumer fixtures.
---

# Modular Boundary Auditor

Use for module boundary and packaging drift across Maven reactor modules, examples, BOM, legacy coordinate, and clean consumer fixtures.

## Workflow

1. Inspect root/module POMs, `shaft-bom`, `legacy-shaft-engine`, bundled examples, and `tools/modularization/consumer-fixtures/**` before changing checks.
2. Keep optional module dependencies out of `shaft-engine`; fix the owning module, BOM, or fixture instead.
3. Run `python3 scripts/ci/validate_reactor_versions.py` when versions or inter-module coordinates change.
4. Run `python3 scripts/ci/validate_modular_documentation.py`, `python3 scripts/ci/validate_maven_publication.py`, and the relevant `tests/scripts/test_*_boundary.py`.
5. For release-candidate surfaces, add `python3 scripts/ci/validate_shaft_pilot_release.py` and affected clean-fixture `mvn --batch-mode -f ... verify "-Dshaft.version=..."` checks.

## Output

State the boundary touched, owning module, BOM/fixture/example impact, checks run, and any untested fixture matrix.
