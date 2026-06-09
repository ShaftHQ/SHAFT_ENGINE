# Publication validation fixtures

These disposable Maven consumers validate the artifacts produced by the complete SHAFT reactor without changing the
committed cold-cache dependency baselines.

`all-modules` imports `shaft-bom` and resolves `shaft-engine`, `shaft-browserstack`, `shaft-video`, and `shaft-visual`
together. Its `verify` lifecycle enforces dependency convergence, rejects duplicate classes, and generates a CycloneDX
JSON SBOM.

Run it after installing the reactor into the local Maven repository:

```bash
mvn clean install -DskipTests -Dgpg.skip
mvn --batch-mode --file tools/modularization/publication-fixtures/all-modules/pom.xml verify
```
