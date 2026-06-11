# Downstream consumer fixtures

These independent Maven projects compile representative downstream usage against SHAFT. Most fixtures use the
canonical `io.github.shafthq:shaft-engine` coordinate, `bom` imports `io.github.shafthq:shaft-bom` and declares a
versionless engine dependency, `browserstack-sdk` uses the optional `io.github.shafthq:shaft-browserstack` integration,
`desktop-video` uses the optional `io.github.shafthq:shaft-video` integration, and `legacy-coordinate` verifies the
POM-only `io.github.shafthq:SHAFT_ENGINE` relocation path. `pilot-core` compiles against provider-neutral Pilot
contracts while enforcing that `shaft-ai` is absent. They intentionally do not share a parent POM so each can be
copied and resolved in isolation.

Run the repository build first, then measure all fixtures with one empty temporary Maven repository each:

```bash
mvn clean install -DskipTests -Dgpg.skip
python3 scripts/ci/measure_consumer_dependencies.py --record
python3 scripts/ci/measure_consumer_dependencies.py --verify
```

Use `--fixture <name>` for a focused run and `--keep-repositories <directory>` to preserve downloaded
repositories for diagnosis. Verification compares every downstream artifact coordinate, scope, size, and checksum,
while excluding the rebuilt SHAFT JAR itself; validate that JAR separately by comparing its sorted entry list. The
committed pre-modularization baseline is under `docs/modularization/dependency-baseline/`.

## Combined publication consumer

`combined-modules/pom.xml` imports `shaft-bom` and resolves the engine plus all optional modules, including `shaft-capture`, together. Its `verify` phase enforces dependency convergence, checks duplicate classes/resources (while excluding BrowserStack's intentionally shaded SDK and documented upstream resource overlaps), and generates a CycloneDX JSON SBOM. `pilot-core/pom.xml` proves provider-neutral Pilot consumers do not resolve `shaft-ai`. `mcp/pom.xml` resolves and copies the preserved `io.github.shafthq:SHAFT_MCP` executable coordinate without adding it to ordinary engine consumers. See `docs/MAVEN_CENTRAL_PUBLICATION.md` for the release dry-run command.
