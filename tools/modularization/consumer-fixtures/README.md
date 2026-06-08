# Downstream consumer fixtures

These independent Maven projects compile representative downstream usage against the legacy
`io.github.shafthq:SHAFT_ENGINE` coordinate. They intentionally do not share a parent POM so each can be
copied and resolved in isolation.

Run the repository build first, then measure all fixtures with one empty temporary Maven repository each:

```bash
mvn clean install -DskipTests -Dgpg.skip
python3 scripts/ci/measure_consumer_dependencies.py --record
python3 scripts/ci/measure_consumer_dependencies.py --verify
```

Use `--fixture <name>` for a focused run and `--keep-repositories <directory>` to preserve downloaded
repositories for diagnosis. The committed baseline is under `docs/modularization/dependency-baseline/`.
