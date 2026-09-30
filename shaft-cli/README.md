# shaft-cli

**Purpose:** Command-line client over the shaft-mcp capability set.

**Use or skip:** Use this module for the CLI surface. Skip it when the task changes only the MCP server internals.

## Humans

From the repository root, compile this module without the test suite:

```bash
mvn -pl shaft-cli -am -DskipTests package
```

Published product usage stays on the user guide: [https://shafthq.github.io/](https://shafthq.github.io/).

## Agents

Load [AGENTS.md](../AGENTS.md) and follow ChaosEngine. This README is orientation only. Do not treat it as policy. Open this directory when the task names `shaft-cli`; otherwise skip it.

## Next

- [Root orientation](../README.md)
- [Reactor parent](../pom.xml)
