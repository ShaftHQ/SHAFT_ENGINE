# Java pack

Optional language pack for Java projects. The installer enables it when a
profile's `installWhen.mavenArtifactIds` matches the root `pom.xml`.

## Maven Tools MCP

- Native mode publishes `.chaos-engine/tool.py maven-tools-mcp`, which
  resolves the shared ChaosEngine cache (managed JDK and the Maven Tools JAR)
  at runtime. Git-tracked `.mcp.json` never embeds workstation-absolute
  java/jar paths. Docker mode may publish a portable image ref.
- Health: doctor reports the `maven-tools-mcp` component; heal it once with
  `install.py repair --component maven-tools-mcp`. Never rebuild it from source.
- Projects without a matching `pom.xml` stay on the portable distribution;
  a missing Maven Tools server is not unhealthy there.

## Managed Maven and JDK

The pack owns the managed Temurin JDK and Maven used by the tool server. The
core installer and doctor stay language-neutral.

## Code

- [pack.json](pack.json) declares the pack and its `maven-tools-mcp` component.
- [maven_tools.py](maven_tools.py) holds the managed JDK, managed Maven, Maven
  Tools cache, and runtime probe; [installer.py](installer.py) holds POM
  artifact detection, provisioning, and `repair --component maven-tools-mcp`.
- [pack_binding.py](../../pack_binding.py) binds each module's `__all__` into
  the `hosts.py` / `install.py` namespace, so core installer tests run without
  Java and patches on the controller still reach pack code.
