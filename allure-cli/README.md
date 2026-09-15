# allure-cli (Maven zip)

Pinned **Allure 3 CLI** (`allure@<version>` npm package) packaged as a Maven
**zip** for air-gapped / corporate-repo provision (#5815).

## Coordinates

| Field | Value |
| --- | --- |
| groupId | `io.github.shafthq` |
| artifactId | `allure-cli` |
| version | same as root `allure.cli.version` / `Internal.allure3Version()` (e.g. `3.17.0`) |
| type | `zip` |

Example dependency:

```xml
<dependency>
  <groupId>io.github.shafthq</groupId>
  <artifactId>allure-cli</artifactId>
  <version>3.17.0</version>
  <type>zip</type>
</dependency>
```

## Zip layout

Unpacking the zip into `~/.m2/repository/allure/allure-cli/<version>/` yields:

```text
node_modules/allure/cli.js
…
package.json
```

That is the same runtime path `AllureManager` already prefers (#5801).

## Local install (produces the zip)

Requires `npm` on `PATH` **once** while building the artifact:

```bash
mvn -f allure-cli/pom.xml clean install -Dgpg.skip
```

Installs:

- `~/.m2/repository/io/github/shafthq/allure-cli/<version>/allure-cli-<version>.zip`
- companion `.pom`

## Consumer / air-gap unpack (no npm)

After the zip is in a Maven repo (local or corporate):

```bash
# Hard air-gap: unpack Maven zip only (fails if the artifact is missing)
mvn -Pprovision-allure-cli-maven -pl shaft-engine -am initialize

# Prefer Maven zip when present, else npm (existing profile, enhanced)
mvn -Pprovision-allure-cli -pl shaft-engine -am initialize
```

Engine bootstrap (`AllureManager.tryProvisionAllureCli`) also unpacks a
**local** Maven zip into the runtime cache before falling back to npm.

## Maven Central

Publishing this zip on Central (CD job, GPG, `verify_maven_central_release.py`)
is deferred to #5833; this module ships the installable layout and consumer
path first.
