# Upgrade to modular SHAFT

This guide upgrades projects from the final pre-modularization release, `10.2.20260605`, to the modular SHAFT release. The Java package namespace remains `com.shaft`; only Maven coordinates and optional dependency packaging change.

## Coordinates: before and after

Before:

```xml
<dependency>
  <groupId>io.github.shafthq</groupId>
  <artifactId>SHAFT_ENGINE</artifactId>
  <version>10.2.20260605</version>
</dependency>
```

After, with the recommended BOM:

```xml
<properties><shaft.version>10.2.20260609</shaft.version></properties>
<dependencyManagement>
  <dependencies>
    <dependency>
      <groupId>io.github.shafthq</groupId>
      <artifactId>shaft-bom</artifactId>
      <version>${shaft.version}</version>
      <type>pom</type>
      <scope>import</scope>
    </dependency>
  </dependencies>
</dependencyManagement>
<dependencies>
  <dependency>
    <groupId>io.github.shafthq</groupId>
    <artifactId>shaft-engine</artifactId>
  </dependency>
</dependencies>
```

Or use an explicit version without the BOM:

```xml
<dependency>
  <groupId>io.github.shafthq</groupId>
  <artifactId>shaft-engine</artifactId>
  <version>${shaft.version}</version>
</dependency>
```

Version `10.2.20260609` is the first modular SHAFT release. Confirm current availability on [Maven Central](https://central.sonatype.com/artifact/io.github.shafthq/shaft-engine).

## Choose optional modules

All optional modules use the same version as `shaft-engine`; omit their `<version>` when importing `shaft-bom`.

| Add this module | Only when you use |
| --- | --- |
| `shaft-browserstack` | BrowserStack SDK integration or `browserstack.yml` SDK orchestration. Ordinary remote WebDriver sessions do not require it. |
| `shaft-video` | SHAFT-managed desktop screen recording and its platform FFmpeg/JAVE payload. Appium driver-native mobile recording remains in `shaft-engine`. |
| `shaft-visual` | OpenCV-backed reference-image matching and visual comparison. Ordinary screenshots and reporting remain in `shaft-engine`. |

```xml
<dependency><groupId>io.github.shafthq</groupId><artifactId>shaft-browserstack</artifactId></dependency>
<dependency><groupId>io.github.shafthq</groupId><artifactId>shaft-video</artifactId></dependency>
<dependency><groupId>io.github.shafthq</groupId><artifactId>shaft-visual</artifactId></dependency>
```

REST Assured API testing, Appium/Flutter/mobile testing, database support and JDBC drivers, CLI, Cucumber, reporting, accessibility, and the public `com.shaft` API remain in `shaft-engine`. API and mobile projects must not add an optional module unless they independently use that optional capability.

## Legacy relocation and support window

The old `io.github.shafthq:SHAFT_ENGINE` coordinate is published as a relocation POM pointing to `io.github.shafthq:shaft-engine` for the modular release line. It is a compatibility bridge, not the recommended declaration. Existing builds can resolve while teams migrate, but new projects and the bundled examples use the lowercase coordinate immediately. The relocation bridge is supported for the modular release's major-version lifecycle; removal requires a future major release and advance deprecation notice.

Relocation selects only `shaft-engine`. Maven cannot infer whether a project needs BrowserStack, desktop video, or OpenCV, so add those providers explicitly.

## CI and cache migration

1. Change cache keys when `pom.xml` hashes are not already part of the key. The coordinate rename and split modules create new paths under `~/.m2/repository/io/github/shafthq/`.
2. Do not copy the old `SHAFT_ENGINE` directory into the new `shaft-engine` path. Let Maven verify checksums and populate it.
3. Run `mvn dependency:purge-local-repository -DmanualInclude=io.github.shafthq:SHAFT_ENGINE,io.github.shafthq:shaft-engine -DreResolve=false` only when a stale local snapshot or failed relocation is suspected.
4. Build once with an empty cache in CI before comparing download size or timing.

### Missing-provider troubleshooting

| Symptom | Action |
| --- | --- |
| BrowserStack SDK classes/configuration are unavailable | Add `shaft-browserstack`; verify `mvn dependency:tree` contains it. |
| Desktop recording reports no provider | Add `shaft-video`; confirm the OS-specific `ws.schild:jave-nativebin-*` artifact resolves. |
| Reference-image matching reports no visual provider | Add `shaft-visual`; confirm `org.openpnp:opencv` resolves. |
| Mobile recording changed unexpectedly | Do not add `shaft-video` for Appium recording; verify `shaft-engine` is aligned and the driver supports native recording. |
| `NoSuchMethodError` or mixed SHAFT modules | Import `shaft-bom`, remove explicit mismatched module versions, and inspect `mvn dependency:tree -Dincludes=io.github.shafthq`. |

## Verified cold-cache download measurements

The final table records compressed classpath JAR bytes from isolated empty Maven repositories. The pre-modularization graph is the committed `10.2.20260605` baseline captured at commit `570a836` before extraction. The modular engine graph is measured from the reactor artifact. Platform rows differ by the JAVE native binary selected by the old POM; `shaft-engine` itself is platform-neutral. MiB uses 1,048,576 bytes.

| Supported platform | Before: `SHAFT_ENGINE` | After: `shaft-engine` only | Saved |
| --- | ---: | ---: | ---: |
| Linux x64 | 352,001,986 (335.70 MiB) | 169,941,464 (162.07 MiB) | 182,060,522 (173.63 MiB / 51.7%) |
| Linux ARM64 | 348,005,291 (331.88 MiB) | 169,941,464 (162.07 MiB) | 178,063,827 (169.81 MiB / 51.2%) |
| Windows x64 | 350,617,708 (334.38 MiB) | 169,941,464 (162.07 MiB) | 180,676,244 (172.31 MiB / 51.5%) |
| macOS x64 | 344,700,722 (328.73 MiB) | 169,941,464 (162.07 MiB) | 174,759,258 (166.66 MiB / 50.7%) |
| macOS ARM64 | 341,487,898 (325.67 MiB) | 169,941,464 (162.07 MiB) | 171,546,434 (163.60 MiB / 50.2%) |

Reproduce the current graph after a reactor build with:

```bash
mvn clean install -DskipTests -Dgpg.skip
python3 scripts/ci/measure_consumer_dependencies.py --fixture api --output target/modular-measurement
```

Starting from the measured Linux x64 baseline, the other platform totals subtract the 28,201,169-byte Linux x64 native JAR and add the resolved native JAR: Linux ARM64 24,204,474 bytes, Windows x64 26,816,891 bytes, macOS x64 20,899,905 bytes, or macOS ARM64 17,687,081 bytes. These are deterministic substitutions of exact Maven Central JAR sizes; they avoid claiming cross-run repository-growth or elapsed-time comparisons as download savings.

## Rollback

If migration blocks a release, revert the POM to `io.github.shafthq:SHAFT_ENGINE:10.2.20260605`, restore the previous dependency cache key, and remove the optional module declarations. This returns to the last monolithic release. Do not mix the old monolithic JAR with modular artifacts in one classpath. Capture `mvn dependency:tree` before rollback so a missing provider or version mismatch can be diagnosed, then reattempt with the BOM.
