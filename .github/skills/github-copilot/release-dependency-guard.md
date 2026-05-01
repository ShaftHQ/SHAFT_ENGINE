# Skill: Release & Dependency Guard

## Skill Name

`release-dependency-guard`

## Description

Validates release-version consistency across `pom.xml`, `Internal.java`, and sample projects. Detects stable dependency updates and property drift. Generates a release readiness report with concrete blockers and remediation steps.

## Trigger Examples

- "Check if the repository is ready for a new release."
- "Are all sample project versions in sync with the main pom.xml?"
- "Find any dependency updates we should apply before releasing."

---

## Step 1 — Version Consistency Check

Verify that the SHAFT engine version is consistent across all required locations:

### Files to Check
| # | File | Field / Pattern |
|---|---|---|
| 1 | `pom.xml` (line 6) | `<version>X.Y.Z</version>` |
| 2 | `src/main/java/com/shaft/properties/internal/Internal.java` | `@DefaultValue("X.Y.Z")` on `shaftEngineVersion()` |
| 3–9 | All `pom.xml` files under `src/main/resources/examples/` | `<shaft_engine.version>X.Y.Z</shaft_engine.version>` |

### Validation Rules
- All locations must declare the **same** version string.
- The version must follow SHAFT's format: `{major}.{quarter}.{YYYYMMDD}`.
- Flag any mismatch as a **release blocker**.

---

## Step 2 — Property Drift Detection

Compare the following properties across the main `pom.xml` and all 7 sample project `pom.xml` files:

| Property | Main `pom.xml` Location |
|---|---|
| `jdk.version` | `<source>` / `<target>` in `maven-compiler-plugin` config |
| `aspectjweaver.version` | `<version>` of `org.aspectj:aspectjweaver` in `<dependencies>` |
| `maven-compiler-plugin.version` | `<version>` of `maven-compiler-plugin` in `<build><plugins>` |
| `maven-resources-plugin.version` | `<version>` of `maven-resources-plugin` in `<build><plugins>` |
| `maven-surefire-plugin.version` | `<version>` of `maven-surefire-plugin` in profiles |
| `surefire-testng.version` | `<version>` of `surefire-testng` inside `maven-surefire-plugin` dependencies |

### Validation Rules
- Each property in every sample project must match the canonical value from the main `pom.xml`.
- Any mismatch is a **release blocker** — provide the exact `sed` command to fix it.

---

## Step 3 — Dependency Update Scan

Check for outdated dependencies:

1. Run or simulate `mvn versions:display-dependency-updates` output.
2. For each dependency with an available update:
   - **Include** if the newer version is **stable** (no `-beta`, `-RC`, `-M`, `-alpha`, `-SNAPSHOT` suffixes).
   - **Exclude** pre-release versions.
3. Classify each update:
   - **Patch** (e.g., `1.2.3` → `1.2.4`): Low risk.
   - **Minor** (e.g., `1.2.3` → `1.3.0`): Medium risk — check changelog for breaking changes.
   - **Major** (e.g., `1.2.3` → `2.0.0`): High risk — requires compatibility review.

---

## Step 4 — Internal Property Audit

Verify that no hardcoded values exist where SHAFT properties should be used:

1. Scan for hardcoded version strings in `AllureManager`, `DriverFactory`, or other core classes.
2. Verify that `allure3Version` and `nodeLtsVersion` are read from `SHAFT.Properties.internal.*`, not inline constants.
3. Flag any `System.getProperty()` calls that bypass SHAFT's property layer.

---

## Step 5 — Release Readiness Assessment

Aggregate all findings into a readiness verdict:

| Verdict | Criteria |
|---|---|
| ✅ **Ready** | No blockers, no critical drift, all versions consistent |
| ⚠️ **Ready with Warnings** | No blockers, but minor drift or optional updates available |
| 🚫 **Not Ready** | One or more release blockers exist |

---

## Step 6 — Output Format

Use this exact structure:

```
## Release & Dependency Guard Report

### Version Consistency
| # | File | Expected | Actual | Status |
|---|---|---|---|---|
| 1 | `pom.xml` | X.Y.Z | X.Y.Z | ✅ / ❌ |
| 2 | `Internal.java` | X.Y.Z | X.Y.Z | ✅ / ❌ |
| 3 | `examples/.../pom.xml` | X.Y.Z | X.Y.Z | ✅ / ❌ |
| ... | ... | ... | ... | ... |

### Property Drift
| Property | Main `pom.xml` | Sample Project(s) | Status |
|---|---|---|---|
| `jdk.version` | [value] | [value(s)] | ✅ / ❌ |
| `aspectjweaver.version` | [value] | [value(s)] | ✅ / ❌ |
| ... | ... | ... | ... |

**Remediation** (if drift detected):
```bash
# Copy-paste ready fix commands
find src/main/resources/examples -name "pom.xml" | xargs sed -i 's|<OLD>|<NEW>|g'
```

### Dependency Updates Available
| Dependency | Current | Available | Type | Risk |
|---|---|---|---|---|
| [groupId:artifactId] | [current] | [available] | Patch / Minor / Major | Low / Medium / High |

### Hardcoded Value Audit
| # | File | Line | Issue | Fix |
|---|---|---|---|---|
| 1 | [file] | [line] | [hardcoded value found] | [use SHAFT.Properties.x.y() instead] |

### Release Readiness
| Verdict | Details |
|---|---|
| ✅ Ready / ⚠️ Warnings / 🚫 Not Ready | [summary of blockers and warnings] |

### Blockers (if any)
1. [Blocker description — must be resolved before release]
2. ...

### Recommended Actions
1. [Highest priority action]
2. [Next action]
3. ...
```

---

## Guardrails

- Do not recommend updating to pre-release or milestone versions of dependencies.
- Do not modify `pom.xml` files directly — provide `sed` commands or explicit instructions.
- Always validate version format against SHAFT's `{major}.{quarter}.{YYYYMMDD}` convention.
- Flag but do not auto-fix `Internal.java` changes — they require manual review.
- If `mvn versions:display-dependency-updates` output is unavailable, clearly state that the dependency scan is based on static analysis only.
