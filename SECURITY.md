# Security Policy

## Supported Versions

SHAFT Engine follows a **latest-release-only** support model. Security fixes are applied to the most recent release exclusively.

| Version | Supported |
|---------|-----------|
| Latest  | ✅ |
| Older   | ❌ |

If you are affected by a vulnerability on an older version, please upgrade to the latest release first.  
Check the current version: [![Maven Central](https://img.shields.io/maven-central/v/io.github.shafthq/SHAFT_ENGINE?style=flat-square&label=latest%20version)](https://central.sonatype.com/artifact/io.github.shafthq/SHAFT_ENGINE)

---

## Reporting a Vulnerability

**Please do NOT open a public GitHub issue for security vulnerabilities.**  
Public disclosure before a fix is available puts all users at risk.

### Preferred: GitHub Private Security Advisory

Use GitHub's built-in private reporting channel:

👉 **[Report a vulnerability privately](https://github.com/ShaftHQ/SHAFT_ENGINE/security/advisories/new)**

This creates a private discussion between you and the maintainers. Your report remains confidential until a fix is released.

### Alternative: Email

If you cannot use GitHub's advisory system, email the maintainer directly:

📧 **mohabmohie@gmail.com**

Include the following in your report:
- A clear description of the vulnerability
- Steps to reproduce (minimal code or configuration)
- Potential impact and affected versions
- Any suggested mitigations or patches (optional but appreciated)

---

## Response Timeline

| Stage | Target |
|-------|--------|
| Acknowledgement | Within **48 hours** of receipt |
| Initial assessment | Within **5 business days** |
| Fix released | Depends on severity — critical issues are prioritized |

You will be credited in the release notes (unless you prefer to remain anonymous).

---

## Scope

The following are **in scope** for security reports:

- Vulnerabilities in SHAFT Engine's own source code (`src/main/java/`)
- Unsafe defaults in configuration or properties that could expose user data
- Dependency vulnerabilities that have a direct exploitable path through SHAFT's public API

The following are **out of scope**:

- Vulnerabilities in test code (`src/test/java/`) — these are example tests, not production code
- Issues in third-party libraries where no exploitable path through SHAFT exists (report these upstream)
- Social engineering or phishing attacks

---

## Dependency Security

SHAFT Engine uses **Dependabot** for automated daily dependency updates and **CodeQL** for continuous static analysis on every pull request. This significantly reduces the window of exposure for known CVEs in transitive dependencies.

Security-related dependency updates are tracked through Dependabot pull requests and are reflected in each [release's changelog](https://github.com/ShaftHQ/SHAFT_ENGINE/releases) when applicable.

---

## Disclosure Policy

SHAFT Engine follows **coordinated disclosure**:

1. Reporter submits a private report.
2. Maintainers confirm and assess the issue.
3. A fix is developed and tested privately.
4. The fix is released as part of the next SHAFT version.
5. A GitHub Security Advisory is published, crediting the reporter.

We ask that you give us a reasonable amount of time to address the issue before any public disclosure.
