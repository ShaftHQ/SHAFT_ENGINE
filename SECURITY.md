# Security Policy

## Supported Versions

SHAFT Engine follows a **latest-release-only** support model. Security fixes are
applied to the most recent release exclusively.

| Version | Supported |
|---------|-----------|
| Latest | Yes |
| Older | No |

If a vulnerability affects you on an older version, upgrade to the latest
release first:
[![Maven Central](https://img.shields.io/maven-central/v/io.github.shafthq/shaft-engine?style=flat-square&label=latest%20version)](https://central.sonatype.com/artifact/io.github.shafthq/shaft-engine)

## Reporting a Vulnerability

**Do not open a public GitHub issue for a security vulnerability.** Public
disclosure before a fix is available puts every user at risk.

**Preferred —
[report a vulnerability privately](https://github.com/ShaftHQ/SHAFT_ENGINE/security/advisories/new).**
This opens a private discussion with the maintainers and stays confidential
until a fix ships.

**Alternative** — if you cannot use GitHub advisories, email
**mohabmohie@gmail.com**.

Include in your report:

- A clear description of the vulnerability.
- Steps to reproduce, with minimal code or configuration.
- Potential impact and affected versions.
- Suggested mitigations or patches (optional, appreciated).

## Response Timeline

| Stage | Target |
|-------|--------|
| Acknowledgement | Within **48 hours** of receipt |
| Initial assessment | Within **5 business days** |
| Fix released | Depends on severity — critical issues are prioritized |

You will be credited in the release notes unless you prefer to stay anonymous.

## Scope

**In scope:**

- Vulnerabilities in SHAFT Engine's own source code (`shaft-engine/src/main/java/`).
- Unsafe configuration or property defaults that could expose user data.
- Dependency vulnerabilities with a direct exploitable path through SHAFT's
  public API.

**Out of scope:**

- Vulnerabilities in test code (`shaft-engine/src/test/java/`) — these are
  example tests, not production code.
- Third-party library issues with no exploitable path through SHAFT; report
  those upstream.
- Social engineering or phishing.

## Dependency Security

SHAFT Engine runs **Dependabot** on a daily schedule and **CodeQL** static
analysis on every pull request, which shortens the exposure window for known
CVEs in transitive dependencies.

Security-related dependency updates land as Dependabot pull requests and appear
in the relevant
[release changelog](https://github.com/ShaftHQ/SHAFT_ENGINE/releases).

## Disclosure Policy

SHAFT Engine follows **coordinated disclosure**:

1. Reporter submits a private report.
2. Maintainers confirm and assess the issue.
3. A fix is developed and tested privately.
4. The fix is released as part of the next SHAFT version.
5. A GitHub Security Advisory is published, crediting the reporter.

We ask that you give us a reasonable amount of time to address the issue before any public disclosure.
