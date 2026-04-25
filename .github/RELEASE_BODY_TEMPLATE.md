<!-- SHAFT_ENGINE Release Body Template
     Rendered automatically by the Maven Central CD workflow (mavenCentral_cd.yml).

     HOW THIS WORKS:
     - The workflow runs `sed` to replace $RELEASE_VERSION with the actual version, then passes the
       result as the `bodyFile` to ncipollo/release-action with `generateReleaseNotes: true`.
     - ncipollo APPENDS the auto-generated changelog (categorized via .github/release.yml) AFTER the
       entire content of this template. It does NOT inject it into any placeholder inside this file.
     - Therefore: do NOT add static sections here that reference optional changelog categories
       (e.g. "Breaking Changes", "Security Fixes") — those sections only appear in the appended
       changelog when PRs carry the matching labels, and referencing them unconditionally creates
       misleading empty-looking entries in every release. -->

<div align="center">

# 🎉 SHAFT_ENGINE $RELEASE_VERSION

**The unified test automation engine — Web · Mobile · API · CLI · Database**

[![Maven Central](https://img.shields.io/maven-central/v/io.github.shafthq/SHAFT_ENGINE?color=blue&logo=apachemaven)](https://central.sonatype.com/artifact/io.github.shafthq/SHAFT_ENGINE)
[![GitHub stars](https://img.shields.io/github/stars/ShaftHQ/SHAFT_ENGINE?style=social)](https://github.com/ShaftHQ/SHAFT_ENGINE/stargazers)

</div>

---

## ⬆️ How to Upgrade

Update the version in your `pom.xml`:

```xml
<properties>
    <shaft_engine.version>$RELEASE_VERSION</shaft_engine.version>
</properties>
```

Or update the dependency directly:

```xml
<dependency>
    <groupId>io.github.shafthq</groupId>
    <artifactId>SHAFT_ENGINE</artifactId>
    <version>$RELEASE_VERSION</version>
</dependency>
```

> [!IMPORTANT]
> **We support only the latest release.** If you encounter any issue, please upgrade to `$RELEASE_VERSION` first before filing a bug report.

---

## 📖 Resources

| Resource | Link |
|---|---|
| 📚 Full Documentation | [shafthq.github.io](https://shafthq.github.io/) |
| 🚀 Getting Started | [Quick-Start Guide](https://shafthq.github.io/) |
| 📋 JavaDocs | [ShaftHQ JavaDoc](https://shafthq.github.io/SHAFT_ENGINE/) |
| 🗺️ Roadmap | [GitHub Projects](https://github.com/orgs/ShaftHQ/projects) |
| 💬 Community | [GitHub Discussions](https://github.com/ShaftHQ/SHAFT_ENGINE/discussions) |
| 🐛 Report a Bug | [Bug Report](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/new?template=bug_report.md) |
| 💡 Request a Feature | [Feature Request](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/new?template=feature_request.md) |
| 💬 Slack | [Join our Slack](https://join.slack.com/t/shaft-engine/shared_invite/zt-oii5i2gg-0ZGnih_Y34NjK7QqDn01Dw) |

---

## 🙌 Get Involved

If SHAFT_ENGINE saves you time, helps your team ship quality software faster, or you just enjoy using it — please consider:

- ⭐ **[Star the repository](https://github.com/ShaftHQ/SHAFT_ENGINE)** — it helps others discover the project and motivates the team!
- 🐛 **Found a bug?** [Open a bug report](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/new?template=bug_report.md)
- 💡 **Have an idea?** [Request a feature](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/new?template=feature_request.md)
- 🤝 **Want to contribute?** Read the [Contributing Guide](https://github.com/ShaftHQ/SHAFT_ENGINE/blob/main/CONTRIBUTING.md)
- 💬 **Questions or feedback?** Join us on [GitHub Discussions](https://github.com/ShaftHQ/SHAFT_ENGINE/discussions) or [Slack](https://join.slack.com/t/shaft-engine/shared_invite/zt-oii5i2gg-0ZGnih_Y34NjK7QqDn01Dw)

---

<!-- The auto-generated, categorized changelog (from .github/release.yml) is appended here by GitHub. -->

