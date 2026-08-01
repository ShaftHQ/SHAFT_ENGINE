# Authoritative practice sources

Accessed 2026-08-02. Playbooks synthesize these sources; they do not reproduce
source text. Source IDs keep citations terse and make review deterministic.

| ID | Authority and URL | Used for |
| --- | --- | --- |
| `SHAFT-GUIDE` | [SHAFT User Guide](https://shafthq.github.io/docs/) | Current SHAFT concepts, test surfaces, configuration, and official usage |
| `SHAFT-MCP` | [SHAFT MCP guide](https://shafthq.github.io/docs/agentic/mcp) | MCP operating boundaries and canonical-catalog discovery |
| `SHAFT-REPORTING` | [SHAFT reporting and evidence](https://shafthq.github.io/docs/features/reporting) | Structured evidence, traces, diagnostics, flake profiles, and redaction |
| `ISTQB-CTFL` | [ISTQB Certified Tester Foundation Level syllabus v4.0.1](https://istqb.org/wp-content/uploads/2024/11/ISTQB_CTFL_Syllabus_v4.0.1.pdf) | Test process, analysis, design, implementation, execution, monitoring, defects, and completion |
| `ISTQB-TM` | [ISTQB Advanced Level Test Management syllabus v3.0](https://istqb.org/wp-content/uploads/2024/11/ISTQB_CTAL-TM_Syllabus_v3.0_zKjKsaN.pdf) | Risk-based planning, progress control, defect workflow, stakeholder and completion reporting |
| `SELENIUM-PRACTICES` | [Selenium test practices](https://www.selenium.dev/documentation/test_practices/) | Browser automation scope, isolation, maintainability, and discouraged behavior |
| `W3C-WCAG22` | [Web Content Accessibility Guidelines 2.2](https://www.w3.org/TR/WCAG22/) | Accessibility success criteria, levels, scope, and conformance limits |
| `W3C-ACT` | [W3C Accessibility Conformance Testing Rules](https://www.w3.org/WAI/standards-guidelines/act/rules/) | Repeatable automated, semi-automated, and manual accessibility checks |
| `ALLURE-RESULTS` | [Allure test statuses](https://allurereport.org/docs/test-statuses/) and [result files](https://allurereport.org/docs/how-it-works-test-result-file/) | Status semantics, structured evidence, messages, traces, and attachments |
| `ALLURE-STABILITY` | [Allure history and retries](https://allurereport.org/docs/history-and-retries/) and [stability analysis](https://allurereport.org/docs/test-stability/) | History identity, retries, trend analysis, and flaky-result investigation |
| `APPIUM` | [Appium documentation](https://appium.io/docs/en/latest/) | Mobile session configuration, platform context, capabilities, and evidence boundaries |
| `ANDROID-TESTING` | [Android testing strategies](https://developer.android.com/training/testing/fundamentals/strategies) | Layered mobile coverage, device scope, accessibility, and release-candidate testing |
| `OWASP-WSTG` | [OWASP Web Security Testing Guide](https://owasp.org/www-project-web-security-testing-guide/stable/) | Risk-based web and service security test design |
| `GOOGLE-SRE-TESTING` | [Google SRE: Testing for Reliability](https://sre.google/sre-book/testing-reliability/) | Reliability, production probes, hermeticity, controlled repetition, and release confidence |
| `GOOGLE-SRE-MONITORING` | [Google SRE: Monitoring Distributed Systems](https://sre.google/sre-book/monitoring-distributed-systems/) | User-facing signals, actionable alerts, symptoms, causes, and monitoring design |

Exact SHAFT MCP schemas remain generated truth in
`shaft-mcp/src/main/resources/META-INF/shaft-mcp/tool-index.json`; no playbook
copies schemas or treats this bibliography as a tool catalog.
