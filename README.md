<div align="center">

<img src="https://shaftengine.netlify.app/img/shaft-automation-hero.png" alt="SHAFT connects Web, Mobile, API, CLI, Database, reporting, and agent workflows" width="900">

# SHAFT

### One Java 25 engine for Web, Mobile, API, CLI, and Database testing.

Fluent automation, synchronized actions, built-in assertions, rich evidence,
and optional agent tools without rebuilding the framework for every surface.

[![GitHub Stars](https://img.shields.io/github/stars/ShaftHQ/SHAFT_ENGINE?style=flat-square&logo=github)](https://github.com/ShaftHQ/SHAFT_ENGINE/stargazers)
[![Maven Central](https://img.shields.io/maven-central/v/io.github.shafthq/shaft-engine?style=flat-square&logo=apachemaven)](https://central.sonatype.com/artifact/io.github.shafthq/shaft-engine)
[![Build](https://img.shields.io/github/actions/workflow/status/ShaftHQ/SHAFT_ENGINE/e2eTests.yml?branch=main&style=flat-square&label=tests)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/e2eTests.yml)
[![Docs](https://img.shields.io/badge/docs-live-5b4bff?style=flat-square)](https://shaftengine.netlify.app/docs/start/overview)

[Start](https://shaftengine.netlify.app/docs/start/quick-start) ·
[Connect an AI agent](https://shaftengine.netlify.app/docs/agentic/mcp) ·
[Explore features](https://shaftengine.netlify.app/docs/features/modules) ·
[Star SHAFT](https://github.com/ShaftHQ/SHAFT_ENGINE)

</div>

## Launch shaft-mcp

Connect Codex to SHAFT browser automation, Capture, and Doctor:

```bash
curl -fsSL https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/scripts/mcp/install-shaft-mcp.sh | sh -s -- --codex
```

Windows PowerShell:

```powershell
irm https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/scripts/mcp/install-shaft-mcp.ps1 | iex; Install-ShaftMcp -Client codex
```

Then ask:

> Use SHAFT to open `https://example.com`, verify the page title, and summarize
> the generated test evidence.

[Copilot / VS Code setup](https://shaftengine.netlify.app/docs/agentic/mcp) ·
[Doctor command](https://shaftengine.netlify.app/docs/agentic/doctor) ·
[Heal command](https://shaftengine.netlify.app/docs/agentic/heal)

## A Complete Web Test

```java
import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.*;

public class SearchTest {
    private SHAFT.GUI.WebDriver driver;

    @BeforeMethod
    public void openBrowser() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @Test
    public void search() {
        driver.browser().navigateToURL("https://duckduckgo.com/")
                .and().element().type(By.name("q"), "SHAFT Engine")
                .and().assertThat().title().contains("DuckDuckGo");
    }

    @AfterMethod(alwaysRun = true)
    public void closeBrowser() {
        driver.quit();
    }
}
```

[Install SHAFT](https://shaftengine.netlify.app/docs/start/installation) ·
[Web guide](https://shaftengine.netlify.app/docs/testing/web)

## Go Straight to Your Goal

| I want to... | Open |
|---|---|
| Test a browser | [Web testing](https://shaftengine.netlify.app/docs/testing/web) |
| Test Android, iOS, or Flutter | [Mobile testing](https://shaftengine.netlify.app/docs/testing/mobile) |
| Test REST or GraphQL APIs | [API testing](https://shaftengine.netlify.app/docs/testing/api) |
| Run terminal or file actions | [CLI testing](https://shaftengine.netlify.app/docs/testing/cli) |
| Query and validate databases | [Database testing](https://shaftengine.netlify.app/docs/testing/database) |
| Diagnose a failed run | [SHAFT Doctor](https://shaftengine.netlify.app/docs/agentic/doctor) |
| Recover an eligible locator | [SHAFT Heal](https://shaftengine.netlify.app/docs/agentic/heal) |
| Upgrade from `SHAFT_ENGINE` | [Upgrade guide](https://shaftengine.netlify.app/docs/start/upgrade) |
| Understand the modules | [Architecture and modules](https://shaftengine.netlify.app/docs/features/architecture) |

## Evidence

SHAFT is published on
[Maven Central](https://central.sonatype.com/artifact/io.github.shafthq/shaft-engine),
listed in the
[Selenium ecosystem](https://www.selenium.dev/ecosystem/#frameworks), and was
recognized through the
[Google Open Source Peer Bonus program](https://opensource.googleblog.com/2023/05/google-open-source-peer-bonus-program-announces-first-group-of-winners-2023.html).

## Contribute

Read [CONTRIBUTING.md](CONTRIBUTING.md), open an
[issue](https://github.com/ShaftHQ/SHAFT_ENGINE/issues), or join the
[community](https://shaftengine.netlify.app/docs/start/overview#community).

Public documentation is maintained in
[ShaftHQ/shafthq.github.io](https://github.com/ShaftHQ/shafthq.github.io).
Engine changes that affect users must include a linked documentation-site pull
request or explain why documentation is not required.

MIT licensed. See [LICENSE](LICENSE).
