<!-- SHAFT Engine - Java Test Automation Framework | Selenium | Appium | REST Assured | Cross-Browser Testing -->
<a id="top"></a>
<div align="center">

<h1>
<picture>
  <source srcset="https://github.com/user-attachments/assets/b2e8454d-97ed-4dd8-91f2-1c09c53ba94e" media="(prefers-color-scheme: light)" width="40"/>
  <source srcset="https://github.com/user-attachments/assets/9cb4a7a8-2de7-486c-adb1-ad254af8c58b"  media="(prefers-color-scheme: dark)" width="40"/>
  <img src="https://github.com/user-attachments/assets/016ebb3c-4090-4f07-a9b3-830fdf4cb696" alt="SHAFT Engine Logo"/>
</picture> SHAFT : Unified Test Automation Engine
</h1>

**Stop reinventing the wheel.** Write your tests once and run them everywhere — across browsers, devices, and APIs — with a single, fluent Java framework.

SHAFT Engine is an open-source **Java test automation framework** built on top of [Selenium](https://www.selenium.dev/), [Appium](https://appium.io/), and [REST Assured](https://rest-assured.io/). It provides a unified, fluent API for **cross-browser testing**, **mobile app testing**, **API testing**, **CLI testing**, and **database testing** — so you can automate anything, from any platform, with zero boilerplate.

[![GitHub Stars](https://img.shields.io/github/stars/ShaftHQ/SHAFT_ENGINE?style=for-the-badge&logo=github&label=Stars&color=gold)](https://github.com/ShaftHQ/SHAFT_ENGINE/stargazers)
[![Maven Central](https://img.shields.io/maven-central/v/io.github.shafthq/SHAFT_ENGINE?style=for-the-badge&logo=apachemaven&label=Maven%20Central&color=indigo)](https://central.sonatype.com/artifact/io.github.shafthq/SHAFT_ENGINE)
[![License](https://img.shields.io/github/license/ShaftHQ/SHAFT_Engine?color=indigo&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/blob/master/LICENSE)
[![E2E Tests](https://img.shields.io/github/actions/workflow/status/SHAFTHQ/SHAFT_Engine/e2eTests.yml?branch=main&color=forestgreen&label=E2E%20Tests&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/e2eTests.yml)
[![Code QL](https://img.shields.io/github/actions/workflow/status/SHAFTHQ/SHAFT_Engine/codeql-analysis.yml?branch=main&label=Security&color=forestgreen&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/codeql-analysis.yml)
[![Codacy](https://img.shields.io/codacy/grade/4d6d48aba396411fa3170184330ba089?style=for-the-badge&color=blue&label=Code%20Quality)](https://www.codacy.com/gh/ShaftHQ/SHAFT_ENGINE/dashboard)
[![Codecov](https://img.shields.io/codecov/c/github/shafthq/shaft_engine?style=for-the-badge&label=Coverage&color=blue)](https://app.codecov.io/gh/ShaftHQ/SHAFT_ENGINE)
[![Contributors](https://img.shields.io/github/contributors/ShaftHQ/SHAFT_ENGINE?color=indigo&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/graphs/contributors)

<a href="https://techforpalestine.org/learn-more" target="_blank"><img alt="Tech for Palestine - Ceasefire Now" src="https://img.shields.io/badge/%F0%9F%87%B5%F0%9F%87%B8_Ceasefire_Now-techforpalestine.org-000000?style=for-the-badge&label=Ceasefire%20Now&color=D83838"></a>

---

### 🏆 Recognition & Trust

<table border="0" align="center">
 <tr>
  <td align="center" valign="top">
   <b>Fully Documented</b><br/><br/>
   <a href="https://ShaftHQ.github.io/"><img width="280" alt="SHAFT Engine User Guide and Documentation" src="https://github.com/ShaftHQ/SHAFT_ENGINE/assets/19201898/bdd6db98-4121-4a86-b7db-fb94b8830d11"></a>
  </td>
  <td align="center" valign="top">
   <b>Award Winning</b><br/><br/>
   <a href="https://opensource.googleblog.com/2023/05/google-open-source-peer-bonus-program-announces-first-group-of-winners-2023.html"><img width="230" alt="Google Open Source Peer Bonus Award Winner 2023" src="https://github.com/user-attachments/assets/a21a9e31-c63d-4712-bd9d-2bd131e5173c"/></a>
  </td>
  <td align="center" valign="top">
   <b>Selenium Ecosystem</b><br/><br/>
   <a href="https://www.selenium.dev/ecosystem/#frameworks"><img width="280" alt="Official Selenium Ecosystem Framework" src="https://github.com/ShaftHQ/SHAFT_ENGINE/assets/19201898/b13d4c2c-72ce-4de6-861f-d143f905c5ab"></a>
  </td>
 </tr>
</table>

</div>

---

## 📖 Table of Contents

- [🚀 Why SHAFT?](#-why-shaft)
- [💡 See the Difference](#-see-the-difference)
- [⚡ Quick Start](#-quick-start)
- [✨ Key Features](#-key-features)
- [🌍 Success Partners](#-success-partners)
- [📚 Documentation](#-documentation)
- [🤝 Community & Support](#-community--support)
- [💖 Sponsor](#-sponsor)
- [📜 License](#-license)

---

## 🚀 Why SHAFT?

If you're tired of managing a fragile patchwork of Selenium helpers, custom wrappers, and retry logic — you're not alone. **SHAFT Engine** replaces all of that with a single, battle-tested framework trusted by **41,000+ engineers** across **10+ countries**.

Built on industry-standard technologies — [Selenium](https://www.selenium.dev/), [Appium](https://appium.io/), and [REST Assured](https://rest-assured.io/) — SHAFT gives you a fluent, chainable API that works across **web browsers**, **mobile apps**, **REST APIs**, **databases**, and **CLIs**. No more boilerplate. No more flaky tests. Just results.

### What Makes SHAFT Different?

| Feature | What You Get |
|---|---|
| 🎯 **Unified API** | One fluent syntax for Web, Mobile, API, CLI, and Database testing |
| 🔧 **Zero Boilerplate** | Smart auto-configuration — no WebDriverManager, no manual waits, no setup code |
| 📊 **Rich Reporting** | Built-in [Allure](https://allurereport.org/) integration with screenshots, videos, and detailed logs |
| 🤖 **AI-Powered Validation** | Visual testing with [OpenCV](https://opencv.org/) and intelligent element detection |
| 🌐 **Cloud-Ready** | Native support for [BrowserStack](https://www.browserstack.com/), [LambdaTest](https://www.lambdatest.com/), and [Selenium Grid](https://www.selenium.dev/documentation/grid/) |
| ⚙️ **Any Test Runner** | Works with [TestNG](https://testng.org/), [JUnit 5](https://junit.org/junit5/), and [Cucumber](https://cucumber.io/) out of the box |
| 🐳 **Containerized** | Ready-to-use Docker support for CI/CD pipelines |
| 📱 **Full Mobile** | Native, Hybrid, and [Flutter](https://flutter.dev/) apps on Android & iOS via [Appium](https://appium.io/) |

### Global Adoption

**41,511 active users** across **10+ countries** trust SHAFT for their test automation needs — from startups to Fortune 500 companies.

[View detailed analytics →](docs/USER_ANALYTICS.md)

---

## 💡 See the Difference

Stop writing dozens of lines of brittle Selenium boilerplate. Here's what SHAFT looks like compared to raw Selenium:

<table>
<tr>
<td>

**❌ Raw Selenium (verbose & fragile)**

</td>
<td>

**✅ SHAFT Engine (clean & reliable)**

</td>
</tr>
<tr>
<td>

```java
WebDriver driver = new ChromeDriver();
driver.get("https://example.com");
WebDriverWait wait =
    new WebDriverWait(driver, Duration.ofSeconds(10));
WebElement el = wait.until(
    ExpectedConditions.visibilityOfElementLocated(
        By.name("q")));
el.sendKeys("SHAFT");
el.sendKeys(Keys.ENTER);
String title = driver.getTitle();
Assert.assertTrue(title.contains("SHAFT"));
driver.quit();
```

</td>
<td>

```java
SHAFT.GUI.WebDriver driver =
    new SHAFT.GUI.WebDriver();
driver.browser().navigateToURL("https://example.com")
  .and().element().type(By.name("q"),
      "SHAFT" + Keys.ENTER)
  .and().assertThat().browser().title()
      .contains("SHAFT");
driver.quit();
```

</td>
</tr>
</table>

> **Fewer lines. Automatic synchronization. Built-in assertions. Beautiful reports.** That's SHAFT.

---

## ⚡ Quick Start

Get up and running in under 5 minutes!

### Option 1: Interactive Project Generator 🎉 *(Recommended)*
The fastest way to create a new SHAFT project with a user-friendly web UI:

👉 **[Launch Project Generator](https://shaftengine.netlify.app/docs/Getting_Started/first_steps_5#option-1-interactive-project-generator-recommended)**

Choose your test runner (TestNG / JUnit 5 / Cucumber), select your platform (Web / Mobile / API), and download a ready-to-use project — complete with CI/CD configuration.

### Option 2: Maven Archetype
Create a new project from the command line:

```bash
mvn archetype:generate \
  -DarchetypeGroupId=io.github.shafthq \
  -DarchetypeArtifactId=shaft-archetype
```

### Option 3: Add to an Existing Project

<details>
<summary><b>Maven</b> — add to your <code>pom.xml</code></summary>

```xml
<dependency>
    <groupId>io.github.shafthq</groupId>
    <artifactId>SHAFT_ENGINE</artifactId>
    <!-- Get the latest version from Maven Central ↓ -->
    <version><!-- SEE BADGE BELOW --></version>
</dependency>
```
</details>

<details>
<summary><b>Gradle (Kotlin DSL)</b> — add to your <code>build.gradle.kts</code></summary>

```kotlin
dependencies {
    // Get the latest version from Maven Central ↓
    testImplementation("io.github.shafthq:SHAFT_ENGINE:<version>")
}
```
</details>

> 💡 Check the latest version: [![Maven Central](https://img.shields.io/maven-central/v/io.github.shafthq/SHAFT_ENGINE?style=flat-square&label=latest%20version)](https://central.sonatype.com/artifact/io.github.shafthq/SHAFT_ENGINE)

### Your First Test

```java
import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.*;

public class QuickStartTest {
    SHAFT.GUI.WebDriver driver;

    @BeforeMethod
    public void setup() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @Test
    public void searchTest() {
        driver.browser().navigateToURL("https://duckduckgo.com/")
              .and().element().type(By.name("q"), "SHAFT_Engine")
              .and().element().click(By.cssSelector("button[type='submit']"))
              .and().assertThat().browser().title().contains("SHAFT_Engine");
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        driver.quit();
    }
}
```

📖 **[Complete Quick Start Guide →](docs/QUICK_START.md)** — includes TestNG, JUnit 5, and Cucumber examples.

---

## ✨ Key Features

<div align="center">

### The 3 Pillars of Successful Test Automation

| 📈 **Scalability** | 🎯 **Reliability** | 🔧 **Maintainability** |
|:---:|:---:|:---:|
| CI/CD Integration | Auto-Synchronization | Fluent Design |
| Cloud Execution | Rich Reporting | Smart Locators |
| Parallel Testing | Video Recording | AI Validations |
| Containerization | Screenshot Capture | Zero Boilerplate |

</div>

### Platform Support

| Category | Supported |
|---|---|
| **Browsers** | Chrome, Edge, Firefox, Safari (Desktop & Mobile) |
| **Mobile Apps** | Native, Hybrid, Flutter (Android & iOS) |
| **APIs** | REST (GET, POST, PUT, PATCH, DELETE) with full response validation |
| **Databases** | MySQL, SQL Server, PostgreSQL, Oracle, IBM DB2, H2, MongoDB |
| **Other** | CLI / Terminal, File Assertions (JSON, YAML, Excel, PDF, CSV) |
| **Test Runners** | TestNG, JUnit 5, Cucumber BDD |

📖 **[Complete Feature List →](docs/FEATURES.md)** · **[Tech Stack →](docs/TECH_STACK.md)** · **[Architecture →](docs/ARCHITECTURE.md)**

---

## 🌍 Success Partners

<div align="center">

### 💎 Sponsors

<a href="https://www.browserstack.com/" target="_blank"><img src="https://ml.globenewswire.com/Resource/Download/745e80b7-4736-424e-b44b-850d2dc41940" alt="BrowserStack - Cross Browser Testing Platform" height="50"></a>
&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://www.lambdatest.com" target="_blank"><img src="https://assets.testmuai.com/resources/images/logos/logo.svg" alt="LambdaTest - Cloud Testing Platform" width="250" height="50"></a>
&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://applitools.com/" target="_blank"><img src="https://www.selenium.dev/images/sponsors/applitools.png" alt="Applitools - Visual AI Testing" height="50"></a>

### 🏢 Trusted By Leading Organizations

<a href="https://www.vodafone.com/careers/professional-career-areas/shared-services"><img height="45" alt="Vodafone - Global Telecommunications" src="https://cdn.cookielaw.org/logos/a018e2cd-22cc-44d6-bd04-a7629b563be5/01928bed-547e-7b46-9d41-f5fe2fd4d6be/d9b809b0-e840-41aa-946e-6d57a5612479/vois-logo.png"></a>
&nbsp;&nbsp;
<a href="https://dxc.com/us/en"><img height="45" alt="DXC Technology - IT Services Company" src="https://github.com/user-attachments/assets/84cb59da-d29d-44fa-9012-b10d2cc671ff"></a>
&nbsp;&nbsp;
<a href="https://www.euronetworldwide.com/"><img height="45" alt="Euronet Worldwide - Electronic Payments" src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/55/Euronet_Worldwide_logo.svg/1920px-Euronet_Worldwide_logo.svg.png"></a>
&nbsp;&nbsp;
<a href="https://www.idemia.com/"><img height="45" alt="IDEMIA - Identity Technologies" src="https://github.com/user-attachments/assets/f20a6eab-a7f9-4f73-b27f-0e808c446fe9"></a>
&nbsp;&nbsp;
<a href="https://www.getgroup.com/"><img height="45" alt="GET Group Holdings - Secure Identity Solutions" src="https://www.getgroup.com/logo.png"></a>

**[View all partners →](docs/SUCCESS_PARTNERS.md)**

</div>

---

## 📚 Documentation

<div align="center">

| Resource | Description |
|----------|-------------|
| 📖 **[User Guide](https://shaftengine.netlify.app/)** | Comprehensive documentation, tutorials, and configuration reference |
| 🏗️ **[Architecture](docs/ARCHITECTURE.md)** | Framework design, module overview, and Mermaid diagrams |
| 🛠️ **[Tech Stack](docs/TECH_STACK.md)** | Technologies and libraries powering SHAFT |
| ✨ **[Features](docs/FEATURES.md)** | Full feature list with platform compatibility matrix |
| 📊 **[User Analytics](docs/USER_ANALYTICS.md)** | Global adoption, geographic breakdown, and growth metrics |

### 🤖 AI-Powered Code Exploration

<a href="https://codewiki.google/github.com/shafthq/shaft_engine" target="_blank"><img src="https://assets.codewiki.google/favicon.ico" alt="Explore SHAFT Engine on Google Code Wiki" width="64"/></a>

**[Explore SHAFT Engine on Google Code Wiki →](https://codewiki.google/github.com/shafthq/shaft_engine)**

Browse, search, and understand the SHAFT Engine codebase using [**Google Code Wiki**](https://codewiki.google/github.com/shafthq/shaft_engine) — Google's AI-powered code exploration tool. Ask questions about the code, navigate complex implementations, and get intelligent explanations of framework internals.

</div>

---

## 🤝 Community & Support

<div align="center">

### Join Our Community

<a href="https://join.slack.com/t/shaft-engine/shared_invite/zt-oii5i2gg-0ZGnih_Y34NjK7QqDn01Dw" target="_blank"><img src="https://a.slack-edge.com/80588/marketing/img/icons/icon_slack_hash_colored.png" alt="Join SHAFT Engine on Slack" height="50"/></a>
&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://www.facebook.com/groups/Automatest" target="_blank"><img src="https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEjLMDbbgNWvnrNY3pjRSfgqZCPIbGMnVRm1jaaoGhgT2Buv-ipatDIe9zjRJIM1b8eZTZm7csh-R1vfHWwwW9nSlEC4agzoLrGqRsRWogha5oZIYS4LXXLSrAg7ekta6niiXxt5XHe_oLU/s200/f_logo_RGB-Blue_1024.png" alt="SHAFT Engine Facebook Community Group" height="50"/></a>

### Contributing

We love contributions from the community! Here's how you can get involved:

- 🐛 [Report a bug](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/new?template=bug_report.md) — found something broken? Let us know
- 💡 [Request a feature](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/new?template=feature_request.md) — have an idea? We'd love to hear it
- 🔀 [Submit a pull request](https://github.com/ShaftHQ/SHAFT_ENGINE/pulls) — jump right in and help build SHAFT

Read our [Contributing Guidelines](CONTRIBUTING.md) and [Code of Conduct](CODE_OF_CONDUCT.md) to get started.

### ⭐ Show Your Support

If SHAFT has saved you time or helped your team ship faster, **please consider giving us a star.** It helps others discover the project and motivates the team to keep improving it.

[![Star SHAFT Engine on GitHub](https://img.shields.io/github/stars/ShaftHQ/SHAFT_ENGINE?style=social)](https://github.com/ShaftHQ/SHAFT_ENGINE/stargazers)

</div>

---

## 💖 Sponsor

SHAFT Engine is free and open source, built with love by a dedicated community. If you or your organization benefit from SHAFT, consider [sponsoring the project](https://github.com/sponsors/MohabMohie) to help ensure its long-term sustainability.

<div align="center">

[![Sponsor SHAFT Engine](https://img.shields.io/badge/Sponsor-%E2%9D%A4-ea4aaa?style=for-the-badge&logo=github)](https://github.com/sponsors/MohabMohie)

</div>

---

## 📜 License

SHAFT Engine is released under the [MIT License](LICENSE) — free to use, modify, and distribute in personal and commercial projects.

---

<div align="center">

<a href="https://ShaftHQ.github.io/" target="_blank">
<picture>
  <source srcset="src/main/resources/images/shaft.png" media="(prefers-color-scheme: light)" width="300"/>
  <source srcset="src/main/resources/images/shaft_white.png" media="(prefers-color-scheme: dark)" width="300"/>
  <img src="src/main/resources/images/shaft.png" alt="SHAFT Engine - Unified Test Automation Framework for Java" width="300"/>
</picture>
</a>

**Stop Reinventing the Wheel. Start Using SHAFT.**

Made with ❤️ by the [SHAFT community](https://github.com/ShaftHQ/SHAFT_ENGINE/graphs/contributors)

[⬆ Back to Top](#top)

</div>

