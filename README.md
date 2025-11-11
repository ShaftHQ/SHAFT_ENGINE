<div align="center">

<h1>
<picture>
  <source srcset="https://github.com/user-attachments/assets/b2e8454d-97ed-4dd8-91f2-1c09c53ba94e" media="(prefers-color-scheme: light)" width="40"/>
  <source srcset="https://github.com/user-attachments/assets/9cb4a7a8-2de7-486c-adb1-ad254af8c58b"  media="(prefers-color-scheme: dark)" width="40"/>
  <img src="https://github.com/user-attachments/assets/016ebb3c-4090-4f07-a9b3-830fdf4cb696"/>
</picture> SHAFT : Unified Test Automation Engine
</h1>

**Write once, test everywhere** - A powerful, unified test automation framework for Web, Mobile, API, CLI, Database, and more.

[![License](https://img.shields.io/github/license/ShaftHQ/SHAFT_Engine?color=indigo&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/blob/master/LICENSE)
[![Contributors](https://img.shields.io/github/contributors/ShaftHQ/SHAFT_ENGINE?color=indigo&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/graphs/contributors)
[![Latest Release](https://img.shields.io/github/v/release/ShaftHQ/shaft_engine?include_prereleases&color=indigo&label=Latest%20Release&style=for-the-badge)](https://central.sonatype.com/artifact/io.github.shafthq/SHAFT_ENGINE)
[![E2E Tests](https://img.shields.io/github/actions/workflow/status/SHAFTHQ/SHAFT_Engine/e2eTests.yml?branch=main&color=forestgreen&label=E2E%20Tests&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/e2eTests.yml)
[![Code QL](https://img.shields.io/github/actions/workflow/status/SHAFTHQ/SHAFT_Engine/codeql-analysis.yml?branch=main&label=Security&color=forestgreen&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/codeql-analysis.yml)
[![Codacy](https://img.shields.io/codacy/grade/4d6d48aba396411fa3170184330ba089?style=for-the-badge&color=blue&label=Code%20Quality)](https://www.codacy.com/gh/ShaftHQ/SHAFT_ENGINE/dashboard)
[![Codecov](https://img.shields.io/codecov/c/github/shafthq/shaft_engine?style=for-the-badge&label=Coverage&color=blue)](https://app.codecov.io/gh/ShaftHQ/SHAFT_ENGINE)

<a href="https://techforpalestine.org/learn-more" target="_blank"><img alt="Tech for Palestine" src="https://img.shields.io/badge/%F0%9F%87%B5%F0%9F%87%B8_Ceasefire_Now-techforpalestine.org-000000?style=for-the-badge&label=Ceasefire%20Now&color=D83838"></a>

---

### 🏆 Recognition & Trust

<table border="0" align="center">
 <tr>
  <td align="center" valign="top">
   <b>Fully Documented</b><br/><br/>
   <a href="https://ShaftHQ.github.io/"><img width="280" alt="User Guide" src="https://github.com/ShaftHQ/SHAFT_ENGINE/assets/19201898/bdd6db98-4121-4a86-b7db-fb94b8830d11"></a>
  </td>
  <td align="center" valign="top">
   <b>Award Winning</b><br/><br/>
   <a href="https://opensource.googleblog.com/2023/05/google-open-source-peer-bonus-program-announces-first-group-of-winners-2023.html"><img width="230" alt="Google Open Source" src="https://github.com/user-attachments/assets/a21a9e31-c63d-4712-bd9d-2bd131e5173c"/></a>
  </td>
  <td align="center" valign="top">
   <b>Selenium Ecosystem</b><br/><br/>
   <a href="https://www.selenium.dev/ecosystem/#frameworks"><img width="280" alt="Selenium Ecosystem" src="https://github.com/ShaftHQ/SHAFT_ENGINE/assets/19201898/b13d4c2c-72ce-4de6-861f-d143f905c5ab"></a>
  </td>
 </tr>
</table>

</div>

---

## 📖 Table of Contents

- [🚀 Why SHAFT?](#-why-shaft)
- [⚡ Quick Start](#-quick-start)
- [✨ Key Features](#-key-features)
- [🌍 Success Partners](#-success-partners)
- [📚 Documentation](#-documentation)
- [🤝 Community & Support](#-community--support)
- [📜 License](#-license)

---

## 🚀 Why SHAFT?

SHAFT Engine is a **unified test automation framework** that eliminates the complexity of managing multiple tools and frameworks. Built on industry-standard technologies like Selenium, Appium, and REST Assured, SHAFT provides a fluent, maintainable API that works across all platforms.

### What Makes SHAFT Different?

- **🎯 Unified API**: Same fluent syntax for Web, Mobile, API, CLI, and Database testing
- **🔧 Zero Boilerplate**: Smart auto-configuration with sensible defaults
- **📊 Rich Reporting**: Built-in Allure integration with screenshots, videos, and detailed logs
- **🤖 AI-Powered**: Visual validation using OpenCV and intelligent element detection
- **🌐 Cloud-Ready**: Native support for BrowserStack, LambdaTest, and Selenium Grid
- **⚙️ Flexible**: Works with TestNG, JUnit5, and Cucumber out of the box

### Global Adoption

**41,511 active users** across **10+ countries** trust SHAFT for their test automation needs.

[View detailed analytics →](docs/USER_ANALYTICS.md)

---

## ⚡ Quick Start

Get up and running in under 5 minutes!

### Option 1: Project Generator 🎉 (Recommended)
The fastest way to create a new SHAFT project with an interactive UI.

👉 **[Launch Project Generator](https://shafthq.github.io/docs/Getting_Started/first_steps_5#option-1-interactive-project-generator-recommended)**

Choose your test runner (TestNG/JUnit/Cucumber), platform (Web/Mobile/API), and download a ready-to-use project.

### Option 2: Maven Archetype
Perfect for command-line users:

```bash
mvn archetype:generate \
  -DarchetypeGroupId=io.github.shafthq \
  -DarchetypeArtifactId=shaft-archetype
```

### Option 3: Add to Existing Project

Add SHAFT to your `pom.xml`:

```xml
<dependency>
    <groupId>io.github.shafthq</groupId>
    <artifactId>SHAFT_ENGINE</artifactId>
    <version><!-- Check latest release --></version>
</dependency>
```

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
    
    @AfterMethod
    public void teardown() {
        driver.quit();
    }
}
```

📖 **[Complete Quick Start Guide →](docs/QUICK_START.md)**

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

**Browsers**: Chrome, Edge, Firefox, Safari (Desktop & Mobile)  
**Mobile**: Native, Hybrid, Flutter apps (Android & iOS)  
**Other**: REST APIs, Databases, CLI, Files (JSON/YAML/Excel/PDF)

📖 **[Complete Feature List →](docs/FEATURES.md)**

---

## 🌍 Success Partners

<div align="center">

### 💎 Sponsors

<a href="https://www.browserstack.com/" target="_blank"><img src="https://ml.globenewswire.com/Resource/Download/745e80b7-4736-424e-b44b-850d2dc41940" alt="BrowserStack" height="50"></a>
&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://applitools.com/" target="_blank"><img src="https://www.selenium.dev/images/sponsors/applitools.png" alt="Applitools" height="50"></a>
&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://jb.gg/OpenSourceSupport" target="_blank"><img src="https://resources.jetbrains.com/storage/products/company/brand/logos/jetbrains.svg" alt="JetBrains" height="50"></a>
&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://www.lambdatest.com" target="_blank"><img src="https://www.lambdatest.com/blog/wp-content/uploads/2024/10/LambdaTest-Logo.png" alt="LambdaTest" width="250" height="50"></a>

### 🏢 Trusted By Leading Organizations

<a href="https://www.vodafone.com/careers/professional-career-areas/shared-services"><img height="45" alt="Vodafone" src="https://www.vodafone.com/_ipx/w_768,q_75/https%3A%2F%2Fimages.ctfassets.net%2Fq7ob9vms4z5k%2F2u767jo6qLM730dcOVs1lN%2F110f5535a8a0505e3b0aef0934a4a07c%2FVOIS.png?url=https%3A%2F%2Fimages.ctfassets.net%2Fq7ob9vms4z5k%2F2u767jo6qLM730dcOVs1lN%2F110f5535a8a0505e3b0aef0934a4a07c%2FVOIS.png&w=768&q=75"></a>
&nbsp;&nbsp;
<a href="https://www.getgroup.com/"><img height="45" alt="GET Group" src="https://media.licdn.com/dms/image/C510BAQFS-mP8SeyOyg/company-logo_200_200/0/1630600086112/get_group_logo?e=2147483647&v=beta&t=mhfpG9gfW0JC4afaWYeHWWrA5AgMOv6g9bP3FnsN40o"></a>
&nbsp;&nbsp;
<a href="https://www.momah.gov.sa/en"><img height="45" alt="MOMRA" src="https://momah.gov.sa/themes/custom/momrah/assets/images/mh-logo-full.png"></a>
&nbsp;&nbsp;
<a href="https://gizasystems.com/"><img height="45" alt="GIZA Systems" src="https://github.com/user-attachments/assets/45e7bc17-b3f3-44c7-a053-2f250ea497b6"></a>
&nbsp;&nbsp;
<a href="https://www.euronetworldwide.com/"><img height="45" alt="Euronet" src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/55/Euronet_Worldwide_logo.svg/1920px-Euronet_Worldwide_logo.svg.png"></a>
&nbsp;&nbsp;
<a href="https://www.incorta.com/"><img height="45" alt="Incorta" src="https://media-exp1.licdn.com/dms/image/C560BAQHUWHhKl0xrCA/company-logo_200_200/0/1660913597037?e=2147483647&v=beta&t=CiDPUEvlIBqztN5gCre-pQ5f7M-03_02IQgJtL18wG8"></a>
&nbsp;&nbsp;
<a href="https://dxc.com/us/en"><img height="45" alt="DXC Technology" src="https://github.com/user-attachments/assets/84cb59da-d29d-44fa-9012-b10d2cc671ff"></a>

**[View all partners →](docs/SUCCESS_PARTNERS.md)**

</div>

---

## 📚 Documentation

<div align="center">

| Resource | Description |
|----------|-------------|
| 📖 **[User Guide](https://shafthq.github.io/)** | Comprehensive documentation and tutorials |
| 🏗️ **[Architecture](docs/ARCHITECTURE.md)** | Framework design and module overview |
| 🛠️ **[Tech Stack](docs/TECH_STACK.md)** | Technologies powering SHAFT |
| 📊 **[User Analytics](docs/USER_ANALYTICS.md)** | Global adoption and growth metrics |

</div>

---

## 🤝 Community & Support

<div align="center">

### Join Our Community

<a href="https://join.slack.com/t/shaft-engine/shared_invite/zt-oii5i2gg-0ZGnih_Y34NjK7QqDn01Dw" target="_blank"><img src="https://a.slack-edge.com/80588/marketing/img/icons/icon_slack_hash_colored.png" alt="Slack" height="50"/></a>
&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://www.facebook.com/groups/Automatest" target="_blank"><img src="https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEjLMDbbgNWvnrNY3pjRSfgqZCPIbGMnVRm1jaaoGhgT2Buv-ipatDIe9zjRJIM1b8eZTZm7csh-R1vfHWwwW9nSlEC4agzoLrGqRsRWogha5oZIYS4LXXLSrAg7ekta6niiXxt5XHe_oLU/s200/f_logo_RGB-Blue_1024.png" alt="Facebook" height="50"/></a>

### Contributing

We welcome contributions! Feel free to:
- 🐛 [Report bugs](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/new?template=bug_report.md)
- 💡 [Request features](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/new?template=feature_request.md)
- 🔀 [Submit pull requests](https://github.com/ShaftHQ/SHAFT_ENGINE/pulls)

Check our [Contributing Guidelines](CONTRIBUTING.md) and [Code of Conduct](CODE_OF_CONDUCT.md).

### Stay Updated

⭐ **Star this repo** to get notified about new releases!

</div>

---

## 📜 License

SHAFT Engine is released under the [MIT License](LICENSE).

---

<div align="center">

> **Stop Reinventing the Wheel! Start Using SHAFT!**

<a href="https://ShaftHQ.github.io/" target="_blank">
<picture>
  <source srcset="src/main/resources/images/shaft.png" media="(prefers-color-scheme: light)" width="300"/>
  <source srcset="src/main/resources/images/shaft_white.png" media="(prefers-color-scheme: dark)" width="300"/>
  <img src="src/main/resources/images/shaft.png" alt="SHAFT_ENGINE" width="300"/>
</picture>
</a>

Made with ❤️ by the SHAFT community

</div>

