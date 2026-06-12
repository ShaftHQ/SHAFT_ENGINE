# Architecture

SHAFT has two kinds of modularity:

1. Maven artifacts control dependency weight.
2. The `SHAFT` facade exposes testing namespaces such as GUI, API, CLI, and DB.

These are related, but they are not one artifact per facade namespace. Most
functionality remains in the required `shaft-engine` JAR.

## Published Maven artifacts

```mermaid
flowchart TB
    Consumer["Consumer project"] --> BOM["shaft-bom<br/>dependency management"]
    Consumer --> Engine["shaft-engine<br/>required runtime"]
    Consumer -.->|optional| PilotCore["shaft-pilot-core<br/>contracts + security"]
    Consumer -.->|optional| Capture["shaft-capture<br/>managed recording + privacy"]
    Consumer -.->|optional| Doctor["shaft-doctor<br/>offline evidence + diagnosis"]
    Consumer -.->|optional| AI["shaft-ai<br/>direct providers"]
    Consumer -.->|optional| BrowserStack["shaft-browserstack<br/>BrowserStack Java SDK"]
    Consumer -.->|optional| Video["shaft-video<br/>local desktop recording"]
    Consumer -.->|optional| Visual["shaft-visual<br/>OpenCV + Eyes + Shutterbug"]
    Agent -.-> MCP["SHAFT_MCP<br/>executable MCP server"]

    BrowserStack --> Engine
    Video --> Engine
    Visual --> Engine
    MCP --> Engine
    MCP --> Capture
    MCP --> Doctor
    MCP --> AI
    PilotCore --> Engine
    Capture --> PilotCore
    Doctor --> PilotCore
    AI --> PilotCore

    Legacy["SHAFT_ENGINE<br/>relocation POM only"] -.->|relocates| Engine

    Engine --> GUI["Web + Appium/Flutter"]
    Engine --> API["REST API"]
    Engine --> Data["DB + CLI + test data"]
    Engine --> Support["validations + reporting<br/>accessibility + properties"]
```

| Artifact             | Packaging      | Consumer purpose                                                                                                         |
|----------------------|----------------|--------------------------------------------------------------------------------------------------------------------------|
| `shaft-engine`       | JAR            | Required facade and core web, mobile, API, database, CLI, reporting, and accessibility implementation.                   |
| `shaft-pilot-core`   | JAR            | Provider-neutral Pilot contracts, consent, redaction, budgets, audit metadata, and deterministic fallback.               |
| `shaft-capture`      | JAR            | Managed Chrome/Edge recording, deterministic privacy classification, versioned schema, and atomic JSON persistence.       |
| `shaft-doctor`       | JAR            | Portable redacted evidence, deterministic diagnosis, optional advisory, and approval-gated isolated repair proposals.       |
| `shaft-ai`           | JAR            | Optional direct OpenAI, Anthropic, Gemini, and Ollama HTTP adapters discovered through `ServiceLoader`.                  |
| `shaft-browserstack` | JAR            | BrowserStack SDK interception and `browserstack.yml` orchestration. Direct BrowserStack sessions stay in `shaft-engine`. |
| `shaft-video`        | JAR            | Local non-headless desktop recording. Appium-native recording stays in `shaft-engine`.                                   |
| `shaft-visual`       | JAR            | Reference-image assertions and image-based lookup through OpenCV, Eyes, and Shutterbug.                                  |
| `SHAFT_MCP`          | executable JAR | Optional MCP server and CLI exposing browser automation, managed capture, Doctor analysis, and packaged direct adapters.  |
| `shaft-bom`          | POM            | Aligns all SHAFT artifact versions; adds no runtime classes.                                                             |
| `SHAFT_ENGINE`       | relocation POM | Temporary legacy-coordinate bridge to `shaft-engine`; adds no optional providers.                                        |

See the [upgrade guide](UPGRADING_TO_MODULAR_SHAFT.md) for method-level
dependency boundaries.

## Facade architecture

```mermaid
graph TB
    SHAFT["SHAFT facade"]

    subgraph "Testing namespaces in shaft-engine"
        GUI["GUI<br/>Web + Mobile"]
        API["API<br/>REST Assured"]
        CLI["CLI<br/>Terminal + Files"]
        DB["DB<br/>JDBC"]
        TestData["TestData<br/>JSON + Excel + CSV + YAML"]
        Validations["Validations<br/>Fluent assertions"]
        Properties["Properties<br/>Configuration"]
        Report["Report<br/>Logs + Attachments"]
    end

    subgraph "GUI components"
        WebDriver["WebDriver lifecycle"]
        Browser["Browser actions"]
        Element["Element actions"]
        Touch["Touch actions"]
        Alert["Alert actions"]
        Locator["Locator builder"]
    end

    subgraph "Test orchestration"
        TestNG["TestNG"]
        JUnit["JUnit"]
        Cucumber["Cucumber"]
        Allure["Allure"]
    end

    SHAFT --> GUI
    SHAFT --> API
    SHAFT --> CLI
    SHAFT --> DB
    SHAFT --> TestData
    SHAFT --> Validations
    SHAFT --> Properties
    SHAFT --> Report

    GUI --> WebDriver
    WebDriver --> Browser
    WebDriver --> Element
    WebDriver --> Touch
    WebDriver --> Alert
    WebDriver --> Locator

    TestNG -.->|uses| SHAFT
    JUnit -.->|uses| SHAFT
    Cucumber -.->|uses| SHAFT
    SHAFT -.->|reports to| Allure
```

## Optional provider discovery

`shaft-visual` and `shaft-video` implement SHAFT-owned provider interfaces and
register them through Java `ServiceLoader`. The facade and orchestration remain
in `shaft-engine`, which means users add a provider JAR without changing test
method calls.

`shaft-browserstack` is a runtime integration rather than a SHAFT provider. It
adds `browserstack-java-sdk`; SHAFT's core BrowserStack driver path also
generates `browserstack.yml`, which the SDK consumes when the optional module is
present.

## Module guides

- [Upgrade and module selection](UPGRADING_TO_MODULAR_SHAFT.md)
- [BrowserStack SDK module](SHAFT_BROWSERSTACK_MODULE.md)
- [Visual processing module](SHAFT_VISUAL_MODULE.md)
- [Desktop video module](SHAFT_VIDEO_MODULE.md)
- [SHAFT Pilot](SHAFT_PILOT.md)
- [SHAFT Pilot release runbook](SHAFT_PILOT_RELEASE.md)
- [SHAFT MCP](SHAFT_MCP.md)
- [SHAFT Pilot AI](SHAFT_PILOT_AI.md)
- [SHAFT Capture](SHAFT_CAPTURE.md)
- [Maven reactor layout](MAVEN_REACTOR.md)

---

[Back to README](../README.md) | [Quick Start](QUICK_START.md)
