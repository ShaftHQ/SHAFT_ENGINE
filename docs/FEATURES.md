# 🚀 Features

The required `shaft-engine` artifact provides the public facade and core test
automation capabilities. Three dependency-heavy integrations are optional.

## Feature-to-module map

| Feature                                                                                          | Maven artifact       |
|--------------------------------------------------------------------------------------------------|----------------------|
| Web, mobile/Appium/Flutter, API, database, CLI, test data, accessibility, reporting, screenshots | `shaft-engine`       |
| Direct BrowserStack WebDriver/Appium sessions and app upload                                     | `shaft-engine`       |
| BrowserStack SDK interception, multi-platform YAML, and SDK orchestration                        | `shaft-browserstack` |
| Appium Android/iOS driver-native recording                                                       | `shaft-engine`       |
| Local non-headless desktop recording                                                             | `shaft-video`        |
| Reference-image assertions and image-path touch actions                                          | `shaft-visual`       |
| Deterministic explainable web element recovery                                                    | `shaft-heal`         |
| Screenshot highlighting, animated GIFs, and `compareImageFolders(...)`                           | `shaft-engine`       |

See the [upgrade guide](UPGRADING_TO_MODULAR_SHAFT.md) for the exact method
boundaries.

## Smart Features

SHAFT provides a lot of out-of-the-box convenience features to facilitate your testing process and eliminate the need for boilerplate code.

All of SHAFT's smart features target the three pillars of successful test automation:
- **Scalability**: The ability to run tests on multiple devices and browsers in parallel.
- **Reliability**: The ability to run tests without flakiness and with detailed reporting.
- **Maintainability**: The ability to easily maintain and update tests as the application changes.

### Scalability

| CI/CD integration |Cloud device farm integration  |Headless testing  |Parallel execution  |Containerized execution  |
| :---: |:---: |:---: |:---: |:---: |
| :white_check_mark: |:white_check_mark: |:white_check_mark: |:white_check_mark: |:white_check_mark: |

### Reliability

| Automated synchronization |      Logging       |     Reporting      | Screenshots/Attachments |                     Video recording                     |
|:-------------------------:|:------------------:|:------------------:|:-----------------------:|:-------------------------------------------------------:|
|    :white_check_mark:     | :white_check_mark: | :white_check_mark: |   :white_check_mark:    | Core for Appium/remote; `shaft-video` for local desktop |

### Maintainability

|   Fluent design    |  Locator builder   |   Smart locators   | Native `WebDriver` access | Element/Browser validations builder | Visual validations |
|:------------------:|:------------------:|:------------------:|:-------------------------:|:-----------------------------------:|:------------------:|
| :white_check_mark: | :white_check_mark: | :white_check_mark: |    :white_check_mark:     |         :white_check_mark:          |   `shaft-visual`   |

## Supported Platforms

### Browsers

|          | Linux | macOS | Windows | Android | iOS |
|   :---   | :---: | :---: | :---:   | :---: | :---:   |
| Google Chrome  | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: |
| Microsoft Edge  | :white_check_mark: | :white_check_mark: | :white_check_mark: |_ | _ |
| Mozilla Firefox  | :white_check_mark: | :white_check_mark: | :white_check_mark: |_ | _ |
| Apple Safari  | _ | :white_check_mark: | _ | _ | :white_check_mark: |

### Apps

|          | Android | iOS | Windows | 
|   :---   | :---: | :---: | :---:   |
| Native  |:white_check_mark: | :white_check_mark: | N/A | 
| Hybrid  | :white_check_mark: | :white_check_mark: | N/A | 
| Flutter | :white_check_mark: | :white_check_mark: | N/A | 
| WPF  |  N/A | N/A |:white_check_mark: |

### Other

| API | Database | CLI | PDF | JSON | YAML | Excel | Property |
| :---: | :---: | :---:|:---:|:---:|:---:|:---:|:---:|
| :white_check_mark: |:white_check_mark: | :white_check_mark: |:white_check_mark: |:white_check_mark: |:white_check_mark: |:white_check_mark: |:white_check_mark: |

### Test Orchestration

| TestNG | JUnit5 | Cucumber |
| :---: |:---: |:---: |
| :white_check_mark: |:white_check_mark: |:white_check_mark: |

---

[← Back to README](../README.md) | [Tech Stack →](TECH_STACK.md)
