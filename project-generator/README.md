# SHAFT Project Generator

AI-Powered Test Automation Project Generator for SHAFT Engine

## Overview

The SHAFT Project Generator is a web-based tool that allows you to quickly generate fully functional test automation projects using the SHAFT Engine framework. With just a few clicks, you can create a complete Maven project tailored to your specific needs.

## Features

- **Multiple Test Runners**: Support for TestNG, JUnit 5, and Cucumber
- **Multiple Target Platforms**: Web GUI, API, and Mobile GUI testing
- **Multiple Execution Environments**: Local, LambdaTest, and BrowserStack
- **GitHub Actions Integration**: Optional GitHub Actions workflows for CI/CD
- **Ready-to-Run Samples**: Pre-configured sample tests for each platform
- **Latest Technologies**: Uses Java 21 and the latest stable SHAFT Engine version

## Getting Started

### Prerequisites

- Java 21 or higher
- Maven 3.9.x or higher

### Running the Generator

1. Navigate to the project-generator directory:
   ```bash
   cd project-generator
   ```

2. Build the project:
   ```bash
   mvn clean install
   ```

3. Run the application:
   ```bash
   mvn spring-boot:run
   ```

4. Open your browser and navigate to:
   ```
   http://localhost:8080
   ```

### Using the Generator

1. **Enter Project Information**:
   - Group ID (e.g., com.example)
   - Artifact ID (e.g., my-automation-project)
   - Version (e.g., 1.0.0)

2. **Configure Test Settings**:
   - Select your test runner (TestNG, JUnit, or Cucumber)
   - Choose your target platform (Web GUI, API, or Mobile GUI)
   - Select execution environment (Local, LambdaTest, or BrowserStack)

3. **Optional GitHub Actions**:
   - Check the box to include a GitHub Actions workflow
   - Select the workflow trigger type

4. **Generate**:
   - Click "Generate Project" to download your customized project as a ZIP file

## Generated Project Structure

The generator creates a complete Maven project with the following structure:

```
<artifact-id>/
├── .github/
│   └── workflows/
│       └── test.yml              # GitHub Actions workflow (if selected)
├── src/
│   ├── main/
│   │   └── resources/
│   │       └── properties/       # Configuration properties (if using LambdaTest/BrowserStack)
│   └── test/
│       ├── java/
│       │   └── <package>/
│       │       └── Sample*Tests.java  # Sample test files
│       └── resources/
│           ├── features/         # Cucumber feature files (if Cucumber selected)
│           └── testDataFiles/
│               └── testData.json # Test data
├── pom.xml                       # Maven configuration
├── .gitignore
└── README.md
```

## Sample Tests

### Web GUI Tests (TestNG/JUnit)

The generated project includes two sample web tests:

1. **SHAFT Engine User Guide Test**: Navigate to the SHAFT Engine user guide, click the "Upgrade Now" button, and verify the URL
2. **DuckDuckGo Search Test**: Search for "SHAFT_Engine" on DuckDuckGo and verify the first result

### Web GUI Tests (Cucumber)

Feature file with scenarios for:
- Navigating to SHAFT Engine user guide and clicking Upgrade Now
- Searching for SHAFT_Engine on DuckDuckGo

### API Tests

Sample API tests using JSONPlaceholder API:
- Get single user and validate response
- Get all posts and validate count
- Create new post via POST request

### Mobile GUI Tests

Sample mobile test structure (requires app configuration):
- Navigate and verify element is displayed
- Perform login action

## Configuration

### LambdaTest Configuration

If you selected LambdaTest as the execution environment, update the credentials in:
```
src/main/resources/properties/lambdatest.properties
```

Replace:
- `YOUR_LAMBDATEST_USERNAME`
- `YOUR_LAMBDATEST_ACCESS_KEY`

### BrowserStack Configuration

If you selected BrowserStack as the execution environment, update the credentials in:
```
src/main/resources/properties/browserstack.properties
```

Replace:
- `YOUR_BROWSERSTACK_USERNAME`
- `YOUR_BROWSERSTACK_ACCESS_KEY`

### GitHub Secrets

For GitHub Actions workflows with LambdaTest or BrowserStack, add the following secrets to your repository:

**For LambdaTest:**
- `LAMBDATEST_USERNAME`
- `LAMBDATEST_ACCESS_KEY`

**For BrowserStack:**
- `BROWSERSTACK_USERNAME`
- `BROWSERSTACK_ACCESS_KEY`

## Template Structure

The generator uses templates stored in the `templates/` directory:

- `templates/pom/`: POM file templates for different test runners
- `templates/tests/`: Sample test file templates
- `templates/workflows/`: GitHub Actions workflow templates
- `templates/properties/`: Properties file templates

## Technology Stack

- **Backend**: Spring Boot 3.4.1
- **Frontend**: HTML, CSS, JavaScript (Vanilla)
- **Build Tool**: Maven
- **Java Version**: 21

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Support

For issues or questions:
- Visit the [SHAFT Engine Documentation](https://shafthq.github.io/)
- Create an issue on the [SHAFT Engine GitHub](https://github.com/ShaftHQ/SHAFT_ENGINE/issues)

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Built with [SHAFT Engine](https://github.com/ShaftHQ/SHAFT_ENGINE)
- Powered by Spring Boot
