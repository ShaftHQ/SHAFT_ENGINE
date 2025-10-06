# SHAFT Engine Project Generator

> **AI-Powered Test Automation Project Generator**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Java Version](https://img.shields.io/badge/Java-21-orange.svg)](https://www.oracle.com/java/)
[![Spring Boot](https://img.shields.io/badge/Spring%20Boot-3.4.1-brightgreen.svg)](https://spring.io/projects/spring-boot)

## 🌟 Overview

The SHAFT Project Generator is a revolutionary web-based tool that instantly creates fully functional test automation projects using the SHAFT Engine framework. Say goodbye to manual project setup and configuration - generate production-ready test automation projects in seconds!

## ✨ Features

### 🎯 Multiple Test Runners
- **TestNG** - Industry-standard testing framework
- **JUnit 5** - Modern, flexible testing framework
- **Cucumber** - BDD framework with Gherkin syntax

### 🎪 Multiple Target Platforms
- **Web GUI** - Browser-based web application testing
- **API** - RESTful API testing
- **Mobile GUI** - Mobile application testing (iOS/Android)

### 🌐 Multiple Execution Environments
- **Local** - Run tests on your local machine
- **LambdaTest** - Cloud-based cross-browser testing
- **BrowserStack** - Cloud-based testing with real devices

### 🚀 Additional Features
- ✅ Pre-configured Maven `pom.xml` with all dependencies
- ✅ Ready-to-run sample tests for each platform
- ✅ Test data management with JSON files
- ✅ Optional GitHub Actions workflows for CI/CD
- ✅ Comprehensive README for generated projects
- ✅ Pre-configured `.gitignore` file

## 🏗️ Architecture

```
project-generator/
├── src/
│   ├── main/
│   │   ├── java/                     # Spring Boot application
│   │   │   └── io.github.shafthq.generator/
│   │   │       ├── controller/       # REST controllers
│   │   │       ├── service/          # Business logic
│   │   │       └── model/            # Data models
│   │   └── resources/
│   │       ├── static/               # CSS & JavaScript
│   │       ├── templates/            # HTML templates (Thymeleaf)
│   │       └── application.properties
│   └── test/                         # Unit tests (future)
├── templates/                        # Project templates
│   ├── pom/                         # Maven POM templates
│   ├── tests/                       # Test file templates
│   ├── workflows/                   # GitHub Actions templates
│   └── properties/                  # Configuration templates
├── Dockerfile                        # Docker configuration
├── docker-compose.yml                # Docker Compose configuration
├── pom.xml                          # Maven configuration
└── README.md                        # This file
```

## 🚀 Quick Start

### Prerequisites

- **Java 21** or higher
- **Maven 3.9.x** or higher

### Installation

1. **Clone the repository**:
   ```bash
   git clone https://github.com/ShaftHQ/SHAFT_ENGINE.git
   cd SHAFT_ENGINE/project-generator
   ```

2. **Build the application**:
   ```bash
   mvn clean install
   ```

3. **Run the application**:
   ```bash
   mvn spring-boot:run
   ```

4. **Access the web interface**:
   Open your browser and navigate to `http://localhost:8080`

### Using Docker

1. **Build Docker image**:
   ```bash
   docker build -t shaft-generator:latest .
   ```

2. **Run container**:
   ```bash
   docker run -p 8080:8080 shaft-generator:latest
   ```

   Or use Docker Compose:
   ```bash
   docker-compose up -d
   ```

## 📖 Usage Guide

### Step-by-Step Instructions

1. **Project Information**
   - Enter your Group ID (e.g., `com.example`)
   - Enter your Artifact ID (e.g., `my-automation-project`)
   - Specify the version (e.g., `1.0.0`)

2. **Test Configuration**
   - Select your test runner (TestNG, JUnit, or Cucumber)
   - Choose your target platform (Web GUI, API, or Mobile GUI)
   - Select execution environment (Local, LambdaTest, or BrowserStack)

3. **GitHub Actions (Optional)**
   - Check the box to include a GitHub Actions workflow
   - Select the workflow trigger type

4. **Generate**
   - Click "Generate Project" to download your customized project

For detailed usage instructions, see [USER_GUIDE.md](USER_GUIDE.md)

## 🎨 Generated Project Structure

```
my-automation-project/
├── .github/
│   └── workflows/
│       └── test.yml                    # CI/CD workflow
├── src/
│   ├── main/
│   │   └── resources/
│   │       └── properties/             # Environment configs
│   └── test/
│       ├── java/                       # Test source code
│       │   └── com.example/
│       │       └── SampleTests.java
│       └── resources/
│           ├── features/               # Cucumber features
│           └── testDataFiles/
│               └── testData.json
├── pom.xml
├── .gitignore
└── README.md
```

## 🧪 Sample Tests

### Web GUI Tests

Generated projects include two ready-to-run web tests:

1. **SHAFT Engine User Guide Test**
   - Navigate to SHAFT Engine documentation
   - Click "Upgrade Now" button
   - Verify navigation to GitHub repository

2. **DuckDuckGo Search Test**
   - Navigate to DuckDuckGo
   - Search for "SHAFT_Engine"
   - Verify first result contains "SHAFT_Engine"

### API Tests

Sample API tests using JSONPlaceholder:

1. Get single user and validate response
2. Get all posts and validate count
3. Create new post via POST request

### Mobile Tests

Sample mobile test structure (requires app configuration):

1. Navigate and verify element display
2. Perform login action

## 🔧 Configuration

### LambdaTest Configuration

Update credentials in the generated project:
```properties
# src/main/resources/properties/lambdatest.properties
lambdatest.username=YOUR_USERNAME
lambdatest.accessKey=YOUR_ACCESS_KEY
```

### BrowserStack Configuration

Update credentials in the generated project:
```properties
# src/main/resources/properties/browserstack.properties
browserstack.username=YOUR_USERNAME
browserstack.accessKey=YOUR_ACCESS_KEY
```

### GitHub Secrets

For CI/CD with cloud platforms, add these secrets to your GitHub repository:

**LambdaTest:**
- `LAMBDATEST_USERNAME`
- `LAMBDATEST_ACCESS_KEY`

**BrowserStack:**
- `BROWSERSTACK_USERNAME`
- `BROWSERSTACK_ACCESS_KEY`

## 🚢 Deployment

The generator can be deployed in various ways:

- **Local Server**: Run the JAR file
- **Docker**: Use provided Dockerfile
- **Cloud Platforms**: Deploy to Heroku, AWS, Azure, or GCP
- **Kubernetes**: Use Docker image with K8s

For detailed deployment instructions, see [DEPLOYMENT.md](DEPLOYMENT.md)

## 📚 Documentation

- [User Guide](USER_GUIDE.md) - Comprehensive guide for using the generator
- [Deployment Guide](DEPLOYMENT.md) - Instructions for deploying the generator
- [SHAFT Engine Docs](https://shafthq.github.io/) - Official SHAFT Engine documentation

## 🛠️ Development

### Building from Source

```bash
# Clone repository
git clone https://github.com/ShaftHQ/SHAFT_ENGINE.git

# Navigate to generator directory
cd SHAFT_ENGINE/project-generator

# Build project
mvn clean install

# Run tests (when available)
mvn test

# Run application
mvn spring-boot:run
```

### Project Structure

- **Backend**: Spring Boot 3.4.1 with Java 21
- **Frontend**: HTML, CSS, JavaScript (Vanilla JS)
- **Build Tool**: Maven
- **Templates**: Plain text templates with variable substitution

### Adding New Templates

1. Create template file in `templates/` directory
2. Add variables using `{{VARIABLE_NAME}}` syntax
3. Update `ProjectGeneratorService.java` to process the template
4. Test the generation with different configurations

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

### How to Contribute

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](../LICENSE) file for details.

## 🙏 Acknowledgments

- Built with [SHAFT Engine](https://github.com/ShaftHQ/SHAFT_ENGINE)
- Powered by [Spring Boot](https://spring.io/projects/spring-boot)
- Inspired by the need for rapid test automation project setup

## 📧 Support

- **Documentation**: [SHAFT Engine Docs](https://shafthq.github.io/)
- **Issues**: [GitHub Issues](https://github.com/ShaftHQ/SHAFT_ENGINE/issues)
- **Discussions**: [GitHub Discussions](https://github.com/ShaftHQ/SHAFT_ENGINE/discussions)

## 🎯 Roadmap

Future enhancements planned:

- [ ] Support for additional test runners (Spock, Gauge)
- [ ] Desktop application testing support
- [ ] Database testing templates
- [ ] Performance testing templates
- [ ] Integration with test management tools
- [ ] AI-powered test generation
- [ ] Custom template management UI
- [ ] Project configuration import/export
- [ ] Multi-language support

## 📊 Statistics

- **Test Runners**: 3 (TestNG, JUnit, Cucumber)
- **Target Platforms**: 3 (Web, API, Mobile)
- **Execution Environments**: 3 (Local, LambdaTest, BrowserStack)
- **Total Configurations**: 27 unique combinations
- **Template Files**: 15+
- **Lines of Code**: 2000+

---

**Made with ❤️ by the SHAFT Engine Team**

[⬆ Back to top](#shaft-engine-project-generator)
