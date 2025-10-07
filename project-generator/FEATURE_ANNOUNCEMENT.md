# 🎉 New Feature: SHAFT Project Generator

## Overview

We're excited to introduce the **SHAFT Project Generator** - an AI-powered web application that instantly creates fully functional test automation projects using the SHAFT Engine framework!

## What is it?

The SHAFT Project Generator is a revolutionary tool that eliminates the tedious manual setup of test automation projects. With just a few clicks in a web interface, you can generate a complete, production-ready Maven project tailored to your specific needs.

## Key Features

✨ **Multiple Test Runners**: TestNG, JUnit 5, and Cucumber
🎪 **Multiple Target Platforms**: Web GUI, API, and Mobile GUI
🌐 **Multiple Execution Environments**: Local, LambdaTest, and BrowserStack
🚀 **Ready-to-Run Sample Tests**: Includes working examples for each platform
⚙️ **GitHub Actions Integration**: Optional CI/CD workflows
📦 **Complete Project Structure**: All necessary files and configurations included

## Quick Start

### Option 1: Run Locally

```bash
# Navigate to the generator directory
cd project-generator

# Build and run
mvn spring-boot:run

# Access at http://localhost:8080
```

### Option 2: Use Docker

```bash
cd project-generator

# Using docker-compose
docker-compose up -d

# Access at http://localhost:8080
```

## How It Works

1. **Configure Your Project**
   - Enter project details (Group ID, Artifact ID, Version)
   - Select test runner (TestNG/JUnit/Cucumber)
   - Choose target platform (Web/API/Mobile)
   - Pick execution environment (Local/LambdaTest/BrowserStack)

2. **Generate**
   - Click "Generate Project" button
   - Download your customized project as a ZIP file

3. **Start Testing**
   ```bash
   unzip your-project.zip
   cd your-project
   mvn clean test
   ```

## What You Get

Your generated project includes:

```
your-project/
├── src/
│   ├── main/resources/properties/    # Configuration files
│   └── test/
│       ├── java/                     # Test source code
│       │   └── SampleTests.java      # Ready-to-run tests
│       └── resources/
│           ├── features/             # Cucumber features (if selected)
│           └── testDataFiles/
│               └── testData.json
├── .github/workflows/                # CI/CD workflows (optional)
├── pom.xml                           # Maven configuration
├── .gitignore
└── README.md                         # Project documentation
```

## Sample Tests Included

### Web GUI Tests (TestNG/JUnit/Cucumber)

1. **SHAFT Engine User Guide Test**
   - Navigate to SHAFT documentation
   - Click "Upgrade Now" button
   - Verify navigation to GitHub

2. **DuckDuckGo Search Test**
   - Search for "SHAFT_Engine"
   - Verify first result contains "SHAFT_Engine"

### API Tests (TestNG/JUnit)

Using JSONPlaceholder API:
- Get single user and validate response
- Get all posts and validate count
- Create new post via POST request

### Mobile Tests (TestNG/JUnit)

Sample mobile test structure:
- Navigate and verify element display
- Perform login action

## Configuration Matrix

| Test Runner | Target Platform | Execution Environment | ✓ |
|------------|----------------|----------------------|---|
| TestNG     | Web GUI        | Local                | ✓ |
| TestNG     | Web GUI        | LambdaTest           | ✓ |
| TestNG     | Web GUI        | BrowserStack         | ✓ |
| TestNG     | API            | Local                | ✓ |
| TestNG     | Mobile GUI     | BrowserStack         | ✓ |
| JUnit      | Web GUI        | Local                | ✓ |
| JUnit      | API            | Local                | ✓ |
| Cucumber   | Web GUI        | Local                | ✓ |
| Cucumber   | Web GUI        | LambdaTest           | ✓ |

**Total: 27 unique project configurations!**

## Documentation

Comprehensive documentation is available:

- 📘 [README.md](project-generator/README.md) - Quick overview
- 📖 [USER_GUIDE.md](project-generator/USER_GUIDE.md) - Detailed usage instructions
- 🚢 [DEPLOYMENT.md](project-generator/DEPLOYMENT.md) - Deployment options
- 🔗 [INTEGRATION.md](project-generator/INTEGRATION.md) - Integration with SHAFT Engine
- 📚 [EXAMPLES.md](project-generator/EXAMPLES.md) - Practical usage examples

## Benefits

### ⏱️ Time Savings
Generate a complete project in **30 seconds** instead of 30-60 minutes of manual setup

### ✅ Consistency
All projects follow SHAFT Engine best practices and conventions

### 🛡️ Error Reduction
Pre-validated configurations eliminate common setup mistakes

### 🎯 Flexibility
Support for multiple test runners, platforms, and execution environments

### 🚀 CI/CD Ready
Optional GitHub Actions workflows for immediate automation

## Technology Stack

- **Backend**: Spring Boot 3.4.1
- **Frontend**: HTML5, CSS3, JavaScript
- **Build Tool**: Maven 3.9.x
- **Java Version**: 21 (latest LTS)
- **Containerization**: Docker & Docker Compose

## Use Cases

### 1. Quick Prototyping
Rapidly create projects to test ideas or proof-of-concepts

### 2. Team Onboarding
New team members get a working project in minutes

### 3. Training & Learning
Perfect for teaching SHAFT Engine and test automation

### 4. Multiple Projects
Easily create separate projects for different testing needs

### 5. Migration Reference
Generate reference projects when migrating to SHAFT Engine

## Screenshots

### Web Interface
Beautiful, intuitive interface for project configuration:
- Clean, modern design
- Clear step-by-step workflow
- Instant validation
- Download ready in seconds

### Generated Project
Complete, professional project structure:
- Clean code following best practices
- Comprehensive README
- Working tests
- CI/CD ready

## Deployment Options

1. **Local Server**: Run on development machine or internal server
2. **Docker**: Containerized deployment for consistency
3. **Cloud Platforms**: Deploy to Heroku, AWS, Azure, or GCP
4. **Kubernetes**: Scale with container orchestration

## Future Enhancements

We're continuously improving the generator. Planned features:

- 🎨 Custom templates management
- 🤖 AI-powered test generation
- 🌍 Multi-language support
- 📊 Performance testing templates
- 🗄️ Database testing templates
- 🔌 More integration options

## Contributing

Contributions are welcome! You can help by:

- 🐛 Reporting bugs
- 💡 Suggesting features
- 📝 Improving documentation
- 🎨 Creating custom templates
- 🧪 Testing different configurations

## Support

- 📚 [SHAFT Engine Documentation](https://shafthq.github.io/)
- 💬 [GitHub Discussions](https://github.com/ShaftHQ/SHAFT_ENGINE/discussions)
- 🐛 [Report Issues](https://github.com/ShaftHQ/SHAFT_ENGINE/issues)

## Getting Started

Ready to try it? Follow these steps:

1. **Clone the repository**
   ```bash
   git clone https://github.com/ShaftHQ/SHAFT_ENGINE.git
   cd SHAFT_ENGINE/project-generator
   ```

2. **Run the generator**
   ```bash
   mvn spring-boot:run
   ```

3. **Access the web interface**
   ```
   http://localhost:8080
   ```

4. **Generate your first project!**

## License

The SHAFT Project Generator is part of SHAFT Engine and is licensed under the MIT License.

---

**Made with ❤️ by the SHAFT Engine Team**

**Stop reinventing the wheel. Start generating projects! 🚀**
