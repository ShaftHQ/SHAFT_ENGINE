# SHAFT Project Generator - User Guide

## Table of Contents
1. [Introduction](#introduction)
2. [Features](#features)
3. [Installation](#installation)
4. [Using the Generator](#using-the-generator)
5. [Project Configurations](#project-configurations)
6. [Generated Project Structure](#generated-project-structure)
7. [Sample Tests](#sample-tests)
8. [Execution Environments](#execution-environments)
9. [GitHub Actions Integration](#github-actions-integration)
10. [Troubleshooting](#troubleshooting)

## Introduction

The SHAFT Project Generator is an AI-powered web application designed to quickly generate fully functional test automation projects using the SHAFT Engine framework. It eliminates the need for manual project setup and configuration, allowing you to start writing tests immediately.

## Features

### Supported Test Runners
- **TestNG**: Industry-standard testing framework for Java
- **JUnit 5**: Modern, flexible testing framework
- **Cucumber**: BDD (Behavior-Driven Development) framework with Gherkin syntax

### Supported Target Platforms
- **Web GUI**: Browser-based web application testing
- **API**: RESTful API testing
- **Mobile GUI**: Mobile application testing (iOS and Android)

### Supported Execution Environments
- **Local**: Run tests on your local machine
- **LambdaTest**: Cloud-based cross-browser testing platform
- **BrowserStack**: Cloud-based testing platform with real devices

### Additional Features
- Pre-configured pom.xml with all necessary dependencies
- Ready-to-run sample tests for each platform
- Test data management with JSON files
- Optional GitHub Actions workflows for CI/CD
- Comprehensive README for generated projects
- .gitignore file for version control

## Installation

### Prerequisites
- **Java**: Version 21 or higher
- **Maven**: Version 3.9.x or higher
- **Git**: For version control (optional)

### Steps

1. Navigate to the project-generator directory:
   ```bash
   cd project-generator
   ```

2. Build the application:
   ```bash
   mvn clean install
   ```

3. Run the application:
   ```bash
   mvn spring-boot:run
   ```

4. Access the web interface:
   - Open your browser
   - Navigate to: `http://localhost:8080`

## Using the Generator

### Step 1: Project Information

Fill in the following fields:

- **Group ID**: Your organization's reverse domain name
  - Example: `com.example` or `org.mycompany`
  - Default: `com.example`

- **Artifact ID**: Your project's unique identifier
  - Example: `my-automation-project`
  - Default: `my-automation-project`
  - Note: This will be used as the project folder name

- **Version**: Your project's version number
  - Example: `1.0.0`
  - Default: `1.0.0`

### Step 2: Test Configuration

Select your test configuration:

- **Test Runner**: Choose between TestNG, JUnit, or Cucumber
  - TestNG: Best for complex test suites with flexible configurations
  - JUnit: Modern framework with excellent IDE support
  - Cucumber: Ideal for BDD with business-readable test scenarios

- **Target Platform**: Choose your testing target
  - Web GUI: For web application testing
  - API: For REST API testing
  - Mobile GUI: For mobile app testing

- **Execution Environment**: Choose where to run tests
  - Local: Run on your local machine (requires browser drivers)
  - LambdaTest: Run on LambdaTest cloud platform
  - BrowserStack: Run on BrowserStack cloud platform

### Step 3: GitHub Actions (Optional)

Check the "Include GitHub Actions Workflow" box if you want CI/CD integration.

Select the workflow trigger:
- **Manual (workflow_dispatch)**: Trigger manually from GitHub Actions tab
- **On Push**: Run automatically on every push to main branch
- **On Pull Request**: Run automatically on pull requests
- **Scheduled**: Run daily at midnight UTC

### Step 4: Generate

Click the "Generate Project" button to download your customized project as a ZIP file.

## Project Configurations

### Configuration Matrix

The generator supports the following combinations:

| Test Runner | Target Platform | Execution Environment |
|-------------|----------------|----------------------|
| TestNG      | Web GUI        | Local/LambdaTest/BrowserStack |
| TestNG      | API            | Local/LambdaTest/BrowserStack |
| TestNG      | Mobile GUI     | Local/LambdaTest/BrowserStack |
| JUnit       | Web GUI        | Local/LambdaTest/BrowserStack |
| JUnit       | API            | Local/LambdaTest/BrowserStack |
| JUnit       | Mobile GUI     | Local/LambdaTest/BrowserStack |
| Cucumber    | Web GUI        | Local/LambdaTest/BrowserStack |
| Cucumber    | API            | Local/LambdaTest/BrowserStack |
| Cucumber    | Mobile GUI     | Local/LambdaTest/BrowserStack |

## Generated Project Structure

```
<artifact-id>/
├── .github/
│   └── workflows/
│       └── test.yml                    # GitHub Actions workflow (optional)
├── src/
│   ├── main/
│   │   └── resources/
│   │       └── properties/
│   │           ├── lambdatest.properties    # LambdaTest config (if selected)
│   │           └── browserstack.properties  # BrowserStack config (if selected)
│   └── test/
│       ├── java/
│       │   └── <package>/
│       │       ├── SampleWebTests.java      # Web GUI tests
│       │       ├── SampleAPITests.java      # API tests
│       │       ├── SampleMobileTests.java   # Mobile tests
│       │       ├── SampleWebSteps.java      # Cucumber step definitions
│       │       └── TestRunner.java          # Cucumber test runner
│       └── resources/
│           ├── features/
│           │   └── SampleWebTests.feature   # Cucumber feature files
│           └── testDataFiles/
│               └── testData.json            # Test data
├── pom.xml                                  # Maven configuration
├── .gitignore                               # Git ignore rules
└── README.md                                # Project documentation
```

## Sample Tests

### Web GUI Tests

#### Flow 1: SHAFT Engine User Guide
- Navigate to the SHAFT Engine user guide at https://shafthq.github.io/
- Click the "Upgrade Now" button
- Verify navigation to the SHAFT Engine GitHub page
- Assert the current URL matches the expected URL

#### Flow 2: DuckDuckGo Search
- Navigate to DuckDuckGo at https://duckduckgo.com/
- Search for "SHAFT_Engine"
- Verify the first search result contains "SHAFT_Engine"

### API Tests

Using JSONPlaceholder API (https://jsonplaceholder.typicode.com):

#### Test 1: Get Single User
- GET request to `/users/1`
- Verify status code 200
- Verify response contains "Leanne Graham"
- Verify user ID equals 1

#### Test 2: Get All Posts
- GET request to `/posts`
- Verify status code 200
- Verify response contains 100 posts

#### Test 3: Create New Post
- POST request to `/posts` with JSON body
- Verify status code 201
- Verify created post title matches request

### Mobile GUI Tests

Sample structure for mobile app testing (requires app configuration):

#### Test 1: App Navigation
- Click login button
- Verify username field is displayed

#### Test 2: Login Action
- Type username
- Type password
- Click submit button
- Verify welcome message is displayed

## Execution Environments

### Local Execution

No additional configuration required. Tests will run on your local machine using Chrome in headless mode by default.

**Running tests:**
```bash
mvn clean test
```

### LambdaTest Execution

#### Setup Steps:

1. **Get Credentials**:
   - Sign up at https://www.lambdatest.com/
   - Get your username and access key from https://accounts.lambdatest.com/detail/profile

2. **Configure Credentials**:
   - Open `src/main/resources/properties/lambdatest.properties`
   - Replace `YOUR_LAMBDATEST_USERNAME` with your username
   - Replace `YOUR_LAMBDATEST_ACCESS_KEY` with your access key

3. **Run Tests**:
   ```bash
   mvn clean test -DexecutionAddress=lambdatest
   ```

### BrowserStack Execution

#### Setup Steps:

1. **Get Credentials**:
   - Sign up at https://www.browserstack.com/
   - Get your username and access key from https://www.browserstack.com/accounts/settings

2. **Configure Credentials**:
   - Open `src/main/resources/properties/browserstack.properties`
   - Replace `YOUR_BROWSERSTACK_USERNAME` with your username
   - Replace `YOUR_BROWSERSTACK_ACCESS_KEY` with your access key

3. **Run Tests**:
   ```bash
   mvn clean test -DexecutionAddress=browserstack
   ```

## GitHub Actions Integration

If you selected to include GitHub Actions workflow, your project will have a pre-configured workflow file at `.github/workflows/test.yml`.

### Setting Up GitHub Secrets

For LambdaTest or BrowserStack execution in GitHub Actions, you need to add your credentials as GitHub Secrets:

1. Go to your GitHub repository
2. Navigate to Settings > Secrets and variables > Actions
3. Click "New repository secret"
4. Add the following secrets:

**For LambdaTest:**
- Name: `LAMBDATEST_USERNAME`, Value: Your LambdaTest username
- Name: `LAMBDATEST_ACCESS_KEY`, Value: Your LambdaTest access key

**For BrowserStack:**
- Name: `BROWSERSTACK_USERNAME`, Value: Your BrowserStack username
- Name: `BROWSERSTACK_ACCESS_KEY`, Value: Your BrowserStack access key

### Workflow Triggers

- **workflow_dispatch**: Manually trigger from Actions tab
- **push**: Automatically runs on push to main branch
- **pull_request**: Automatically runs on pull requests to main branch
- **schedule**: Runs daily at midnight UTC

### Viewing Results

1. Go to the "Actions" tab in your GitHub repository
2. Click on the workflow run
3. View test results and download Allure reports

## Troubleshooting

### Common Issues

#### Issue: Application fails to start
**Solution**: 
- Verify Java 21 is installed: `java -version`
- Verify Maven is installed: `mvn -version`
- Check if port 8080 is available

#### Issue: Generated project doesn't compile
**Solution**:
- Run `mvn clean install` in the generated project directory
- Verify internet connection for Maven dependency download

#### Issue: Tests fail to run locally
**Solution**:
- For Web GUI tests: Ensure Chrome browser is installed
- For API tests: Verify internet connection
- For Mobile tests: Configure Appium or cloud service properly

#### Issue: Cannot download generated project
**Solution**:
- Check browser's download settings
- Ensure pop-ups are not blocked
- Try a different browser

#### Issue: LambdaTest/BrowserStack tests fail
**Solution**:
- Verify credentials are correct
- Check account has active subscription
- Ensure proper network connectivity

### Getting Help

If you encounter issues not covered here:

1. Check the [SHAFT Engine Documentation](https://shafthq.github.io/)
2. Visit the [SHAFT Engine GitHub](https://github.com/ShaftHQ/SHAFT_ENGINE)
3. Create an issue with details about your problem
4. Join the SHAFT Engine community for support

## Best Practices

### Project Configuration
- Use descriptive artifact IDs that reflect your project's purpose
- Follow semantic versioning (e.g., 1.0.0, 1.1.0, 2.0.0)
- Use lowercase with hyphens for artifact IDs

### Test Organization
- Keep test data in JSON files for easy maintenance
- Use page object model for Web GUI tests
- Organize tests logically by feature or module

### Version Control
- Initialize Git repository after extracting the project
- Use meaningful commit messages
- Create branches for new features
- Use pull requests for code reviews

### Continuous Integration
- Set up GitHub Actions for automated testing
- Run tests on every pull request
- Monitor test results regularly
- Fix failing tests promptly

## Conclusion

The SHAFT Project Generator simplifies the process of creating test automation projects with SHAFT Engine. By following this guide, you should be able to generate and run your test automation projects efficiently.

For more information about SHAFT Engine capabilities and advanced features, visit the [official documentation](https://shafthq.github.io/).
