# SHAFT Project Generator - Usage Examples

This document provides practical examples of using the SHAFT Project Generator for different scenarios.

## Example 1: Simple Web Automation Project with TestNG

### Scenario
You want to create a basic web automation project using TestNG for local execution.

### Steps

1. **Access the Generator**:
   - Open browser: `http://localhost:8080`

2. **Configure Project**:
   - Group ID: `com.mycompany`
   - Artifact ID: `web-automation-tests`
   - Version: `1.0.0`
   - Test Runner: `TestNG`
   - Target Platform: `Web GUI`
   - Execution Environment: `Local`
   - GitHub Workflow: ✓ (checked)
   - Workflow Trigger: `workflow_dispatch`

3. **Generate**:
   - Click "Generate Project"
   - Download `web-automation-tests.zip`

4. **Setup**:
   ```bash
   unzip web-automation-tests.zip
   cd web-automation-tests
   mvn clean test
   ```

### Result
- Project structure created
- Sample web tests ready to run
- GitHub Actions workflow included
- Tests can be executed locally

---

## Example 2: API Testing with JUnit

### Scenario
You need an API testing project using JUnit 5 for cloud execution on LambdaTest.

### Steps

1. **Configure Project**:
   - Group ID: `com.mycompany`
   - Artifact ID: `api-automation-tests`
   - Version: `1.0.0`
   - Test Runner: `JUnit`
   - Target Platform: `API`
   - Execution Environment: `LambdaTest`
   - GitHub Workflow: ✓ (checked)
   - Workflow Trigger: `push`

2. **Generate and Setup**:
   ```bash
   unzip api-automation-tests.zip
   cd api-automation-tests
   ```

3. **Configure LambdaTest Credentials**:
   Edit `src/main/resources/properties/lambdatest.properties`:
   ```properties
   lambdatest.username=your_username
   lambdatest.accessKey=your_access_key
   ```

4. **Run Tests**:
   ```bash
   mvn clean test -DexecutionAddress=lambdatest
   ```

### Result
- API test project with JUnit
- LambdaTest configuration included
- GitHub Actions configured for auto-run on push
- Sample API tests using JSONPlaceholder

---

## Example 3: BDD with Cucumber for Web Testing

### Scenario
Create a BDD project using Cucumber for web testing on BrowserStack.

### Steps

1. **Configure Project**:
   - Group ID: `org.example`
   - Artifact ID: `bdd-web-tests`
   - Version: `2.0.0`
   - Test Runner: `Cucumber`
   - Target Platform: `Web GUI`
   - Execution Environment: `BrowserStack`
   - GitHub Workflow: ✓ (checked)
   - Workflow Trigger: `pull_request`

2. **Generate and Setup**:
   ```bash
   unzip bdd-web-tests.zip
   cd bdd-web-tests
   ```

3. **Configure BrowserStack**:
   Edit `src/main/resources/properties/browserstack.properties`:
   ```properties
   browserstack.username=your_username
   browserstack.accessKey=your_access_key
   ```

4. **Run Tests**:
   ```bash
   mvn clean test -DexecutionAddress=browserstack
   ```

### Project Structure
```
bdd-web-tests/
├── src/
│   ├── test/
│   │   ├── java/
│   │   │   └── org/example/
│   │   │       ├── SampleWebSteps.java
│   │   │       └── TestRunner.java
│   │   └── resources/
│   │       ├── features/
│   │       │   └── SampleWebTests.feature
│   │       └── testDataFiles/
│   │           └── testData.json
├── .github/
│   └── workflows/
│       └── test.yml
└── pom.xml
```

### Result
- Cucumber BDD project
- Feature files with Gherkin syntax
- Step definitions implemented
- BrowserStack configuration
- PR-triggered GitHub Actions

---

## Example 4: Mobile App Testing

### Scenario
Create a mobile automation project for testing Android/iOS apps.

### Steps

1. **Configure Project**:
   - Group ID: `com.mobile`
   - Artifact ID: `mobile-app-tests`
   - Version: `1.0.0`
   - Test Runner: `TestNG`
   - Target Platform: `Mobile GUI`
   - Execution Environment: `BrowserStack`
   - GitHub Workflow: ✓ (checked)
   - Workflow Trigger: `schedule`

2. **Additional Configuration Needed**:

   After generation, update the test class with your app details:

   ```java
   @BeforeClass
   public void beforeClass() {
       SHAFT.Properties.platform.set().executionAddress("browserstack");
       SHAFT.Properties.browserStack.set().platformVersion("13.0");
       SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
       SHAFT.Properties.browserStack.set().appName("YourApp.apk");
       // Upload your app to BrowserStack and use app URL
       SHAFT.Properties.browserStack.set().appUrl("bs://your-app-id");
   }
   ```

3. **Update Properties**:
   Edit `src/main/resources/properties/browserstack.properties` with your app details.

### Result
- Mobile test project structure
- Appium integration via SHAFT
- BrowserStack mobile configuration
- Scheduled daily execution via GitHub Actions

---

## Example 5: Multi-Configuration Testing

### Scenario
You need multiple projects for different testing needs.

### Strategy
Generate separate projects for each need:

1. **Web Tests (Local Development)**:
   - Test Runner: TestNG
   - Platform: Web GUI
   - Environment: Local
   - Use: Development and debugging

2. **Web Tests (CI/CD)**:
   - Test Runner: TestNG
   - Platform: Web GUI
   - Environment: LambdaTest
   - Use: Automated testing in pipeline

3. **API Tests (Integration)**:
   - Test Runner: JUnit
   - Platform: API
   - Environment: Local
   - Use: Integration and contract testing

4. **E2E Tests (Acceptance)**:
   - Test Runner: Cucumber
   - Platform: Web GUI
   - Environment: BrowserStack
   - Use: End-to-end acceptance testing

### Benefits
- Separate concerns
- Optimized configurations
- Easier maintenance
- Clear purposes

---

## Example 6: Team Collaboration Setup

### Scenario
Set up test automation for a development team.

### Complete Setup

1. **Generate Base Project**:
   ```bash
   # Project lead generates the base project
   - Artifact ID: team-automation-framework
   - GitHub Workflow: ✓ (with push trigger)
   ```

2. **Initialize Git Repository**:
   ```bash
   cd team-automation-framework
   git init
   git add .
   git commit -m "Initial commit: Generated SHAFT project"
   ```

3. **Create GitHub Repository**:
   ```bash
   # Create repo on GitHub
   git remote add origin https://github.com/yourorg/team-automation-framework.git
   git push -u origin main
   ```

4. **Configure GitHub Secrets**:
   - Go to repository Settings > Secrets
   - Add execution environment credentials:
     - `LAMBDATEST_USERNAME`
     - `LAMBDATEST_ACCESS_KEY`
     - Or `BROWSERSTACK_USERNAME` and `BROWSERSTACK_ACCESS_KEY`

5. **Team Development**:
   ```bash
   # Team members clone the repository
   git clone https://github.com/yourorg/team-automation-framework.git
   cd team-automation-framework
   mvn clean test  # Run tests locally
   ```

6. **Workflow**:
   - Developers create feature branches
   - Write tests following generated examples
   - Create pull requests
   - GitHub Actions automatically runs tests
   - Merge after review and passing tests

### Result
- Standardized test framework across team
- Automated CI/CD pipeline
- Consistent code structure
- Easy onboarding for new team members

---

## Example 7: Migrating Existing Project

### Scenario
You have an existing project and want to adopt SHAFT Engine.

### Migration Strategy

1. **Generate Reference Project**:
   - Use same Test Runner as existing project
   - Same Target Platform
   - Generate with all options to see complete structure

2. **Compare Structures**:
   ```bash
   # Extract generated project
   unzip reference-project.zip
   
   # Compare key files
   diff your-project/pom.xml reference-project/pom.xml
   diff your-project/src/test/java reference-project/src/test/java
   ```

3. **Update Dependencies**:
   - Copy SHAFT Engine dependency from generated pom.xml
   - Update test runner version if needed
   - Add required plugins

4. **Refactor Tests**:
   - Study generated sample tests
   - Refactor your tests to use SHAFT API
   - Follow SHAFT patterns and conventions

5. **Add Configuration**:
   - Copy properties files
   - Adapt GitHub Actions workflow
   - Update test data structure

### Benefits
- Smooth migration path
- Reference implementation available
- Best practices guidance
- Minimal disruption

---

## Example 8: Learning and Training

### Scenario
Teaching team members test automation with SHAFT Engine.

### Training Approach

1. **Generate Training Projects**:
   - Create one project for each test type:
     - Web automation
     - API testing
     - Mobile testing

2. **Hands-on Exercises**:
   ```bash
   # Students generate their own projects
   - Artifact ID: student-name-learning-project
   - Follow along with instructor
   - Run sample tests
   - Modify tests
   - Create new tests
   ```

3. **Learning Path**:
   - **Week 1**: Web automation basics
     - Run generated web tests
     - Understand test structure
     - Add new test cases
   
   - **Week 2**: API testing
     - Run generated API tests
     - Learn REST assertions
     - Create API test suite
   
   - **Week 3**: BDD with Cucumber
     - Understand Gherkin syntax
     - Write feature files
     - Implement step definitions
   
   - **Week 4**: CI/CD and Cloud
     - Configure cloud platforms
     - Set up GitHub Actions
     - Run tests in cloud

### Result
- Structured learning experience
- Hands-on practice
- Ready-to-use examples
- Confidence in SHAFT Engine

---

## Tips and Best Practices

### 1. Project Naming
- Use descriptive artifact IDs
- Follow naming conventions:
  - `company-product-tests`
  - `api-automation-suite`
  - `mobile-app-testing`

### 2. Version Management
- Start with `1.0.0`
- Use semantic versioning
- Increment for significant changes

### 3. Test Data
- Use JSON files for test data
- Keep data separate from tests
- Use meaningful data names

### 4. GitHub Workflow
- Start with `workflow_dispatch` for manual control
- Move to `push` or `pull_request` when stable
- Use `schedule` for nightly runs

### 5. Cloud Platform Selection
- **LambdaTest**: Great for web testing, good free tier
- **BrowserStack**: Excellent for mobile, real devices
- **Local**: Best for development and debugging

### 6. Test Organization
```
src/test/java/
├── pages/          # Page objects
├── tests/          # Test classes
├── steps/          # Cucumber steps
└── utils/          # Helper classes
```

### 7. Continuous Improvement
- Review generated tests regularly
- Update to latest SHAFT version
- Enhance sample tests
- Share learnings with team

---

## Common Issues and Solutions

### Issue 1: Tests fail to run locally
**Solution**: 
```bash
# Ensure Chrome is installed
# For headless execution, no browser needed
mvn clean test -DheadlessExecution=true
```

### Issue 2: Cloud platform authentication fails
**Solution**: 
- Verify credentials in properties file
- Check account subscription status
- Ensure proper internet connectivity

### Issue 3: GitHub Actions workflow not triggering
**Solution**: 
- Check trigger configuration
- Verify secrets are set correctly
- Review workflow YAML syntax

### Issue 4: Maven build fails
**Solution**: 
```bash
# Clean and rebuild
mvn clean install -U

# Check Java version
java -version  # Should be 21 or higher
```

---

## Next Steps

After generating your project:

1. **Explore the Code**: Review generated tests and understand structure
2. **Run Tests**: Execute tests locally to verify setup
3. **Customize**: Adapt tests to your application
4. **Expand**: Add more tests following generated patterns
5. **Integrate**: Set up CI/CD pipeline
6. **Maintain**: Keep dependencies and tests updated

## Additional Resources

- [SHAFT Engine Documentation](https://shafthq.github.io/)
- [User Guide](USER_GUIDE.md)
- [Deployment Guide](DEPLOYMENT.md)
- [Integration Guide](INTEGRATION.md)

---

**Happy Testing with SHAFT Engine! 🚀**
