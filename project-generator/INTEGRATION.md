# SHAFT Project Generator - Integration with SHAFT Engine

## Overview

The SHAFT Project Generator is a standalone web application that complements the SHAFT Engine framework by providing an AI-powered, wizard-like interface for rapidly creating fully functional test automation projects. This document explains how the generator integrates with and enhances the SHAFT Engine ecosystem.

## Purpose and Benefits

### Problem Statement
Before the project generator, users had to:
1. Manually create Maven project structure
2. Configure pom.xml with correct dependencies
3. Set up proper package structure
4. Create sample test files
5. Configure properties for different execution environments
6. Set up GitHub Actions workflows manually

This process was time-consuming, error-prone, and could take 30-60 minutes for a new project.

### Solution
The SHAFT Project Generator automates this entire process:
- **Time Savings**: Generate a complete project in 30 seconds instead of 30-60 minutes
- **Consistency**: All projects follow SHAFT Engine best practices
- **Error Reduction**: Pre-validated configurations eliminate common setup mistakes
- **Flexibility**: Supports multiple test runners, platforms, and execution environments
- **CI/CD Ready**: Optional GitHub Actions workflows included

## Architecture and Design

### Technology Stack
- **Backend**: Spring Boot 3.4.1
- **Frontend**: HTML5, CSS3, Vanilla JavaScript
- **Build Tool**: Maven 3.9.x
- **Java Version**: 21 (latest LTS)
- **Containerization**: Docker support included

### Design Principles
1. **Simplicity**: Minimal dependencies, clean architecture
2. **Maintainability**: Template-based system for easy updates
3. **Extensibility**: Easy to add new test runners or platforms
4. **Portability**: Can be deployed anywhere (local, cloud, container)

## Integration Points

### 1. SHAFT Engine Version Management
The generator always uses the latest stable SHAFT Engine version in generated projects:

```xml
<dependency>
    <groupId>io.github.shafthq</groupId>
    <artifactId>SHAFT_ENGINE</artifactId>
    <version>9.3.20250928</version>
</dependency>
```

**Update Strategy**: When a new SHAFT Engine version is released, update the version in the POM templates.

### 2. Sample Test Generation
The generator creates sample tests that demonstrate SHAFT Engine best practices:

- **Fluent API Usage**: All tests use SHAFT's fluent API pattern
- **Page Object Pattern**: Web tests follow page object model
- **Test Data Management**: JSON-based test data configuration
- **Assertions**: Use SHAFT's built-in assertion framework
- **Locators**: Use SHAFT's Locator builder for element identification

### 3. Configuration Integration
Generated projects include proper configuration for:

- **Execution Platforms**: Local, LambdaTest, BrowserStack
- **Target Platforms**: Web, API, Mobile
- **Test Runners**: TestNG, JUnit, Cucumber
- **Reporting**: Allure reporting pre-configured
- **CI/CD**: GitHub Actions workflows

## Template System

### Template Structure
```
templates/
├── pom/
│   ├── pom-testng-template.xml
│   ├── pom-junit-template.xml
│   └── pom-cucumber-template.xml
├── tests/
│   ├── SampleWebTests-TestNG.java
│   ├── SampleWebTests-JUnit.java
│   ├── SampleWebTests.feature
│   ├── SampleWebSteps-Cucumber.java
│   ├── TestRunner-Cucumber.java
│   ├── SampleAPITests-TestNG.java
│   ├── SampleAPITests-JUnit.java
│   ├── SampleMobileTests-TestNG.java
│   ├── SampleMobileTests-JUnit.java
│   └── testData.json
├── workflows/
│   ├── local-workflow.yml
│   ├── lambdatest-workflow.yml
│   └── browserstack-workflow.yml
└── properties/
    ├── lambdatest.properties
    └── browserstack.properties
```

### Variable Substitution
Templates use simple variable substitution:
- `{{GROUP_ID}}`: User's Maven group ID
- `{{ARTIFACT_ID}}`: User's artifact ID
- `{{VERSION}}`: Project version
- `{{PACKAGE_NAME}}`: Derived from group ID
- `{{WORKFLOW_TRIGGER}}`: GitHub Actions trigger configuration

### Adding New Templates

To add support for a new test runner or platform:

1. **Create Template Files**:
   ```bash
   templates/tests/SampleTests-NewRunner.java
   templates/pom/pom-newrunner-template.xml
   ```

2. **Update Service Class**:
   ```java
   // In ProjectGeneratorService.java
   case "newrunner" -> "pom/pom-newrunner-template.xml";
   ```

3. **Update Frontend**:
   ```html
   <!-- In index.html -->
   <option value="NewRunner">New Runner</option>
   ```

4. **Test Generation**:
   - Build project
   - Test with different configurations
   - Verify generated projects compile and run

## Sample Tests Explained

### Web GUI Sample Tests

#### Test 1: SHAFT Engine User Guide
```java
@Test
public void testNavigateToSHAFTUserGuideAndClickUpgradeNow() {
    driver.browser().navigateToURL(shaftUserGuideUrl)
          .and().element().click(upgradeNowButton)
          .and().browser().navigateToURL(expectedUrlAfterUpgrade)
          .and().assertThat().browser().url().isEqualTo(expectedUrlAfterUpgrade);
}
```

**Demonstrates**:
- Browser navigation
- Element interaction
- Chained actions
- URL assertion

#### Test 2: DuckDuckGo Search
```java
@Test
public void testSearchForSHAFTEngineOnDuckDuckGo() {
    driver.browser().navigateToURL(duckDuckGoUrl)
          .and().element().type(searchBox, testData.getTestData("searchQuery") + Keys.ENTER)
          .and().assertThat(firstSearchResult).text().contains(testData.getTestData("expectedTextInFirstResult"));
}
```

**Demonstrates**:
- Browser navigation
- Element typing
- Test data usage
- Text assertion

### API Sample Tests

```java
@Test
public void testGetSingleUser() {
    api.get("/users/1").perform();
    
    api.assertThat().response().isEqualTo(200)
       .and().assertThat().body().contains("Leanne Graham")
       .and().assertThat().jsonPath("$.id").isEqualTo(1);
}
```

**Demonstrates**:
- REST API GET request
- Status code assertion
- Response body assertion
- JSONPath assertion

### Mobile Sample Tests

```java
@Test
public void testMobileAppLogin() {
    driver.element().type(usernameField, "testuser")
          .and().element().type(passwordField, "testpassword")
          .and().element().click(submitButton);
    
    driver.assertThat(welcomeMessage).exists();
}
```

**Demonstrates**:
- Mobile element interaction
- Multiple actions
- Element existence assertion

## Deployment Options

### Option 1: Standalone Web Application
- Run on internal server
- Access via browser
- Suitable for team use

### Option 2: Docker Container
- Easy deployment
- Consistent environment
- Scalable

### Option 3: Cloud Platform
- Heroku, AWS, Azure, GCP
- Public access
- High availability

### Option 4: GitHub Actions Integration
- Trigger generation from CI/CD
- Automated project setup
- Integration with other tools

## Maintenance and Updates

### Regular Maintenance Tasks

1. **SHAFT Engine Version Updates**:
   - Monitor SHAFT Engine releases
   - Update version in POM templates
   - Test with new version
   - Update documentation

2. **Dependency Updates**:
   - Update Spring Boot version
   - Update other dependencies
   - Test compatibility

3. **Template Improvements**:
   - Enhance sample tests
   - Add new features
   - Fix bugs

4. **Documentation Updates**:
   - Keep guides current
   - Add new examples
   - Update screenshots

### Version Control Strategy

- **Template Changes**: Always backwards compatible
- **Application Changes**: Follow semantic versioning
- **SHAFT Version**: Track separately, update templates accordingly

## Future Enhancements

### Planned Features
1. **Custom Templates**: Allow users to create custom templates
2. **Template Marketplace**: Share templates with community
3. **Advanced Configuration**: More detailed project customization
4. **Integration**: Direct GitHub repository creation
5. **AI Enhancement**: AI-powered test generation
6. **Multi-Language**: Support for other programming languages
7. **Database Support**: Add database testing templates
8. **Performance Testing**: Add performance test templates

### Community Contributions
- Submit template improvements
- Report bugs and issues
- Suggest new features
- Contribute documentation

## Best Practices

### For Generator Maintainers
1. **Test Thoroughly**: Test all configuration combinations
2. **Document Changes**: Keep documentation up-to-date
3. **Version Carefully**: Use semantic versioning
4. **Communicate**: Announce updates to users
5. **Gather Feedback**: Listen to user needs

### For Generator Users
1. **Stay Updated**: Use the latest generator version
2. **Provide Feedback**: Report issues and suggestions
3. **Share Templates**: Contribute useful templates
4. **Follow Conventions**: Use SHAFT Engine best practices
5. **Test Generated Projects**: Verify projects work as expected

## Support and Resources

### Getting Help
- **Documentation**: Comprehensive guides available
- **GitHub Issues**: Report bugs and feature requests
- **SHAFT Community**: Join discussions and get support
- **Examples**: Review generated projects for reference

### Contributing
1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## Conclusion

The SHAFT Project Generator is a powerful tool that significantly reduces the time and effort required to start new test automation projects with SHAFT Engine. By providing a simple, web-based interface and intelligent project generation, it helps teams adopt SHAFT Engine faster and more consistently.

The generator exemplifies the SHAFT Engine philosophy: making test automation easier, more efficient, and more accessible to everyone.

## References

- [SHAFT Engine Documentation](https://shafthq.github.io/)
- [SHAFT Engine GitHub](https://github.com/ShaftHQ/SHAFT_ENGINE)
- [Spring Boot Documentation](https://spring.io/projects/spring-boot)
- [Maven Documentation](https://maven.apache.org/)
- [Docker Documentation](https://docs.docker.com/)
