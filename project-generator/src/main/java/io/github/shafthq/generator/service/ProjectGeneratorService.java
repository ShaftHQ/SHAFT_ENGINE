package io.github.shafthq.generator.service;

import io.github.shafthq.generator.model.ProjectConfiguration;
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry;
import org.apache.commons.compress.archivers.zip.ZipArchiveOutputStream;
import org.springframework.stereotype.Service;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

@Service
public class ProjectGeneratorService {

    private static final String TEMPLATE_BASE_PATH = "../templates/";

    public byte[] generateProject(ProjectConfiguration config) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        
        try (ZipArchiveOutputStream zipOut = new ZipArchiveOutputStream(baos)) {
            // Generate pom.xml
            String pomContent = generatePomXml(config);
            addFileToZip(zipOut, "pom.xml", pomContent);

            // Generate test files
            generateTestFiles(zipOut, config);

            // Generate test data
            String testDataContent = loadTemplate("tests/testData.json");
            addFileToZip(zipOut, "src/test/resources/testDataFiles/testData.json", testDataContent);

            // Generate properties files if needed
            if (!"Local".equalsIgnoreCase(config.getExecutionEnvironment())) {
                generatePropertiesFiles(zipOut, config);
            }

            // Generate GitHub Actions workflow if requested
            if (config.isIncludeGitHubWorkflow()) {
                generateGitHubWorkflow(zipOut, config);
            }

            // Generate README
            String readmeContent = generateReadme(config);
            addFileToZip(zipOut, "README.md", readmeContent);

            // Generate .gitignore
            String gitignoreContent = generateGitignore();
            addFileToZip(zipOut, ".gitignore", gitignoreContent);

            zipOut.finish();
        }

        return baos.toByteArray();
    }

    private String generatePomXml(ProjectConfiguration config) throws IOException {
        String templateName = switch (config.getTestRunner().toLowerCase()) {
            case "junit" -> "pom/pom-junit-template.xml";
            case "cucumber" -> "pom/pom-cucumber-template.xml";
            default -> "pom/pom-testng-template.xml";
        };

        String template = loadTemplate(templateName);
        return template
                .replace("{{GROUP_ID}}", config.getGroupId())
                .replace("{{ARTIFACT_ID}}", config.getArtifactId())
                .replace("{{VERSION}}", config.getVersion());
    }

    private void generateTestFiles(ZipArchiveOutputStream zipOut, ProjectConfiguration config) throws IOException {
        String packagePath = config.getPackageName().replace(".", "/");
        String baseTestPath = "src/test/java/" + packagePath + "/";

        if ("Cucumber".equalsIgnoreCase(config.getTestRunner())) {
            // Generate Cucumber files
            String stepDefContent = loadTemplate("tests/SampleWebSteps-Cucumber.java")
                    .replace("{{PACKAGE_NAME}}", config.getPackageName());
            addFileToZip(zipOut, baseTestPath + "SampleWebSteps.java", stepDefContent);

            String runnerContent = loadTemplate("tests/TestRunner-Cucumber.java")
                    .replace("{{PACKAGE_NAME}}", config.getPackageName());
            addFileToZip(zipOut, baseTestPath + "TestRunner.java", runnerContent);

            String featureContent = loadTemplate("tests/SampleWebTests.feature");
            addFileToZip(zipOut, "src/test/resources/features/SampleWebTests.feature", featureContent);
        } else {
            // Generate TestNG or JUnit tests
            String suffix = "TestNG".equalsIgnoreCase(config.getTestRunner()) ? "TestNG" : "JUnit";
            
            if ("Web GUI".equalsIgnoreCase(config.getTargetPlatform())) {
                String testContent = loadTemplate("tests/SampleWebTests-" + suffix + ".java")
                        .replace("{{PACKAGE_NAME}}", config.getPackageName());
                addFileToZip(zipOut, baseTestPath + "SampleWebTests.java", testContent);
            } else if ("API".equalsIgnoreCase(config.getTargetPlatform())) {
                String testContent = loadTemplate("tests/SampleAPITests-" + suffix + ".java")
                        .replace("{{PACKAGE_NAME}}", config.getPackageName());
                addFileToZip(zipOut, baseTestPath + "SampleAPITests.java", testContent);
            } else if ("Mobile GUI".equalsIgnoreCase(config.getTargetPlatform())) {
                String testContent = loadTemplate("tests/SampleMobileTests-" + suffix + ".java")
                        .replace("{{PACKAGE_NAME}}", config.getPackageName());
                addFileToZip(zipOut, baseTestPath + "SampleMobileTests.java", testContent);
            }
        }
    }

    private void generatePropertiesFiles(ZipArchiveOutputStream zipOut, ProjectConfiguration config) throws IOException {
        String propertiesTemplate = config.getExecutionEnvironment().equalsIgnoreCase("LambdaTest")
                ? "properties/lambdatest.properties"
                : "properties/browserstack.properties";

        String propertiesContent = loadTemplate(propertiesTemplate)
                .replace("{{ARTIFACT_ID}}", config.getArtifactId())
                .replace("{{PLATFORM}}", determinePlatform(config))
                .replace("{{BROWSER_NAME}}", "chrome")
                .replace("{{OS}}", "Windows")
                .replace("{{OS_VERSION}}", "10");

        String fileName = config.getExecutionEnvironment().equalsIgnoreCase("LambdaTest")
                ? "lambdatest.properties"
                : "browserstack.properties";

        addFileToZip(zipOut, "src/main/resources/properties/" + fileName, propertiesContent);
    }

    private void generateGitHubWorkflow(ZipArchiveOutputStream zipOut, ProjectConfiguration config) throws IOException {
        String workflowTemplate = switch (config.getExecutionEnvironment().toLowerCase()) {
            case "lambdatest" -> "workflows/lambdatest-workflow.yml";
            case "browserstack" -> "workflows/browserstack-workflow.yml";
            default -> "workflows/local-workflow.yml";
        };

        String workflowContent = loadTemplate(workflowTemplate)
                .replace("{{WORKFLOW_TRIGGER}}", generateWorkflowTrigger(config.getWorkflowTrigger()));

        addFileToZip(zipOut, ".github/workflows/test.yml", workflowContent);
    }

    private String generateWorkflowTrigger(String trigger) {
        return switch (trigger.toLowerCase()) {
            case "push" -> "push:\n    branches: [ main ]";
            case "pull_request" -> "pull_request:\n    branches: [ main ]";
            case "schedule" -> "schedule:\n    - cron: '0 0 * * *'";
            default -> "workflow_dispatch:";
        };
    }

    private String generateReadme(ProjectConfiguration config) {
        return String.format("""
                # %s
                
                This project was generated using the SHAFT Project Generator.
                
                ## Project Configuration
                - **Group ID**: %s
                - **Artifact ID**: %s
                - **Version**: %s
                - **Test Runner**: %s
                - **Target Platform**: %s
                - **Execution Environment**: %s
                
                ## Getting Started
                
                ### Prerequisites
                - Java 21 or higher
                - Maven 3.9.x or higher
                
                ### Running Tests
                
                To run the tests locally:
                ```bash
                mvn clean test
                ```
                
                %s
                
                ## Project Structure
                ```
                %s/
                ├── .github/
                │   └── workflows/          # GitHub Actions workflows
                ├── src/
                │   ├── main/
                │   │   └── resources/
                │   │       └── properties/ # Configuration properties
                │   └── test/
                │       ├── java/           # Test source code
                │       └── resources/      # Test resources
                ├── pom.xml
                └── README.md
                ```
                
                ## Documentation
                
                For more information about SHAFT Engine, visit:
                - [SHAFT Engine Documentation](https://shafthq.github.io/)
                - [SHAFT Engine GitHub](https://github.com/ShaftHQ/SHAFT_ENGINE)
                
                ## Support
                
                If you encounter any issues, please refer to the [SHAFT Engine documentation](https://shafthq.github.io/) or create an issue on the [GitHub repository](https://github.com/ShaftHQ/SHAFT_ENGINE/issues).
                """,
                config.getArtifactId(),
                config.getGroupId(),
                config.getArtifactId(),
                config.getVersion(),
                config.getTestRunner(),
                config.getTargetPlatform(),
                config.getExecutionEnvironment(),
                generateExecutionInstructions(config),
                config.getArtifactId()
        );
    }

    private String generateExecutionInstructions(ProjectConfiguration config) {
        if ("Local".equalsIgnoreCase(config.getExecutionEnvironment())) {
            return "";
        }

        String platform = config.getExecutionEnvironment();
        return String.format("""
                ### Running Tests on %s
                
                1. Update the credentials in `src/main/resources/properties/%s.properties`
                2. Replace `YOUR_%s_USERNAME` and `YOUR_%s_ACCESS_KEY` with your actual credentials
                3. Run tests with: `mvn clean test`
                
                For GitHub Actions execution, make sure to add your credentials as GitHub Secrets:
                - `%s_USERNAME`
                - `%s_ACCESS_KEY`
                """,
                platform,
                platform.toLowerCase(),
                platform.toUpperCase(),
                platform.toUpperCase(),
                platform.toUpperCase(),
                platform.toUpperCase()
        );
    }

    private String generateGitignore() {
        return """
                # Compiled class files
                *.class
                
                # Log files
                *.log
                
                # Maven
                target/
                pom.xml.tag
                pom.xml.releaseBackup
                pom.xml.versionsBackup
                pom.xml.next
                release.properties
                dependency-reduced-pom.xml
                buildNumber.properties
                .mvn/timing.properties
                .mvn/wrapper/maven-wrapper.jar
                
                # IDE
                .idea/
                *.iml
                .vscode/
                .classpath
                .project
                .settings/
                
                # OS
                .DS_Store
                Thumbs.db
                
                # SHAFT Engine
                allure-report/
                allure-results/
                test-output/
                extent-reports/
                video-recordings/
                """;
    }

    private String determinePlatform(ProjectConfiguration config) {
        if ("Mobile GUI".equalsIgnoreCase(config.getTargetPlatform())) {
            return "Android";
        }
        return "Windows 10";
    }

    private String loadTemplate(String templatePath) throws IOException {
        Path path = Paths.get(TEMPLATE_BASE_PATH + templatePath);
        return Files.readString(path, StandardCharsets.UTF_8);
    }

    private void addFileToZip(ZipArchiveOutputStream zipOut, String fileName, String content) throws IOException {
        ZipArchiveEntry entry = new ZipArchiveEntry(fileName);
        entry.setSize(content.getBytes(StandardCharsets.UTF_8).length);
        zipOut.putArchiveEntry(entry);
        zipOut.write(content.getBytes(StandardCharsets.UTF_8));
        zipOut.closeArchiveEntry();
    }
}
