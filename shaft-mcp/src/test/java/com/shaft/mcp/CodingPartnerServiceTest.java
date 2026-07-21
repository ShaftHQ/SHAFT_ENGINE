package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CodingPartnerServiceTest {
    @TempDir
    Path temp;

    @Test
    void planSurfacesWorkspaceReuseBeforeGeneratingMissingCode() throws Exception {
        Path page = temp.resolve("src/test/java/pages/LoginPage.java");
        Files.createDirectories(page.getParent());
        Files.writeString(page, """
                package pages;

                import com.shaft.driver.SHAFT;
                import org.openqa.selenium.By;

                public class LoginPage {
                    private final SHAFT.GUI.WebDriver browser;
                    private final By emailInput = SHAFT.GUI.Locator.inputField("Email");

                    public LoginPage(SHAFT.GUI.WebDriver browser) {
                        this.browser = browser;
                    }

                    public LoginPage loginAs(String username) {
                        browser.element().type(emailInput, username);
                        return this;
                    }
                }
                """);

        McpCodingPartnerPlan plan = service().plan(
                temp.toString(),
                "Log in with valid credentials and verify the account menu",
                "webdriver",
                "src/test/java/pages/LoginPage.java",
                "driver.findElement(By.id(\"email\")).sendKeys(username);",
                List.of("target/shaft-traces/latest"),
                10);

        assertAll(
                () -> assertEquals("1.2", plan.schemaVersion()),
                () -> assertEquals("WebDriver", plan.backend()),
                () -> assertTrue(plan.workingSetSummary().contains("LoginPage.java")),
                () -> assertFalse(plan.reuseMatches().isEmpty()),
                () -> assertEquals("LoginPage", plan.reuseMatches().get(0).className()),
                () -> assertEquals("src/test/java/pages/LoginPage.java", plan.recommendedTargetSourcePath()),
                () -> assertEquals("loginAs", plan.recommendedInsertionAnchor()),
                () -> assertEquals(2, plan.stepPlan().size()),
                () -> assertEquals(1, plan.stepPlan().get(0).index()),
                () -> assertEquals("Log in with valid credentials", plan.stepPlan().get(0).instruction()),
                () -> assertTrue(plan.stepPlan().get(0).reuseHint().contains("LoginPage.java")),
                () -> assertTrue(plan.stepPlan().get(0).proofTool().contains("browser_open_intent")),
                () -> assertEquals("verify the account menu", plan.stepPlan().get(1).instruction()),
                () -> assertTrue(plan.reuseMatches().get(0).locatorSummaries().stream()
                        .anyMatch(summary -> summary.contains("emailInput"))),
                () -> assertTrue(plan.reuseMatches().get(0).actionSummaries().stream()
                        .anyMatch(summary -> summary.contains("loginAs"))),
                () -> assertTrue(plan.missingCodeItems().stream()
                        .anyMatch(item -> item.contains("raw Selenium"))),
                () -> assertTrue(plan.suggestedMcpCalls().contains("capture_target_candidates")),
                () -> assertTrue(plan.nextActions().stream()
                        .anyMatch(action -> "browser_open_intent".equals(action.toolName())
                                && action.requiresConfirmation())),
                () -> assertTrue(plan.nextActions().stream()
                        .anyMatch(action -> "capture_record_at_target_code_blocks".equals(action.toolName())
                                && action.arguments().containsValue("src/test/java/pages/LoginPage.java"))),
                () -> assertTrue(plan.nextActions().stream()
                        .anyMatch(action -> "capture_target_candidates".equals(action.toolName())
                                && Integer.valueOf(10).equals(action.arguments().get("maxResults")))),
                () -> assertTrue(plan.nextActions().stream()
                        .anyMatch(action -> "capture_record_at_target_code_blocks".equals(action.toolName())
                                && Boolean.FALSE.equals(action.arguments().get("overwrite")))),
                () -> assertTrue(plan.nextActions().stream()
                        .filter(action -> "test_code_guardrails_check".equals(action.toolName()))
                        .allMatch(action -> action.requiresConfirmation()
                                && !action.arguments().containsKey("code"))),
                () -> assertTrue(plan.verificationCommand().contains("test-compile")),
                () -> assertTrue(plan.evidencePaths().contains("target/shaft-traces/latest")),
                () -> assertTrue(plan.warnings().stream().anyMatch(warning -> warning.contains("approval"))));
    }

    @Test
    void planPreservesTypedEvidencePackArguments() throws Exception {
        Path page = temp.resolve("src/test/java/tests/LoginTest.java");
        Files.createDirectories(page.getParent());
        Files.writeString(page, """
                package tests;

                public class LoginTest {
                }
                """);

        McpCodingPartnerPlan plan = service().plan(
                temp.toString(),
                "Verify login evidence",
                "webdriver",
                "src/test/java/tests/LoginTest.java",
                "",
                List.of("target/shaft-traces/login.zip"),
                10);

        McpCodingPartnerNextAction evidencePack = plan.nextActions().stream()
                .filter(action -> "capture_evidence_pack".equals(action.toolName()))
                .findFirst()
                .orElseThrow();
        Map<?, ?> arguments = evidencePack.arguments();

        assertAll(
                () -> assertEquals("", arguments.get("reportPath")),
                () -> assertEquals(List.of(), arguments.get("screenshotPaths")),
                () -> assertEquals("src/test/java/tests/LoginTest.java", arguments.get("sourcePath")));
    }

    @Test
    void planSurfacesMobileBackendActions() throws Exception {
        Path page = temp.resolve("src/test/java/pages/LoginScreen.java");
        Files.createDirectories(page.getParent());
        Files.writeString(page, """
                package pages;

                import com.shaft.driver.SHAFT;
                import io.appium.java_client.AppiumBy;
                import org.openqa.selenium.By;

                public class LoginScreen {
                    private final SHAFT.GUI.WebDriver driver;
                    private final By loginButton = AppiumBy.accessibilityId("login");

                    public LoginScreen(SHAFT.GUI.WebDriver driver) {
                        this.driver = driver;
                    }

                    public LoginScreen tapLogin() {
                        driver.element().click(loginButton);
                        return this;
                    }
                }
                """);

        McpCodingPartnerPlan plan = service().plan(
                temp.toString(),
                "Tap the login button in the Android app",
                "mobile",
                "src/test/java/pages/LoginScreen.java",
                "AppiumBy.accessibilityId(\"login\")",
                List.of(),
                10);

        assertAll(
                () -> assertEquals("Mobile", plan.backend()),
                () -> assertTrue(plan.suggestedMcpCalls().contains("mobile_get_accessibility_tree")),
                () -> assertTrue(plan.suggestedMcpCalls().contains("capture_record_at_target_code_blocks")),
                () -> assertTrue(plan.stepPlan().stream()
                        .allMatch(step -> "mobile_get_accessibility_tree".equals(step.proofTool()))));
    }

    @Test
    void planRejectsRepositoryOutsideWorkspace() throws Exception {
        Path outside = Files.createTempDirectory("outside-partner");

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service().plan(
                        outside.toString(),
                        "Open checkout",
                        "webdriver",
                        "",
                        "",
                        List.of(),
                        5));

        assertTrue(failure.getMessage().contains("workspace"));
    }

    @Test
    void planRejectsCurrentSourceOutsideRepositoryEvenWhenItIsInsideWorkspace() throws Exception {
        Path repository = temp.resolve("repo");
        Files.createDirectories(repository);
        Path outsideSource = temp.resolve("other/OutsideTest.java");
        Files.createDirectories(outsideSource.getParent());
        Files.writeString(outsideSource, "class OutsideTest {}");

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service().plan(
                        repository.toString(),
                        "Open checkout",
                        "webdriver",
                        outsideSource.toString(),
                        "",
                        List.of(),
                        5));

        assertTrue(failure.getMessage().contains("repository"));
    }

    @Test
    void diffInsertsReviewedCodeAfterExistingMethod() throws Exception {
        Path page = temp.resolve("src/test/java/pages/LoginPage.java");
        Files.createDirectories(page.getParent());
        Files.writeString(page, """
                package pages;

                import com.shaft.driver.SHAFT;
                import org.openqa.selenium.By;

                public class LoginPage {
                    private final SHAFT.GUI.WebDriver browser;

                    public LoginPage loginAs(String username) {
                        browser.element().type(email, username);
                        return this;
                    }
                }
                """);

        McpCodingPartnerDiff diff = service().diff(
                temp.toString(),
                "src/test/java/pages/LoginPage.java",
                List.of("""
                        ```java
                        public LoginPage assertDashboard() {
                            browser.assertThat().browser().url().contains("/dashboard");
                            return this;
                        }
                        ```"""),
                "loginAs");

        assertAll(
                () -> assertEquals("1.0", diff.schemaVersion()),
                () -> assertEquals("src/test/java/pages/LoginPage.java", diff.targetSourcePath()),
                () -> assertEquals("loginAs", diff.insertionAnchor()),
                () -> assertTrue(diff.targetExists()),
                () -> assertTrue(diff.insertedLineCount() > 0),
                () -> assertTrue(diff.unifiedDiff().startsWith("--- a/src/test/java/pages/LoginPage.java")),
                () -> assertTrue(diff.unifiedDiff().contains("+++ b/src/test/java/pages/LoginPage.java")),
                () -> assertTrue(diff.unifiedDiff().contains("@@ ")),
                () -> assertFalse(diff.unifiedDiff().contains("```")),
                () -> assertTrue(diff.unifiedDiff().contains("+public LoginPage assertDashboard() {")),
                // the insertion lands after loginAs's closing brace, before the class closing brace
                () -> assertTrue(diff.unifiedDiff().contains(" }")),
                () -> assertTrue(diff.warnings().stream().anyMatch(warning -> warning.contains("preview-only"))));
    }

    @Test
    void diffForMissingTargetProducesNewFileHunk() throws Exception {
        McpCodingPartnerDiff diff = service().diff(
                temp.toString(),
                "src/test/java/tests/NewFlowTest.java",
                List.of("public class NewFlowTest {\n}"),
                "");

        assertAll(
                () -> assertFalse(diff.targetExists()),
                () -> assertTrue(diff.unifiedDiff().contains("--- /dev/null")),
                () -> assertTrue(diff.unifiedDiff().contains("@@ -0,0 +1,")),
                () -> assertTrue(diff.warnings().stream().anyMatch(warning -> warning.contains("does not exist"))));
    }

    @Test
    void diffFlagsRawSeleniumInsertion() throws Exception {
        Path page = temp.resolve("src/test/java/pages/HomePage.java");
        Files.createDirectories(page.getParent());
        Files.writeString(page, """
                package pages;

                public class HomePage {
                }
                """);

        McpCodingPartnerDiff diff = service().diff(
                temp.toString(),
                "src/test/java/pages/HomePage.java",
                List.of("driver.findElement(By.id(\"email\")).sendKeys(username);"),
                "");

        assertTrue(diff.warnings().stream().anyMatch(warning -> warning.contains("raw Selenium")));
    }

    @Test
    void diffRejectsMissingCodeBlocks() {
        Path repository = temp;
        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service().diff(repository.toString(), "src/test/java/pages/HomePage.java", List.of("  "), "x"));
        assertTrue(failure.getMessage().contains("code block"));
    }

    private CodingPartnerService service() {
        return new CodingPartnerService(McpWorkspacePolicy.of(temp));
    }
}
