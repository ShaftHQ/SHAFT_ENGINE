package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

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
                () -> assertEquals("1.0", plan.schemaVersion()),
                () -> assertEquals("WebDriver", plan.backend()),
                () -> assertTrue(plan.workingSetSummary().contains("LoginPage.java")),
                () -> assertFalse(plan.reuseMatches().isEmpty()),
                () -> assertEquals("LoginPage", plan.reuseMatches().get(0).className()),
                () -> assertTrue(plan.reuseMatches().get(0).locatorSummaries().stream()
                        .anyMatch(summary -> summary.contains("emailInput"))),
                () -> assertTrue(plan.reuseMatches().get(0).actionSummaries().stream()
                        .anyMatch(summary -> summary.contains("loginAs"))),
                () -> assertTrue(plan.missingCodeItems().stream()
                        .anyMatch(item -> item.contains("raw Selenium"))),
                () -> assertTrue(plan.suggestedMcpCalls().contains("capture_target_candidates")),
                () -> assertTrue(plan.suggestedMcpCalls().contains("test_code_guardrails_check")),
                () -> assertTrue(plan.verificationCommand().contains("test-compile")),
                () -> assertTrue(plan.evidencePaths().contains("target/shaft-traces/latest")),
                () -> assertTrue(plan.warnings().stream().anyMatch(warning -> warning.contains("approval"))));
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

    private CodingPartnerService service() {
        return new CodingPartnerService(McpWorkspacePolicy.of(temp));
    }
}
