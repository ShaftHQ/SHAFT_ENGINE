package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TestAutomationServiceTest {
    private final TestAutomationService service = new TestAutomationService();

    @Test
    void scenarioCatalogCoversRequiredAgentUseCases() {
        McpScenarioCatalogResult result = service.testAutomationScenarios("all", "", 0);
        Set<String> ids = new HashSet<>();

        for (McpTestAutomationScenario scenario : result.scenarios()) {
            assertFalse(scenario.id().isBlank());
            assertTrue(ids.add(scenario.id()), "duplicate scenario id: " + scenario.id());
            assertFalse(scenario.userPrompts().isEmpty(), scenario.id());
            assertFalse(scenario.mcpTools().isEmpty(), scenario.id());
            assertFalse(scenario.agentActions().isEmpty(), scenario.id());
            assertFalse(scenario.repoPattern().isEmpty(), scenario.id());
            assertFalse(scenario.guardrails().isEmpty(), scenario.id());
            assertFalse(scenario.completionCriteria().isEmpty(), scenario.id());
        }

        assertTrue(ids.contains("api-openapi-contract-suite"));
        assertTrue(ids.contains("web-pom-fluent-test"));
        assertTrue(ids.contains("web-playwright-pom-fluent-test"));
        assertTrue(ids.contains("web-playwright-record-replay"));
        assertTrue(ids.contains("mobile-native-appium"));
        assertTrue(ids.contains("failure-doctor-analysis"));
        assertTrue(result.guidanceRules().stream().anyMatch(rule -> rule.contains("Thread.sleep")));
        assertTrue(result.guidanceRules().stream().anyMatch(rule -> rule.contains("absolute XPath")));
        assertTrue(result.guidanceRules().stream().anyMatch(rule -> rule.contains("driver.findElement")));
        assertTrue(result.guidanceRules().stream().anyMatch(rule -> rule.contains("hard-coded secrets")));
    }

    @Test
    void scenarioCatalogFiltersByAreaAndIntent() {
        McpScenarioCatalogResult result = service.testAutomationScenarios("api", "swagger contract", 3);

        assertEquals("api", result.area());
        assertTrue(result.scenarios().stream().anyMatch(scenario -> scenario.id().equals("api-openapi-contract-suite")));
        assertTrue(result.scenarios().stream().allMatch(scenario -> scenario.areas().contains("api")));
        assertTrue(result.scenarios().size() <= 3);
    }

    @Test
    void guardrailRejectsThreadSleepAndAbsoluteXpath() {
        McpCodeGuardrailResult result = service.checkGeneratedCode("java", """
                import org.openqa.selenium.By;

                class LoginTest {
                    void waitsWrong() throws Exception {
                        Thread.sleep(1000);
                        By login = By.xpath("/html/body/div/form/button");
                    }
                }
                """);

        assertFalse(result.passed());
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("THREAD_SLEEP")));
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("ABSOLUTE_XPATH")));
    }

    @Test
    void guardrailAllowsPreferredLocatorsAndWarnsOnPageFactory() {
        McpCodeGuardrailResult result = service.checkGeneratedCode("java", """
                import com.shaft.driver.SHAFT;
                import org.openqa.selenium.By;
                import org.openqa.selenium.support.FindBy;

                class LoginPage {
                    @FindBy(id = "legacy")
                    private Object legacy;
                    private final By email = SHAFT.GUI.Locator.inputField("Email");
                    private final By login = SHAFT.GUI.Locator.clickableField("Log In");
                    private final By scopedFallback = By.xpath("//button[@type='submit']");
                }
                """);

        assertTrue(result.passed());
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("PAGE_FACTORY")
                && violation.severity().equals("WARNING")));
    }

    @Test
    void guardrailWarnsOnRawSeleniumWaitsDriverCallsHeadedSetupAndSystemProperties() {
        McpCodeGuardrailResult result = service.checkGeneratedCode("java", """
                import org.openqa.selenium.By;
                import org.openqa.selenium.WebDriver;
                import org.openqa.selenium.chrome.ChromeOptions;

                class GeneratedLoginTest {
                    void antiPatterns(WebDriver driver) {
                        driver.manage().timeouts().implicitlyWait(java.time.Duration.ofSeconds(5));
                        driver.findElement(By.id("login")).click();
                        driver.findElements(By.cssSelector(".error"));
                        ChromeOptions options = new ChromeOptions();
                        options.setHeadless(false);
                        String baseUrl = System.getProperty("base.url");
                    }
                }
                """);

        assertTrue(result.passed());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("Lexical guardrail check only")));
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("IMPLICIT_WAIT")
                && violation.severity().equals("WARNING") && violation.line() == 7));
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("RAW_FIND_ELEMENT")
                && violation.severity().equals("WARNING") && violation.line() == 8));
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("RAW_FIND_ELEMENT")
                && violation.severity().equals("WARNING") && violation.line() == 9));
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("HEADED_BROWSER")
                && violation.severity().equals("WARNING") && violation.line() == 11));
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("DIRECT_SYSTEM_PROPERTY")
                && violation.severity().equals("WARNING") && violation.line() == 12));
    }

    @Test
    void guardrailRejectsHardcodedSecretsAndRedactsSecretSnippet() {
        McpCodeGuardrailResult result = service.checkGeneratedCode("java", """
                import java.util.Map;

                class GeneratedApiTest {
                    void leaksSecret(Map<String, String> headers) {
                        headers.put("Authorization", "Bearer sk_live_1234567890abcdef");
                    }
                }
                """);

        assertFalse(result.passed());
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("HARDCODED_SECRET")
                && violation.severity().equals("ERROR")
                && violation.line() == 5
                && violation.snippet().contains("[REDACTED]")
                && !violation.snippet().contains("sk_live_1234567890abcdef")));
    }

    @Test
    void guardrailAllowsShaftFacadeAndSmartLocatorUsage() {
        McpCodeGuardrailResult result = service.checkGeneratedCode("java", """
                import com.shaft.driver.SHAFT;
                import org.openqa.selenium.By;

                class LoginPage {
                    private final By email = SHAFT.GUI.Locator.inputField("Email");
                    private final By submit = SHAFT.GUI.Locator.clickableField("Sign In");

                    void login(SHAFT.GUI.WebDriver browser) {
                        browser.element().type(email, "user@example.com");
                        browser.element().click(submit);
                    }
                }
                """);

        assertTrue(result.passed());
        assertTrue(result.violations().isEmpty());
    }
}
