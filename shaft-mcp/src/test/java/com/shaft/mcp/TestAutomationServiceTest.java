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
}
