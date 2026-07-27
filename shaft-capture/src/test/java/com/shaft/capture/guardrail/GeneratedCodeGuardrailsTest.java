package com.shaft.capture.guardrail;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GeneratedCodeGuardrailsTest {

    @Test
    void rejectsThreadSleepAndAbsoluteXpath() {
        GuardrailCheckResult result = GeneratedCodeGuardrails.check("""
                import org.openqa.selenium.By;

                class LoginTest {
                    void waitsWrong() throws Exception {
                        Thread.sleep(1000);
                        By login = By.xpath("/html/body/div/form/button");
                        By fallback = SHAFT.GUI.Locator.xpath("//button[@type='submit']");
                    }
                }
                """);

        assertFalse(result.passed());
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("THREAD_SLEEP")));
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("ABSOLUTE_XPATH")));
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("SHAFT_LOCATOR_XPATH")));
    }

    @Test
    void allowsPreferredLocatorsAndWarnsOnPageFactory() {
        GuardrailCheckResult result = GeneratedCodeGuardrails.check("""
                import com.shaft.driver.SHAFT;
                import com.shaft.gui.internal.locator.Role;
                import org.openqa.selenium.By;
                import org.openqa.selenium.support.FindBy;

                class LoginPage {
                    @FindBy(id = "legacy")
                    private Object legacy;
                    private final By email = SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build();
                    private final By login = SHAFT.GUI.Locator.hasRole(Role.BUTTON).build();
                    private final By scopedFallback = By.xpath("//button[@type='submit']");
                }
                """);

        assertTrue(result.passed());
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("PAGE_FACTORY")
                && violation.severity().equals("WARNING")));
    }

    @Test
    void warnsOnRawSeleniumWaitsDriverCallsHeadedSetupAndSystemProperties() {
        GuardrailCheckResult result = GeneratedCodeGuardrails.check("""
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
    void rejectsHardcodedSecretsAndRedactsSecretSnippet() {
        GuardrailCheckResult result = GeneratedCodeGuardrails.check("""
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
    void allowsShaftFacadeAndRoleBasedLocatorUsage() {
        GuardrailCheckResult result = GeneratedCodeGuardrails.check("""
                import com.shaft.driver.SHAFT;
                import com.shaft.gui.internal.locator.Role;
                import org.openqa.selenium.By;

                class LoginPage {
                    private final By email = SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build();
                    private final By submit = SHAFT.GUI.Locator.hasRole(Role.BUTTON).build();

                    void login(SHAFT.GUI.WebDriver browser) {
                        browser.element().type(email, "user@example.com");
                        browser.element().click(submit);
                    }
                }
                """);

        assertTrue(result.passed());
        assertTrue(result.violations().isEmpty());
    }

    @Test
    void rejectsIntentBasedSmartLocatorUsage() {
        GuardrailCheckResult result = GeneratedCodeGuardrails.check("""
                import com.shaft.driver.SHAFT;
                import org.openqa.selenium.By;

                class LoginPage {
                    private final By email = SHAFT.GUI.Locator.inputField("Email");
                    private final By submit = SHAFT.GUI.Locator.clickableField("Sign In");
                }
                """);

        assertFalse(result.passed());
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("SMART_LOCATOR")
                && violation.severity().equals("ERROR") && violation.line() == 5));
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("SMART_LOCATOR")
                && violation.severity().equals("ERROR") && violation.line() == 6));
    }

    @Test
    void rejectsLocatorDeclaredInsideTestAnnotatedClass() {
        GuardrailCheckResult result = GeneratedCodeGuardrails.check("""
                import com.shaft.driver.SHAFT;
                import com.shaft.gui.internal.locator.Role;
                import org.openqa.selenium.By;
                import org.testng.annotations.Test;

                class CheckoutTest {
                    private final By checkoutButton = SHAFT.GUI.Locator.hasRole(Role.BUTTON)
                            .hasText("Checkout").build();

                    @Test
                    void checkout(SHAFT.GUI.WebDriver driver) {
                        driver.element().click(checkoutButton);
                    }
                }
                """);

        assertFalse(result.passed());
        assertTrue(result.violations().stream().anyMatch(violation -> violation.kind().equals("POM_VIOLATION")
                && violation.severity().equals("ERROR")));
    }

    @Test
    void allowsLocatorsDeclaredInSeparatePageObjectFromTestClass() {
        GuardrailCheckResult result = GeneratedCodeGuardrails.check("""
                import com.shaft.driver.SHAFT;
                import com.shaft.gui.internal.locator.Role;
                import org.openqa.selenium.By;
                import org.testng.annotations.Test;

                public final class LoginPage {
                    private final By email = SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build();
                    private final By submit = SHAFT.GUI.Locator.hasRole(Role.BUTTON).build();

                    public LoginPage login(SHAFT.GUI.WebDriver driver, String user, String password) {
                        driver.element().type(email, user);
                        driver.element().click(submit);
                        return this;
                    }
                }

                class LoginTest {
                    @Test
                    void login(SHAFT.GUI.WebDriver driver) {
                        new LoginPage(driver).login(driver, "user@example.com", "secret123");
                    }
                }
                """);

        assertTrue(result.passed());
        assertTrue(result.violations().stream().noneMatch(violation -> violation.kind().equals("POM_VIOLATION")));
    }

    @Test
    void noCodeSuppliedYieldsNoViolationsAndPasses() {
        GuardrailCheckResult result = GeneratedCodeGuardrails.check("");

        assertTrue(result.passed());
        assertTrue(result.violations().isEmpty());
    }

    @Test
    void allowsTemplatePlaceholderExampleWordAndGenericKeywordOnlySecretValues() {
        GuardrailCheckResult result = GeneratedCodeGuardrails.check("""
                class Config {
                    private String token = "${SECRET_TOKEN}";
                    private String password = "example_password_1";
                    private String apiKey = "password";
                }
                """);

        assertTrue(result.passed());
        assertTrue(result.violations().stream().noneMatch(violation -> violation.kind().equals("HARDCODED_SECRET")));
    }
}
