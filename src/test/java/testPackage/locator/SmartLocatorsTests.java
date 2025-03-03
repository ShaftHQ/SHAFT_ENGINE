package testPackage.locator;

import org.openqa.selenium.Keys;
import org.testng.annotations.Test;
import testPackage.Tests;

@Test
public class SmartLocatorsTests extends Tests {
    // Phase 2: Beta Testing

    // Integration with SHAFT Actions (including synchronization, reporting, logging, and more)

    // AI-Powered research with help from Claude 3.5 Sonnet
    // AI-Enhanced Logic with help from GitHub Copilot
    // 11 patterns for input fields
    // 21 patterns for clickable fields
    // AI (and personal experience) adjusted priorities

    public void testSmartLocators1() {
        driver.get().browser().navigateToURL("https://www.saucedemo.com/v1/index.html")
                .and().element().type("Username", "standard_user")
                .and().type("Password", "secret_sauce")
                .and().click("LOGIN");
    }

    public void testSmartLocators2() {
        driver.get().browser().navigateToURL("https://practicetestautomation.com/practice-test-login/")
                .and().element().type("Username", "student")
                .and().type("Password", "Password123")
                .and().click("Submit");
    }

    public void testSmartLocators3() {
        driver.get().browser().navigateToURL("http://testphp.vulnweb.com/login.php")
                .and().element().type("Username", "test")
                .and().type("Password", "test")
                .and().click("login");
    }

    public void testSmartLocators4() {
        driver.get().browser().navigateToURL("http://vbsca.ca/login/login.asp")
                .and().element().type("Username", "test")
                .and().type("Password", "test")
                .and().click("Login");
    }

    public void testSmartLocators5() {
        driver.get().browser().navigateToURL("https://www.facebook.com/login.php/")
                .and().element().type("Email", "test")
                .and().type("Password", "test")
                .and().click("Log in");
    }

    public void testSmartLocators6() {
        driver.get().browser().navigateToURL("https://www.selenium.dev/selenium/web/web-form.html")
                .and().element().type("Text input", "text")
                .and().type("Password", "password")
                .and().type("Textarea", "a lot\nof text")
                .and().type("Dropdown (datalist)", "New York")
                .and().type("Date picker", "02/26/2025")
                .and().click("Submit");
    }

    public void testSmartLocators7() {
        driver.get().browser().navigateToURL("https://duckduckgo.com/")
                .and().element().type("Search", "Selenium" + Keys.ENTER);
    }

    public void testSmartLocators8() {
        driver.get().browser().navigateToURL("https://www.selenium.dev/selenium/web/login.html")
                .and().element().type("Username", "test")
                .and().type("Password", "test")
                .and().click("Login");
    }

    public void testSmartLocators9() {
        driver.get().browser().navigateToURL("https://moatazeldebsy.github.io/test-automation-practices/#/forms")
                .and().element().type("Username", "USERNAME")
                .and().type("Email", "EMAIL@DOMAIN.COM")
                .and().type("Password", "PASSWORD")
                .and().click("Sign In");
    }
}
