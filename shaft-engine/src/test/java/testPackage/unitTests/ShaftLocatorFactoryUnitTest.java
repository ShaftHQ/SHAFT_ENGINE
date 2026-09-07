package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumBy;
import org.openqa.selenium.By;
import org.testng.annotations.Test;

import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;

public class ShaftLocatorFactoryUnitTest {
    @Test
    public void generatedCodeLocatorFactoriesShouldReturnExpectedLocatorTypes() {
        assertLocatorContains(SHAFT.GUI.Locator.id("login"), "login");
        assertLocatorContains(SHAFT.GUI.Locator.name("username"), "username");
        assertLocatorContains(SHAFT.GUI.Locator.tagName("button"), "button");
        assertLocatorContains(SHAFT.GUI.Locator.className("primary"), "primary");
        assertLocatorContains(SHAFT.GUI.Locator.cssSelector("#submit"), "#submit");
        assertLocatorContains(SHAFT.GUI.Locator.xpath("//button"), "//button");
        assertLocatorContains(SHAFT.GUI.Locator.accessibilityId("login"), "login");
        assertLocatorContains(SHAFT.GUI.Locator.androidUiAutomator("new UiSelector()"), "UiSelector");
        assertLocatorContains(SHAFT.GUI.Locator.iosPredicateString("name == 'Login'"), "Login");
        assertLocatorContains(SHAFT.GUI.Locator.iosClassChain("**/XCUIElementTypeButton"), "XCUIElementTypeButton");
    }

    @Test
    public void flutterLocatorFactoriesShouldExposeAppiumFlutterStrategies() {
        assertLocatorContains(SHAFT.GUI.Locator.flutterKey("LoginButton"), "LoginButton");
        assertLocatorStrategy(SHAFT.GUI.Locator.flutterKey("LoginButton"), "flutterKey");
        assertLocatorContains(SHAFT.GUI.Locator.flutterText("Please Login"), "Please Login");
        assertLocatorStrategy(SHAFT.GUI.Locator.flutterText("Please Login"), "flutterText");
        assertLocatorContains(SHAFT.GUI.Locator.flutterTextContaining("Please"), "Please");
        assertLocatorStrategy(SHAFT.GUI.Locator.flutterTextContaining("Please"), "flutterTextContaining");
        assertLocatorContains(SHAFT.GUI.Locator.flutterType("TextField"), "TextField");
        assertLocatorStrategy(SHAFT.GUI.Locator.flutterType("TextField"), "flutterType");
        assertLocatorContains(SHAFT.GUI.Locator.flutterSemanticsLabel("login_button"), "login_button");
        assertLocatorStrategy(SHAFT.GUI.Locator.flutterSemanticsLabel("login_button"), "flutterSemanticsLabel");

        By descendant = SHAFT.GUI.Locator.flutterDescendant(
                AppiumBy.flutterType("Column"),
                AppiumBy.flutterKey("LoginButton"));
        assertLocatorStrategy(descendant, "flutterDescendant");

        By ancestor = SHAFT.GUI.Locator.flutterAncestor(
                AppiumBy.flutterKey("LoginButton"),
                AppiumBy.flutterType("Scaffold"));
        assertLocatorStrategy(ancestor, "flutterAncestor");
    }

    private void assertLocatorContains(By locator, String expectedToken) {
        assertNotNull(locator);
        assertTrue(locator.toString().contains(expectedToken),
                "Expected locator to contain '" + expectedToken + "' but was: " + locator);
    }

    private void assertLocatorStrategy(By locator, String strategyToken) {
        assertNotNull(locator);
        assertTrue(locator.toString().contains(strategyToken),
                "Expected locator strategy '" + strategyToken + "' but was: " + locator);
    }
}
