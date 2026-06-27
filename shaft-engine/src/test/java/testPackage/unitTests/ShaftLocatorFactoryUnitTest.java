package testPackage.unitTests;

import com.shaft.driver.SHAFT;
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

    private void assertLocatorContains(By locator, String expectedToken) {
        assertNotNull(locator);
        assertTrue(locator.toString().contains(expectedToken));
    }
}
