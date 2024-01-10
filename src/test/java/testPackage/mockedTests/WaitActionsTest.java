package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class WaitActionsTest {
    SHAFT.GUI.WebDriver driver;
    String testElement = "data:text/html,<input type=\"text\"/><br><br>";
    By locator = SHAFT.GUI.Locator.hasTagName("input").build();

    @Test
    public void lambdaExpression() {
        driver.element().type(locator, "first string")
                .and().waitUntil(webDriver -> webDriver.findElement(locator).getAttribute("value").equalsIgnoreCase("first string"))
                .and().element().assertThat(locator).text().isEqualTo("first string")
                .perform();
    }

    @Test
    public void expectedCondition() {
        driver.element().type(locator, "second ")
                .typeAppend(locator, "string")
                .and().waitUntil(ExpectedConditions.attributeToBe(locator, "value", "second string"))
                .and().element().assertThat(locator).text().isEqualTo("second string")
                .perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL(testElement);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.quit();
    }
}
