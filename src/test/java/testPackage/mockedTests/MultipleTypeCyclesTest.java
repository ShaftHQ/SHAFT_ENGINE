package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class MultipleTypeCyclesTest {
    ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    String testElement = "data:text/html,<input type=\"text\"><br><br>";
    By locator = SHAFT.GUI.Locator.hasTagName("input").build();

    @Test
    public void typeClearTypeTypeAppend() {
        driver.get().element().type(locator, "first string")
                .type(locator, "second ")
                .typeAppend(locator, "string")
                .and().assertThat(locator).text().isEqualTo("second string")
                .perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(SHAFT.GUI.WebDriver.getInstance());
        driver.get().browser().navigateToURL(testElement);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
        driver.remove();
    }
}
