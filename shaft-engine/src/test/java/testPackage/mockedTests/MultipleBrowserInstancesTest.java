package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.ArrayList;

public class MultipleBrowserInstancesTest {
    ArrayList<SHAFT.GUI.WebDriver> drivers = new ArrayList<>();
    String testElement = "data:text/html,<input type=\"text\"/><br><br>";
    By locator = SHAFT.GUI.Locator.hasTagName("input").build();

    @Test(enabled = false)
    public void multipleInstancesSwitching() {
        drivers.add(new SHAFT.GUI.WebDriver());
        drivers.get(0).browser().navigateToURL(testElement)
                .and().element().type(locator, "first string");

        drivers.add(new SHAFT.GUI.WebDriver());
        drivers.get(1).browser().navigateToURL(testElement)
                .and().element().type(locator, "second string");

        drivers.get(0).element().assertThat(locator).text().isEqualTo("first string")
                .perform();
        drivers.get(1).element().assertThat(locator).text().isEqualTo("second string")
                .perform();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        drivers.forEach(SHAFT.GUI.WebDriver::quit);
    }
}
