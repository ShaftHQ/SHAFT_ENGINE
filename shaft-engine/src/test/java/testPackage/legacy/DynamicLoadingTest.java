package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.TestPageServer;

public class DynamicLoadingTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void dynamicLoading_elementIsHidden() {
        new BrowserActions(driver.get().getDriver()).navigateToURL(TestPageServer.url("dynamicLoadingFixture.html?example=1"));
        new ElementActions(driver.get().getDriver()).click(By.xpath("//button[text()='Start']"));
        Validations.assertThat().element(driver.get().getDriver(), By.id("finish")).text().contains("Hello World!").perform();
    }

    @Test
    public void dynamicLoading_elementIsRendered() {
        new BrowserActions(driver.get().getDriver()).navigateToURL(TestPageServer.url("dynamicLoadingFixture.html?example=2"));
        new ElementActions(driver.get().getDriver()).click(By.xpath("//button[text()='Start']"));
        Validations.assertThat().element(driver.get().getDriver(), By.id("finish")).text().contains("Hello World!").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (driver.get() != null) {
            driver.get().quit();
        }
        driver.remove();
    }
}
