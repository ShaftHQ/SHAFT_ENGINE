package testPackage;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class DynamicLoadingTest {
    private static final ThreadLocal<WebDriver> driver = new ThreadLocal<>();

    @Test
    public void dynamicLoading_elementIsHidden() {
        new BrowserActions(driver.get()).navigateToURL("https://the-internet.herokuapp.com/dynamic_loading/1");
        new ElementActions(driver.get()).click(By.xpath("//button[text()='Start']"));
        Validations.assertThat().element(driver.get(), By.id("finish")).text().contains("Hello World!").perform();
    }

    @Test
    public void dynamicLoading_elementIsRendered() {
        new BrowserActions(driver.get()).navigateToURL("https://the-internet.herokuapp.com/dynamic_loading/2");
        new ElementActions(driver.get()).click(By.xpath("//button[text()='Start']"));
        Validations.assertThat().element(driver.get(), By.id("finish")).text().contains("Hello World!").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new DriverFactory().getDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        new BrowserActions(driver.get()).closeCurrentWindow();
    }
}
