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
    WebDriver driver;

    @Test
    public void dynamicLoading_elementIsHidden() {
        new BrowserActions(driver).navigateToURL("https://the-internet.herokuapp.com/dynamic_loading/1");
        new ElementActions(driver).click(By.xpath("//button[text()='Start']"));
        Validations.assertThat().element(driver, By.id("finish")).text().contains("Hello World!").perform();
    }

    @Test
    public void dynamicLoading_elementIsRendered() {
        new BrowserActions(driver).navigateToURL("https://the-internet.herokuapp.com/dynamic_loading/2");
        new ElementActions(driver).click(By.xpath("//button[text()='Start']"));
        Validations.assertThat().element(driver, By.id("finish")).text().contains("Hello World!").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = new DriverFactory().getDriver();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        new BrowserActions(driver).closeCurrentWindow();
    }
}
