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
        BrowserActions.getInstance(driver).navigateToURL("https://the-internet.herokuapp.com/dynamic_loading/1");
        ElementActions.getInstance(driver).click(By.xpath("//button[text()='Start']"));
        Validations.assertThat().element(driver, By.id("finish")).text().contains("Hello World!").perform();
    }

    @Test
    public void dynamicLoading_elementIsRendered() {
        BrowserActions.getInstance(driver).navigateToURL("https://the-internet.herokuapp.com/dynamic_loading/2");
        ElementActions.getInstance(driver).click(By.xpath("//button[text()='Start']"));
        Validations.assertThat().element(driver, By.id("finish")).text().contains("Hello World!").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = DriverFactory.getHelper().getDriver();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        BrowserActions.getInstance(driver).closeCurrentWindow();
    }
}
