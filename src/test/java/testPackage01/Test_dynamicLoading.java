package testPackage01;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_dynamicLoading {
    ThreadLocal<WebDriver> driver = new ThreadLocal<>();

    @Test
    public void dynamicLoading_elementIsHidden() {
        BrowserActions.navigateToURL(driver.get(), "https://the-internet.herokuapp.com/dynamic_loading/1");
        ElementActions.click(driver.get(), By.xpath("//button[text()='Start']"));
        Assertions.assertElementAttribute(driver.get(), By.id("finish"), "text", "Hello World!", Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.POSITIVE);
    }

    @Test
    public void dynamicLoading_elementIsRendered() {
        BrowserActions.navigateToURL(driver.get(), "https://the-internet.herokuapp.com/dynamic_loading/2");
        ElementActions.click(driver.get(), By.xpath("//button[text()='Start']"));
        Assertions.assertElementAttribute(driver.get(), By.id("finish"), "text", "Hello World!", Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.POSITIVE);
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(DriverFactory.getDriver());
    }

    @AfterMethod
    public void afterMethod() {
        BrowserActions.closeCurrentWindow(driver.get());
        driver.remove();
    }
}
