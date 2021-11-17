package testPackage01;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_chainableElementActions {
    WebDriver driver;

    @BeforeMethod
    public void beforeMethod(){
        driver = DriverFactory.getDriver();
    }
    @Test
    public void chainElementActions() {
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
        By searchBox = By.name("q");
        new ElementActions(driver).type(searchBox, "chained type 1")
                .type(searchBox, "chained type 2")
                .typeAppend(searchBox, "345");
        Validations.assertThat()
                        .element(driver, searchBox)
                                .text()
                                        .equals("chained type 2345");
    }

    @AfterMethod
    public void afterMethod(){
        DriverFactory.closeAllDrivers();
    }
}
