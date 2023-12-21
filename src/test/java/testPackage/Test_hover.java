package testPackage;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.*;

public class Test_hover {
    WebDriver driver;

//    @Test(description = "TC001 - Test hover function.")
    public void dragAndDrop() {
        new BrowserActions(driver).navigateToURL("http://store.demoqa.com/"); // PASSED
        new ElementActions(driver).hover(By.xpath("//a[contains(text(),'Product Category')]"))
                .click(By.xpath("//a[contains(text(),'iPhones')]"));
    }

    @BeforeMethod
    public void beforeClass() {
        driver = DriverFactory.getHelper().getDriver();
    }

    @AfterMethod(alwaysRun = true)
    public void afterClass() {
        new BrowserActions(driver).closeCurrentWindow();
    }
}
