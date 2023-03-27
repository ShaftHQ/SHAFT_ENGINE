package testPackage;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;

public class Test_hover {
    WebDriver driver;

    //@Test(priority = 0, description = "TC001 - Test hover function.")
    public void dragAndDrop() {
        BrowserActions.getInstance().navigateToURL("http://store.demoqa.com/"); // PASSED

        ElementActions.getInstance().hover(By.xpath("//a[contains(text(),'Product Category')]"))
                .click(By.xpath("//a[contains(text(),'iPhones')]"));
    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
        driver = DriverFactory.getDriver();
    }

    @AfterClass(alwaysRun = true)
    public void afterClass() {
        BrowserActions.getInstance().closeCurrentWindow();
    }
}
