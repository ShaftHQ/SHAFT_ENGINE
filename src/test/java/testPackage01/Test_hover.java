package testPackage01;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_hover {
    WebDriver driver;

    //@Test(priority = 0, description = "TC001 - Test hover function.")
    public void dragAndDrop() {
        BrowserActions.navigateToURL(driver, "http://store.demoqa.com/"); // PASSED

        ElementActions.hover(driver, By.xpath("//a[contains(text(),'Product Category')]"));
        ElementActions.click(driver, By.xpath("//a[contains(text(),'iPhones')]"));
    }

    @Test(priority = 0, description = "TC001 - Test hover function.")
    public void dragAndDrop2() {
        BrowserActions.navigateToURL(driver, "https://incorta.com/"); // PASSED

        ElementActions.hover(driver, By.xpath("//a[text()='About']"));
        ElementActions.click(driver, By.xpath("//li[@role='menuitem']//a[text()='Team']"));
    }


    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
        driver = BrowserFactory.getBrowser(BrowserFactory.BrowserType.GOOGLE_CHROME);
    }
}