package testPackage01;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.browser.BrowserActions;
import com.shaft.browser.BrowserFactory;
import com.shaft.element.ElementActions;

public class Test_hover {
    WebDriver driver;

    @Test(priority = 0, description = "TC001 - Test hover function.")
    public void dragAndDrop() {
	BrowserActions.navigateToURL(driver, "http://store.demoqa.com/"); // PASSED

	ElementActions.hover(driver, By.xpath("//a[contains(text(),'Product Category')]"));
	ElementActions.click(driver, By.xpath("//a[contains(text(),'iPhones')]"));
    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
	driver = BrowserFactory.getBrowser("GoogleChrome");
    }
}