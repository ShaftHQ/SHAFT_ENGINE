package testPackage01;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.browser.BrowserActions;
import com.shaft.browser.BrowserFactory;
import com.shaft.element.ElementActions;
import com.shaft.validation.Assertions;

public class Test_navigation2 {
    WebDriver driver;

    @Test
    public void f() {
	BrowserActions.navigateToURL(driver, "http://35.184.27.139:9091/incorta/#/login");
	ElementActions.type(driver, By.id("tenant"), "demo");
	ElementActions.type(driver, By.id("username"), "admin");
	ElementActions.type(driver, By.id("password"), "admin");
	ElementActions.click(driver, By.xpath("//button[@data-testid='login-submit']"));
	BrowserActions.navigateToURL(driver, "http://35.184.27.139:9091/incorta/#/~/security/users");
	Assertions.assertElementExists(driver, By.xpath("//li[@class='inc-header-item']//a/span[contains(.,'Security')]"), true);
	
	// BrowserActions.navigateToURL(driver, "https://www.google.com/ncr",
	// "google.com");
	// BrowserActions.navigateToURL(driver, "https://www.w3schools.com/");
	// BrowserActions.navigateToURL(driver,
	// "http://35.184.27.139:9091/incorta/#/~/security/users",
	// "http://35.184.27.139:9091/incorta/#/login");
    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
	driver = BrowserFactory.getBrowser();
    }
}
