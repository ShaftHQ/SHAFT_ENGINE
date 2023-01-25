package testPackage;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_SwitchToNewTab {
	ThreadLocal<WebDriver> driver = new ThreadLocal<>();

	@Test
	public void SwitchToNewTabTest() {
		BrowserActions.navigateToURL(driver.get(), "https://duckduckgo.com/?");
		BrowserActions.switchToNewTab(driver.get(), "https://www.google.com/");
		By searchbar = By.cssSelector("input[name=q]");
		new ElementActions(driver.get()).type(searchbar, "SHAFT_Engine").keyPress(searchbar, Keys.ENTER);
	}

	@BeforeMethod
	public void beforeMethod() {
		driver.set(DriverFactory.getDriver());
	}

	@AfterMethod
	public void afterMethod() {
		DriverFactory.closeAllDrivers();
	}

}
