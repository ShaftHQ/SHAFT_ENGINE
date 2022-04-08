package testPackage01;

import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;

public class Test_SwitchToNewTap {
	ThreadLocal<WebDriver> driver = new ThreadLocal<>();

	@Test
	public void SwitchToNewTapTest() {
		BrowserActions.navigateToURL(driver.get(), "https://duckduckgo.com/");
		By searchbar = By.id("search_form_input_homepage");
		new ElementActions(driver.get()).type(searchbar, "SHAFT_Engine").keyPress(searchbar, Keys.ENTER);
		ElementActions.switchToNewTap(driver.get(), "https://www.google.com/");

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