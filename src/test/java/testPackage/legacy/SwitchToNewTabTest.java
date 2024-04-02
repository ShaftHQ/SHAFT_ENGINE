package testPackage.legacy;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WindowType;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import poms.GoogleSearch;

public class SwitchToNewTabTest {
	private static final ThreadLocal<WebDriver> driver = new ThreadLocal<>();
    @Test
    public void switchToNewTab() {
		new BrowserActions(driver.get()).navigateToURL("https://duckduckgo.com/?");
		new BrowserActions(driver.get()).navigateToURL("https://www.google.com/", WindowType.TAB);
        By searchbar = GoogleSearch.getSearchBox_textField();
		new ElementActions(driver.get()).type(searchbar, "SHAFT_Engine").keyPress(searchbar, Keys.ENTER);
    }

	@BeforeMethod
	public void beforeMethod() {
		driver.set(new DriverFactory().getDriver());
	}

	@AfterMethod(alwaysRun = true)
	public void afterMethod() {
		driver.get().quit();
	}

}
