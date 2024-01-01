package testPackage;

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
	WebDriver driver;
    @Test
    public void switchToNewTab() {
		new BrowserActions(driver).navigateToURL("https://duckduckgo.com/?");
		new BrowserActions(driver).navigateToURL("https://www.google.com/", WindowType.TAB);
        By searchbar = GoogleSearch.getSearchBox_textField();
		new ElementActions(driver).type(searchbar, "SHAFT_Engine").keyPress(searchbar, Keys.ENTER);
    }

	@BeforeMethod
	public void beforeMethod() {
		driver = new DriverFactory().getDriver();
	}

	@AfterMethod(alwaysRun = true)
	public void afterMethod() {
		driver.quit();
	}

}
