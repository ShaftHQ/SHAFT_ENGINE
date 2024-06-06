package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WindowType;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import poms.GoogleSearch;

public class SwitchToNewTabTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    @Test
    public void switchToNewTab() {
        driver.get().browser().navigateToURL("https://duckduckgo.com/?");
        driver.get().browser().navigateToURL("https://www.google.com/", WindowType.TAB);
        By searchbar = GoogleSearch.getSearchBox_textField();
        driver.get().element().type(searchbar, "SHAFT_Engine").keyPress(searchbar, Keys.ENTER);
    }

	@BeforeMethod
	public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
	}

	@AfterMethod(alwaysRun = true)
	public void afterMethod() {
		driver.get().quit();
	}

}
