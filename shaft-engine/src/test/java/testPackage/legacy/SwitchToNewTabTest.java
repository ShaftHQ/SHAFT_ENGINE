package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WindowType;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class SwitchToNewTabTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    @Test
    public void switchToNewTab() {
        driver.get().browser().navigateToURL("data:text/html;charset=utf-8,<html><title>First tab</title><body></body></html>");
        driver.get().browser().navigateToURL("data:text/html;charset=utf-8,<html><title>Second tab</title><body><input name='q'></body></html>", WindowType.TAB);
        By searchbar = By.name("q");
        driver.get().element().type(searchbar, "SHAFT_Engine").type(searchbar, Keys.ENTER);
    }

	@BeforeMethod
	public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
	}

	@AfterMethod(alwaysRun = true)
	public void afterMethod() {
        if (driver.get() != null) {
            driver.get().quit();
        }
        driver.remove();
	}

}
