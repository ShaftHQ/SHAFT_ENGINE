package unitTests;

import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;

public class tests_browserActions {
    WebDriver driver;

    @Test
    public void allBrowserActions() {
	BrowserActions.navigateToURL(driver, "https://duckduckgo.com");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
	BrowserActions.navigateBack(driver);
	BrowserActions.navigateForward(driver);
	BrowserActions.getCurrentURL(driver);
	BrowserActions.getCurrentWindowTitle(driver);
	BrowserActions.getPageSource(driver);
	BrowserActions.refreshCurrentPage(driver);
	BrowserActions.setWindowSize(driver, 1280, 720);
	BrowserActions.getWindowPosition(driver);
	BrowserActions.fullScreenWindow(driver);
	BrowserActions.maximizeWindow(driver);
	BrowserActions.getWindowSize(driver);
	BrowserActions.getWindowHandle(driver);
	BrowserActions.closeCurrentWindow(driver);
    }

    @BeforeClass
    public void beforeClass() {
	driver = BrowserFactory.getBrowser();
    }
}
