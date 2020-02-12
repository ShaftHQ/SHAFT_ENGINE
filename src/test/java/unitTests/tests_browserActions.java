package unitTests;

import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;

public class tests_browserActions {
    private static ThreadLocal<WebDriver> driver = new ThreadLocal<WebDriver>();

    @Test
    public void navigateToURL() {
	BrowserActions.navigateToURL(driver.get(), "https://duckduckgo.com");
    }

    @Test
    public void navigateToURLWithRedirection() {
	BrowserActions.navigateToURL(driver.get(), "https://www.google.com/ncr", "https://www.google.com");
    }

    @Test
    public void navigateBack() {
	BrowserActions.navigateToURL(driver.get(), "https://www.google.com/ncr", "https://www.google.com");
	BrowserActions.navigateBack(driver.get());
    }

    @Test
    public void navigateForward() {
	BrowserActions.navigateToURL(driver.get(), "https://www.google.com/ncr", "https://www.google.com");
	BrowserActions.navigateBack(driver.get());
	BrowserActions.navigateForward(driver.get());
    }

    @Test
    public void getCurrentURL() {
	BrowserActions.getCurrentURL(driver.get());
    }

    @Test
    public void getCurrentWindowTitle() {
	BrowserActions.getCurrentWindowTitle(driver.get());
    }

    @Test
    public void getPageSource() {
	BrowserActions.getPageSource(driver.get());
    }

    @Test
    public void refreshCurrentPage() {
	BrowserActions.refreshCurrentPage(driver.get());
    }

    @Test
    public void setWindowSize() {
	BrowserActions.setWindowSize(driver.get(), 1280, 720);
    }

    @Test
    public void getWindowPosition() {
	BrowserActions.getWindowPosition(driver.get());
    }

    @Test
    public void fullScreenWindow() {
	BrowserActions.fullScreenWindow(driver.get());
    }

    @Test
    public void maximizeWindow() {
	BrowserActions.maximizeWindow(driver.get());
    }

    @Test
    public void getWindowSize() {
	BrowserActions.getWindowSize(driver.get());
    }

    @Test
    public void getWindowHandle() {
	BrowserActions.getWindowHandle(driver.get());
    }

    @BeforeMethod
    public void beforeMethod() {
	driver.set(BrowserFactory.getBrowser());
	BrowserActions.navigateToURL(driver.get(), "https://duckduckgo.com");
    }

    @AfterMethod
    public void AfterMethod() {
	BrowserActions.navigateToURL(driver.get(), "https://duckduckgo.com");
	BrowserActions.closeCurrentWindow(driver.get());
    }
}
