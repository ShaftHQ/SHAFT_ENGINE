package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class BrowserActionsTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void navigateToURL() {
        driver.get().browser().navigateToURL("https://duckduckgo.com/?");
    }

    @Test
    public void navigateToURLWithRedirection() {
        driver.get().browser().navigateToURL("https://www.google.com/ncr", "https://www.google.com");
    }

    @Test
    public void navigateBack() {
        driver.get().browser().navigateToURL("https://duckduckgo.com/?");
        driver.get().browser().navigateToURL("https://shafthq.github.io/");
        driver.get().browser().navigateBack();
        driver.get().assertThat().browser().url().contains("duckduckgo").perform();
    }

    @Test
    public void navigateForward() {
        driver.get().browser().navigateToURL("https://duckduckgo.com/?");
        driver.get().browser().navigateToURL("https://shafthq.github.io/");
        driver.get().browser().navigateBack();
        driver.get().browser().navigateForward();
        driver.get().assertThat().browser().url().contains("shafthq").perform();
    }

    @Test
    public void getCurrentURL() {
        driver.get().browser().getCurrentURL();
    }

    @Test
    public void getCurrentWindowTitle() {
        driver.get().browser().getCurrentWindowTitle();
    }

    @Test
    public void getPageSource() {
        driver.get().browser().getPageSource();
    }

    @Test
    public void refreshCurrentPage() {
        driver.get().browser().refreshCurrentPage();
    }

    @Test
    public void setWindowSize() {
        driver.get().browser().setWindowSize(1280, 720);
    }

    @Test
    public void getWindowPosition() {
        driver.get().browser().getWindowPosition();
    }

    @Test
    public void fullScreenWindow() {
        driver.get().browser().fullScreenWindow();
    }

    @Test
    public void maximizeWindow() {
        driver.get().browser().maximizeWindow();
    }

    @Test
    public void getWindowSize() {
        driver.get().browser().getWindowSize();
    }

    @Test
    public void getWindowHandle() {
        driver.get().browser().getWindowHandle();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("https://shafthq.github.io/");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().browser().closeCurrentWindow();
    }
}
