package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.Cookie;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.TestPageServer;

import java.util.IdentityHashMap;
import java.util.Map;

/**
 * Chrome E2E coverage for {@code reuseBrowserSession}: one browser for the class and clean state per test.
 */
public class ReuseBrowserSessionE2ETests {
    private static final Map<Object, Boolean> BROWSERS = new IdentityHashMap<>();
    private SHAFT.GUI.WebDriver driver;

    /**
     * Turns session reuse on for this class only.
     */
    @BeforeClass
    public void enableReuse() {
        SHAFT.Properties.flags.set().reuseBrowserSession(true);
    }

    /**
     * Opens the fixture page through a new {@code SHAFT.GUI.WebDriver}.
     */
    @BeforeMethod
    public void open() {
        driver = new SHAFT.GUI.WebDriver();
        BROWSERS.put(driver.getDriver(), true);
        driver.browser().navigateToURL(TestPageServer.url("cookieFixture.html"));
    }

    /**
     * Sets a cookie that must not leak into the next test.
     */
    @Test(priority = 1)
    public void firstTestSetsCookie() {
        driver.getDriver().manage().addCookie(new Cookie("reuse", "leak"));
        Assert.assertNotNull(driver.getDriver().manage().getCookieNamed("reuse"));
    }

    /**
     * The cookie from the first test is gone after the reset.
     */
    @Test(priority = 2)
    public void secondTestStartsClean() {
        Assert.assertNull(driver.getDriver().manage().getCookieNamed("reuse"));
    }

    /**
     * All three tests ran in a single browser.
     */
    @Test(priority = 3)
    public void thirdTestSharesTheSameBrowser() {
        Assert.assertEquals(BROWSERS.size(), 1, "reuseBrowserSession should start one browser per class");
    }

    /**
     * Resets the browser between tests.
     */
    @AfterMethod(alwaysRun = true)
    public void quit() {
        if (driver != null) {
            driver.quit();
        }
    }

    /**
     * Restores the default.
     */
    @AfterClass(alwaysRun = true)
    public void disableReuse() {
        SHAFT.Properties.flags.set().reuseBrowserSession(false);
    }
}
