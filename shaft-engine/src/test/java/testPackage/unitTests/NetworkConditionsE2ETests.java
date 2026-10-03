package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.JavascriptExecutor;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.TestPageServer;

/**
 * Chrome E2E coverage for {@code driver.browser().network()} offline and throttle emulation.
 */
public class NetworkConditionsE2ETests {
    private static final String FETCH = "const done = arguments[arguments.length - 1]; const t = performance.now();"
            + "fetch(arguments[0] + '?n=' + Math.random(), {cache: 'no-store'})"
            + ".then(() => done(performance.now() - t)).catch(() => done(-1));";
    private final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private String fixture;

    /**
     * Opens a hermetic local fixture page.
     */
    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        fixture = TestPageServer.url("clickableFixture.html");
        driver.get().browser().navigateToURL(fixture);
    }

    /**
     * Offline emulation makes fetch fail and online restores it.
     */
    @Test
    public void offlineShouldFailFetchUntilRestored() {
        driver.get().browser().network().offline();
        Assert.assertEquals(fetchMillis(), -1.0, "fetch should fail while offline");
        driver.get().browser().network().online();
        Assert.assertTrue(fetchMillis() >= 0, "fetch should succeed after online()");
    }

    /**
     * Throttled latency delays a local fetch by at least the configured milliseconds.
     */
    @Test
    public void throttleShouldAddLatency() {
        driver.get().browser().network().throttle(500, 0, 0);
        Assert.assertTrue(fetchMillis() >= 500, "fetch should take at least 500 ms under 500 ms latency");
    }

    private double fetchMillis() {
        Object result = ((JavascriptExecutor) driver.get().getDriver()).executeAsyncScript(FETCH, fixture);
        return ((Number) result).doubleValue();
    }

    /**
     * Restores networking and quits the driver.
     */
    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (driver.get() != null) {
            driver.get().quit();
        }
        driver.remove();
    }
}
