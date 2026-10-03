package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.internal.BrowserConsoleErrorGate;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;
import testPackage.TestPageServer;
import testPackage.Tests;

/**
 * Local E2E for {@code failOnBrowserConsoleErrors} (#6400) against a hermetic fixture page.
 */
public class BrowserConsoleErrorGateE2ETest extends Tests {

    @Test
    public void gateReportsOnlyNonAllowlistedConsoleErrors() {
        driver.get().browser().navigateToURL(TestPageServer.url("consoleErrorFixture.html"));
        driver.get().assertThat().element(By.id("ready")).exists().perform();
        try {
            driver.get().browser().console().messages();
        } catch (UnsupportedOperationException unsupported) {
            throw new SkipException("Browser console capture is not available for this session.");
        }
        SHAFT.Properties.web.set().failOnBrowserConsoleErrors(true).browserConsoleErrorAllowlist("favicon\\.ico");

        AssertionError failure = BrowserConsoleErrorGate.checkCurrentTest();

        Assert.assertNotNull(failure, "a non-allowlisted console error must fail the test");
        Assert.assertTrue(failure.getMessage().contains("Uncaught checkout failure"), failure.getMessage());
        Assert.assertFalse(failure.getMessage().contains("favicon"), failure.getMessage());
        Assert.assertNull(BrowserConsoleErrorGate.checkCurrentTest(), "the buffer is cleared after each check");
    }
}
