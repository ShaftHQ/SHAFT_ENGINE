package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.properties.internal.Properties;
import org.mockito.Mockito;
import org.openqa.selenium.*;
import org.openqa.selenium.chromium.ChromiumDriver;
import org.openqa.selenium.devtools.DevToolsException;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.awt.HeadlessException;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.startsWith;
import static org.mockito.Mockito.anyMap;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@Test(singleThreaded = true)
public class BrowserActionsHelperCoverageUnitTest {
    private BrowserActionsHelper helper;
    private WebDriver driver;
    private WebDriver.Navigation navigation;
    private WebDriver.Options options;
    private WebDriver.Window window;

    @BeforeMethod(alwaysRun = true)
    public void setUp() {
        SHAFT.Properties.timeouts.set().waitForLazyLoading(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");

        helper = new BrowserActionsHelper(true);
        driver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
        navigation = mock(WebDriver.Navigation.class);
        options = mock(WebDriver.Options.class);
        window = mock(WebDriver.Window.class);

        when(driver.navigate()).thenReturn(navigation);
        when(driver.manage()).thenReturn(options);
        when(options.window()).thenReturn(window);
        when(window.getSize()).thenReturn(new Dimension(1200, 800));
        doNothing().when(navigation).to(anyString());
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void shouldCoverNavigationAndUrlValidationPaths() {
        when(driver.getCurrentUrl()).thenReturn("https://example.com/start", "https://example.com/target", "https://example.com/target");
        helper.checkNavigationWasSuccessful(driver, "https://example.com/start", "https://example.com/target", "https://example.com/target");

        when(driver.getCurrentUrl()).thenReturn("https://example.com/initial", "https://example.com/final");
        helper.checkNavigationWasSuccessful(driver, "https://example.com/initial", "https://example.com/source", "https://example.com/final");

        helper.navigateToNewUrl(driver, "about:blank", SHAFT.Properties.paths.testData() + "/dummy.html", "");
        verify(navigation).to(startsWith("file://"));
    }

    @Test
    public void shouldCoverWebsiteDownAndFailureReportingBranches() {
        when(driver.getPageSource()).thenReturn("<html>healthy page</html>", "This site can’t be reached");

        helper.confirmThatWebsiteIsNotDown(driver, "https://example.com");

        Assert.assertThrows(RuntimeException.class,
                () -> helper.confirmThatWebsiteIsNotDown(driver, "https://example.com"));
        Assert.assertThrows(RuntimeException.class, helper::failAction);
        Assert.assertThrows(RuntimeException.class,
                () -> helper.failAction(new RuntimeException("forced")));
        Assert.assertThrows(RuntimeException.class,
                () -> helper.failAction(driver, "forced"));
        Assert.assertThrows(RuntimeException.class,
                () -> helper.failAction(driver, "forced", new RuntimeException("forced")));
        Assert.assertThrows(RuntimeException.class,
                () -> helper.failAction(driver, "customAction", "forced"));
        Assert.assertThrows(RuntimeException.class,
                () -> helper.failAction(driver, "customAction", "forced", new RuntimeException("forced")));
    }

    @Test
    public void shouldCoverCaptureSnapshotFallbackBranches() {
        when(driver.getPageSource()).thenReturn("<html>fallback source</html>");
        Assert.assertTrue(helper.capturePageSnapshot(driver).contains("fallback source"));

        WebDriver failingDriver = mock(WebDriver.class);
        when(failingDriver.getPageSource()).thenThrow(new WebDriverException("forced"));
        Assert.assertThrows(RuntimeException.class, () -> helper.capturePageSnapshot(failingDriver));

        ChromiumDriver chromiumDriver = mock(ChromiumDriver.class);
        when(chromiumDriver.executeCdpCommand(eq("Page.captureSnapshot"), anyMap()))
                .thenThrow(new DevToolsException("temporary issue"))
                .thenReturn(Map.of("data", "snapshot-data"));
        Assert.assertEquals(helper.capturePageSnapshot(chromiumDriver), "snapshot-data");
    }

    @Test
    public void shouldCoverBasicAuthHttpAndDomainExtractionPaths() {
        String httpAuthUrl = helper.formatUrlForBasicAuthentication("user", "pass", "http://example.com/secure");
        String httpsAuthUrl = helper.formatUrlForBasicAuthentication("user", "pass", "https://example.com/secure");
        Assert.assertTrue(httpAuthUrl.startsWith("http://user:pass@example.com"));
        Assert.assertTrue(httpsAuthUrl.startsWith("https://user:pass@example.com"));

        Assert.assertEquals(helper.getDomainNameFromUrl("https://www.sub.example.co.uk/path"), "example.co.uk");
    }

    @Test
    public void shouldCoverWindowMaximizeFallbackBranches() {
        Assert.assertEquals(helper.attemptMaximizeUsingSeleniumWebDriver(driver, "remote", "firefox", "Linux"),
                new Dimension(1200, 800));

        Mockito.doThrow(new WebDriverException("forced")).when(window).maximize();
        Assert.assertEquals(helper.attemptMaximizeUsingSeleniumWebDriver(driver, "remote", "firefox", "Linux"),
                new Dimension(1200, 800));

        Mockito.doThrow(new HeadlessException()).when(window).setPosition(Mockito.any(org.openqa.selenium.Point.class));
        when(((JavascriptExecutor) driver).executeScript(anyString())).thenReturn(null);
        Assert.assertEquals(helper.attemptMaximizeUsingToolkitAndJavascript(driver, 1024, 768),
                new Dimension(1200, 800));
    }

    @Test
    public void shouldCoverNonSilentReportingBranches() {
        BrowserActionsHelper nonSilentHelper = new BrowserActionsHelper(false);
        nonSilentHelper.passAction((WebDriver) null, "customAction", "<html></html>");
        nonSilentHelper.passAction((WebDriver) null, "customAction", "short value");
    }
}
