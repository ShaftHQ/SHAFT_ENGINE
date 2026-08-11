package testPackage.unitTests;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.BrowserType;
import com.microsoft.playwright.Page;
import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.capabilities.AutomationCapabilities;
import com.shaft.gui.capabilities.AutomationFeature;
import com.shaft.gui.capabilities.internal.AutomationCapabilityResolver;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.mac.Mac2Driver;
import io.appium.java_client.windows.WindowsDriver;
import io.appium.java_client.remote.SupportsContextSwitching;
import org.openqa.selenium.HasCapabilities;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.bidi.HasBiDi;
import org.openqa.selenium.bidi.BiDi;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.logging.Logs;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Optional;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;
import static org.mockito.Mockito.when;

public class AutomationCapabilityResolverUnitTest {

    @Test
    @SuppressWarnings("removal")
    public void seleniumShouldRequireNegotiatedProtocolObjectsInsteadOfCapabilityNames() {
        boolean originalBiDi = SHAFT.Properties.platform.enableBiDi();
        try {
            SHAFT.Properties.platform.set().enableBiDi(true);
            MutableCapabilities rawCapabilities = new MutableCapabilities();
            rawCapabilities.setCapability("browserName", "chrome");
            rawCapabilities.setCapability("browserVersion", "140");
            rawCapabilities.setCapability("platformName", "windows");
            rawCapabilities.setCapability("webSocketUrl", false);
            WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(
                    HasCapabilities.class, HasBiDi.class, HasDevTools.class));
            when(((HasCapabilities) driver).getCapabilities()).thenReturn(rawCapabilities);
            when(((HasBiDi) driver).maybeGetBiDi()).thenReturn(Optional.of(mock(BiDi.class)));
            when(((HasDevTools) driver).maybeGetDevTools()).thenReturn(Optional.empty());

            AutomationCapabilities capabilities = AutomationCapabilityResolver.forWebDriver(driver);

            Assert.assertEquals(capabilities.backend(), AutomationBackend.SELENIUM_WEBDRIVER);
            Assert.assertFalse(capabilities.supports(AutomationFeature.BIDI));
            Assert.assertFalse(capabilities.supports(AutomationFeature.NETWORK_OBSERVATION));
            Assert.assertFalse(capabilities.supports(AutomationFeature.CONSOLE_LOGS));
            Assert.assertFalse(capabilities.supports(AutomationFeature.NETWORK_INTERCEPTION));
            Assert.assertFalse(capabilities.supports(AutomationFeature.SCRIPT_EXECUTION));

            rawCapabilities.setCapability("webSocketUrl", "");
            Assert.assertFalse(AutomationCapabilityResolver.forWebDriver(driver)
                    .supports(AutomationFeature.BIDI));

            rawCapabilities.setCapability("webSocketUrl", "ws://localhost/session/1");
            SHAFT.Properties.platform.set().enableBiDi(false);
            AutomationCapabilities negotiatedCapabilities = AutomationCapabilityResolver.forWebDriver(driver);
            Assert.assertTrue(negotiatedCapabilities.supports(AutomationFeature.BIDI));
            Assert.assertTrue(negotiatedCapabilities.supports(AutomationFeature.NETWORK_OBSERVATION));
            Assert.assertTrue(negotiatedCapabilities.supports(AutomationFeature.CONSOLE_LOGS));
        } finally {
            SHAFT.Properties.platform.set().enableBiDi(originalBiDi);
            SHAFT.Properties.clearForCurrentThread();
        }
    }

    @Test
    public void genericAppiumDriverShouldAdvertiseOnlyCapabilitiesProvenByItsInterfaces() {
        AppiumDriver driver = mock(AppiumDriver.class);
        when(driver.getCapabilities()).thenReturn(appiumCapabilities("custom", "unknown"));

        AutomationCapabilities capabilities = AutomationCapabilityResolver.forWebDriver(driver);

        Assert.assertEquals(capabilities.backend(), AutomationBackend.APPIUM);
        Assert.assertFalse(capabilities.supports(AutomationFeature.MOBILE_AUTOMATION));
        Assert.assertFalse(capabilities.supports(AutomationFeature.TOUCH_GESTURES));
        Assert.assertFalse(capabilities.supports(AutomationFeature.APP_LIFECYCLE));
        Assert.assertFalse(capabilities.supports(AutomationFeature.DEVICE_CONTROL));
        Assert.assertFalse(capabilities.supports(AutomationFeature.BIOMETRICS));
        Assert.assertFalse(capabilities.supports(AutomationFeature.PERFORMANCE_DATA));
        Assert.assertFalse(capabilities.supports(AutomationFeature.FILE_TRANSFER));
        Assert.assertFalse(capabilities.supports(AutomationFeature.SCREEN_RECORDING));
        Assert.assertFalse(capabilities.supports(AutomationFeature.STORAGE));
    }

    @Test
    public void seleniumConsoleCapabilityShouldFollowTheLiveBrowserLogType() {
        WebDriver driver = mock(WebDriver.class);
        WebDriver.Options options = mock(WebDriver.Options.class);
        Logs logs = mock(Logs.class);
        when(driver.manage()).thenReturn(options);
        when(options.logs()).thenReturn(logs);
        when(logs.getAvailableLogTypes()).thenReturn(java.util.Set.of("browser"));

        Assert.assertTrue(AutomationCapabilityResolver.forWebDriver(driver)
                .supports(AutomationFeature.CONSOLE_LOGS));

        when(logs.getAvailableLogTypes()).thenReturn(java.util.Set.of());
        Assert.assertFalse(AutomationCapabilityResolver.forWebDriver(driver)
                .supports(AutomationFeature.CONSOLE_LOGS));
    }

    @Test
    public void appiumStorageCapabilityShouldFollowTheLiveWebContext() {
        AppiumDriver driver = mock(AppiumDriver.class,
                withSettings().extraInterfaces(SupportsContextSwitching.class));
        SupportsContextSwitching contexts = (SupportsContextSwitching) driver;
        when(driver.getCapabilities()).thenReturn(appiumCapabilities("UiAutomator2", "android"));
        when(contexts.getContext()).thenReturn("NATIVE_APP", "WEBVIEW_com.example");

        Assert.assertFalse(AutomationCapabilityResolver.forWebDriver(driver)
                .supports(AutomationFeature.STORAGE));
        AutomationCapabilities webView = AutomationCapabilityResolver.forWebDriver(driver);
        Assert.assertTrue(webView.supports(AutomationFeature.STORAGE));
        Assert.assertTrue(webView.supports(AutomationFeature.SCRIPT_EXECUTION));
    }

    @Test
    public void appiumProfilesShouldFollowPinnedDriverInterfaces() {
        AndroidDriver android = mock(AndroidDriver.class);
        when(android.getCapabilities()).thenReturn(appiumCapabilities("UiAutomator2", "android"));
        IOSDriver ios = mock(IOSDriver.class);
        when(ios.getCapabilities()).thenReturn(appiumCapabilities("XCUITest", "ios"));
        WindowsDriver windows = mock(WindowsDriver.class);
        when(windows.getCapabilities()).thenReturn(appiumCapabilities("Windows", "windows"));
        Mac2Driver mac = mock(Mac2Driver.class);
        when(mac.getCapabilities()).thenReturn(appiumCapabilities("Mac2", "mac"));

        AutomationCapabilities androidCapabilities = AutomationCapabilityResolver.forWebDriver(android);
        AutomationCapabilities iosCapabilities = AutomationCapabilityResolver.forWebDriver(ios);
        AutomationCapabilities windowsCapabilities = AutomationCapabilityResolver.forWebDriver(windows);
        AutomationCapabilities macCapabilities = AutomationCapabilityResolver.forWebDriver(mac);

        Assert.assertTrue(androidCapabilities.supports(AutomationFeature.PERFORMANCE_DATA));
        Assert.assertTrue(androidCapabilities.supports(AutomationFeature.MOBILE_AUTOMATION));
        Assert.assertTrue(androidCapabilities.supports(AutomationFeature.BIOMETRICS));
        Assert.assertTrue(iosCapabilities.supports(AutomationFeature.MOBILE_AUTOMATION));
        Assert.assertTrue(iosCapabilities.supports(AutomationFeature.BIOMETRICS));
        Assert.assertFalse(iosCapabilities.supports(AutomationFeature.PERFORMANCE_DATA));
        Assert.assertTrue(windowsCapabilities.supports(AutomationFeature.TOUCH_GESTURES));
        Assert.assertTrue(windowsCapabilities.supports(AutomationFeature.FILE_TRANSFER));
        Assert.assertTrue(windowsCapabilities.supports(AutomationFeature.SCREEN_RECORDING));
        Assert.assertFalse(windowsCapabilities.supports(AutomationFeature.MOBILE_AUTOMATION));
        Assert.assertFalse(windowsCapabilities.supports(AutomationFeature.BIOMETRICS));
        Assert.assertFalse(windowsCapabilities.supports(AutomationFeature.DEVICE_CONTROL));
        Assert.assertFalse(macCapabilities.supports(AutomationFeature.MOBILE_AUTOMATION));
    }

    @Test
    @SuppressWarnings("removal")
    public void appiumShouldExposeBiDiOnlyWhenTheLiveChannelIsNegotiated() {
        boolean originalBiDi = SHAFT.Properties.platform.enableBiDi();
        try {
            SHAFT.Properties.platform.set().enableBiDi(false);
            AndroidDriver android = mock(AndroidDriver.class);
            MutableCapabilities rawCapabilities = appiumCapabilities("UiAutomator2", "android");
            rawCapabilities.setCapability("webSocketUrl", "ws://localhost/appium/session/1");
            when(android.getCapabilities()).thenReturn(rawCapabilities);
            when(android.maybeGetBiDi()).thenReturn(Optional.empty());

            Assert.assertFalse(AutomationCapabilityResolver.forWebDriver(android)
                    .supports(AutomationFeature.BIDI));

            when(android.maybeGetBiDi()).thenReturn(Optional.of(mock(BiDi.class)));
            Assert.assertTrue(AutomationCapabilityResolver.forWebDriver(android)
                    .supports(AutomationFeature.BIDI));
        } finally {
            SHAFT.Properties.platform.set().enableBiDi(originalBiDi);
            SHAFT.Properties.clearForCurrentThread();
        }
    }

    @Test
    public void absentOrDisconnectedPlaywrightSessionShouldFailClosed() {
        Assert.assertEquals(AutomationCapabilityResolver.forPlaywright(null).backend(), AutomationBackend.UNKNOWN);

        PlaywrightSession missingPageSession = mock(PlaywrightSession.class);
        Assert.assertEquals(AutomationCapabilityResolver.forPlaywright(missingPageSession).backend(),
                AutomationBackend.UNKNOWN);

        PlaywrightSession closedPageSession = mock(PlaywrightSession.class);
        Page closedPage = mock(Page.class);
        when(closedPageSession.page()).thenReturn(closedPage);
        when(closedPage.isClosed()).thenReturn(true);
        Assert.assertEquals(AutomationCapabilityResolver.forPlaywright(closedPageSession).backend(),
                AutomationBackend.UNKNOWN);

        PlaywrightSession missingContextSession = mock(PlaywrightSession.class);
        Page openPage = mock(Page.class);
        when(missingContextSession.page()).thenReturn(openPage);
        when(openPage.isClosed()).thenReturn(false);
        Assert.assertEquals(AutomationCapabilityResolver.forPlaywright(missingContextSession).backend(),
                AutomationBackend.UNKNOWN);

        PlaywrightSession session = mock(PlaywrightSession.class);
        Browser browser = mock(Browser.class);
        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);
        when(session.browser()).thenReturn(browser);
        when(session.browserContext()).thenReturn(context);
        when(session.page()).thenReturn(page);
        when(page.isClosed()).thenReturn(false);
        when(browser.isConnected()).thenReturn(false);

        AutomationCapabilities capabilities = AutomationCapabilityResolver.forPlaywright(session);

        Assert.assertEquals(capabilities.backend(), AutomationBackend.UNKNOWN);
        Assert.assertFalse(capabilities.supports(AutomationFeature.BROWSER_AUTOMATION));
    }

    @Test
    public void playwrightSessionWithoutBrowserObjectShouldFailClosed() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);
        when(session.browserContext()).thenReturn(context);
        when(session.page()).thenReturn(page);
        when(page.isClosed()).thenReturn(false);

        AutomationCapabilities capabilities = AutomationCapabilityResolver.forPlaywright(session);

        Assert.assertEquals(capabilities.backend(), AutomationBackend.UNKNOWN);
        Assert.assertFalse(capabilities.supports(AutomationFeature.PERMISSIONS));
    }

    @Test
    public void livePlaywrightContextWithoutPageShouldExposeOnlyContextLevelFeatures() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Browser browser = mock(Browser.class);
        BrowserContext context = mock(BrowserContext.class);
        when(session.browser()).thenReturn(browser);
        when(session.browserContext()).thenReturn(context);
        when(browser.isConnected()).thenReturn(true);

        AutomationCapabilities capabilities = AutomationCapabilityResolver.forPlaywright(session);

        Assert.assertEquals(capabilities.backend(), AutomationBackend.MICROSOFT_PLAYWRIGHT);
        Assert.assertTrue(capabilities.supports(AutomationFeature.PERMISSIONS));
        Assert.assertTrue(capabilities.supports(AutomationFeature.STORAGE));
        Assert.assertFalse(capabilities.supports(AutomationFeature.BROWSER_AUTOMATION));
        Assert.assertFalse(capabilities.supports(AutomationFeature.SCRIPT_EXECUTION));

        when(context.isClosed()).thenReturn(true);
        Assert.assertEquals(AutomationCapabilityResolver.forPlaywright(session).backend(), AutomationBackend.UNKNOWN);
    }

    @Test
    public void livePlaywrightSessionShouldExposeItsNativeContracts() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Browser browser = mock(Browser.class);
        BrowserType browserType = mock(BrowserType.class);
        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);
        when(session.browser()).thenReturn(browser);
        when(session.browserContext()).thenReturn(context);
        when(session.page()).thenReturn(page);
        when(browser.isConnected()).thenReturn(true);
        when(browser.browserType()).thenReturn(browserType);
        when(browserType.name()).thenReturn("chromium");
        when(browser.version()).thenReturn("140");
        when(page.isClosed()).thenReturn(false);

        AutomationCapabilities capabilities = AutomationCapabilityResolver.forPlaywright(session);

        Assert.assertEquals(capabilities.backend(), AutomationBackend.MICROSOFT_PLAYWRIGHT);
        Assert.assertTrue(capabilities.supports(AutomationFeature.BROWSER_AUTOMATION));
        Assert.assertTrue(capabilities.supports(AutomationFeature.NETWORK_INTERCEPTION));
        Assert.assertTrue(capabilities.supports(AutomationFeature.TRACE));
    }

    private static MutableCapabilities appiumCapabilities(String automationName, String platformName) {
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("appium:automationName", automationName);
        capabilities.setCapability("platformName", platformName);
        return capabilities;
    }
}
