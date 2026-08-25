package com.shaft.driver.internal.DriverFactory;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.internal.MobileTraceMetadata;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.Platform;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.chromium.ChromiumOptions;
import org.openqa.selenium.edge.EdgeOptions;
import org.openqa.selenium.firefox.FirefoxOptions;
import org.openqa.selenium.ie.InternetExplorerDriver;
import org.openqa.selenium.ie.InternetExplorerOptions;
import org.openqa.selenium.remote.CapabilityType;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.safari.SafariOptions;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Map;

@Test(singleThreaded = true)
public class OptionsManagerCoverageUnitTest {
    private String originalUserHome;
    private Path testUserHome;

    @BeforeMethod(alwaysRun = true)
    public void setUp() throws Exception {
        originalUserHome = System.getProperty("user.home");
        testUserHome = Files.createTempDirectory("options-manager-home-");
        System.setProperty("user.home", testUserHome.toString());

        SHAFT.Properties.platform.set().executionAddress("local").targetPlatform(Platform.LINUX.name()).enableBiDi(false).driverProxySettings(false).proxySettings("");
        SHAFT.Properties.web.set().targetBrowserName("chrome").headlessExecution(false).incognitoMode(false).forceBrowserDownload(false).isMobileEmulation(false).mobileEmulationIsCustomDevice(false).mobileEmulationDeviceName("").mobileEmulationUserAgent("");
        SHAFT.Properties.flags.set().disableCache(false).disableSslCertificateCheck(false).automaticallyAddRecommendedChromeOptions(true).autoCloseDriverInstance(true).autoMaximizeBrowserWindow(true);
        SHAFT.Properties.reporting.set().captureWebDriverLogs(false);
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(false);
        SHAFT.Properties.performance.set().isEnabled(false).port(9222);
        SHAFT.Properties.timeouts.set().pageLoadTimeout(30).scriptExecutionTimeout(30);
        SHAFT.Properties.mobile.set().browserName("").app("").appPackage("").appActivity("");
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        System.setProperty("user.home", originalUserHome);
        Properties.clearForCurrentThread();
    }

    @Test
    public void shouldConfigureFirefoxOptionsAndSeleniumManagerConfig() throws Exception {
        SHAFT.Properties.platform.set().executionAddress("http://localhost:4444").targetPlatform(Platform.WINDOWS.name()).driverProxySettings(true).proxySettings("127.0.0.1:8888").enableBiDi(true);
        SHAFT.Properties.web.set().headlessExecution(true).incognitoMode(true).forceBrowserDownload(true);
        SHAFT.Properties.flags.set().disableCache(true).disableSslCertificateCheck(true);

        OptionsManager manager = new OptionsManager();
        MutableCapabilities customOptions = new MutableCapabilities();
        customOptions.setCapability("customCapability", "value");

        manager.setDriverOptions(DriverFactory.DriverType.FIREFOX, customOptions);

        FirefoxOptions firefoxOptions = manager.getFfOptions();
        Assert.assertNotNull(firefoxOptions);
        Assert.assertEquals(firefoxOptions.getCapability("customCapability"), "value");
        Assert.assertEquals(firefoxOptions.getCapability("acceptInsecureCerts"), true);
        Assert.assertEquals(firefoxOptions.getCapability("webSocketUrl"), false);
        String firefoxOptionsAsString = firefoxOptions.toString();
        Assert.assertTrue(firefoxOptionsAsString.contains("-headless"));
        Assert.assertTrue(firefoxOptionsAsString.contains("-private"));
        Assert.assertEquals(String.valueOf(firefoxOptions.getCapability("browserVersion")), "stable");

        Path seleniumConfigPath = testUserHome.resolve(".cache").resolve("selenium").resolve("se-config.toml");
        String config = Files.readString(seleniumConfigPath, StandardCharsets.UTF_8);
        Assert.assertTrue(config.contains("force-browser-download = true"));
        Assert.assertTrue(config.contains("proxy = \"127.0.0.1:8888\""));

        SHAFT.Properties.web.set().forceBrowserDownload(false);
        SHAFT.Properties.platform.set().proxySettings("");
        manager.setDriverOptions(DriverFactory.DriverType.FIREFOX, new MutableCapabilities());
        String updatedConfig = Files.readString(seleniumConfigPath, StandardCharsets.UTF_8);
        Assert.assertFalse(updatedConfig.contains("force-browser-download"));
        Assert.assertFalse(updatedConfig.contains("proxy = "));
    }

    @Test
    public void shouldConfigureChromiumOptionsAcrossBranches() {
        SHAFT.Properties.platform.set().executionAddress("remote-grid").targetPlatform(Platform.WINDOWS.name()).driverProxySettings(true).proxySettings("10.0.0.1:8080").enableBiDi(true);
        SHAFT.Properties.web.set().targetBrowserName("chrome").headlessExecution(true).incognitoMode(false).isMobileEmulation(true).mobileEmulationIsCustomDevice(true).mobileEmulationWidth(390).mobileEmulationHeight(844).mobileEmulationPixelRatio(3.0).mobileEmulationUserAgent("custom-agent");
        SHAFT.Properties.flags.set().automaticallyAddRecommendedChromeOptions(true).autoCloseDriverInstance(false).autoMaximizeBrowserWindow(false).disableSslCertificateCheck(true);
        SHAFT.Properties.performance.set().isEnabled(true).port(9223);
        SHAFT.Properties.reporting.set().captureWebDriverLogs(true);
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);

        OptionsManager manager = new OptionsManager();
        MutableCapabilities customChromeOptions = new MutableCapabilities();
        customChromeOptions.setCapability("customOption", "customValue");
        manager.setDriverOptions(DriverFactory.DriverType.CHROME, customChromeOptions);

        ChromeOptions chromeOptions = manager.getChOptions();
        Assert.assertNotNull(chromeOptions);
        Assert.assertEquals(chromeOptions.getCapability("customOption"), "customValue");
        String chromeOptionsAsString = chromeOptions.toString();
        Assert.assertTrue(chromeOptionsAsString.contains("--headless"));
        Assert.assertTrue(chromeOptionsAsString.contains("--window-position=0,0"));
        Assert.assertTrue(chromeOptionsAsString.contains("ignore-certificate-errors"));
        Assert.assertNotNull(chromeOptions.getCapability("goog:loggingPrefs"));
        Assert.assertEquals(chromeOptions.getCapability(CapabilityType.ACCEPT_INSECURE_CERTS), true);
        Assert.assertEquals(chromeOptions.getCapability(CapabilityType.ENABLE_DOWNLOADS), true);
        assertSilentChromiumDownloads(chromeOptions);
        Assert.assertEquals(chromeOptions.getCapability("webSocketUrl"), true);
        Assert.assertTrue(chromeOptionsAsString.contains("mobileEmulation"));
        Assert.assertTrue(chromeOptionsAsString.contains("detach"));
        Assert.assertNotNull(chromeOptions.getCapability(CapabilityType.PROXY));
        Assert.assertNull(chromeOptions.getCapability("enableVideo"));
        Assert.assertEquals(chromeOptions.getCapability("se:recordVideo"), true);
        Assert.assertNotNull(chromeOptions.getCapability("selenoid:options"));
        Assert.assertNotNull(chromeOptions.getCapability("moon:options"));

        SHAFT.Properties.web.set().incognitoMode(true);
        SHAFT.Properties.platform.set().enableBiDi(true);
        manager.setDriverOptions(DriverFactory.DriverType.CHROME, new MutableCapabilities());
        Assert.assertTrue(manager.getChOptions().toString().contains("--incognito"));
        Assert.assertFalse(Boolean.parseBoolean(String.valueOf(manager.getChOptions().getCapability("webSocketUrl"))));

        SHAFT.Properties.flags.set().autoCloseDriverInstance(true);
        manager.setDriverOptions(DriverFactory.DriverType.EDGE, new MutableCapabilities());
        EdgeOptions edgeOptions = manager.getEdOptions();
        Assert.assertNotNull(edgeOptions);
        String edgeOptionsAsString = edgeOptions.toString();
        Assert.assertTrue(edgeOptionsAsString.contains("--headless"));
        Assert.assertTrue(edgeOptionsAsString.contains("inPrivate"));
        assertSilentChromiumDownloads(edgeOptions);
    }

    @Test
    public void chromiumAndFirefoxShouldSuppressDownloadPromptToConfiguredDirectoryWhenRecommendedOptionsAreOff() {
        SHAFT.Properties.flags.set().automaticallyAddRecommendedChromeOptions(false);

        OptionsManager manager = new OptionsManager();
        manager.setDriverOptions(DriverFactory.DriverType.CHROME, new MutableCapabilities());
        assertSilentChromiumDownloads(manager.getChOptions());
        Assert.assertFalse(chromiumPrefs(manager.getChOptions()).containsKey("credentials_enable_service"));

        manager.setDriverOptions(DriverFactory.DriverType.EDGE, new MutableCapabilities());
        assertSilentChromiumDownloads(manager.getEdOptions());

        manager.setDriverOptions(DriverFactory.DriverType.FIREFOX, new MutableCapabilities());
        assertSilentFirefoxDownloads(manager.getFfOptions());
    }

    @Test
    public void mobileEmulationProfileShouldExposeResolvedCustomDeviceMetadata() {
        SHAFT.Properties.web.set()
                .isMobileEmulation(true)
                .mobileEmulationIsCustomDevice(true)
                .mobileEmulationWidth(390)
                .mobileEmulationHeight(844)
                .mobileEmulationPixelRatio(3.0)
                .mobileEmulationUserAgent("custom-agent");

        Map<String, String> profile = MobileTraceMetadata.seleniumMobileEmulationProfile("EDGE");

        Assert.assertEquals(profile.get("deviceProfile.mode"), "custom");
        Assert.assertEquals(profile.get("deviceProfile.viewport"), "390x844");
        Assert.assertEquals(profile.get("deviceProfile.deviceScaleFactor"), "3.0");
        Assert.assertEquals(profile.get("deviceProfile.userAgent"), "custom-agent");
        Assert.assertEquals(profile.get("deviceProfile.mobile"), "true");
        Assert.assertEquals(profile.get("deviceProfile.touch"), "true");
    }

    @Test
    public void mobileEmulationProfileShouldPreferLiveBrowserMetadataWhenAvailable() {
        SHAFT.Properties.web.set()
                .isMobileEmulation(true)
                .mobileEmulationIsCustomDevice(false)
                .mobileEmulationDeviceName("Pixel 7");
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class);
        Mockito.when(driver.executeScript("return window.innerWidth + 'x' + window.innerHeight;"))
                .thenReturn("412x915");
        Mockito.when(driver.executeScript("return String(window.devicePixelRatio || 1);"))
                .thenReturn("2.625");
        Mockito.when(driver.executeScript("return navigator.userAgent || '';"))
                .thenReturn("Mozilla/5.0 Pixel 7 Mobile");
        Mockito.when(driver.executeScript("return String((navigator.maxTouchPoints || 0) > 0);"))
                .thenReturn("true");
        Mockito.when(driver.executeScript("return String(/Mobi|Android|iPhone|iPad/i.test(navigator.userAgent || ''));"))
                .thenReturn("true");

        Map<String, String> profile = MobileTraceMetadata.seleniumMobileEmulationProfile(driver, "CHROME");

        Assert.assertEquals(profile.get("deviceProfile.viewport"), "412x915");
        Assert.assertEquals(profile.get("deviceProfile.deviceScaleFactor"), "2.625");
        Assert.assertEquals(profile.get("deviceProfile.userAgent"), "Mozilla/5.0 Pixel 7 Mobile");
        Assert.assertEquals(profile.get("deviceProfile.touch"), "true");
        Assert.assertEquals(profile.get("deviceProfile.mobile"), "true");
    }

    @Test
    public void shouldConfigureIeAndSafariOptions() {
        SHAFT.Properties.platform.set().executionAddress("remote").targetPlatform(Platform.MAC.name()).driverProxySettings(true).proxySettings("proxy-server:3128");
        SHAFT.Properties.flags.set().disableCache(true).disableSslCertificateCheck(true);

        OptionsManager manager = new OptionsManager();
        MutableCapabilities customOptions = new MutableCapabilities();
        customOptions.setCapability("customFlag", true);

        manager.setDriverOptions(DriverFactory.DriverType.IE, customOptions);
        InternetExplorerOptions ieOptions = manager.getIeOptions();
        Assert.assertNotNull(ieOptions);
        Assert.assertTrue(ieOptions.toString().contains(String.valueOf(InternetExplorerDriver.IE_ENSURE_CLEAN_SESSION)));
        Assert.assertTrue(ieOptions.toString().contains("applicationCacheEnabled"));
        Assert.assertEquals(ieOptions.getCapability("customFlag"), true);
        Assert.assertNotNull(ieOptions.getCapability(CapabilityType.PROXY));

        manager.setDriverOptions(DriverFactory.DriverType.SAFARI, customOptions);
        SafariOptions safariOptions = manager.getSfOptions();
        Assert.assertNotNull(safariOptions);
        Assert.assertEquals(safariOptions.getCapability("safari:cleanSession"), "true");
        Assert.assertEquals(safariOptions.getCapability("acceptInsecureCerts"), true);
        Assert.assertEquals(safariOptions.getCapability("customFlag"), true);
        Assert.assertNotNull(safariOptions.getCapability(CapabilityType.PROXY));
    }

    @Test
    public void shouldInitializeAppiumCapabilitiesForNativeAndMobileWeb() {
        SHAFT.Properties.platform.set().targetPlatform("android");
        SHAFT.Properties.mobile.set().browserName("");
        ThreadLocalPropertiesManager.setProperty("mobile_app", "bs://demo-app");
        ThreadLocalPropertiesManager.setProperty("mobile_autoAcceptAlerts", "true");
        ThreadLocalPropertiesManager.setProperty("mobile_systemPort", "8200");
        ThreadLocalPropertiesManager.setProperty("mobile_buildLabel", "nightly");

        OptionsManager manager = new OptionsManager();
        manager.setDriverOptions(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE, new MutableCapabilities());
        manager.initializeMobileDesiredCapabilities();

        Assert.assertEquals(manager.getAppiumCapabilities().getPlatformName(), Platform.ANDROID);
        Assert.assertEquals(manager.getAppiumCapabilities().getCapability("appium:autoAcceptAlerts"), true);
        Assert.assertEquals(manager.getAppiumCapabilities().getCapability("appium:systemPort"), 8200);
        Assert.assertEquals(manager.getAppiumCapabilities().getCapability("appium:buildLabel"), "nightly");
        Assert.assertEquals(manager.getAppiumCapabilities().getCapability("appium:appWaitActivity"), "*");

        SHAFT.Properties.mobile.set().browserName("chrome");
        manager.setDriverOptions(DriverFactory.DriverType.APPIUM_CHROME, new MutableCapabilities());
        manager.initializeMobileDesiredCapabilities();
        Assert.assertEquals(manager.getAppiumCapabilities().getBrowserName(), "chrome");
        Assert.assertNotNull(manager.getAppiumCapabilities().getCapability("pageLoadStrategy"));

        SHAFT.Properties.platform.set().targetPlatform("ios");
        SHAFT.Properties.mobile.set().browserName("");
        manager.setDriverOptions(DriverFactory.DriverType.APPIUM_FLUTTER, new MutableCapabilities());
        manager.initializeMobileDesiredCapabilities();
        Assert.assertEquals(manager.getAppiumCapabilities().getPlatformName(), Platform.IOS);

        SHAFT.Properties.platform.set().targetPlatform("windows");
        SHAFT.Properties.web.set().targetBrowserName("WindowsApp");
        SHAFT.Properties.mobile.set().automationName("Windows").app("C:\\Windows\\System32\\notepad.exe");
        manager.setDriverOptions(DriverFactory.DriverType.APPIUM_WINDOWS, new MutableCapabilities());
        manager.initializeMobileDesiredCapabilities();
        Assert.assertEquals(manager.getAppiumCapabilities().getPlatformName(), Platform.WINDOWS);
        Assert.assertEquals(String.valueOf(manager.getAppiumCapabilities().getCapability(CapabilityType.PLATFORM_NAME)).toLowerCase(), "windows");
        Assert.assertEquals(manager.getAppiumCapabilities().getCapability("appium:automationName"), "Windows");
        Assert.assertEquals(manager.getAppiumCapabilities().getCapability("appium:app"), "C:\\Windows\\System32\\notepad.exe");
    }

    @Test
    public void shouldBuildTypedFlutterDriverOptionsForFlutterSessions() {
        SHAFT.Properties.platform.set().targetPlatform("android");
        SHAFT.Properties.mobile.set().browserName("")
                .flutterElementWaitTimeout(5)
                .flutterServerLaunchTimeout(20)
                .flutterSystemPort(8300)
                .flutterEnableMockCamera(true);

        OptionsManager manager = new OptionsManager();
        manager.setDriverOptions(DriverFactory.DriverType.APPIUM_FLUTTER, new MutableCapabilities());

        Assert.assertEquals(manager.getAppiumCapabilities().getCapability("appium:flutterElementWaitTimeout"), Duration.ofSeconds(5).toMillis());
        Assert.assertEquals(manager.getAppiumCapabilities().getCapability("appium:flutterServerLaunchTimeout"), Duration.ofSeconds(20).toMillis());
        Assert.assertEquals(manager.getAppiumCapabilities().getCapability("appium:flutterSystemPort"), 8300);
        Assert.assertEquals(manager.getAppiumCapabilities().getCapability("appium:flutterEnableMockCamera"), true);
    }

    @Test
    public void shouldBuildFlutterCapabilitiesWhenCustomDriverOptionsIsNull() {
        SHAFT.Properties.platform.set().targetPlatform("android");
        SHAFT.Properties.mobile.set().browserName("");

        OptionsManager manager = new OptionsManager();
        manager.setDriverOptions(DriverFactory.DriverType.APPIUM_FLUTTER, null);

        Assert.assertNotNull(manager.getAppiumCapabilities());
    }

    private static void assertSilentChromiumDownloads(ChromiumOptions<?> options) {
        Assert.assertEquals(options.getCapability(CapabilityType.ENABLE_DOWNLOADS), true);
        Map<String, Object> prefs = chromiumPrefs(options);
        Assert.assertEquals(prefs.get("download.prompt_for_download"), Boolean.FALSE);
        Assert.assertEquals(prefs.get("download.directory_upgrade"), Boolean.TRUE);
        Assert.assertEquals(prefs.get("profile.default_content_setting_values.automatic_downloads"), 1);
        Assert.assertEquals(String.valueOf(prefs.get("download.default_directory")), expectedDownloadsDirectory());
        Assert.assertFalse(String.valueOf(prefs.get("download.default_directory")).endsWith(File.separator));
    }

    private static void assertSilentFirefoxDownloads(FirefoxOptions options) {
        Map<String, Object> prefs = firefoxPrefs(options);
        Assert.assertEquals(prefs.get("browser.download.folderList"), 2);
        Assert.assertEquals(String.valueOf(prefs.get("browser.download.dir")), expectedDownloadsDirectory());
        Assert.assertEquals(prefs.get("browser.download.useDownloadDir"), Boolean.TRUE);
        Assert.assertEquals(prefs.get("browser.download.alwaysOpenPanel"), Boolean.FALSE);
        Assert.assertEquals(prefs.get("browser.download.always_ask_before_handling_new_types"), Boolean.FALSE);
        Assert.assertEquals(prefs.get("browser.download.manager.showWhenStarting"), Boolean.FALSE);
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> chromiumPrefs(ChromiumOptions<?> options) {
        Object chromeOptions = options.getCapability("goog:chromeOptions");
        if (!(chromeOptions instanceof Map<?, ?>)) {
            chromeOptions = options.getCapability("ms:edgeOptions");
        }
        Assert.assertTrue(chromeOptions instanceof Map, "Chromium options map missing: " + chromeOptions);
        Object prefs = ((Map<String, Object>) chromeOptions).get("prefs");
        Assert.assertTrue(prefs instanceof Map, "Chromium prefs missing: " + prefs);
        return (Map<String, Object>) prefs;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> firefoxPrefs(FirefoxOptions options) {
        Object firefoxOptions = options.getCapability("moz:firefoxOptions");
        Assert.assertTrue(firefoxOptions instanceof Map, "moz:firefoxOptions missing: " + firefoxOptions);
        Object prefs = ((Map<String, Object>) firefoxOptions).get("prefs");
        Assert.assertTrue(prefs instanceof Map, "Firefox prefs missing: " + prefs);
        return (Map<String, Object>) prefs;
    }

    private static String expectedDownloadsDirectory() {
        String configured = SHAFT.Properties.paths.downloads().replace("/", File.separator);
        String directory = System.getProperty("user.dir") + File.separatorChar + configured;
        while (directory.endsWith(File.separator) && directory.length() > 1) {
            directory = directory.substring(0, directory.length() - 1);
        }
        return directory;
    }

}
