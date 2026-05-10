package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.OptionsManager;
import io.appium.java_client.android.AndroidDriver;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.ImmutableCapabilities;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.devtools.Command;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.DevToolsException;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.edge.EdgeDriver;
import org.openqa.selenium.edge.EdgeOptions;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.firefox.FirefoxOptions;
import org.openqa.selenium.ie.InternetExplorerDriver;
import org.openqa.selenium.logging.LogEntries;
import org.openqa.selenium.logging.LogEntry;
import org.openqa.selenium.logging.Logs;
import org.openqa.selenium.remote.DesiredCapabilities;
import org.openqa.selenium.remote.http.HttpResponse;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.safari.SafariOptions;
import org.openqa.selenium.safari.SafariDriver;
import io.github.bonigarcia.wdm.WebDriverManager;
import org.mockito.MockedConstruction;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;

@Test(singleThreaded = true)
public class DriverFactoryHelperAdditionalUnitTest {
    private final boolean savedDisableCache = SHAFT.Properties.flags.disableCache();
    private final boolean savedAutoMaximizeBrowserWindow = SHAFT.Properties.flags.autoMaximizeBrowserWindow();
    private final String savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
    private final String savedTargetPlatform = SHAFT.Properties.platform.targetPlatform();
    private final String savedMobileBrowserName = SHAFT.Properties.mobile.browserName();
    private final String savedMobileAutomationName = SHAFT.Properties.mobile.automationName();
    private final boolean savedCaptureWebDriverLogs = SHAFT.Properties.reporting.captureWebDriverLogs();

    @AfterMethod(alwaysRun = true)
    public void cleanup() throws Exception {
        setKillSwitch(false);
        SHAFT.Properties.reporting.set().captureWebDriverLogs(savedCaptureWebDriverLogs);
        SHAFT.Properties.mobile.set()
                .browserName(savedMobileBrowserName)
                .automationName(savedMobileAutomationName);
        SHAFT.Properties.platform.set()
                .executionAddress(savedExecutionAddress)
                .targetPlatform(savedTargetPlatform)
                .enableBiDi(true);
        SHAFT.Properties.flags.set()
                .disableCache(savedDisableCache)
                .autoMaximizeBrowserWindow(savedAutoMaximizeBrowserWindow);
        Properties.clearForCurrentThread();
    }

    private static void setKillSwitch(boolean value) throws Exception {
        Field killSwitchField = DriverFactoryHelper.class.getDeclaredField("killSwitch");
        killSwitchField.setAccessible(true);
        killSwitchField.setBoolean(null, value);
    }

    @Test
    public void mobileExecutionGuardsShouldReflectPlatformAndBrowserProperties() {
        SHAFT.Properties.platform.set().targetPlatform("Android");
        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Validations.assertThat().object(DriverFactoryHelper.isMobileNativeExecution()).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(DriverFactoryHelper.isMobileWebExecution()).isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(DriverFactoryHelper.isNotMobileExecution()).isEqualTo(false).perform();

        SHAFT.Properties.mobile.set().browserName("chrome");
        SHAFT.Validations.assertThat().object(DriverFactoryHelper.isMobileNativeExecution()).isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(DriverFactoryHelper.isMobileWebExecution()).isEqualTo(true).perform();
    }

    @Test
    public void getDriverTypeFromNameAndNormalizeShouldHandleEdgeCases() throws Exception {
        Method getDriverType = DriverFactoryHelper.class.getDeclaredMethod("getDriverTypeFromName", String.class);
        getDriverType.setAccessible(true);
        Object chrome = getDriverType.invoke(null, " chrome ");
        SHAFT.Validations.assertThat().object(chrome.toString()).isEqualTo("CHROME").perform();

        Method normalize = DriverFactoryHelper.class.getDeclaredMethod("normalizeRemoteServerPingBaseUrl", String.class);
        normalize.setAccessible(true);
        SHAFT.Validations.assertThat().object(normalize.invoke(null, "localhost:4444/wd/hub"))
                .isEqualTo("http://localhost:4444/wd/hub/").perform();
        try {
            normalize.invoke(null, "http://bad host");
            throw new AssertionError("Expected MalformedURLException but no exception was thrown.");
        } catch (InvocationTargetException invocationTargetException) {
            SHAFT.Validations.assertThat().object(invocationTargetException.getCause() instanceof java.net.MalformedURLException)
                    .isEqualTo(true).perform();
        }
    }

    @Test
    public void redactAndCaptureScreenshotHelpersShouldHandleSuccessAndFailure() throws Exception {
        Method redact = DriverFactoryHelper.class.getDeclaredMethod("redactUriCredentials", String.class);
        redact.setAccessible(true);
        SHAFT.Validations.assertThat().object(redact.invoke(null, "https://user:key@example.com/wd/hub"))
                .isEqualTo("https://***:***@example.com/wd/hub").perform();
        SHAFT.Validations.assertThat().object(redact.invoke(null, "not-a-valid-uri%%%")).isEqualTo("not-a-valid-uri%%%").perform();

        DriverFactoryHelper helper = new DriverFactoryHelper();
        Method capture = DriverFactoryHelper.class.getDeclaredMethod("captureLaunchScreenshot");
        capture.setAccessible(true);

        SHAFT.Properties.platform.set().targetPlatform("android");
        SHAFT.Properties.mobile.set().browserName("");
        WebDriver screenshotDriver = org.mockito.Mockito.mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        org.mockito.Mockito.when(((TakesScreenshot) screenshotDriver).getScreenshotAs(OutputType.BYTES)).thenReturn("x".getBytes());
        helper.setDriver(screenshotDriver);
        Object success = capture.invoke(helper);
        SHAFT.Validations.assertThat().object(success instanceof List).isEqualTo(true).perform();

        WebDriver emptyScreenshotDriver = org.mockito.Mockito.mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        org.mockito.Mockito.when(((TakesScreenshot) emptyScreenshotDriver).getScreenshotAs(OutputType.BYTES)).thenReturn(new byte[0]);
        helper.setDriver(emptyScreenshotDriver);
        SHAFT.Validations.assertThat().object(capture.invoke(helper)).isNull().perform();

        SHAFT.Properties.mobile.set().browserName("chrome");
        helper.setDriver(screenshotDriver);
        SHAFT.Validations.assertThat().object(capture.invoke(helper)).isNull().perform();
    }

    @Test
    public void closeDriverShouldHandleNullAndDriverExceptions() {
        SHAFT.Properties.platform.set().executionAddress("local");
        DriverFactoryHelper helper = new DriverFactoryHelper();
        helper.closeDriver(null);

        WebDriver driver = org.mockito.Mockito.mock(WebDriver.class);
        org.mockito.Mockito.doThrow(new RuntimeException("close fail")).when(driver).close();
        helper.closeDriver(driver);
        org.mockito.Mockito.verify(driver).close();
        org.mockito.Mockito.verify(driver).quit();
        helper.setDriver(driver);
        helper.closeDriver();
        SHAFT.Validations.assertThat().object(helper.getDriver()).isNull().perform();
    }

    @Test
    public void initializeDriverWithExistingDriverShouldAttachReference() {
        DriverFactoryHelper helper = new DriverFactoryHelper();
        WebDriver driver = org.mockito.Mockito.mock(WebDriver.class);
        helper.initializeDriver(driver);
        SHAFT.Validations.assertThat().object(helper.getDriver()).isEqualTo(driver).perform();
    }

    @Test
    public void initializeDriverWrappersShouldResolveDriverTypeAndDelegate() {
        TestableDriverFactoryHelper helper = new TestableDriverFactoryHelper();
        SHAFT.Properties.web.set().targetBrowserName("chrome");
        SHAFT.Properties.mobile.set().browserName("");
        helper.initializeDriver();
        SHAFT.Validations.assertThat().object(helper.capturedDriverType.toString()).isEqualTo("CHROME").perform();

        helper.initializeDriver(DriverFactoryHelper.getTargetBrowserName().isBlank()
                ? com.shaft.driver.DriverFactory.DriverType.CHROME
                : com.shaft.driver.DriverFactory.DriverType.valueOf(DriverFactoryHelper.getTargetBrowserName().toUpperCase()));
        SHAFT.Validations.assertThat().object(helper.capturedDriverType.toString()).isEqualTo("CHROME").perform();

        MutableCapabilities customCaps = new MutableCapabilities();
        customCaps.setCapability("sample", "value");
        helper.initializeDriver(customCaps);
        SHAFT.Validations.assertThat().object(helper.capturedCustomDriverOptions).isEqualTo(customCaps).perform();
    }

    @Test
    public void attachWebDriverLogsShouldAttachAndHandleUnsupportedLogs() throws Exception {
        SHAFT.Properties.reporting.set().captureWebDriverLogs(true);
        DriverFactoryHelper helper = new DriverFactoryHelper();
        WebDriver driver = org.mockito.Mockito.mock(WebDriver.class);
        WebDriver.Options options = org.mockito.Mockito.mock(WebDriver.Options.class);
        Logs logs = org.mockito.Mockito.mock(Logs.class);
        Set<String> availableLogTypes = new HashSet<>();
        availableLogTypes.add("browser");
        LogEntries entries = new LogEntries(Collections.singletonList(new LogEntry(Level.INFO, System.currentTimeMillis(), "sample log")));

        org.mockito.Mockito.when(driver.manage()).thenReturn(options);
        org.mockito.Mockito.when(options.logs()).thenReturn(logs);
        org.mockito.Mockito.when(logs.getAvailableLogTypes()).thenReturn(availableLogTypes);
        org.mockito.Mockito.when(logs.get("browser")).thenReturn(entries);
        helper.setDriver(driver);

        Method attachWebDriverLogs = DriverFactoryHelper.class.getDeclaredMethod("attachWebDriverLogs");
        attachWebDriverLogs.setAccessible(true);
        attachWebDriverLogs.invoke(helper);

        org.mockito.Mockito.when(logs.getAvailableLogTypes()).thenThrow(new org.openqa.selenium.WebDriverException("unsupported"));
        attachWebDriverLogs.invoke(helper);
    }

    @Test
    public void connectToRemoteServerAndRemoteConnectionShouldHandleMalformedAndroidHubUrl() throws Exception {
        SHAFT.Properties.platform.set().targetPlatform("android");
        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Properties.platform.set().executionAddress("http://bad host");
        DriverFactoryHelper.initializeSystemProperties();

        Method connectToRemoteServer = DriverFactoryHelper.class.getDeclaredMethod("connectToRemoteServer", org.openqa.selenium.Capabilities.class);
        connectToRemoteServer.setAccessible(true);
        try {
            connectToRemoteServer.invoke(null, new MutableCapabilities());
            throw new AssertionError("Expected exception from connectToRemoteServer but no exception was thrown.");
        } catch (InvocationTargetException invocationTargetException) {
            SHAFT.Validations.assertThat().object(invocationTargetException.getCause() != null).isEqualTo(true).perform();
        }

        Method attemptRemoteServerConnection = DriverFactoryHelper.class.getDeclaredMethod("attemptRemoteServerConnection", org.openqa.selenium.Capabilities.class);
        attemptRemoteServerConnection.setAccessible(true);
        try {
            attemptRemoteServerConnection.invoke(null, new MutableCapabilities());
            throw new AssertionError("Expected exception from attemptRemoteServerConnection but no exception was thrown.");
        } catch (InvocationTargetException invocationTargetException) {
            SHAFT.Validations.assertThat().object(invocationTargetException.getCause() != null).isEqualTo(true).perform();
        }
    }

    @Test
    public void setRemoteDriverInstanceAndCreateRemoteDriverShouldHandleMalformedRemoteAddress() throws Exception {
        SHAFT.Properties.platform.set().targetPlatform("android");
        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Properties.platform.set().executionAddress("http://bad host");
        SHAFT.Properties.timeouts.set().waitForRemoteServerToBeUp(false);
        DriverFactoryHelper.initializeSystemProperties();

        DriverFactoryHelper helper = new DriverFactoryHelper();
        Method setRemoteDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("setRemoteDriverInstance", org.openqa.selenium.Capabilities.class);
        setRemoteDriverInstance.setAccessible(true);
        try {
            setRemoteDriverInstance.invoke(helper, new MutableCapabilities());
            throw new AssertionError("Expected exception from setRemoteDriverInstance but no exception was thrown.");
        } catch (InvocationTargetException invocationTargetException) {
            SHAFT.Validations.assertThat().object(invocationTargetException.getCause() != null).isEqualTo(true).perform();
        }

        Method createNewRemoteDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("createNewRemoteDriverInstance", com.shaft.driver.DriverFactory.DriverType.class);
        createNewRemoteDriverInstance.setAccessible(true);
        try {
            createNewRemoteDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.APPIUM_MOBILE_NATIVE);
            throw new AssertionError("Expected exception from createNewRemoteDriverInstance but no exception was thrown.");
        } catch (InvocationTargetException invocationTargetException) {
            SHAFT.Validations.assertThat().object(invocationTargetException.getCause() != null).isEqualTo(true).perform();
        }
    }

    @Test
    public void createNewLocalDriverInstanceShouldHandleKnownErrorMessages() throws Exception {
        DriverFactoryHelper helper = new DriverFactoryHelper();
        OptionsManager mockedOptionsManager = org.mockito.Mockito.mock(OptionsManager.class);
        ChromeOptions chromeOptions = new ChromeOptions();
        org.mockito.Mockito.when(mockedOptionsManager.getChOptions())
                .thenThrow(new RuntimeException("Failed to initialize BiDi Mapper"))
                .thenThrow(new RuntimeException("DevToolsActivePort file doesn't exist"))
                .thenReturn(chromeOptions);

        Field optionsManagerField = DriverFactoryHelper.class.getDeclaredField("optionsManager");
        optionsManagerField.setAccessible(true);
        optionsManagerField.set(helper, mockedOptionsManager);

        Method createNewLocalDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("createNewLocalDriverInstance", com.shaft.driver.DriverFactory.DriverType.class, int.class);
        createNewLocalDriverInstance.setAccessible(true);
        SHAFT.Properties.platform.set().targetPlatform("windows");

        try {
            createNewLocalDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.CHROME, 0);
        } catch (InvocationTargetException ignored) {
            // expected due failAction
        }

        SHAFT.Validations.assertThat().object(SHAFT.Properties.platform.enableBiDi()).isEqualTo(false).perform();
        org.mockito.Mockito.verify(mockedOptionsManager, org.mockito.Mockito.atLeast(2)).getChOptions();
    }

    @Test
    public void disableCacheEdgeAndChromeShouldSendDevToolsCommandsAndHandleDevToolsException() throws Exception {
        SHAFT.Properties.flags.set().disableCache(true);
        DriverFactoryHelper helper = new DriverFactoryHelper();
        WebDriver driver = org.mockito.Mockito.mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(HasDevTools.class));
        DevTools devTools = org.mockito.Mockito.mock(DevTools.class);
        org.mockito.Mockito.when(((HasDevTools) driver).getDevTools()).thenReturn(devTools);
        org.mockito.Mockito.when(devTools.send(org.mockito.ArgumentMatchers.any(Command.class))).thenReturn(new HttpResponse().setStatus(200));
        helper.setDriver(driver);

        Method disableCacheEdgeAndChrome = DriverFactoryHelper.class.getDeclaredMethod("disableCacheEdgeAndChrome");
        disableCacheEdgeAndChrome.setAccessible(true);
        disableCacheEdgeAndChrome.invoke(helper);

        org.mockito.Mockito.when(devTools.send(org.mockito.ArgumentMatchers.any(Command.class)))
                .thenThrow(new DevToolsException("forced devtools failure", new RuntimeException("forced")));
        disableCacheEdgeAndChrome.invoke(helper);
    }

    @Test
    public void createNewDockerizedDriverInstanceShouldFailFastForUnsupportedDriverType() throws Exception {
        DriverFactoryHelper helper = new DriverFactoryHelper();
        Method createNewDockerizedDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("createNewDockerizedDriverInstance", com.shaft.driver.DriverFactory.DriverType.class);
        createNewDockerizedDriverInstance.setAccessible(true);
        try {
            createNewDockerizedDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.IE);
            throw new AssertionError("Expected exception from createNewDockerizedDriverInstance but no exception was thrown.");
        } catch (InvocationTargetException invocationTargetException) {
            SHAFT.Validations.assertThat().object(invocationTargetException.getCause() != null).isEqualTo(true).perform();
        }
    }

    @Test
    public void closeDriverShouldHandleDockerizedExecutionBranch() {
        SHAFT.Properties.platform.set().executionAddress("dockerized");
        DriverFactoryHelper helper = new DriverFactoryHelper();
        WebDriver driver = org.mockito.Mockito.mock(WebDriver.class);
        helper.closeDriver(driver);
        org.mockito.Mockito.verify(driver, org.mockito.Mockito.never()).quit();
    }

    @Test
    public void initializeDriverWithExplicitTypeShouldHandleMobileWebAndFlutterPaths() {
        DriverFactoryHelper helper = new DriverFactoryHelper();
        SHAFT.Properties.platform.set().targetPlatform("android");
        SHAFT.Properties.platform.set().executionAddress("http://bad host");
        SHAFT.Properties.timeouts.set().waitForRemoteServerToBeUp(false);

        SHAFT.Properties.mobile.set().browserName("chrome");
        try {
            helper.initializeDriver(com.shaft.driver.DriverFactory.DriverType.CHROME, new MutableCapabilities());
        } catch (Throwable ignored) {
            // expected due malformed hub URL
        }

        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Properties.mobile.set().automationName("flutter integration");
        try {
            helper.initializeDriver(com.shaft.driver.DriverFactory.DriverType.CHROME, new MutableCapabilities());
        } catch (Throwable ignored) {
            // expected due malformed hub URL
        }
    }

    @Test
    public void configureRemoteDriverInstanceShouldRouteToExpectedBranches() throws Exception {
        SHAFT.Properties.platform.set().executionAddress("http://bad host");
        SHAFT.Properties.timeouts.set().waitForRemoteServerToBeUp(false);
        DriverFactoryHelper.initializeSystemProperties();
        DriverFactoryHelper helper = new DriverFactoryHelper();
        Method configureRemoteDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("configureRemoteDriverInstance", com.shaft.driver.DriverFactory.DriverType.class, DesiredCapabilities.class);
        configureRemoteDriverInstance.setAccessible(true);

        SHAFT.Properties.platform.set().targetPlatform("android");
        try {
            configureRemoteDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.SAFARI, new DesiredCapabilities());
        } catch (InvocationTargetException ignored) {
            // expected due malformed hub URL
        }

        SHAFT.Properties.platform.set().targetPlatform("windows");
        try {
            configureRemoteDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.CHROME, new DesiredCapabilities());
        } catch (InvocationTargetException ignored) {
            // expected due malformed hub URL
        }

        try {
            configureRemoteDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.APPIUM_MOBILE_NATIVE, new DesiredCapabilities());
        } catch (InvocationTargetException ignored) {
            // expected due malformed hub URL
        }
    }

    @Test
    public void initializeDriverShouldCoverRemoteDesktopSuccessPathUsingMockedRemoteWebDriverConstruction() {
        SHAFT.Properties.platform.set().targetPlatform("windows");
        SHAFT.Properties.platform.set().executionAddress("http://localhost:4444");
        SHAFT.Properties.web.set().targetBrowserName("safari").headlessExecution(false);
        SHAFT.Properties.flags.set().autoMaximizeBrowserWindow(false);
        SHAFT.Properties.healenium.set().healEnabled(false);
        SHAFT.Properties.timeouts.set().waitForRemoteServerToBeUp(false);

        DriverFactoryHelper helper = new DriverFactoryHelper();
        try (MockedConstruction<RemoteWebDriver> ignored = org.mockito.Mockito.mockConstruction(RemoteWebDriver.class,
                (mock, context) -> org.mockito.Mockito.when(mock.getCapabilities()).thenReturn(new ImmutableCapabilities()))) {
            helper.initializeDriver(com.shaft.driver.DriverFactory.DriverType.CHROME, null);
            SHAFT.Validations.assertThat().object(helper.getDriver()).isNotNull().perform();
        }
    }

    @Test
    public void initializeDriverShouldCoverRemoteMobileNativeSuccessPathUsingMockedAndroidDriverConstruction() {
        SHAFT.Properties.platform.set().targetPlatform("android");
        SHAFT.Properties.platform.set().executionAddress("http://localhost:4723");
        SHAFT.Properties.mobile.set().browserName("").automationName("UIAutomator2");
        SHAFT.Properties.healenium.set().healEnabled(false);
        SHAFT.Properties.timeouts.set().waitForRemoteServerToBeUp(false);

        DriverFactoryHelper helper = new DriverFactoryHelper();
        try (MockedConstruction<AndroidDriver> ignored = org.mockito.Mockito.mockConstruction(AndroidDriver.class)) {
            helper.initializeDriver(com.shaft.driver.DriverFactory.DriverType.APPIUM_MOBILE_NATIVE, null);
            SHAFT.Validations.assertThat().object(helper.getDriver()).isNotNull().perform();
        }
    }

    @Test
    public void createNewLocalDriverInstanceShouldCoverSuccessfulDriverBranchesUsingMockedConstructions() throws Exception {
        SHAFT.Properties.platform.set().targetPlatform("windows");
        SHAFT.Properties.web.set().headlessExecution(false);
        SHAFT.Properties.flags.set().disableCache(false);
        DriverFactoryHelper helper = new DriverFactoryHelper();
        Method createNewLocalDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("createNewLocalDriverInstance", com.shaft.driver.DriverFactory.DriverType.class, int.class);
        createNewLocalDriverInstance.setAccessible(true);

        try (MockedConstruction<ChromeDriver> ignoredChrome = org.mockito.Mockito.mockConstruction(ChromeDriver.class);
             MockedConstruction<EdgeDriver> ignoredEdge = org.mockito.Mockito.mockConstruction(EdgeDriver.class);
             MockedConstruction<FirefoxDriver> ignoredFirefox = org.mockito.Mockito.mockConstruction(FirefoxDriver.class);
             MockedConstruction<InternetExplorerDriver> ignoredIe = org.mockito.Mockito.mockConstruction(InternetExplorerDriver.class);
             MockedConstruction<SafariDriver> ignoredSafari = org.mockito.Mockito.mockConstruction(SafariDriver.class)) {
            createNewLocalDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.CHROME, 0);
            createNewLocalDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.EDGE, 0);
            createNewLocalDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.FIREFOX, 0);
            createNewLocalDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.IE, 0);
            createNewLocalDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.SAFARI, 0);
            SHAFT.Validations.assertThat().object(helper.getDriver()).isNotNull().perform();
        }
    }

    @Test
    public void createNewDockerizedDriverInstanceShouldCoverSuccessfulChromeBranchWithMockedWebDriverManager() throws Exception {
        SHAFT.Properties.platform.set().targetPlatform("windows");
        DriverFactoryHelper helper = new DriverFactoryHelper();
        Method createNewDockerizedDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("createNewDockerizedDriverInstance", com.shaft.driver.DriverFactory.DriverType.class);
        createNewDockerizedDriverInstance.setAccessible(true);
        WebDriverManager mockedManager = org.mockito.Mockito.mock(WebDriverManager.class, org.mockito.Mockito.RETURNS_SELF);
        RemoteWebDriver mockedRemoteDriver = org.mockito.Mockito.mock(RemoteWebDriver.class);
        org.mockito.Mockito.when(mockedManager.create()).thenReturn(mockedRemoteDriver);

        try (org.mockito.MockedStatic<WebDriverManager> webDriverManagerMockedStatic = org.mockito.Mockito.mockStatic(WebDriverManager.class)) {
            webDriverManagerMockedStatic.when(WebDriverManager::chromedriver).thenReturn(mockedManager);
            createNewDockerizedDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.CHROME);
            SHAFT.Validations.assertThat().object(helper.getDriver()).isEqualTo(mockedRemoteDriver).perform();
        }
    }

    @Test
    public void initializeDriverShouldCoverDesktopLocalHeadlessAndBrowserSpecificWindowBranchesWithMockedDrivers() {
        SHAFT.Properties.platform.set().targetPlatform("windows");
        SHAFT.Properties.platform.set().executionAddress("local");
        SHAFT.Properties.healenium.set().healEnabled(false);
        SHAFT.Properties.flags.set().disableCache(false).autoMaximizeBrowserWindow(true);
        SHAFT.Properties.web.set().targetBrowserName("chrome").headlessExecution(true);

        DriverFactoryHelper chromeHelper = new DriverFactoryHelper();
        try (MockedConstruction<ChromeDriver> ignoredChrome = org.mockito.Mockito.mockConstruction(ChromeDriver.class,
                (mock, context) -> {
                    WebDriver.Options options = org.mockito.Mockito.mock(WebDriver.Options.class);
                    WebDriver.Window window = org.mockito.Mockito.mock(WebDriver.Window.class);
                    org.mockito.Mockito.when(mock.manage()).thenReturn(options);
                    org.mockito.Mockito.when(options.window()).thenReturn(window);
                })) {
            chromeHelper.initializeDriver(com.shaft.driver.DriverFactory.DriverType.CHROME, null);
            SHAFT.Validations.assertThat().object(chromeHelper.getDriver()).isNotNull().perform();
        }

    }

    @Test
    public void createNewLocalDriverInstanceShouldCoverAdditionalFailureAndRetryBranches() throws Exception {
        DriverFactoryHelper helper = new DriverFactoryHelper();
        OptionsManager mockedOptionsManager = org.mockito.Mockito.mock(OptionsManager.class);
        org.mockito.Mockito.when(mockedOptionsManager.getChOptions())
                .thenThrow(new RuntimeException("cannot create default profile directory"))
                .thenThrow(new RuntimeException("generic failure"))
                .thenThrow(new RuntimeException("generic failure"));
        Field optionsManagerField = DriverFactoryHelper.class.getDeclaredField("optionsManager");
        optionsManagerField.setAccessible(true);
        optionsManagerField.set(helper, mockedOptionsManager);
        Method createNewLocalDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("createNewLocalDriverInstance", com.shaft.driver.DriverFactory.DriverType.class, int.class);
        createNewLocalDriverInstance.setAccessible(true);

        try {
            createNewLocalDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.CHROME, 0);
        } catch (InvocationTargetException ignored) {
            // expected
        }

        try {
            createNewLocalDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.CHROME, 1);
        } catch (InvocationTargetException ignored) {
            // expected
        }
    }

    private static class TestableDriverFactoryHelper extends DriverFactoryHelper {
        private com.shaft.driver.DriverFactory.DriverType capturedDriverType;
        private MutableCapabilities capturedCustomDriverOptions;

        @Override
        public void initializeDriver(com.shaft.driver.DriverFactory.DriverType driverType, MutableCapabilities customDriverOptions) {
            this.capturedDriverType = driverType;
            this.capturedCustomDriverOptions = customDriverOptions;
        }
    }
}
