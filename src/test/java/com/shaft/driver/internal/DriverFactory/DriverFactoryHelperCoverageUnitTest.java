package com.shaft.driver.internal.DriverFactory;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.OptionsManager;
import io.appium.java_client.android.AndroidDriver;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
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
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;

@Test(singleThreaded = true)
public class DriverFactoryHelperCoverageUnitTest {
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
        resetStaticField("targetBrowserName", "");
        resetStaticField("TARGET_HUB_URL", null);
        removeWebDriverManagerThreadLocal();
        Properties.clearForCurrentThread();
    }


    private static void resetStaticField(String fieldName, Object value) throws Exception {
        Field field = DriverFactoryHelper.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(null, value);
    }

    @SuppressWarnings("unchecked")
    private static void removeWebDriverManagerThreadLocal() throws Exception {
        Field webDriverManagerField = DriverFactoryHelper.class.getDeclaredField("webDriverManager");
        webDriverManagerField.setAccessible(true);
        ((ThreadLocal<WebDriverManager>) webDriverManagerField.get(null)).remove();
    }

    private static void setOptionsManager(DriverFactoryHelper helper, OptionsManager optionsManager) throws Exception {
        Field optionsManagerField = DriverFactoryHelper.class.getDeclaredField("optionsManager");
        optionsManagerField.setAccessible(true);
        optionsManagerField.set(helper, optionsManager);
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

        Method attachWebDriverLogsWithDriver = DriverFactoryHelper.class.getDeclaredMethod("attachWebDriverLogs", WebDriver.class);
        attachWebDriverLogsWithDriver.setAccessible(true);
        attachWebDriverLogsWithDriver.invoke(helper, new Object[]{null});
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
    public void localDriverInitializationLockShouldSerializeParallelBrowserResolution() throws Exception {
        Method runWithLocalDriverInitializationLock = DriverFactoryHelper.class.getDeclaredMethod("runWithLocalDriverInitializationLock", Runnable.class);
        runWithLocalDriverInitializationLock.setAccessible(true);
        CountDownLatch ready = new CountDownLatch(2);
        CountDownLatch start = new CountDownLatch(1);
        AtomicInteger activeInitializers = new AtomicInteger(0);
        AtomicInteger maximumConcurrentInitializers = new AtomicInteger(0);
        ExecutorService executorService = Executors.newFixedThreadPool(2);

        Future<?> firstInitializer = executorService.submit(() -> invokeLocalDriverInitializationLock(runWithLocalDriverInitializationLock, ready, start, activeInitializers, maximumConcurrentInitializers));
        Future<?> secondInitializer = executorService.submit(() -> invokeLocalDriverInitializationLock(runWithLocalDriverInitializationLock, ready, start, activeInitializers, maximumConcurrentInitializers));
        ready.await(5, TimeUnit.SECONDS);
        start.countDown();
        firstInitializer.get(5, TimeUnit.SECONDS);
        secondInitializer.get(5, TimeUnit.SECONDS);
        executorService.shutdownNow();

        SHAFT.Validations.assertThat().object(maximumConcurrentInitializers.get()).isEqualTo(1).perform();
    }

    private static void invokeLocalDriverInitializationLock(Method runWithLocalDriverInitializationLock, CountDownLatch ready, CountDownLatch start,
                                                            AtomicInteger activeInitializers, AtomicInteger maximumConcurrentInitializers) {
        ready.countDown();
        try {
            start.await(5, TimeUnit.SECONDS);
            runWithLocalDriverInitializationLock.invoke(null, (Runnable) () -> {
                int active = activeInitializers.incrementAndGet();
                maximumConcurrentInitializers.updateAndGet(maximum -> Math.max(maximum, active));
                try {
                    Thread.sleep(150);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    throw new AssertionError("Serialized local driver initialization should not be interrupted.", e);
                } finally {
                    activeInitializers.decrementAndGet();
                }
            });
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new AssertionError("Local driver initialization lock test should not be interrupted.", e);
        } catch (Exception e) {
            throw new AssertionError("Local driver initialization lock should not fail.", e);
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

    @Test
    public void constructorAndUrlHelpersShouldCoverRemainingDeterministicBranches() throws Exception {
        WebDriver driver = org.mockito.Mockito.mock(WebDriver.class);
        DriverFactoryHelper helper = new DriverFactoryHelper(driver);
        SHAFT.Validations.assertThat().object(helper.getDriver()).isEqualTo(driver).perform();

        Method normalize = DriverFactoryHelper.class.getDeclaredMethod("normalizeRemoteServerPingBaseUrl", String.class);
        normalize.setAccessible(true);
        SHAFT.Validations.assertThat().object(normalize.invoke(null, "https://example.com")).isEqualTo("https://example.com/").perform();
        SHAFT.Validations.assertThat().object(normalize.invoke(null, "https://example.com/wd/hub/"))
                .isEqualTo("https://example.com/wd/hub/").perform();

        Method redact = DriverFactoryHelper.class.getDeclaredMethod("redactUriCredentials", String.class);
        redact.setAccessible(true);
        SHAFT.Validations.assertThat().object(redact.invoke(null, "https://example.com/wd/hub"))
                .isEqualTo("https://example.com/wd/hub").perform();
    }

    @Test
    public void threadLocalPropertiesShouldResolveSystemPropertiesAndMobileExecutionGuards() throws Exception {
        ThreadLocalPropertiesManager.setProperty("executionAddress", "grid.example.test:4444");
        ThreadLocalPropertiesManager.setProperty("targetOperatingSystem", "iOS");
        ThreadLocalPropertiesManager.setProperty("browserName", "safari");
        SHAFT.Validations.assertThat().object(ThreadLocalPropertiesManager.getProperty("executionAddress"))
                .isEqualTo("grid.example.test:4444").perform();

        SHAFT.Properties.platform.set().executionAddress(ThreadLocalPropertiesManager.getProperty("executionAddress"));
        SHAFT.Properties.platform.set().targetPlatform(ThreadLocalPropertiesManager.getProperty("targetOperatingSystem"));
        SHAFT.Properties.mobile.set().browserName(ThreadLocalPropertiesManager.getProperty("browserName"));
        DriverFactoryHelper.initializeSystemProperties();
        Field targetHubUrl = DriverFactoryHelper.class.getDeclaredField("TARGET_HUB_URL");
        targetHubUrl.setAccessible(true);
        SHAFT.Validations.assertThat().object(targetHubUrl.get(null)).isEqualTo("http://grid.example.test:4444").perform();
        SHAFT.Validations.assertThat().object(DriverFactoryHelper.isMobileWebExecution()).isEqualTo(true).perform();
    }

    @Test
    public void closeDriverShouldCoverSuccessfulGenericAndDockerizedCleanupBranches() throws Exception {
        DriverFactoryHelper helper = new DriverFactoryHelper();
        WebDriver driver = org.mockito.Mockito.mock(WebDriver.class);
        helper.closeDriver(driver);
        org.mockito.Mockito.verify(driver).close();
        org.mockito.Mockito.verify(driver).quit();

        WebDriver exceptionDriver = org.mockito.Mockito.mock(WebDriver.class);
        org.mockito.Mockito.doThrow(new IllegalStateException("generic teardown failure")).when(exceptionDriver).quit();
        helper.closeDriver(exceptionDriver);
        org.mockito.Mockito.verify(exceptionDriver).close();
        org.mockito.Mockito.verify(exceptionDriver).quit();

        SHAFT.Properties.platform.set().executionAddress("dockerized");
        WebDriver dockerDriver = org.mockito.Mockito.mock(WebDriver.class);
        WebDriverManager mockedManager = org.mockito.Mockito.mock(WebDriverManager.class, org.mockito.Mockito.RETURNS_SELF);
        org.mockito.Mockito.when(mockedManager.getDockerRecordingPath(dockerDriver)).thenReturn(java.nio.file.Path.of("target/docker-recording.mp4"));
        Field webDriverManagerField = DriverFactoryHelper.class.getDeclaredField("webDriverManager");
        webDriverManagerField.setAccessible(true);
        @SuppressWarnings("unchecked")
        ThreadLocal<WebDriverManager> managerThreadLocal = (ThreadLocal<WebDriverManager>) webDriverManagerField.get(null);
        managerThreadLocal.set(mockedManager);
        helper.closeDriver(dockerDriver);
        org.mockito.Mockito.verify(mockedManager).quit(dockerDriver);
    }

    @Test
    public void createNewLocalDriverInstanceShouldCoverRecoverableOptionMutationBranches() throws Exception {
        DriverFactoryHelper helper = new DriverFactoryHelper();
        OptionsManager mockedOptionsManager = org.mockito.Mockito.mock(OptionsManager.class);
        ChromeOptions chromeOptions = new ChromeOptions();
        EdgeOptions edgeOptions = new EdgeOptions();
        FirefoxOptions firefoxOptions = new FirefoxOptions();
        org.mockito.Mockito.when(mockedOptionsManager.getChOptions())
                .thenThrow(new RuntimeException("DevToolsActivePort file doesn't exist"))
                .thenReturn(chromeOptions)
                .thenThrow(new RuntimeException("Failed to initialize BiDi Mapper"))
                .thenReturn(chromeOptions);
        org.mockito.Mockito.when(mockedOptionsManager.getEdOptions())
                .thenThrow(new RuntimeException("DevToolsActivePort file doesn't exist"))
                .thenReturn(edgeOptions)
                .thenThrow(new RuntimeException("Failed to initialize BiDi Mapper"))
                .thenReturn(edgeOptions);
        org.mockito.Mockito.when(mockedOptionsManager.getFfOptions())
                .thenThrow(new RuntimeException("Failed to initialize BiDi Mapper"))
                .thenReturn(firefoxOptions);
        setOptionsManager(helper, mockedOptionsManager);
        Method createNewLocalDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("createNewLocalDriverInstance", com.shaft.driver.DriverFactory.DriverType.class, int.class);
        createNewLocalDriverInstance.setAccessible(true);

        invokeExpectingFailure(createNewLocalDriverInstance, helper, com.shaft.driver.DriverFactory.DriverType.CHROME, 0);
        SHAFT.Validations.assertThat().object(chromeOptions.toString().contains("--remote-debugging-pipe")).isEqualTo(true).perform();
        invokeExpectingFailure(createNewLocalDriverInstance, helper, com.shaft.driver.DriverFactory.DriverType.EDGE, 0);
        SHAFT.Validations.assertThat().object(edgeOptions.toString().contains("--remote-debugging-pipe")).isEqualTo(true).perform();
        invokeExpectingFailure(createNewLocalDriverInstance, helper, com.shaft.driver.DriverFactory.DriverType.FIREFOX, 0);
        SHAFT.Validations.assertThat().object(firefoxOptions.getCapability("webSocketUrl")).isEqualTo(false).perform();
        invokeExpectingFailure(createNewLocalDriverInstance, helper, com.shaft.driver.DriverFactory.DriverType.CHROME, 0);
        SHAFT.Validations.assertThat().object(chromeOptions.getCapability("webSocketUrl")).isEqualTo(false).perform();
        invokeExpectingFailure(createNewLocalDriverInstance, helper, com.shaft.driver.DriverFactory.DriverType.EDGE, 0);
        SHAFT.Validations.assertThat().object(edgeOptions.getCapability("webSocketUrl")).isEqualTo(false).perform();
    }

    @Test
    public void createNewRemoteDriverInstanceShouldCoverKillSwitchAndErrorBranchesFromOptionsResolution() throws Exception {
        DriverFactoryHelper helper = new DriverFactoryHelper();
        SHAFT.Properties.web.set().headlessExecution(true);
        DriverFactoryHelper.initializeSystemProperties();
        Method createNewRemoteDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("createNewRemoteDriverInstance", com.shaft.driver.DriverFactory.DriverType.class);
        createNewRemoteDriverInstance.setAccessible(true);

        OptionsManager unreachableOptions = org.mockito.Mockito.mock(OptionsManager.class);
        org.mockito.Mockito.when(unreachableOptions.getChOptions()).thenThrow(new org.openqa.selenium.remote.UnreachableBrowserException("offline"));
        setOptionsManager(helper, unreachableOptions);
        invokeExpectingFailure(createNewRemoteDriverInstance, helper, com.shaft.driver.DriverFactory.DriverType.CHROME);
        SHAFT.Validations.assertThat().object(DriverFactoryHelper.isKillSwitch()).isEqualTo(true).perform();
        setKillSwitch(false);

        OptionsManager forwardingOptions = org.mockito.Mockito.mock(OptionsManager.class);
        org.mockito.Mockito.when(forwardingOptions.getChOptions()).thenThrow(new org.openqa.selenium.WebDriverException("Error forwarding the new session cannot find matching node"));
        setOptionsManager(helper, forwardingOptions);
        invokeExpectingFailure(createNewRemoteDriverInstance, helper, com.shaft.driver.DriverFactory.DriverType.CHROME);

        OptionsManager genericOptions = org.mockito.Mockito.mock(OptionsManager.class);
        org.mockito.Mockito.when(genericOptions.getChOptions()).thenThrow(new org.openqa.selenium.WebDriverException("generic remote failure"));
        setOptionsManager(helper, genericOptions);
        invokeExpectingFailure(createNewRemoteDriverInstance, helper, com.shaft.driver.DriverFactory.DriverType.CHROME);

        OptionsManager missingClassOptions = org.mockito.Mockito.mock(OptionsManager.class);
        org.mockito.Mockito.when(missingClassOptions.getChOptions()).thenThrow(new NoClassDefFoundError("missing selenium binding"));
        setOptionsManager(helper, missingClassOptions);
        invokeExpectingFailure(createNewRemoteDriverInstance, helper, com.shaft.driver.DriverFactory.DriverType.CHROME);
    }

    @Test
    public void createNewDockerizedDriverInstanceShouldCoverAdditionalBrowserAndFailureBranches() throws Exception {
        SHAFT.Properties.web.set().headlessExecution(true);
        DriverFactoryHelper helper = new DriverFactoryHelper();
        Method createNewDockerizedDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("createNewDockerizedDriverInstance", com.shaft.driver.DriverFactory.DriverType.class);
        createNewDockerizedDriverInstance.setAccessible(true);
        WebDriverManager mockedManager = org.mockito.Mockito.mock(WebDriverManager.class, org.mockito.Mockito.RETURNS_SELF);
        RemoteWebDriver mockedRemoteDriver = org.mockito.Mockito.mock(RemoteWebDriver.class);
        org.mockito.Mockito.when(mockedManager.create()).thenReturn(mockedRemoteDriver);

        try (org.mockito.MockedStatic<WebDriverManager> webDriverManagerMockedStatic = org.mockito.Mockito.mockStatic(WebDriverManager.class)) {
            webDriverManagerMockedStatic.when(WebDriverManager::firefoxdriver).thenReturn(mockedManager);
            webDriverManagerMockedStatic.when(WebDriverManager::edgedriver).thenReturn(mockedManager);
            webDriverManagerMockedStatic.when(WebDriverManager::safaridriver).thenReturn(mockedManager);
            createNewDockerizedDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.FIREFOX);
            createNewDockerizedDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.EDGE);
            createNewDockerizedDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.SAFARI);
            SHAFT.Validations.assertThat().object(helper.getDriver()).isEqualTo(mockedRemoteDriver).perform();
        }

        WebDriverManager failingManager = org.mockito.Mockito.mock(WebDriverManager.class, org.mockito.Mockito.RETURNS_SELF);
        org.mockito.Mockito.when(failingManager.create()).thenThrow(new io.github.bonigarcia.wdm.config.WebDriverManagerException("docker unavailable"));
        try (org.mockito.MockedStatic<WebDriverManager> webDriverManagerMockedStatic = org.mockito.Mockito.mockStatic(WebDriverManager.class)) {
            webDriverManagerMockedStatic.when(WebDriverManager::chromedriver).thenReturn(failingManager);
            invokeExpectingFailure(createNewDockerizedDriverInstance, helper, com.shaft.driver.DriverFactory.DriverType.CHROME);
        }
    }

    private static void invokeExpectingFailure(Method method, Object target, Object... args) throws Exception {
        try {
            method.invoke(target, args);
        } catch (InvocationTargetException ignored) {
            // These branches intentionally end in SHAFT's FailureReporter after the deterministic code path executes.
        }
    }

    @Test
    public void remotePingAndSetRemoteDriverInstanceShouldCoverSuccessfulStatusCheckBranches() throws Exception {
        com.sun.net.httpserver.HttpServer server = com.sun.net.httpserver.HttpServer.create(new java.net.InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/wd/hub/status/", exchange -> {
            byte[] response = "{\"value\":{\"ready\":true}}".getBytes(java.nio.charset.StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, response.length);
            exchange.getResponseBody().write(response);
            exchange.close();
        });
        server.start();
        try {
            resetStaticField("TARGET_HUB_URL", "http://127.0.0.1:" + server.getAddress().getPort() + "/wd/hub");
            Method attemptRemoteServerPing = DriverFactoryHelper.class.getDeclaredMethod("attemptRemoteServerPing");
            attemptRemoteServerPing.setAccessible(true);
            SHAFT.Validations.assertThat().object(attemptRemoteServerPing.invoke(null)).isEqualTo(200).perform();

            SHAFT.Properties.timeouts.set().waitForRemoteServerToBeUp(true);
            SHAFT.Properties.flags.set().forceCheckStatusOfRemoteServer(true);
            SHAFT.Properties.platform.set().executionAddress("http://127.0.0.1:" + server.getAddress().getPort() + "/wd/hub").targetPlatform("Linux");
            DriverFactoryHelper.initializeSystemProperties();
            DriverFactoryHelper helper = new DriverFactoryHelper();
            Method setRemoteDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("setRemoteDriverInstance", org.openqa.selenium.Capabilities.class);
            setRemoteDriverInstance.setAccessible(true);
            try (MockedConstruction<RemoteWebDriver> ignored = org.mockito.Mockito.mockConstruction(RemoteWebDriver.class,
                    (mock, context) -> org.mockito.Mockito.when(mock.getCapabilities()).thenReturn(new ImmutableCapabilities()))) {
                setRemoteDriverInstance.invoke(helper, new ChromeOptions());
                SHAFT.Validations.assertThat().object(helper.getDriver()).isNotNull().perform();
            }
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void configureRemoteDriverInstanceShouldCoverDesktopOptionRoutingBranches() throws Exception {
        SHAFT.Properties.timeouts.set().waitForRemoteServerToBeUp(false);
        SHAFT.Properties.platform.set().executionAddress("http://localhost:4444").targetPlatform("Linux");
        DriverFactoryHelper.initializeSystemProperties();
        DriverFactoryHelper helper = new DriverFactoryHelper();
        OptionsManager mockedOptionsManager = org.mockito.Mockito.mock(OptionsManager.class);
        org.mockito.Mockito.when(mockedOptionsManager.getFfOptions()).thenReturn(new FirefoxOptions());
        org.mockito.Mockito.when(mockedOptionsManager.getIeOptions()).thenReturn(new org.openqa.selenium.ie.InternetExplorerOptions());
        org.mockito.Mockito.when(mockedOptionsManager.getEdOptions()).thenReturn(new EdgeOptions());
        org.mockito.Mockito.when(mockedOptionsManager.getSfOptions()).thenReturn(new SafariOptions());
        setOptionsManager(helper, mockedOptionsManager);
        Method configureRemoteDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("configureRemoteDriverInstance", com.shaft.driver.DriverFactory.DriverType.class, DesiredCapabilities.class);
        configureRemoteDriverInstance.setAccessible(true);

        try (MockedConstruction<RemoteWebDriver> ignored = org.mockito.Mockito.mockConstruction(RemoteWebDriver.class,
                (mock, context) -> org.mockito.Mockito.when(mock.getCapabilities()).thenReturn(new ImmutableCapabilities()))) {
            configureRemoteDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.FIREFOX, new DesiredCapabilities());
            configureRemoteDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.IE, new DesiredCapabilities());
            configureRemoteDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.EDGE, new DesiredCapabilities());
            configureRemoteDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.SAFARI, new DesiredCapabilities());
            SHAFT.Validations.assertThat().object(helper.getDriver()).isNotNull().perform();
        }
    }

    @Test
    public void configureRemoteDriverInstanceShouldCoverMobileSafariAndAppiumRoutingBranches() throws Exception {
        SHAFT.Properties.timeouts.set().waitForRemoteServerToBeUp(false);
        SHAFT.Properties.platform.set().executionAddress("http://localhost:4723").targetPlatform("Android");
        SHAFT.Properties.mobile.set().browserName("");
        DriverFactoryHelper.initializeSystemProperties();
        DriverFactoryHelper helper = new DriverFactoryHelper();
        Method configureRemoteDriverInstance = DriverFactoryHelper.class.getDeclaredMethod("configureRemoteDriverInstance", com.shaft.driver.DriverFactory.DriverType.class, DesiredCapabilities.class);
        configureRemoteDriverInstance.setAccessible(true);

        try (MockedConstruction<AndroidDriver> ignored = org.mockito.Mockito.mockConstruction(AndroidDriver.class)) {
            configureRemoteDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.SAFARI, new DesiredCapabilities());
            configureRemoteDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.APPIUM_MOBILE_NATIVE, new DesiredCapabilities());
            configureRemoteDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.APPIUM_SAMSUNG_BROWSER, new DesiredCapabilities());
            configureRemoteDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.APPIUM_BROWSER, new DesiredCapabilities());
            configureRemoteDriverInstance.invoke(helper, com.shaft.driver.DriverFactory.DriverType.APPIUM_FLUTTER, new DesiredCapabilities());
            SHAFT.Validations.assertThat().object(helper.getDriver()).isNotNull().perform();
        }
    }

    @Test
    public void initializeDriverShouldCoverDockerizedAndHeadlessCustomWindowBranchesWithMockedManagers() {
        SHAFT.Properties.platform.set().targetPlatform("windows").executionAddress("dockerized");
        SHAFT.Properties.web.set().targetBrowserName("chrome").headlessExecution(true).browserWindowWidth(1234).browserWindowHeight(678);
        SHAFT.Properties.flags.set().disableCache(false).autoMaximizeBrowserWindow(false);
        SHAFT.Properties.healenium.set().healEnabled(false);
        WebDriverManager mockedManager = org.mockito.Mockito.mock(WebDriverManager.class, org.mockito.Mockito.RETURNS_SELF);
        RemoteWebDriver mockedRemoteDriver = org.mockito.Mockito.mock(RemoteWebDriver.class);
        WebDriver.Options options = org.mockito.Mockito.mock(WebDriver.Options.class);
        WebDriver.Window window = org.mockito.Mockito.mock(WebDriver.Window.class);
        org.mockito.Mockito.when(mockedRemoteDriver.manage()).thenReturn(options);
        org.mockito.Mockito.when(options.window()).thenReturn(window);
        org.mockito.Mockito.when(mockedManager.create()).thenReturn(mockedRemoteDriver);

        try (org.mockito.MockedStatic<WebDriverManager> webDriverManagerMockedStatic = org.mockito.Mockito.mockStatic(WebDriverManager.class)) {
            webDriverManagerMockedStatic.when(WebDriverManager::chromedriver).thenReturn(mockedManager);
            DriverFactoryHelper helper = new DriverFactoryHelper();
            helper.initializeDriver(com.shaft.driver.DriverFactory.DriverType.CHROME, null);
            org.mockito.Mockito.verify(window).setSize(new org.openqa.selenium.Dimension(1234, 678));
            SHAFT.Validations.assertThat().object(helper.getDriver()).isEqualTo(mockedRemoteDriver).perform();
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
