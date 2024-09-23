package com.shaft.driver.internal.DriverFactory;

import com.epam.healenium.SelfHealingDriver;
import com.shaft.driver.DriverFactory.DriverType;
import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.internal.video.RecordManager;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertiesHelper;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ProgressBarLogger;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.Setting;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.github.bonigarcia.wdm.WebDriverManager;
import io.github.bonigarcia.wdm.config.WebDriverManagerException;
import io.qameta.allure.Step;
import lombok.*;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.*;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.devtools.Command;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.DevToolsException;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.edge.EdgeDriver;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.ie.InternetExplorerDriver;
import org.openqa.selenium.remote.*;
import org.openqa.selenium.safari.SafariDriver;
import org.testng.Reporter;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class DriverFactoryHelper {
    private static final String WEB_DRIVER_MANAGER_MESSAGE = "Identifying OS/Driver combination. Please note that if a new browser/driver executable will be downloaded it may take some time depending on your connection...";
    private static final String WEB_DRIVER_MANAGER_DOCKERIZED_MESSAGE = "Identifying target OS/Browser and setting up the dockerized environment automatically. Please note that if a new docker container will be downloaded it may take some time depending on your connection...";
    private static final ThreadLocal<WebDriverManager> webDriverManager = new ThreadLocal<>();
    @Getter(AccessLevel.PUBLIC)
    private static final Dimension TARGET_WINDOW_SIZE = new Dimension(1920, 1080);
    private static final long appiumServerInitializationTimeout = TimeUnit.MINUTES.toSeconds(SHAFT.Properties.timeouts.timeoutForRemoteServerToBeUp()); // seconds
    private static final int appiumServerInitializationPollingInterval = 1; // seconds
    private static final long remoteServerInstanceCreationTimeout = TimeUnit.MINUTES.toSeconds(SHAFT.Properties.timeouts.remoteServerInstanceCreationTimeout()); // seconds
    private static final int appiumServerPreparationPollingInterval = 1; // seconds
    // TODO: implement pass and fail actions to enable initial factory method screenshot and append it to animated GIF
    private static String TARGET_HUB_URL;
    @Getter(AccessLevel.PUBLIC)
    private static String targetBrowserName = "";
    @Getter(AccessLevel.PUBLIC)
    private static boolean killSwitch = false;
    private final OptionsManager optionsManager = new OptionsManager();
    @Setter
    @Getter
    private WebDriver driver;

    public DriverFactoryHelper() {
    }

    public DriverFactoryHelper(WebDriver driver) {
        setDriver(driver);
    }

    /**
     * Checks to see if the execution is a mobile-native execution
     *
     * @return true if it's a mobile mobile-native execution
     */
    public static boolean isMobileNativeExecution() {
        var isMobileExecution = Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform()) || Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform());
        var isNativeExecution = SHAFT.Properties.mobile.browserName().isBlank();
        return isMobileExecution && isNativeExecution;
    }

    /**
     * Checks to see if the execution is a mobile-web execution
     *
     * @return true if it's a mobile mobile-web execution
     */
    public static boolean isMobileWebExecution() {
        var isMobileExecution = Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform()) || Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform());
        var isNativeExecution = SHAFT.Properties.mobile.browserName().isBlank();
        return isMobileExecution && !isNativeExecution;
    }

    /**
     * Checks to see if the execution is a web-based execution
     *
     * @return true if it's a web-based execution
     */
    public static boolean isNotMobileExecution() {
        var isMobileExecution = Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform()) || Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform());
        return !isMobileExecution;
    }

    protected static void failAction(String testData, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        String message = "Driver Factory Action \"" + actionName + "\" failed.";
        if (testData != null) {
            message = message + " With the following test data \"" + testData + "\".";
        }
        if (rootCauseException != null && rootCauseException.length >= 1) {
            FailureReporter.fail(DriverFactoryHelper.class, message, rootCauseException[0]);
        } else {
            FailureReporter.fail(message);
        }
    }

    private static DriverType getDriverTypeFromName(String driverName) {
        int values = DriverType.values().length;
        for (var i = 0; i < values; i++) {
            var expectedName = driverName.trim().toLowerCase();
            var supportedName = Arrays.asList(DriverType.values()).get(i).getValue().toLowerCase();

            if (expectedName.contains(supportedName) || supportedName.contains(expectedName)) {
                return Arrays.asList(DriverType.values()).get(i);
            }
        }
        failAction("Unsupported Driver Type \"" + driverName + "\".");
        return DriverType.CHROME;
    }

    @SneakyThrows(InterruptedException.class)
    private static int attemptRemoteServerPing() {
        boolean serverReady = false;
        var session = new SHAFT.API(TARGET_HUB_URL);
        var statusCode = 500;
        var startTime = System.currentTimeMillis();
        do {
            try {
                statusCode = session.get("status/").perform().andReturn().statusCode();
                if (statusCode >= 200 && statusCode < 300) {
                    serverReady = true;
                }
            } catch (Throwable throwable1) {
                try {
                    statusCode = session.get("wd/hub/status/").perform().andReturn().statusCode();
                    if (statusCode >= 200 && statusCode < 300) {
                        serverReady = true;
                    }
                } catch (Throwable throwable2) {
                    // do nothing
                    ReportManagerHelper.logDiscrete(throwable1, Level.DEBUG);
                    ReportManagerHelper.logDiscrete(throwable2, Level.DEBUG);
                }
            }
            if (!serverReady) {
                //noinspection BusyWait
                Thread.sleep(TimeUnit.SECONDS.toMillis(appiumServerInitializationPollingInterval));
            }
        } while (!serverReady && (System.currentTimeMillis() - startTime < TimeUnit.SECONDS.toMillis(appiumServerInitializationTimeout)));
        if (!serverReady) {
            failAction("Failed to connect to remote server. It was still not ready after " + TimeUnit.SECONDS.toMinutes(appiumServerInitializationTimeout) + " minutes.");
        }
        return statusCode;
    }

    @SneakyThrows({MalformedURLException.class, InterruptedException.class})
    private static WebDriver attemptRemoteServerConnection(Capabilities capabilities) {
        WebDriver driver = null;
        boolean isRemoteConnectionEstablished = false;
        var startTime = System.currentTimeMillis();
        Exception exception = null;
        do {
            try {
                driver = connectToRemoteServer(capabilities, false);
                isRemoteConnectionEstablished = true;
            } catch (SessionNotCreatedException | URISyntaxException sessionNotCreatedException1) {
                exception = sessionNotCreatedException1;
                String message = sessionNotCreatedException1.getMessage();
                if (message.contains("missing in the capabilities")
                        || message.contains("not allowed on your current plan")
                        || message.contains("has been exhausted")) {
                    break;
                } else {
                    try {
                        driver = connectToRemoteServer(capabilities, true);
                        isRemoteConnectionEstablished = true;
                    } catch (SessionNotCreatedException |
                             URISyntaxException sessionNotCreatedException2) {
                        // do nothing
                        exception = sessionNotCreatedException2;
                        ReportManagerHelper.logDiscrete(sessionNotCreatedException1, Level.DEBUG);
                        ReportManagerHelper.logDiscrete(sessionNotCreatedException2, Level.DEBUG);
                    }
                }
            }
            if (!isRemoteConnectionEstablished) {
                //terminate in case of any other exception
                //noinspection BusyWait
                Thread.sleep(TimeUnit.SECONDS.toMillis(appiumServerPreparationPollingInterval));
            }
        } while (!isRemoteConnectionEstablished && (System.currentTimeMillis() - startTime < TimeUnit.SECONDS.toMillis(remoteServerInstanceCreationTimeout)));
        if (!isRemoteConnectionEstablished) {
            failAction("Failed to connect to remote server. Session was still not created after " + TimeUnit.SECONDS.toMinutes(remoteServerInstanceCreationTimeout) + " minutes.", exception);
        }
        return driver;
    }

    private static WebDriver connectToRemoteServer(Capabilities capabilities, boolean isLegacy) throws MalformedURLException, URISyntaxException {
        var targetHubUrl = isLegacy ? TARGET_HUB_URL + "wd/hub" : TARGET_HUB_URL;
        var targetLambdaTestHubURL = targetHubUrl.replace("http", "https");
        var targetPlatform = Properties.platform.targetPlatform();
        var targetMobileHubUrl = targetHubUrl.replace("@", "@mobile-").replace("http", "https");

        if (targetPlatform.equalsIgnoreCase(Platform.ANDROID.toString())) {
            if (SHAFT.Properties.platform.executionAddress().contains("lambdatest") && !isMobileWebExecution()) {
                return new AndroidDriver(new URI(targetMobileHubUrl).toURL(), capabilities);
            } else {
                if (SHAFT.Properties.platform.executionAddress().contains("lambdatest")) {
                    return new AndroidDriver(new URI(targetLambdaTestHubURL).toURL(), capabilities);
                } else {
                    return new AndroidDriver(new URI(targetHubUrl).toURL(), capabilities);
                }
            }
        } else if (targetPlatform.equalsIgnoreCase(Platform.IOS.toString())) {
            if (SHAFT.Properties.platform.executionAddress().contains("lambdatest") && !isMobileWebExecution()) {
                return new IOSDriver(new URI(targetMobileHubUrl).toURL(), capabilities);
            } else {
                if (SHAFT.Properties.platform.executionAddress().contains("lambdatest")) {
                    return new IOSDriver(new URI(targetLambdaTestHubURL).toURL(), capabilities);
                } else {
                    return new IOSDriver(new URI(targetHubUrl).toURL(), capabilities);
                }
            }
        } else {
            if (SHAFT.Properties.platform.executionAddress().contains("lambdatest")) {
                return new RemoteWebDriver(new URI(targetLambdaTestHubURL).toURL(), capabilities, false);
            } else {
                return new RemoteWebDriver(new URI(targetHubUrl).toURL(), capabilities, false);
            }
        }
    }

    public static void initializeSystemProperties() {
        PropertiesHelper.postProcessing();
        TARGET_HUB_URL = (SHAFT.Properties.platform.executionAddress().trim().toLowerCase().startsWith("http")) ? SHAFT.Properties.platform.executionAddress() : "http://" + SHAFT.Properties.platform.executionAddress() + "/";
    }

    public void closeDriver() {
        closeDriver(driver);
        setDriver(null);
    }

    public void closeDriver(WebDriver driver) {
        if (driver != null) {
            if (SHAFT.Properties.visuals.videoParamsScope().equals("DriverSession")) {
                RecordManager.attachVideoRecording();
            }
            try {
                attachWebDriverLogs();
                //if dockerized wdm.quit the relevant one
                if (SHAFT.Properties.platform.executionAddress().toLowerCase().contains("dockerized")) {
                    var pathToRecording = webDriverManager.get().getDockerRecordingPath(driver);
                    webDriverManager.get().quit(driver);
                    RecordManager.attachVideoRecording(pathToRecording);
                } else {
                    try {
                        driver.close();
                    } catch (Exception e) {
                        //ignore
                    }
                    driver.quit();
                }
            } catch (WebDriverException | NullPointerException e) {
                // driver was already closed at an earlier stage
            } catch (Exception e) {
                ReportManagerHelper.logDiscrete(e);
            } finally {
                webDriverManager.remove();
                ReportManager.log("Successfully Closed Driver.");
            }
        } else {
            ReportManager.log("Driver is already closed.");
        }
    }

    private void disableCacheEdgeAndChrome() {
        try {
            if (SHAFT.Properties.flags.disableCache() && driver instanceof HasDevTools hasDevToolsDriver) {
                DevTools devTools = hasDevToolsDriver.getDevTools();
                devTools.createSessionIfThereIsNotOne();
                LinkedHashMap<String, Object> params = new LinkedHashMap<>();
                params.put("maxPostDataSize", 100000000);
                devTools.send(new Command<>("Network.enable", Map.copyOf(params)));
                params = new LinkedHashMap<>();
                params.put("cacheDisabled", true);
                devTools.send(new Command<>("Network.setCacheDisabled", params));
                devTools.send(new Command<>("Network.clearBrowserCookies", Map.copyOf(new LinkedHashMap<>())));
            }
        } catch (DevToolsException e) {
            ReportManagerHelper.logDiscrete(e);
            ReportManager.logDiscrete("We recommend disabling this property 'SHAFT.Properties.flags.disableCache()' or downgrading your browser.");
        }
    }

    private void createNewLocalDriverInstance(DriverType driverType, int retryAttempts) {
        String targetPlatform = Properties.platform.targetPlatform().toLowerCase();
        String initialLog = "Attempting to run locally on: \"" + targetPlatform + " | " + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\"";
        if (SHAFT.Properties.web.headlessExecution()) {
            initialLog = initialLog + ", Headless Execution";
        }
        initialLog = initialLog.replace(targetPlatform, JavaHelper.convertToSentenceCase(targetPlatform));
        ReportManager.logDiscrete(initialLog + ".");
        try {
            ReportManager.logDiscrete(WEB_DRIVER_MANAGER_MESSAGE);
            switch (driverType) {
                case FIREFOX -> setDriver(new FirefoxDriver(optionsManager.getFfOptions()));
                case IE -> setDriver(new InternetExplorerDriver(optionsManager.getIeOptions()));
                case CHROME -> {
                    setDriver(new ChromeDriver(optionsManager.getChOptions()));
                    disableCacheEdgeAndChrome();
                }
                case EDGE -> {
                    setDriver(new EdgeDriver(optionsManager.getEdOptions()));
                    disableCacheEdgeAndChrome();
                }
                case SAFARI -> setDriver(new SafariDriver(optionsManager.getSfOptions()));
                default ->
                        failAction("Unsupported Driver Type \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\".");
            }
            ReportManager.log(initialLog.replace("Attempting to run locally on", "Successfully Opened") + ".");
        } catch (Exception exception) {
            String message = exception.getMessage();
            if (message.contains("cannot create default profile directory")) {
                // this exception happens when the profile directory is not correct, very specific case
                // should fail immediately
                failAction("Failed to create new Browser Session", exception);
            } else if (message.contains("DevToolsActivePort file doesn't exist")) {
                // this exception was observed with `Windows_Edge_Local` pipeline to happen randomly
                // suggested fix as per titus fortner: https://bugs.chromium.org/p/chromedriver/issues/detail?id=4403#c35
                switch (driverType) {
                    case DriverType.CHROME -> {
                        var chOptions = optionsManager.getChOptions();
                        chOptions.addArguments("--remote-debugging-pipe");
                        optionsManager.setChOptions(chOptions);
                    }
                    case DriverType.EDGE -> {
                        var edOptions = optionsManager.getEdOptions();
                        edOptions.addArguments("--remote-debugging-pipe");
                        optionsManager.setEdOptions(edOptions);
                    }
                }
            } else if (message.contains("Failed to initialize BiDi Mapper")) {
                // this exception happens in some corner cases where the capabilities are not compatible with BiDi mode
                // should force disable BiDi and try again
                SHAFT.Properties.platform.set().enableBiDi(false);
                switch (driverType) {
                    case DriverType.FIREFOX -> {
                        var ffOptions = optionsManager.getFfOptions();
                        ffOptions.setCapability("webSocketUrl", SHAFT.Properties.platform.enableBiDi());
                        optionsManager.setFfOptions(ffOptions);
                    }
                    case DriverType.CHROME -> {
                        var chOptions = optionsManager.getChOptions();
                        chOptions.setCapability("webSocketUrl", SHAFT.Properties.platform.enableBiDi());
                        optionsManager.setChOptions(chOptions);
                    }
                    case DriverType.EDGE -> {
                        var edOptions = optionsManager.getEdOptions();
                        edOptions.setCapability("webSocketUrl", SHAFT.Properties.platform.enableBiDi());
                        optionsManager.setEdOptions(edOptions);
                    }
                }
            } else if (message.contains("The Safari instance is already paired with another WebDriver session.")) {
                //this issue happens when running locally via safari/mac platform
                // attempting blind fix by trying to quit existing safari instances if any
                try {
                    SHAFT.CLI.terminal().performTerminalCommands(Arrays.asList(
                            "osascript -e 'quit app \"Safari\"'", "osascript -e 'quit app \"SafariDriver\"'",
                            "pkill -x Safari", "pkill -x SafariDriver",
                            "killall Safari", "killall SafariDriver"));
                    //minimizing retry attempts to save execution time
                    retryAttempts = 0;
                } catch (Throwable throwable) {
                    // ignore
                }
            } else if (exception.getMessage().contains("java.util.concurrent.TimeoutException")) {
                // this happens in case an auto closable BiDi session was left hanging
                // the default timeout is 30 seconds, so this wait will waste 26 and the following will waste 5 more
                // the desired effect will be to wait for the bidi session to timeout
                try {
                    Thread.sleep(26000);
                } catch (InterruptedException e) {
                    //do nothing
                }
            }
            // attempting blind fix by trying to quit existing driver if any
            try {
                driver.quit();
            } catch (Throwable throwable) {
                // ignore
            } finally {
                setDriver(null);
            }
            // evaluating retry attempts
            if (retryAttempts > 0) {
                try {
                    Thread.sleep(5000);
                } catch (InterruptedException e) {
                    //do nothing
                }
                createNewLocalDriverInstance(driverType, retryAttempts - 1);
            }
            failAction("Failed to create new Browser Session", exception);
        }
    }

    private void createNewDockerizedDriverInstance(DriverType driverType) {
        String initialLog = "Attempting to run dockerized on: \"" + Properties.platform.targetPlatform() + " | " + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\"";
        if (SHAFT.Properties.web.headlessExecution()) {
            initialLog = initialLog + ", Headless Execution";
        }
        ReportManager.log(initialLog + ".");

        try {
            ReportManager.logDiscrete(WEB_DRIVER_MANAGER_DOCKERIZED_MESSAGE);
            switch (driverType) {
                case FIREFOX ->
                        webDriverManager.set(WebDriverManager.firefoxdriver().capabilities(optionsManager.getFfOptions()));
                case CHROME ->
                        webDriverManager.set(WebDriverManager.chromedriver().capabilities(optionsManager.getChOptions()));
                case EDGE ->
                        webDriverManager.set(WebDriverManager.edgedriver().capabilities(optionsManager.getEdOptions()));
                case SAFARI ->
                        webDriverManager.set(WebDriverManager.safaridriver().capabilities(optionsManager.getSfOptions()));
                default ->
                        failAction("Unsupported Driver Type \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\". We only support Chrome, Edge, Firefox, and Safari in this dockerized mode.");
            }
            RemoteWebDriver remoteWebDriver = (RemoteWebDriver) webDriverManager.get().proxy(SHAFT.Properties.platform.proxy()).browserInDocker().dockerShmSize("2g").enableVnc().viewOnly().avoidUseChromiumDriverSnap().dockerScreenResolution(TARGET_WINDOW_SIZE.getWidth() + "x" + TARGET_WINDOW_SIZE.getHeight() + "x24")
//                    .dockerVolumes("\\local\\path:\\container\\path")
                    .enableRecording().dockerRecordingOutput(SHAFT.Properties.paths.video()).create();
            remoteWebDriver.setFileDetector(new LocalFileDetector());
//            driver =ThreadGuard.protect(remoteWebDriver));
            setDriver(remoteWebDriver);
            ReportManager.log("Successfully Opened " + JavaHelper.convertToSentenceCase(driverType.getValue()) + ".");
        } catch (WebDriverManagerException exception) {
            failAction("Failed to create new Dockerized Browser Session, are you sure Docker is available on your machine?", exception);
        }
    }

    private void createNewRemoteDriverInstance(DriverType driverType) {
        var initialLog = new StringBuilder();
        initialLog.append("Attempting to run remotely on: \"").append(Properties.platform.targetPlatform());

        if (!Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform()) && !Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            initialLog.append(" | ").append(JavaHelper.convertToSentenceCase(driverType.getValue()));
        }

        initialLog.append(" | ").append(TARGET_HUB_URL).append("\"");

        if (SHAFT.Properties.web.headlessExecution() && !Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform()) && !Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            initialLog.append(", Headless Execution");
        }
        ReportManager.log(initialLog + ".");

        if (Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform()) || Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            optionsManager.initializeMobileDesiredCapabilities();
        }

        try {
            configureRemoteDriverInstance(driverType, optionsManager.getAppiumCapabilities());
        } catch (UnreachableBrowserException e) {
            killSwitch = true;
            failAction("Unreachable Browser, terminated test suite execution.", e);
        } catch (WebDriverException e) {
            if (e.getMessage().contains("Error forwarding the new session cannot find")) {
                ReportManager.logDiscrete("Failed to run remotely on: \"" + Properties.platform.targetPlatform() + "\", \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\", \"" + TARGET_HUB_URL + "\".");
                failAction("Error forwarding the new session: Couldn't find a node that matches the desired capabilities.", e);
            } else {
                ReportManager.logDiscrete("Failed to run remotely on: \"" + Properties.platform.targetPlatform() + "\", \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\", \"" + TARGET_HUB_URL + "\".");
                failAction("Unhandled Error.", e);
            }
        } catch (NoClassDefFoundError e) {
            failAction("Failed to create Remote WebDriver instance", e);
        }
    }

    @Step("Setting up remote driver instance")
    private void setRemoteDriverInstance(Capabilities capabilities) {
        // stage 1: ensure that the server is up and running
        if (SHAFT.Properties.timeouts.waitForRemoteServerToBeUp()) {
            ReportManager.logDiscrete("Attempting to connect to remote server for up to " + TimeUnit.SECONDS.toMinutes(appiumServerInitializationTimeout) + "min.");
            try {
                TARGET_HUB_URL = TARGET_HUB_URL.contains("0.0.0.0") ? TARGET_HUB_URL.replace("0.0.0.0", "localhost") : TARGET_HUB_URL;
                if (Properties.flags.forceCheckStatusOfRemoteServer()) {
                    var statusCode = attemptRemoteServerPing();
                    ReportManager.logDiscrete("Remote server is online, established successful connection with status code: " + statusCode + ".");
                }
            } catch (Throwable throwable) {
                ReportManagerHelper.logDiscrete(throwable, Level.DEBUG);
                failAction("Failed to connect to remote server.", throwable);
            }
        }

        // stage 2: create remote driver instance (requires some time with dockerized appium)
        ReportManager.logDiscrete("Attempting to instantiate remote driver instance for up to " + TimeUnit.SECONDS.toMinutes(remoteServerInstanceCreationTimeout) + "min.");
        try (ProgressBarLogger pblogger = new ProgressBarLogger("Instantiating...", (int) remoteServerInstanceCreationTimeout)) {
            setDriver(attemptRemoteServerConnection(capabilities));
            ((RemoteWebDriver) driver).setFileDetector(new LocalFileDetector());
            if (!isNotMobileExecution() && SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase("Android")) {
                // https://github.com/appium/appium-uiautomator2-driver#settings-api
                ((AppiumDriver) driver).setSetting(Setting.WAIT_FOR_IDLE_TIMEOUT, 5000);
                ((AppiumDriver) driver).setSetting(Setting.ALLOW_INVISIBLE_ELEMENTS, true);
                ((AppiumDriver) driver).setSetting(Setting.IGNORE_UNIMPORTANT_VIEWS, false);
                ((AppiumDriver) driver).setSetting("enableMultiWindows", true);
//        elementResponseAttributes, shouldUseCompactResponses
                ((AppiumDriver) driver).setSetting(Setting.MJPEG_SCALING_FACTOR, 25);
                ((AppiumDriver) driver).setSetting(Setting.MJPEG_SERVER_SCREENSHOT_QUALITY, 100);
                ((AppiumDriver) driver).setSetting("mjpegBilinearFiltering", true);
                // ((AppiumDriver) driver).setSetting("limitXPathContextScope", false);
//                ((AppiumDriver) driver).setSetting("disableIdLocatorAutocompletion", true);
//        https://github.com/appium/appium-uiautomator2-driver#mobile-deeplink
//        http://code2test.com/appium-tutorial/how-to-use-uiselector-in-appium/
//        https://github.com/appium/appium-uiautomator2-driver
            }
            ReportManager.logDiscrete("Successfully instantiated remote driver instance.");
        } catch (Throwable throwable) {
            failAction("Failed to instantiate remote driver instance.", throwable);
        }
    }

    private void configureRemoteDriverInstance(DriverType driverType, DesiredCapabilities appiumDesiredCapabilities) {
        switch (driverType) {
            case FIREFOX -> setRemoteDriverInstance(optionsManager.getFfOptions());
            case IE -> setRemoteDriverInstance(optionsManager.getIeOptions());
            case CHROME, CHROMIUM -> setRemoteDriverInstance(optionsManager.getChOptions());
            case EDGE -> setRemoteDriverInstance(optionsManager.getEdOptions());
            case SAFARI, WEBKIT -> {
                if (!Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform()) && !Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
                    setRemoteDriverInstance(optionsManager.getSfOptions());
                } else {
                    setRemoteDriverInstance(appiumDesiredCapabilities);
                }
            }
            case APPIUM_CHROME, APPIUM_CHROMIUM -> {
                WebDriverManager.chromedriver().browserVersion(SHAFT.Properties.mobile.browserVersion()).setup();
                appiumDesiredCapabilities.setCapability("chromedriverExecutable", WebDriverManager.chromedriver().getDownloadedDriverPath());
                setRemoteDriverInstance(appiumDesiredCapabilities);
            }
            case APPIUM_MOBILE_NATIVE, APPIUM_SAMSUNG_BROWSER, APPIUM_BROWSER ->
                    setRemoteDriverInstance(appiumDesiredCapabilities);
            default ->
                    failAction("Unsupported Driver Type \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\".");
        }
        var driverName = driverType.getValue();
        if (driverName.contains("MobileApp")) {
            driverName = driverName.replace("Mobile", Properties.platform.targetPlatform());
        }
        ReportManager.log("Successfully Opened \"" + JavaHelper.convertToSentenceCase(driverName) + "\".");
    }

    private void attachWebDriverLogs() {
        // TODO: capture logs and record video in case of retrying failed test
        if (SHAFT.Properties.reporting.captureWebDriverLogs()) {
            try {
                var driverLogs = driver.manage().logs();
                driverLogs.getAvailableLogTypes().forEach(logType -> {
                    var logBuilder = new StringBuilder();
                    driverLogs.get(logType).getAll().forEach(logEntry -> logBuilder.append(logEntry.toString()).append(System.lineSeparator()));
                    ReportManagerHelper.attach("Selenium WebDriver Logs", logType, logBuilder.toString());
                });
            } catch (WebDriverException e) {
                // exception when the defined logging is not supported
            }
        }
    }

    public void initializeDriver() {
        var mobile_browserName = SHAFT.Properties.mobile.browserName();
        String targetBrowserName = SHAFT.Properties.web.targetBrowserName();
        // it's null in case of native cucumber execution
        if (Reporter.getCurrentTestResult() != null) {
            var overridingBrowserName = Reporter.getCurrentTestResult().getTestContext().getCurrentXmlTest().getParameter("targetBrowserName");
            if (overridingBrowserName != null && !overridingBrowserName.isBlank()) {
                targetBrowserName = overridingBrowserName;
            }
        }
        DriverFactoryHelper.targetBrowserName = targetBrowserName;
        initializeDriver(getDriverTypeFromName((mobile_browserName.isBlank()) ? targetBrowserName : mobile_browserName), null);
    }

    public void initializeDriver(@NonNull DriverType driverType) {
        initializeDriver(driverType, null);
    }

    public void initializeDriver(MutableCapabilities customDriverOptions) {
        var mobile_browserName = SHAFT.Properties.mobile.browserName();
        String targetBrowserName;

        var overridingBrowserName = Reporter.getCurrentTestResult().getTestContext().getCurrentXmlTest().getParameter("targetBrowserName");
        if (overridingBrowserName != null && !overridingBrowserName.isBlank()) {
            targetBrowserName = overridingBrowserName;
        } else {
            targetBrowserName = SHAFT.Properties.web.targetBrowserName();
        }
        DriverFactoryHelper.targetBrowserName = (mobile_browserName == null || mobile_browserName.isBlank()) ? targetBrowserName : mobile_browserName;
        initializeDriver((getDriverTypeFromName(DriverFactoryHelper.targetBrowserName)), customDriverOptions);
    }

    public void initializeDriver(@NonNull DriverType driverType, MutableCapabilities customDriverOptions) {
        initializeSystemProperties();
        try {
            var isMobileExecution = Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform()) || Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform());
            if (isMobileExecution) {
                //mobile execution
                if (isMobileWebExecution()) {
                    // org.openqa.selenium.InvalidArgumentException: Parameters were incorrect. We wanted {"required":["x","y","width","height"]} and you sent ["width","height"]
                    SHAFT.Properties.web.set().headlessExecution(false);
                    if (SHAFT.Properties.mobile.browserName().equalsIgnoreCase(DriverType.APPIUM_SAMSUNG_BROWSER.getValue())) {
                        driverType = DriverType.APPIUM_SAMSUNG_BROWSER;
                    } else {
                        driverType = DriverType.APPIUM_CHROME;
                    }
                } else {
                    driverType = DriverType.APPIUM_MOBILE_NATIVE;
                }
                optionsManager.setDriverOptions(driverType, customDriverOptions);
                createNewRemoteDriverInstance(driverType);
            } else {
                //desktop execution
                optionsManager.setDriverOptions(driverType, customDriverOptions);
                switch (SHAFT.Properties.platform.executionAddress()) {
                    case "local" -> createNewLocalDriverInstance(driverType, 6);
                    case "dockerized" -> createNewDockerizedDriverInstance(driverType);
                    default -> createNewRemoteDriverInstance(driverType);
                }
            }

            if (SHAFT.Properties.web.headlessExecution()) {
                driver.manage().window().setSize(new Dimension(TARGET_WINDOW_SIZE.getWidth(), TARGET_WINDOW_SIZE.getHeight()));
            } else {
                Dimension browserWindowSize = new Dimension(SHAFT.Properties.web.browserWindowWidth(), SHAFT.Properties.web.browserWindowHeight());
                if (!isMobileExecution && !SHAFT.Properties.flags.autoMaximizeBrowserWindow()) {
                    driver.manage().window().setSize(browserWindowSize);
                }
            }

            if (!isMobileExecution) {
                var targetBrowserName = SHAFT.Properties.web.targetBrowserName().toLowerCase();
                if (SHAFT.Properties.flags.autoMaximizeBrowserWindow() && (targetBrowserName.contains(Browser.SAFARI.browserName().toLowerCase()) || targetBrowserName.contains(Browser.FIREFOX.browserName().toLowerCase()))) {
                    new BrowserActions(this).maximizeWindow();
                }
            }
            // start session recording
            RecordManager.startVideoRecording(driver);
        } catch (NullPointerException e) {
            FailureReporter.fail(DriverFactoryHelper.class, "Unhandled Exception with Driver Type \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\".", e);
        }

        if (SHAFT.Properties.healenium.healEnabled()) {
            ReportManager.logDiscrete("Initializing Healenium's Self Healing Driver...");
//            driver =ThreadGuard.protect(SelfHealingDriver.create(driver)));
            setDriver(SelfHealingDriver.create(driver));
        }
    }

    public void initializeDriver(@NonNull WebDriver driver) {
        initializeSystemProperties();
        ReportManager.log("Attaching to existing driver session '" + driver + "'.");
        setDriver(driver);
    }
}
