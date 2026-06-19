package com.shaft.driver.internal.DriverFactory;

import com.epam.healenium.SelfHealingDriver;
import com.shaft.driver.DriverFactory.DriverType;
import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptionRule;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptor;
import com.shaft.gui.internal.healing.HealingManager;
import com.shaft.gui.internal.healing.HealingStrategy;
import com.shaft.gui.internal.video.RecordManager;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertiesHelper;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ProgressBarLogger;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.Setting;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.remote.AutomationName;
import io.github.bonigarcia.wdm.WebDriverManager;
import io.github.bonigarcia.wdm.config.WebDriverManagerException;
import io.qameta.allure.Step;
import lombok.*;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.*;
import org.openqa.selenium.bidi.BiDiProvider;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chromium.AddHasCdp;
import org.openqa.selenium.devtools.Command;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.DevToolsException;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.edge.EdgeDriver;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.ie.InternetExplorerDriver;
import org.openqa.selenium.remote.*;
import org.openqa.selenium.remote.http.ClientConfig;
import org.openqa.selenium.remote.http.ConnectionFailedException;
import org.openqa.selenium.safari.SafariDriver;
import org.testng.Reporter;

import java.io.ByteArrayInputStream;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * Internal helper responsible for initializing, managing, and closing WebDriver sessions.
 */
public class DriverFactoryHelper {
    private static final String WEB_DRIVER_MANAGER_MESSAGE = "Resolving the local browser driver binary.";
    private static final String WEB_DRIVER_MANAGER_DOCKERIZED_MESSAGE = "Resolving the Docker browser image and driver binary.";
    private static final String SELENIUM_WEBSOCKET_LISTENER_LOGGER = "org.openqa.selenium.remote.http.WebSocket$Listener";
    private static final ThreadLocal<WebDriverManager> webDriverManager = new ThreadLocal<>();
    private static final Object LOCAL_DRIVER_INITIALIZATION_LOCK = new Object();
    @Getter(AccessLevel.PUBLIC)
    private static final Dimension TARGET_WINDOW_SIZE = new Dimension(1920, 1080);
    private static final long appiumServerInitializationTimeout = TimeUnit.MINUTES.toSeconds(SHAFT.Properties.timeouts.timeoutForRemoteServerToBeUp()); // seconds
    private static final int appiumServerInitializationPollingInterval = 1; // seconds
    private static final long remoteServerInstanceCreationTimeout = TimeUnit.MINUTES.toSeconds(SHAFT.Properties.timeouts.remoteServerInstanceCreationTimeout()); // seconds
    private static final int appiumServerPreparationPollingInterval = 1; // seconds
    private static final long localDriverInitializationRetryDelayMs = TimeUnit.SECONDS.toMillis(1);
    private static final long localDriverBiDiTimeoutGraceMs = TimeUnit.SECONDS.toMillis(5);
    private static final ThreadLocal<String> TARGET_HUB_URL = new ThreadLocal<>();
    @Getter(AccessLevel.PUBLIC)
    private static volatile String targetBrowserName = "";
    @Getter(AccessLevel.PUBLIC)
    private static volatile boolean killSwitch = false;
    private final OptionsManager optionsManager = new OptionsManager();
    @Setter
    @Getter
    private WebDriver driver;
    private BrowserNetworkInterceptor browserNetworkInterceptor;

    /**
     * Creates a helper instance without an attached WebDriver.
     */
    public DriverFactoryHelper() {
    }

    /**
     * Creates a helper instance attached to an existing WebDriver session.
     *
     * @param driver the active WebDriver session
     */
    public DriverFactoryHelper(WebDriver driver) {
        setDriver(driver);
    }

    /**
     * Registers a browser network interception rule for the active WebDriver session.
     *
     * @param rule the rule to activate
     */
    public void registerBrowserNetworkInterceptionRule(BrowserNetworkInterceptionRule rule) {
        getBrowserNetworkInterceptor().addRule(rule);
    }

    /**
     * Clears browser network interception rules for the active WebDriver session.
     */
    public void clearBrowserNetworkInterceptors() {
        if (browserNetworkInterceptor != null) {
            browserNetworkInterceptor.clear();
        }
    }

    private BrowserNetworkInterceptor getBrowserNetworkInterceptor() {
        if (driver == null) {
            throw new IllegalStateException("Cannot configure browser network interception before creating a WebDriver session.");
        }
        if (browserNetworkInterceptor == null) {
            browserNetworkInterceptor = new BrowserNetworkInterceptor(driver);
        }
        return browserNetworkInterceptor;
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

    private static RuntimeException createSessionInitializationTimeoutFailure(String sessionDescription, long timeoutInSeconds, long startTimeInMillis, Throwable primaryCause, List<Throwable> priorFailures, String targetEndpoint) {
        var message = "Timed out while " + sessionDescription + ".";
        if (timeoutInSeconds > 0) {
            message = message + " Configured timeout: " + timeoutInSeconds + "s.";
        }
        message = message + " Elapsed time: " + (System.currentTimeMillis() - startTimeInMillis) + "ms.";
        var redactedTarget = redactUriCredentials(targetEndpoint);
        if (!redactedTarget.isBlank()) {
            message = message + " Target: `" + redactedTarget + "`.";
        }
        var timeoutException = new java.util.concurrent.TimeoutException(message);
        if (primaryCause != null) {
            timeoutException.initCause(primaryCause);
        }
        addSuppressedFailures(timeoutException, priorFailures, primaryCause);
        var initializationFailure = new RuntimeException("Failed to initialize session before timeout. " + message, timeoutException);
        addSuppressedFailures(initializationFailure, priorFailures, primaryCause);
        return initializationFailure;
    }

    private static RuntimeException createSessionInitializationFailure(String sessionDescription, long startTimeInMillis, Throwable primaryCause, List<Throwable> priorFailures, String targetEndpoint) {
        var message = "Failed to initialize " + sessionDescription + " session. Elapsed time: " + (System.currentTimeMillis() - startTimeInMillis) + "ms.";
        var redactedTarget = redactUriCredentials(targetEndpoint);
        if (!redactedTarget.isBlank()) {
            message = message + " Target: `" + redactedTarget + "`.";
        }
        var initializationFailure = (primaryCause == null)
                ? new RuntimeException(message)
                : new RuntimeException(message, primaryCause);
        addSuppressedFailures(initializationFailure, priorFailures, primaryCause);
        return initializationFailure;
    }

    private static void addSuppressedFailures(Throwable failureTarget, List<Throwable> candidateFailures, Throwable rootCause) {
        if (failureTarget == null || candidateFailures == null) {
            return;
        }
        for (var candidateFailure : candidateFailures) {
            if (candidateFailure == null || candidateFailure == failureTarget || candidateFailure == rootCause) {
                continue;
            }
            var alreadySuppressed = false;
            for (var suppressedFailure : failureTarget.getSuppressed()) {
                if (suppressedFailure == candidateFailure) {
                    alreadySuppressed = true;
                    break;
                }
            }
            if (!alreadySuppressed) {
                failureTarget.addSuppressed(candidateFailure);
            }
        }
    }

    private static boolean isCausedBy(Throwable throwable, Class<? extends Throwable> expectedCauseType) {
        var currentThrowable = throwable;
        while (currentThrowable != null) {
            if (expectedCauseType.isInstance(currentThrowable)) {
                return true;
            }
            currentThrowable = currentThrowable.getCause();
        }
        return false;
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

    @SneakyThrows({InterruptedException.class, MalformedURLException.class})
    private static int attemptRemoteServerPing() {
        boolean serverReady = false;
        var session = new SHAFT.API(normalizeRemoteServerPingBaseUrl(getTargetHubUrl()));
        var statusCode = 500;
        var pingFailures = new ArrayList<Throwable>();
        var startTime = System.currentTimeMillis();
        do {
            try {
                statusCode = session.get("status/").perform().getResponse().andReturn().statusCode();
                if (statusCode >= 200 && statusCode < 300) {
                    serverReady = true;
                }
            } catch (Throwable throwable1) {
                pingFailures.add(throwable1);
                // do nothing
                ReportManagerHelper.logDiscrete(throwable1, Level.DEBUG);
            }
            if (!serverReady) {
                //noinspection BusyWait
                Thread.sleep(TimeUnit.SECONDS.toMillis(appiumServerInitializationPollingInterval));
            }
        } while (!serverReady && (System.currentTimeMillis() - startTime < TimeUnit.SECONDS.toMillis(appiumServerInitializationTimeout)));
        if (!serverReady) {
            var primaryFailure = pingFailures.isEmpty() ? null : pingFailures.get(pingFailures.size() - 1);
            throw createSessionInitializationTimeoutFailure(
                    "checking the remote server status endpoint",
                    appiumServerInitializationTimeout,
                    startTime,
                    primaryFailure,
                    pingFailures,
                    getTargetHubUrl());
        }
        return statusCode;
    }

    /**
     * Normalizes the remote server base URL used by ping requests so appending {@code status/}
     * always produces a valid endpoint.
     *
     * <p>Normalization rules:
     * <ul>
     *   <li>Adds {@code http://} when scheme is missing</li>
     *   <li>Defaults missing or blank path to {@code /}</li>
     *   <li>Ensures the path ends with a trailing {@code /}</li>
     * </ul>
     *
     * @param remoteServerUrl the configured remote server address
     * @return normalized base URL with explicit scheme and a trailing slash in path form
     */
    private static String normalizeRemoteServerPingBaseUrl(String remoteServerUrl) throws MalformedURLException {
        if (remoteServerUrl == null || remoteServerUrl.isBlank()) {
            throw new MalformedURLException("Remote server URL must not be null or blank.");
        }
        var normalizedUrl = remoteServerUrl.trim();
        if (!normalizedUrl.toLowerCase(Locale.ROOT).startsWith("http")) {
            normalizedUrl = "http://" + normalizedUrl;
        }
        URI uri;
        try {
            uri = URI.create(normalizedUrl);
        } catch (IllegalArgumentException illegalArgumentException) {
            var malformedURLException = new MalformedURLException("Invalid remote server URL `" + remoteServerUrl + "`.");
            malformedURLException.addSuppressed(illegalArgumentException);
            throw malformedURLException;
        }
        var path = uri.getPath();
        String normalizedPath;
        if (path == null || path.isBlank()) {
            normalizedPath = "/";
        } else if (path.endsWith("/")) {
            normalizedPath = path;
        } else {
            normalizedPath = path + "/";
        }
        return uri.resolve(normalizedPath).toString();
    }

    /**
     * Redacts user-info (credentials) from a URI string to prevent secret leakage in logs.
     * If parsing fails, returns the original string unchanged.
     *
     * @param url the URI string that may contain embedded credentials
     * @return the URI with user-info replaced by {@code ***:***}, or the original string on parse failure
     */
    private static String redactUriCredentials(String url) {
        if (url == null) {
            return "";
        }
        try {
            URI uri = URI.create(url);
            String userInfo = uri.getUserInfo();
            if (userInfo != null && !userInfo.isEmpty()) {
                return url.replace(userInfo + "@", "***:***@");
            }
        } catch (IllegalArgumentException ignored) {
            // fall through – return as-is
        }
        return url;
    }

    private static ClientConfig createRemoteWebDriverClientConfig(URL targetExecutionUrl) {
        var timeout = Duration.ofSeconds(remoteServerInstanceCreationTimeout);
        return ClientConfig.defaultConfig()
                .baseUrl(targetExecutionUrl)
                .connectionTimeout(timeout)
                .readTimeout(timeout);
    }

    @SneakyThrows({InterruptedException.class, MalformedURLException.class})
    private static WebDriver attemptRemoteServerConnection(Capabilities capabilities) {
        WebDriver driver = null;
        boolean isRemoteConnectionEstablished = false;
        var startTime = System.currentTimeMillis();
        Throwable primaryFailure = null;
        var connectionFailures = new ArrayList<Throwable>();
        do {
            try {
                driver = connectToRemoteServer(capabilities);
                isRemoteConnectionEstablished = true;
            } catch (URISyntaxException uriSyntaxException) {
                // URL is malformed — retrying will not help; break immediately
                primaryFailure = uriSyntaxException;
                connectionFailures.add(uriSyntaxException);
                ReportManagerHelper.logDiscrete(uriSyntaxException, Level.DEBUG);
                break;
            } catch (SessionNotCreatedException |
                     ConnectionFailedException sessionNotCreatedException) {
                primaryFailure = sessionNotCreatedException;
                connectionFailures.add(sessionNotCreatedException);
                String message = sessionNotCreatedException.getMessage();
                if (message != null &&
                        (message.contains("missing in the capabilities")
                        || message.contains("not allowed on your current plan")
                        || message.contains("has been exhausted"))) {
                    break;
                } else {
                    ReportManagerHelper.logDiscrete(sessionNotCreatedException, Level.DEBUG);
                }
            } catch (TimeoutException timeoutException) {
                primaryFailure = timeoutException;
                connectionFailures.add(timeoutException);
                ReportManagerHelper.logDiscrete(timeoutException, Level.DEBUG);
            } catch (WebDriverException webDriverException) {
                if (!isCausedBy(webDriverException, java.util.concurrent.TimeoutException.class)) {
                    throw webDriverException;
                }
                primaryFailure = webDriverException;
                connectionFailures.add(webDriverException);
                ReportManagerHelper.logDiscrete(webDriverException, Level.DEBUG);
            }
            if (!isRemoteConnectionEstablished) {
                //noinspection BusyWait
                Thread.sleep(TimeUnit.SECONDS.toMillis(appiumServerPreparationPollingInterval));
            }
        } while (!isRemoteConnectionEstablished && (System.currentTimeMillis() - startTime < TimeUnit.SECONDS.toMillis(remoteServerInstanceCreationTimeout)));
        if (!isRemoteConnectionEstablished) {
            long timeoutLimitInMillis = TimeUnit.SECONDS.toMillis(remoteServerInstanceCreationTimeout);
            if (System.currentTimeMillis() - startTime >= timeoutLimitInMillis) {
                throw createSessionInitializationTimeoutFailure(
                        "creating a remote WebDriver session",
                        remoteServerInstanceCreationTimeout,
                        startTime,
                        primaryFailure,
                        connectionFailures,
                        getTargetHubUrl());
            }
            throw createSessionInitializationFailure(
                    "creating a remote WebDriver session",
                    startTime,
                    primaryFailure,
                    connectionFailures,
                    getTargetHubUrl());
        }
        return driver;
    }

    private static WebDriver connectToRemoteServer(Capabilities capabilities) throws URISyntaxException, MalformedURLException {
        var targetHubUrl = getTargetHubUrl();
        if (targetHubUrl == null || targetHubUrl.isBlank()) {
            throw new MalformedURLException("Remote server URL must not be null or blank.");
        }
        var targetLambdaTestHubURL = targetHubUrl.startsWith("https://")
                ? targetHubUrl
                : targetHubUrl.replaceFirst("^http://", "https://");
        var targetPlatform = Properties.platform.targetPlatform();
        var targetMobileHubUrl = targetHubUrl.replace("@", "@mobile-");
        if (targetMobileHubUrl.startsWith("http://")) {
            targetMobileHubUrl = targetMobileHubUrl.replaceFirst("^http://", "https://");
        }

        var isAndroidExecution = targetPlatform.equalsIgnoreCase(Platform.ANDROID.toString());
        var isIosExecution = targetPlatform.equalsIgnoreCase(Platform.IOS.toString());
        var isLambdaTestExecution = SHAFT.Properties.platform.executionAddress().contains("lambdatest");

        var targetExecutionUrl = "";

        if (isLambdaTestExecution && !isMobileWebExecution()) targetExecutionUrl = targetMobileHubUrl;
        else if (isLambdaTestExecution) targetExecutionUrl = targetLambdaTestHubURL;
        else targetExecutionUrl = targetHubUrl;

        ReportManagerHelper.logDiscrete("Target Execution URI used for remote connection: `" + redactUriCredentials(targetExecutionUrl) + "`.", Level.DEBUG);
        ReportManagerHelper.logDiscrete("Capabilities after processing: `" + capabilities.toString() + "`.", Level.DEBUG);
        try {
            var targetExecutionUrlObject = URI.create(targetExecutionUrl).toURL();
            //builder code block, has issues in many cases, test it locally via grid before using it
//            if (isAndroidExecution) return AndroidDriver.builder().address(targetExecutionUrl).oneOf(capabilities).build();
//            else if (isIosExecution) return IOSDriver.builder().address(targetExecutionUrl).oneOf(capabilities).build();
//            else return RemoteWebDriver.builder().address(targetExecutionUrl).oneOf(capabilities).build();
            // legacy constructor-based code block
            if (isAndroidExecution) return new AndroidDriver(targetExecutionUrlObject, capabilities);
            else if (isIosExecution) return new IOSDriver(targetExecutionUrlObject, capabilities);
            else {
                var driver = new RemoteWebDriver(targetExecutionUrlObject, capabilities, createRemoteWebDriverClientConfig(targetExecutionUrlObject));
                driver.setFileDetector(new LocalFileDetector());
                var augmenter = new Augmenter();
                var targetBrowser = SHAFT.Properties.web.targetBrowserName().toLowerCase();
                if (Arrays.asList("chrome", "edge").contains(targetBrowser)) {
                    augmenter.addDriverAugmentation(new AddHasCdp() {
                        @Override
                        public Map<String, CommandInfo> getAdditionalCommands() {
                            return Map.of();
                        }
                    });
                }
                if (SHAFT.Properties.platform.enableBiDi())
                    augmenter.addDriverAugmentation(new BiDiProvider());
                return augmenter.augment(driver);
            }
        } catch (Throwable throwable) {
            ReportManagerHelper.logDiscrete(throwable, Level.DEBUG);
            throw throwable;
        }
    }

    /**
     * Initializes and normalizes system-level execution properties before driver creation.
     */
    public static void initializeSystemProperties() {
        PropertiesHelper.postProcessing();
        var executionAddress = SHAFT.Properties.platform.executionAddress();
        if (executionAddress == null || executionAddress.isBlank()) {
            setTargetHubUrl(null);
            return;
        }
        executionAddress = executionAddress.trim();
        setTargetHubUrl(executionAddress.toLowerCase(Locale.ROOT).startsWith("http")
                ? executionAddress
                : "http://" + executionAddress);
    }

    private static String getTargetHubUrl() {
        return TARGET_HUB_URL.get();
    }

    private static void setTargetHubUrl(String targetHubUrl) {
        if (targetHubUrl == null) {
            TARGET_HUB_URL.remove();
        } else {
            TARGET_HUB_URL.set(targetHubUrl);
        }
    }

    /**
     * Closes the currently attached driver session and clears the local driver reference.
     */
    public void closeDriver() {
        closeDriver(driver);
        setDriver(null);
    }

    /**
     * Closes the given WebDriver session and performs all associated teardown tasks:
     * attaches video recording (if scope is DriverSession), collects WebDriver logs,
     * handles dockerized driver cleanup, and removes the WebDriverManager reference.
     *
     * <p>The method handles the following edge cases gracefully:
     * <ul>
     *   <li>Driver already closed — logs at DEBUG level and continues</li>
     *   <li>{@code null} driver — logs an informational message and returns</li>
     *   <li>Exceptions during {@code close()} or {@code quit()} — caught and logged so the
     *       remaining teardown (log attachment, state cleanup) still executes</li>
     * </ul>
     *
     * @param driver the WebDriver instance to close; if {@code null}, the method is a no-op
     */
    @Step("Close Driver Session")
    public void closeDriver(WebDriver driver) {
        if (driver != null) {
            java.util.logging.Logger seleniumWebSocketLogger = java.util.logging.Logger.getLogger(SELENIUM_WEBSOCKET_LISTENER_LOGGER);
            java.util.logging.Level previousWebSocketLoggerLevel = seleniumWebSocketLogger.getLevel();
            seleniumWebSocketLogger.setLevel(java.util.logging.Level.SEVERE);
            if (SHAFT.Properties.visuals.videoParamsScope().equals("DriverSession")) {
                RecordManager.attachVideoRecording();
            }
            try {
                attachWebDriverLogs(driver);
                clearBrowserNetworkInterceptors();
                //if dockerized wdm.quit the relevant one
                if (SHAFT.Properties.platform.executionAddress().toLowerCase().contains("dockerized")) {
                    var pathToRecording = webDriverManager.get().getDockerRecordingPath(driver);
                    webDriverManager.get().quit(driver);
                    RecordManager.attachVideoRecording(pathToRecording);
                } else {
                    try {
                        driver.close();
                    } catch (Exception e) {
                        ReportManagerHelper.logDiscrete(e, Level.DEBUG);
                    }
                    driver.quit();
                }
            } catch (WebDriverException | NullPointerException e) {
                // driver was already closed at an earlier stage
                ReportManagerHelper.logDiscrete(e, Level.DEBUG);
            } catch (Exception e) {
                ReportManagerHelper.logDiscrete(e);
            } finally {
                HealingManager.clear(driver);
                browserNetworkInterceptor = null;
                seleniumWebSocketLogger.setLevel(previousWebSocketLoggerLevel);
                webDriverManager.remove();
                ReportManager.log("Closed the WebDriver session.");
            }
        } else {
            ReportManager.log("WebDriver session was already closed.");
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
            ReportManagerHelper.logDiscrete(e, Level.DEBUG);
            ReportManager.logDiscrete("Could not disable browser cache. Disable 'SHAFT.Properties.flags.disableCache()' or use a compatible browser version.");
        }
    }

    private static void runWithLocalDriverInitializationLock(Runnable localDriverInitializer) {
        synchronized (LOCAL_DRIVER_INITIALIZATION_LOCK) {
            localDriverInitializer.run();
        }
    }

    private void createNewLocalDriverInstance(DriverType driverType, int retryAttempts) {
        String targetPlatform = Properties.platform.targetPlatform().toLowerCase();
        String initialLog = "Starting local WebDriver session: \"" + targetPlatform + " | " + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\"";
        if (SHAFT.Properties.web.headlessExecution()) {
            initialLog = initialLog + ", headless";
        }
        initialLog = initialLog.replace(targetPlatform, JavaHelper.convertToSentenceCase(targetPlatform));
        ReportManager.logDiscrete(initialLog + ".");
        var localDriverFailures = new ArrayList<Throwable>();
        var initializationStartTime = System.currentTimeMillis();
        for (int attempt = 0; attempt <= retryAttempts; attempt++) {
            try {
                ReportManager.logDiscrete(WEB_DRIVER_MANAGER_MESSAGE);
                runWithLocalDriverInitializationLock(() -> {
                    switch (driverType) {
                        case FIREFOX -> setDriver(new FirefoxDriver(optionsManager.getFfOptions()));
                        case IE -> setDriver(new InternetExplorerDriver(optionsManager.getIeOptions()));
                        case CHROME -> {
                            setDriver(new ChromeDriver(optionsManager.getChOptions()));
                            disableCacheEdgeAndChrome();
                        }
                        case EDGE -> {
                            // Fix for Microsoft Edge CDN migration from msedgedriver.azureedge.net to msedgedriver.microsoft.com
                            // This ensures Selenium Manager uses the correct download URL for EdgeDriver.
                            ThreadLocalPropertiesManager.setGlobalProperty("SE_DRIVER_MIRROR_URL", "https://msedgedriver.microsoft.com");
                            setDriver(new EdgeDriver(optionsManager.getEdOptions()));
                            disableCacheEdgeAndChrome();
                        }
                        case SAFARI -> setDriver(new SafariDriver(optionsManager.getSfOptions()));
                        default ->
                                failAction("Unsupported Driver Type \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\".");
                    }
                });
                String successMessage = initialLog.replace("Starting local WebDriver session", "Started local WebDriver session") + ".";
                successMessage = successMessage + " (attempt " + (attempt + 1) + "/" + (retryAttempts + 1) + ", elapsed " + (System.currentTimeMillis() - initializationStartTime) + "ms).";
                List<Object> launchScreenshot = captureLaunchScreenshot();
                if (launchScreenshot != null) {
                    ReportManagerHelper.log(successMessage, List.of(launchScreenshot));
                } else {
                    ReportManager.log(successMessage);
                }
                return;
            } catch (Exception exception) {
                localDriverFailures.add(exception);
                String message = exception.getMessage();
                boolean shouldRetry = attempt < retryAttempts;
                if (message != null && message.contains("cannot create default profile directory")) {
                    // this exception happens when the profile directory is not correct, very specific case
                    // should fail immediately
                    shouldRetry = false;
                } else if (message != null && message.contains("DevToolsActivePort file doesn't exist")) {
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
                } else if (message != null && message.contains("Failed to initialize BiDi Mapper")) {
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
                } else if (message != null && message.contains("The Safari instance is already paired with another WebDriver session.")) {
                    //this issue happens when running locally via safari/mac platform
                    // attempting blind fix by trying to quit existing safari instances if any
                    try {
                        SHAFT.CLI.terminal().performTerminalCommands(Arrays.asList(
                                "osascript -e 'quit app \"Safari\"'", "osascript -e 'quit app \"SafariDriver\"'",
                                "pkill -x Safari", "pkill -x SafariDriver",
                                "killall Safari", "killall SafariDriver"));
                        //minimizing retry attempts to save execution time
                        shouldRetry = false;
                    } catch (Throwable throwable) {
                        ReportManagerHelper.logDiscrete(throwable, Level.DEBUG);
                    }
                } else if (isCausedBy(exception, java.util.concurrent.TimeoutException.class)
                        || (message != null && message.contains("java.util.concurrent.TimeoutException"))) {
                    // this happens in case an auto closable BiDi session was left hanging
                    // the default timeout is around 30 seconds, so this grace period gives the socket cleanup a chance
                    // before the next retry.
                    if (shouldRetry) {
                        try {
                            Thread.sleep(localDriverBiDiTimeoutGraceMs);
                        } catch (InterruptedException e) {
                            Thread.currentThread().interrupt();
                            ReportManagerHelper.logDiscrete(e, Level.DEBUG);
                        }
                    }
                }
                // attempting blind fix by trying to quit existing driver if any
                try {
                    if (driver != null) {
                        driver.quit();
                    }
                } catch (Throwable throwable) {
                    ReportManagerHelper.logDiscrete(throwable, Level.DEBUG);
                } finally {
                    setDriver(null);
                }
                if (!shouldRetry) {
                    var failure = createSessionInitializationFailure(
                            "local",
                            initializationStartTime,
                            exception,
                            localDriverFailures,
                            null);
                    failAction("Failed to create new Browser Session", failure);
                }
                try {
                    Thread.sleep(localDriverInitializationRetryDelayMs);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    ReportManagerHelper.logDiscrete(e, Level.DEBUG);
                }
            }
        }
    }

    private void createNewDockerizedDriverInstance(DriverType driverType) {
        String initialLog = "Starting Docker WebDriver session: \"" + Properties.platform.targetPlatform() + " | " + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\"";
        if (SHAFT.Properties.web.headlessExecution()) {
            initialLog = initialLog + ", headless";
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
            String successMessageDockerized = "Started Docker WebDriver session for " + JavaHelper.convertToSentenceCase(driverType.getValue()) + ".";
            List<Object> launchScreenshotDockerized = captureLaunchScreenshot();
            if (launchScreenshotDockerized != null) {
                ReportManagerHelper.log(successMessageDockerized, List.of(launchScreenshotDockerized));
            } else {
                ReportManager.log(successMessageDockerized);
            }
        } catch (WebDriverManagerException exception) {
            failAction("Failed to create new Dockerized Browser Session, are you sure Docker is available on your machine?", exception);
        }
    }

    private void createNewRemoteDriverInstance(DriverType driverType) {
        var initialLog = new StringBuilder();
        initialLog.append("Starting remote WebDriver session: \"").append(Properties.platform.targetPlatform());

        if (!Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform()) && !Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            initialLog.append(" | ").append(JavaHelper.convertToSentenceCase(driverType.getValue()));
        }

        initialLog.append(" | ").append(redactUriCredentials(getTargetHubUrl())).append("\"");

        if (SHAFT.Properties.web.headlessExecution() && !Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform()) && !Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            initialLog.append(", headless");
        }
        ReportManager.log(initialLog + ".");

        if (Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform()) || Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            optionsManager.initializeMobileDesiredCapabilities();
        }

        var remoteSessionStartTime = System.currentTimeMillis();
        try {
            configureRemoteDriverInstance(driverType, optionsManager.getAppiumCapabilities());
        } catch (UnreachableBrowserException e) {
            killSwitch = true;
            failAction("Unreachable Browser, terminated test suite execution.", createSessionInitializationFailure(
                    "remote session orchestration",
                    remoteSessionStartTime,
                    e,
                    List.of(e),
                    getTargetHubUrl()));
        } catch (WebDriverException e) {
            if (e.getMessage() !=null && e.getMessage().contains("Error forwarding the new session cannot find")) {
                ReportManager.logDiscrete("Remote WebDriver session failed for \"" + Properties.platform.targetPlatform() + "\", \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\", \"" + redactUriCredentials(getTargetHubUrl()) + "\".");
                failAction("Error forwarding the new session: Couldn't find a node that matches the desired capabilities.", createSessionInitializationFailure(
                        "remote session orchestration",
                        remoteSessionStartTime,
                        e,
                        List.of(e),
                        getTargetHubUrl()));
            } else {
                ReportManager.logDiscrete("Remote WebDriver session failed for \"" + Properties.platform.targetPlatform() + "\", \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\", \"" + redactUriCredentials(getTargetHubUrl()) + "\".");
                failAction("Unhandled Error.", createSessionInitializationFailure(
                        "remote session orchestration",
                        remoteSessionStartTime,
                        e,
                        List.of(e),
                        getTargetHubUrl()));
            }
        } catch (NoClassDefFoundError e) {
            failAction("Failed to create Remote WebDriver instance", createSessionInitializationFailure(
                    "remote session orchestration",
                    remoteSessionStartTime,
                    e,
                    List.of(e),
                    getTargetHubUrl()));
        }
    }

    @Step("Setting up remote driver instance")
    private void setRemoteDriverInstance(Capabilities capabilities) {
        // stage 1: ensure that the server is up and running
        if (SHAFT.Properties.timeouts.waitForRemoteServerToBeUp()) {
            ReportManager.logDiscrete("Waiting up to " + TimeUnit.SECONDS.toMinutes(appiumServerInitializationTimeout) + " minute(s) for the remote server to become ready.");
            var remoteReadinessStart = System.currentTimeMillis();
            try {
                var targetHubUrl = getTargetHubUrl();
                setTargetHubUrl(targetHubUrl != null && targetHubUrl.contains("0.0.0.0")
                        ? targetHubUrl.replace("0.0.0.0", "localhost")
                        : targetHubUrl);
                if (Properties.flags.forceCheckStatusOfRemoteServer()) {
                    var statusCode = attemptRemoteServerPing();
                    ReportManager.logDiscrete("Remote server is ready. Status code: " + statusCode + ".");
                }
                ReportManager.logDiscrete("Remote server readiness completed in " + (System.currentTimeMillis() - remoteReadinessStart) + "ms.");
            } catch (Throwable throwable) {
                ReportManagerHelper.logDiscrete(throwable, Level.DEBUG);
                failAction(
                        "Failed to connect to remote server.",
                        createSessionInitializationFailure(
                                "remote readiness",
                                remoteReadinessStart,
                                throwable,
                                null,
                                getTargetHubUrl()));
            }
        }

        // stage 2: create remote driver instance (requires some time with dockerized appium)
        ReportManager.logDiscrete("Creating the remote WebDriver session. Timeout: " + TimeUnit.SECONDS.toMinutes(remoteServerInstanceCreationTimeout) + " minute(s).");
        var remoteCreationStart = System.currentTimeMillis();
        try (ProgressBarLogger pblogger = new ProgressBarLogger("Instantiating...", (int) remoteServerInstanceCreationTimeout)) {
            setDriver(attemptRemoteServerConnection(capabilities));
            if (driver instanceof RemoteWebDriver remoteWebDriver)
                remoteWebDriver.setFileDetector(new LocalFileDetector());
            if (!isNotMobileExecution()
                    && SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase("Android")
                    && (driver instanceof AppiumDriver appiumDriver)) {
                // https://github.com/appium/appium-uiautomator2-driver#settings-api
                appiumDriver.setSetting(Setting.WAIT_FOR_IDLE_TIMEOUT, 5000);
                appiumDriver.setSetting(Setting.ALLOW_INVISIBLE_ELEMENTS, true);
                appiumDriver.setSetting(Setting.IGNORE_UNIMPORTANT_VIEWS, false);
                appiumDriver.setSetting("enableMultiWindows", true);
//        elementResponseAttributes, shouldUseCompactResponses
                appiumDriver.setSetting(Setting.MJPEG_SCALING_FACTOR, 25);
                appiumDriver.setSetting(Setting.MJPEG_SERVER_SCREENSHOT_QUALITY, 100);
                appiumDriver.setSetting("mjpegBilinearFiltering", true);
                // appiumDriver.setSetting("limitXPathContextScope", false);
//                appiumDriver.setSetting("disableIdLocatorAutocompletion", true);
//        https://github.com/appium/appium-uiautomator2-driver#mobile-deeplink
//        http://code2test.com/appium-tutorial/how-to-use-uiselector-in-appium/
//        https://github.com/appium/appium-uiautomator2-driver
            }
            ReportManager.logDiscrete("Remote WebDriver session was created.");
            ReportManager.logDiscrete("Remote WebDriver session creation completed in " + (System.currentTimeMillis() - remoteCreationStart) + "ms.");
        } catch (Throwable throwable) {
            //Root cause: "java.lang.NumberFormatException: Error at index 4 in: "4723wd""
            //this happens when the URL has an unsupported format
            Throwable throwable1 = throwable;
            if (FailureReporter.getRootCause(throwable1).contains("NumberFormatException")) {
                var newException = new MalformedURLException("Invalid remote server URL `" + getTargetHubUrl() + "`. Use one of the supported patterns: `local`, `dockerized`, `browserstack`, `host:port`, `http(s)://host:port[/path]`.");
                newException.addSuppressed(throwable1);
                throwable1 = newException;
            }
            failAction("Failed to create Remote WebDriver instance.", createSessionInitializationFailure(
                    "remote",
                    remoteCreationStart,
                    throwable1,
                    null,
                    getTargetHubUrl()));
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
            case APPIUM_MOBILE_NATIVE, APPIUM_SAMSUNG_BROWSER, APPIUM_BROWSER, APPIUM_FLUTTER ->
                    setRemoteDriverInstance(appiumDesiredCapabilities);
            default ->
                    failAction("Unsupported Driver Type \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\".");
        }
        var driverName = driverType.getValue();
        if (driverName.contains("MobileApp")) {
            driverName = driverName.replace("Mobile", Properties.platform.targetPlatform());
        }
        String successMessageRemote = "Started remote WebDriver session for \"" + JavaHelper.convertToSentenceCase(driverName) + "\".";
        List<Object> launchScreenshotRemote = captureLaunchScreenshot();
        if (launchScreenshotRemote != null) {
            ReportManagerHelper.log(successMessageRemote, List.of(launchScreenshotRemote));
        } else {
            ReportManager.log(successMessageRemote);
        }
    }

    /**
     * Captures a viewport screenshot after the driver session is established.
     * Returns the screenshot in the standard attachment-list format used by
     * {@link ReportManagerHelper#log(String, List)} so that it can be nested
     * directly inside the driver-startup Allure step rather than
     * appearing as a separate top-level step.
     * Returns {@code null} when no screenshot could be taken (e.g. remote server error).
     * Failures are logged discretely so they never mask the primary driver-creation outcome.
     *
     * @return attachment list {@code [type, name, InputStream]}, or {@code null} on failure
     */
    private List<Object> captureLaunchScreenshot() {
        if (!shouldAttachLaunchScreenshot()) {
            return null;
        }
        try {
            byte[] screenshot = ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
            if (screenshot.length > 0) {
                return Arrays.asList("Screenshot", "App Launch Screenshot", new ByteArrayInputStream(screenshot));
            }
        } catch (Exception e) {
            ReportManager.logDiscrete("Could not capture launch screenshot [" + e.getClass().getSimpleName() + "]: " + e.getMessage());
        }
        return null;
    }

    /**
     * Determines whether a launch screenshot should be attached to the driver-startup step.
     * This attachment is only relevant for native mobile app launches.
     *
     * @return {@code true} when running native mobile automation; otherwise {@code false}
     */
    private boolean shouldAttachLaunchScreenshot() {
        return isMobileNativeExecution();
    }

    private void attachWebDriverLogs() {
        attachWebDriverLogs(driver);
    }

    private void attachWebDriverLogs(WebDriver driverToCollectLogsFrom) {
        if (SHAFT.Properties.reporting.captureWebDriverLogs()) {
            if (driverToCollectLogsFrom == null) {
                return;
            }
            try {
                var driverLogs = driverToCollectLogsFrom.manage().logs();
                driverLogs.getAvailableLogTypes().forEach(logType -> {
                    var logBuilder = new StringBuilder();
                    driverLogs.get(logType).getAll().forEach(logEntry -> logBuilder.append(logEntry).append(System.lineSeparator()));
                    ReportManagerHelper.attach("Selenium WebDriver Logs", logType, logBuilder.toString());
                });
            } catch (Exception e) {
                // exception when the defined logging is not supported or the driver mock/session cannot expose logs
                ReportManagerHelper.logDiscrete(e, Level.DEBUG);
            }
        }
    }

    /**
     * Initializes a driver using the currently configured target browser/mobile settings.
     */
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

    /**
     * Initializes a driver using the supplied driver type and default options.
     *
     * @param driverType the desired driver type
     */
    public void initializeDriver(@NonNull DriverType driverType) {
        initializeDriver(driverType, null);
    }

    /**
     * Initializes a driver using configured browser name and custom capabilities.
     *
     * @param customDriverOptions custom capabilities to merge into default options
     */
    public void initializeDriver(MutableCapabilities customDriverOptions) {
        var mobile_browserName = SHAFT.Properties.mobile.browserName();
        String targetBrowserName = SHAFT.Properties.web.targetBrowserName();

        if (Reporter.getCurrentTestResult() != null) {
            var overridingBrowserName = Reporter.getCurrentTestResult().getTestContext().getCurrentXmlTest().getParameter("targetBrowserName");
            if (overridingBrowserName != null && !overridingBrowserName.isBlank()) {
                targetBrowserName = overridingBrowserName;
            }
        }
        DriverFactoryHelper.targetBrowserName = (mobile_browserName == null || mobile_browserName.isBlank()) ? targetBrowserName : mobile_browserName;
        initializeDriver((getDriverTypeFromName(DriverFactoryHelper.targetBrowserName)), customDriverOptions);
    }

    /**
     * Initializes a driver using an explicit driver type and optional custom capabilities.
     *
     * @param driverType the desired driver type
     * @param customDriverOptions custom capabilities to merge into default options
     */
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
                } else if (AutomationName.FLUTTER_INTEGRATION.equalsIgnoreCase(SHAFT.Properties.mobile.automationName())) {
                    // Flutter app execution - detected by automationName
                    driverType = DriverType.APPIUM_FLUTTER;
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

            if (!isMobileExecution && SHAFT.Properties.web.headlessExecution()) {
                if (SHAFT.Properties.flags.autoMaximizeBrowserWindow()) {
                    driver.manage().window().setSize(new Dimension(TARGET_WINDOW_SIZE.getWidth(), TARGET_WINDOW_SIZE.getHeight()));
                } else {
                    driver.manage().window().setSize(new Dimension(SHAFT.Properties.web.browserWindowWidth(), SHAFT.Properties.web.browserWindowHeight()));
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
            FailureReporter.fail(DriverFactoryHelper.class, "Unhandled exception with driver type \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\".", e);
        }

        if (HealingStrategy.current().usesHealenium()) {
            ReportManager.logDiscrete("Wrapping the session with Healenium self-healing driver.");
//            driver =ThreadGuard.protect(SelfHealingDriver.create(driver)));
            setDriver(SelfHealingDriver.create(driver));
        }
    }

    /**
     * Attaches the helper to an already initialized native WebDriver session.
     *
     * @param driver existing WebDriver instance
     */
    public void initializeDriver(@NonNull WebDriver driver) {
        initializeSystemProperties();
        ReportManager.log("Attached to existing WebDriver session '" + driver + "'.");
        setDriver(driver);
    }
}
