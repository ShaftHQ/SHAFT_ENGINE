package com.shaft.properties.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.listeners.internal.UpdateChecker;
import com.shaft.tools.internal.security.GoogleTink;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.AllureManager;
import com.shaft.tools.io.internal.ProjectStructureManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.aeonbits.owner.ConfigFactory;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.remote.Browser;

import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Initializes and post-processes framework configuration properties for runtime.
 *
 * <p>This helper loads default/custom property files, configures typed OWNER interfaces,
 * and applies platform-specific overrides after initialization.</p>
 *
 * <p>Thread safety: this class uses static state and should be initialized during engine
 * startup before parallel execution begins.</p>
 */
public class PropertiesHelper {
    /**
     * Where the bundled default property files get materialized for the user's project. The
     * conventional location is inside the project sources ({@code src/main/resources/properties}),
     * but that is wrong for SHAFT's own module test suites: bootstrap copies would rewrite
     * repo-tracked property files with EOL-only churn on every run. Setting the
     * {@code shaft.properties.bootstrapDirectory} system property (e.g. to a {@code target/}
     * folder in surefire) redirects every bootstrap write without changing user-facing behavior.
     */
    static final String BOOTSTRAP_DIRECTORY_PROPERTY = "shaft.properties.bootstrapDirectory";
    private static final String CONVENTIONAL_TARGET_PROPERTIES_FOLDER_PATH = "src/main/resources/properties";
    private static final AtomicBoolean postProcessingDone = new AtomicBoolean(false);
    private static final int DOWNLOAD_MAX_ATTEMPTS = 3;
    private static final Duration DOWNLOAD_BASE_DELAY = Duration.ofMillis(500);
    private static final Duration DOWNLOAD_MAX_DELAY = Duration.ofSeconds(2);

    /**
     * Initializes framework properties for standard execution.
     */
    public static void initialize() {
        //initialize default properties
        initializeDefaultProperties(false);
        //attach property files
        attachPropertyFiles();
        //load properties
        loadProperties();
        // Reset post-processing guard so next postProcessing() call re-evaluates overrides
        postProcessingDone.set(false);
    }

    /**
     * Initializes framework properties for AI-agent execution mode.
     */
    public static void initializeAiAgent() {
        //initialize default properties
        initializeDefaultProperties(true);
        //attach property files
        attachPropertyFiles();
        //load properties
        loadProperties();
        // Reset post-processing guard so next postProcessing() call re-evaluates overrides
        postProcessingDone.set(false);
    }

    private static void loadProperties() {
        //read custom property files (if any) into system properties
        PropertyFileManager.readCustomPropertyFiles();
        // Clear any stale per-thread overrides from before the load (e.g., during re-initialization).
        Properties.clearForCurrentThread();
        //load base property objects - these are the global defaults inherited by all test threads.
        Properties.basePaths = ConfigFactory.create(Paths.class); //reload paths in case the user changed something
        Properties.basePlatform = ConfigFactory.create(Platform.class);
        Properties.baseWeb = ConfigFactory.create(Web.class);
        Properties.baseMobile = ConfigFactory.create(Mobile.class);
        Properties.baseBrowserStack = ConfigFactory.create(BrowserStack.class);
        Properties.internal = ConfigFactory.create(Internal.class);
        Properties.baseFlags = ConfigFactory.create(Flags.class);
        Properties.cucumber = ConfigFactory.create(Cucumber.class);
        Properties.baseHealenium = ConfigFactory.create(Healenium.class);
        Properties.baseHealing = ConfigFactory.create(Healing.class);
        Properties.baseJira = ConfigFactory.create(Jira.class);
        Properties.basePattern = ConfigFactory.create(Pattern.class);
        Properties.baseReporting = ConfigFactory.create(Reporting.class);
        Properties.baseAllure = ConfigFactory.create(Allure.class);
        Properties.baseTinkey = ConfigFactory.create(Tinkey.class);
        Properties.testNG = ConfigFactory.create(TestNG.class);
        Properties.log4j = ConfigFactory.create(Log4j.class);
        Properties.baseVisuals = ConfigFactory.create(Visuals.class);
        Properties.baseTimeouts = ConfigFactory.create(Timeouts.class);
        Properties.basePerformance = ConfigFactory.create(Performance.class);
        Properties.baseLambdaTest = ConfigFactory.create(LambdaTest.class);
        Properties.baseApi = ConfigFactory.create(API.class);
        Properties.basePilot = ConfigFactory.create(Pilot.class);
        Properties.baseNaturalActions = ConfigFactory.create(NaturalActions.class);
        Properties.basePlaywright = ConfigFactory.create(Playwright.class);
        Properties.baseCapture = ConfigFactory.create(Capture.class);
        Properties.initialized = true;
    }

    public static void setKeySystemProperties() {
        //load paths as the default properties path is needed for the next step
        Properties.basePaths = ConfigFactory.create(Paths.class);
        // Selenium, Log4j, and ReportPortal read these from System; keep global copy for SHAFT too
        String propertiesPath = SHAFT.Properties.paths.properties();
        ThreadLocalPropertiesManager.setGlobalProperty("rp.properties.path", propertiesPath);
        System.setProperty("rp.properties.path", propertiesPath);
        ThreadLocalPropertiesManager.setGlobalProperty("webdriver.http.factory", "jdk-http-client");
        System.setProperty("webdriver.http.factory", "jdk-http-client");
        String log4jConfigPath = PropertyFileManager.getLog4jConfigPath();
        ThreadLocalPropertiesManager.setGlobalProperty("log4j.configurationFile", log4jConfigPath);
        System.setProperty("log4j.configurationFile", log4jConfigPath);
        ThreadLocalPropertiesManager.setGlobalProperty("allure.testng.hide.configuration.failures", "true");
        System.setProperty("allure.testng.hide.configuration.failures", "true");
        ThreadLocalPropertiesManager.setGlobalProperty("allure.testng.hide.disabled.tests", "true");
        System.setProperty("allure.testng.hide.disabled.tests", "true");
    }

    /**
     * Initializes SHAFT engine services without loading TestNG listener types.
     *
     * @param runType current execution framework mode
     */
    public static void bootstrapEngine(ProjectStructureManager.RunType runType) {
        setKeySystemProperties();
        io.qameta.allure.Allure.getLifecycle();
        ReportManagerHelper.setDiscreteLogging(true);
        if (runType == ProjectStructureManager.RunType.AI_AGENT) {
            initializeAiAgent();
        } else {
            initialize();
        }
        ReportManager.logDiscrete("Initializing Engine Setup...");
        if (!ProjectStructureManager.RunType.AI_AGENT.equals(runType))
            SHAFT.Properties.reporting.set().disableLogging(true);
        switch (runType) {
            case TESTNG ->
                    Thread.ofVirtual().start(() -> ProjectStructureManager.initialize(ProjectStructureManager.RunType.TESTNG));
            case CUCUMBER ->
                    Thread.ofVirtual().start(() -> ProjectStructureManager.initialize(ProjectStructureManager.RunType.CUCUMBER));
            case JUNIT ->
                    Thread.ofVirtual().start(() -> ProjectStructureManager.initialize(ProjectStructureManager.RunType.JUNIT));
            case AI_AGENT -> {
                ProjectStructureManager.initialize(ProjectStructureManager.RunType.AI_AGENT);
                SHAFT.Properties.web.set().forceBrowserDownload(true);
            }
            default -> throw new IllegalStateException("Unsupported run type: " + runType);
        }
        configureJVMProxy();
        Thread.ofVirtual().start(() -> {
            GoogleTink.initialize();
            GoogleTink.decrypt();
        });
        SHAFT.Properties.reporting.set().disableLogging(false);
        ReportManagerHelper.logEngineVersion();
        Thread.ofVirtual().start(UpdateChecker::check);
        Thread.ofVirtual().start(ImageProcessingActions::loadOpenCVIfAvailable);
        AllureManager.initializeAllureReportingEnvironment();
        Thread.ofVirtual().start(ReportManagerHelper::cleanExecutionSummaryReportDirectory);
        ReportManagerHelper.setDiscreteLogging(SHAFT.Properties.reporting.alwaysLogDiscreetly());
        ReportManagerHelper.setDebugMode(SHAFT.Properties.reporting.debugMode());
    }

    private static void configureJVMProxy() {
        String proxyServerSettings = SHAFT.Properties.platform.proxy();
        if (SHAFT.Properties.platform.jvmProxySettings() && !proxyServerSettings.isEmpty()) {
            String[] proxyHostPort = proxyServerSettings.split(":");
            ThreadLocalPropertiesManager.setGlobalProperty("http.proxyHost", proxyHostPort[0]);
            System.setProperty("http.proxyHost", proxyHostPort[0]);
            ThreadLocalPropertiesManager.setGlobalProperty("http.proxyPort", proxyHostPort[1]);
            System.setProperty("http.proxyPort", proxyHostPort[1]);
            ThreadLocalPropertiesManager.setGlobalProperty("https.proxyHost", proxyHostPort[0]);
            System.setProperty("https.proxyHost", proxyHostPort[0]);
            ThreadLocalPropertiesManager.setGlobalProperty("https.proxyPort", proxyHostPort[1]);
            System.setProperty("https.proxyPort", proxyHostPort[1]);
            ThreadLocalPropertiesManager.setGlobalProperty("ftp.proxyHost", proxyHostPort[0]);
            System.setProperty("ftp.proxyHost", proxyHostPort[0]);
            ThreadLocalPropertiesManager.setGlobalProperty("ftp.proxyPort", proxyHostPort[1]);
            System.setProperty("ftp.proxyPort", proxyHostPort[1]);
        }
    }

    /**
     * Applies runtime overrides based on platform and execution context.
     */
    public static void postProcessing() {
        if (!postProcessingDone.compareAndSet(false, true)) {
            return;
        }
        ReportManager.logDiscrete("Post processing some properties to support platforms-specific restrictions.");
        overrideScreenShotTypeForMacPlatform();
        overrideForcedFlagsForMobilePlatforms();
        overrideTargetOperatingSystemForLocalExecution();
        overrideScreenScalingFactorForWindows();
        overrideScreenMaximizationForRemoteExecution();
        overridePropertiesForMaximumPerformanceMode();
        overridePropertiesForEvidenceLevel();
        setMobilePlatform();
        overrideScreenShotTypeForAnimatedGIF();
        overrideScreenshotTypeForSafariBrowser();
        overrideScreenshotTypeForParallelExecution();
    }

    private static void overrideScreenshotTypeForParallelExecution() {
        if (!Properties.testNG.parallel().equals("NONE"))
            SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
    }

    private static void overrideScreenScalingFactorForWindows() {
        if (Properties.platform.targetPlatform().equalsIgnoreCase(org.openqa.selenium.Platform.WINDOWS.toString())) {
            try {
                int res = Toolkit.getDefaultToolkit().getScreenResolution();
                double scale = (double) res / 96;
                Properties.visuals.set().screenshotParamsScalingFactor(scale);
            } catch (java.awt.HeadlessException headlessException) {
                //ignore the exception if running in headless OS => used by claude
            }
        }
    }

    private static void overrideForcedFlagsForMobilePlatforms() {
        if (Arrays.asList(org.openqa.selenium.Platform.ANDROID.toString().toLowerCase(),
                org.openqa.selenium.Platform.IOS.toString().toLowerCase()).contains(Properties.platform.targetPlatform().toLowerCase())) {
            SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
            SHAFT.Properties.flags.set().clearBeforeTypingMode("off");
            SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(false);
            SHAFT.Properties.flags.set().enableTrueNativeMode(true);
            SHAFT.Properties.flags.set().forceCheckTextWasTypedCorrectly(false);
            SHAFT.Properties.flags.set().respectBuiltInWaitsInNativeMode(false);
            SHAFT.Properties.flags.set().handleNonSelectDropDown(false);
            SHAFT.Properties.flags.set().validateSwipeToElement(false);
            SHAFT.Properties.flags.set().scrollingMode("w3c");
        }
    }

    private static void overrideScreenShotTypeForMacPlatform() {
        if (Properties.platform.targetPlatform().equalsIgnoreCase(org.openqa.selenium.Platform.MAC.toString())) {
            SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
        }
    }

    private static void overrideScreenMaximizationForRemoteExecution() {
        if (!SHAFT.Properties.platform.executionAddress().equalsIgnoreCase("local")) {
            SHAFT.Properties.flags.set().autoMaximizeBrowserWindow(false);
        }
    }

    private static void overrideScreenShotTypeForAnimatedGIF() {
        if (SHAFT.Properties.visuals.createAnimatedGif()) {
            SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
        }
    }

    private static void overrideTargetOperatingSystemForLocalExecution() {
        var executionAddress = Properties.platform.executionAddress();
        if (executionAddress.equals("local")) {
            if (SystemUtils.IS_OS_WINDOWS) {
                Properties.platform.set().targetPlatform(org.openqa.selenium.Platform.WINDOWS.toString());
            } else if (SystemUtils.IS_OS_LINUX) {
                Properties.platform.set().targetPlatform(org.openqa.selenium.Platform.LINUX.toString());
            } else if (SystemUtils.IS_OS_MAC) {
                Properties.platform.set().targetPlatform(org.openqa.selenium.Platform.MAC.toString());
            }
        }
    }

    private static void overrideScreenshotTypeForSafariBrowser() {
        if (SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
        }
    }

    public static void setMobilePlatform() {
        String targetOperatingSystem = Properties.platform.targetPlatform();
        if (Arrays.asList("android", "ios").contains(targetOperatingSystem.toLowerCase())) {
            Properties.mobile.set().platformName(Properties.platform.targetPlatform().toLowerCase());
        }
    }

    private static void downloadDefaultProperties() {
        ReportManager.logDiscrete("Downloading default properties to AI-agent runtime workspace...");

        var propertiesFolderPath = resolveAiAgentPath("target" + File.separator + "temp" + File.separator + "properties");
        Properties.paths.set().properties(propertiesFolderPath);

        Arrays.asList(
                "TestNG.properties",
                "cucumber.properties",
                "custom.properties",
                "customWebdriverCapabilities.properties",
                "junit-platform.properties",
                "log4j2.properties",
                "reportportal.properties").forEach(PropertiesHelper::downloadPropertiesFile);
    }

    private static String targetPropertiesFolderPath() {
        String override = System.getProperty(BOOTSTRAP_DIRECTORY_PROPERTY, "").trim();
        return override.isBlank() ? CONVENTIONAL_TARGET_PROPERTIES_FOLDER_PATH : override.replace('\\', '/');
    }

    private static String defaultPropertiesFolderPath() {
        return targetPropertiesFolderPath() + "/default";
    }

    private static void initializeDefaultProperties(boolean forceDownload) {
        if (forceDownload){
            downloadDefaultProperties();
        } else {
            // The classpath entry name is fixed by the bundled jar layout and is deliberately not
            // affected by the bootstrap-directory override, which only redirects local writes.
            URL propertiesFolder = PropertyFileManager.class.getResource(
                    CONVENTIONAL_TARGET_PROPERTIES_FOLDER_PATH.replace("src/main", "") + "/default/");
            var propertiesFolderPath = propertiesFolder != null
                    ? PropertyFileManager.resolveClasspathResourceLocation(propertiesFolder)
                    : defaultPropertiesFolderPath();

            boolean isExternalRun = propertiesFolderPath.contains("file:") && propertiesFolderPath.contains(".jar!");

            var fileActions = FileActions.getInstance(true);

            // always override default properties
            if (isExternalRun) {
                try {
                    if (propertiesFolderPath.contains("file:")) {
                        fileActions.copyFolderFromJar(propertiesFolderPath, defaultPropertiesFolderPath());
                    } else {
                        throw new IOException("Properties folder path does not contain 'file:' protocol, indicating it is not running from a jar file.");
                    }
                } catch (Throwable ignored) {
                    ReportManager.logDiscrete("Could not copy default properties from the jar. Downloading defaults instead.");
                    downloadDefaultProperties();
                }
            }
        }
        // override target properties only if they do not exist
        overrideTargetProperties(forceDownload);
    }

    private static void overrideTargetProperties(boolean aiAgentMode) {
        var fileActions = FileActions.getInstance(true);
        var propertiesFolderPath = PropertyFileManager.resolveBundledDefaultPropertiesFolderPath();
        boolean isExternalRun = propertiesFolderPath.contains("file:") && propertiesFolderPath.contains(".jar!");
        var targetPropertiesFolderPath = aiAgentMode
                ? resolveAiAgentPath(targetPropertiesFolderPath())
                : targetPropertiesFolderPath();

        Arrays.asList("/custom.properties")
                .forEach(file -> {
                    if (!fileActions.doesFileExist(targetPropertiesFolderPath + file)) {
                        if (isExternalRun) {
                            try {
                                if (propertiesFolderPath.contains("file:")) {
                                    // propertiesFolderPath already points inside the bundled "default"
                                    // jar entry, which is where the template file actually lives --
                                    // stripping "/default" here breaks the entry-name match in
                                    // copyFileFromJar and silently mis-copies to a nested
                                    // "default/<file>" destination instead of the intended target path.
                                    fileActions.copyFileFromJar(propertiesFolderPath, targetPropertiesFolderPath, file.replace("/", ""));
                                } else {
                                    throw new IOException("Properties folder path does not contain 'file:' protocol, indicating it is not running from a jar file.");
                                }
                            } catch (Throwable ignored) {
                                downloadPropertiesFile(targetPropertiesFolderPath, file.replace("/", ""));
                            }
                        } else {
                            fileActions.copyFile(PropertyFileManager.resolveCustomPropertiesTemplatePath(),
                                    targetPropertiesFolderPath + file);
                        }
                    }
                });
        Properties.paths.set().properties(targetPropertiesFolderPath);
    }

    private static void downloadPropertiesFile(String fileName) {
        downloadPropertiesFile(Properties.paths.properties(), fileName);
    }

    private static void downloadPropertiesFile(String targetFolderPath, String fileName) {
        var baseURI = "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/refs/heads/main/shaft-engine/src/main/resources/properties/default/";
        downloadWithRetry(() -> FileActions.getInstance(true).downloadFile(baseURI + fileName,
                        targetFolderPath + File.separator + fileName),
                DOWNLOAD_MAX_ATTEMPTS, DOWNLOAD_BASE_DELAY, DOWNLOAD_MAX_DELAY);
    }

    /**
     * Retries a download attempt with full-jittered exponential backoff. GitHub's raw-content CDN
     * rate-limits (HTTP 429) anonymous requests, and several downloads can land in the same
     * throttling window (e.g. concurrent CI matrix jobs); a few short, jittered retries let the
     * probe recover without exhausting the installer's overall startup deadline.
     *
     * @param attempt      the download action to retry
     * @param maxAttempts  total attempts, including the first
     * @param baseDelay    delay before the first retry
     * @param maxDelay     upper bound for the backoff delay
     */
    static void downloadWithRetry(Runnable attempt, int maxAttempts, Duration baseDelay, Duration maxDelay) {
        for (int attemptNumber = 1; attemptNumber <= maxAttempts; attemptNumber++) {
            try {
                attempt.run();
                return;
            } catch (RuntimeException | Error retryable) {
                if (attemptNumber == maxAttempts) {
                    throw retryable;
                }
                ReportManager.logDiscrete("Properties download failed (attempt " + attemptNumber + "/" + maxAttempts
                        + "). Retrying...", Level.DEBUG);
                var bound = baseDelay.multipliedBy(1L << (attemptNumber - 1));
                if (bound.compareTo(maxDelay) > 0) {
                    bound = maxDelay;
                }
                try {
                    Thread.sleep(ThreadLocalRandom.current().nextLong(bound.toMillis() + 1));
                } catch (InterruptedException interruptedException) {
                    Thread.currentThread().interrupt();
                    throw retryable;
                }
            }
        }
    }

    private static void attachPropertyFiles() {
        var targetPropertiesFolderPath = Properties.paths.properties();
        ReportManager.logDiscrete("Reading properties directory: " + targetPropertiesFolderPath, Level.DEBUG);
        FileUtils.listFiles(new File(targetPropertiesFolderPath), new String[]{"properties"},
                false).forEach(propertyFile -> ReportManager.logDiscrete("Loading properties file: " + propertyFile, Level.DEBUG));
    }

    private static String resolveAiAgentPath(String path) {
        String workspaceRoot = SHAFT.Properties.paths.aiAgentWorkspaceRoot();
        if (workspaceRoot == null || workspaceRoot.isBlank()) {
            return path;
        }
        Path configuredPath = Path.of(path);
        if (configuredPath.isAbsolute()) {
            return configuredPath.normalize().toString();
        }
        return Path.of(workspaceRoot).resolve(configuredPath).normalize().toString();
    }

    private static void overridePropertiesForMaximumPerformanceMode() {
        int maximumPerformanceMode = SHAFT.Properties.flags.maximumPerformanceMode();
        switch (maximumPerformanceMode) {
            case 1, 2 -> {
                SHAFT.Properties.healenium.set().healEnabled(false);
                SHAFT.Properties.healing.set().strategy("disabled");
                SHAFT.Properties.flags.set().autoMaximizeBrowserWindow(false);
                SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly");
                SHAFT.Properties.visuals.set().screenshotParamsHighlightElements(false);
                SHAFT.Properties.visuals.set().screenshotParamsHighlightMethod("AI");
                SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
                SHAFT.Properties.visuals.set().screenshotParamsWatermark(true);
                SHAFT.Properties.visuals.set().createAnimatedGif(false);
                SHAFT.Properties.visuals.set().videoParamsRecordVideo(false);
                SHAFT.Properties.reporting.set().debugMode(false);
                SHAFT.Properties.reporting.set().captureElementName(false);
                SHAFT.Properties.reporting.set().captureWebDriverLogs(false);
                SHAFT.Properties.reporting.set().attachFullLog(false);
                SHAFT.Properties.performance.set().generatePerformanceReport(false);
                SHAFT.Properties.flags.set().telemetryEnabled(false);
                SHAFT.Properties.web.set().headlessExecution(false);
                if (maximumPerformanceMode == 2 && !DriverFactory.DriverType.SAFARI.getValue().equals(SHAFT.Properties.web.targetBrowserName())) {
                    SHAFT.Properties.web.set().headlessExecution(true);
                }
            }
            case 0 -> {
                // do nothing
            }
        }
    }

    static void overridePropertiesForEvidenceLevel() {
        switch (normalizeEvidenceLevel(SHAFT.Properties.reporting.evidenceLevel())) {
            case "CUSTOM" -> {
                // respect granular evidence properties
            }
            case "FAILURE_ONLY" -> applyEvidenceLevel("FailuresOnly", "FailuresOnly", false, false,
                    false, false, true, true, "failure");
            case "BALANCED" -> applyEvidenceLevel("ValidationPointsOnly", "FailuresOnly", false, false,
                    false, false, true, true, "failure");
            case "FAST" -> applyEvidenceLevel("Never", "Never", false, false,
                    false, false, false, false, "failure");
            case "FULL" -> applyEvidenceLevel("Always", "Always", true, true,
                    true, true, true, true, "always");
            default -> throw new IllegalArgumentException("Unsupported evidenceLevel \""
                    + SHAFT.Properties.reporting.evidenceLevel()
                    + "\". Supported values: CUSTOM, FAILURE_ONLY, BALANCED, FAST, FULL.");
        }
    }

    private static String normalizeEvidenceLevel(String evidenceLevel) {
        if (evidenceLevel == null || evidenceLevel.isBlank()) {
            return "FAILURE_ONLY";
        }
        return evidenceLevel.trim().replace('-', '_').replace(' ', '_').toUpperCase();
    }

    private static void applyEvidenceLevel(String screenshots, String pageSource, boolean gif, boolean video,
                                           boolean webDriverLogs, boolean fullLog, boolean diagnostics,
                                           boolean trace, String traceMode) {
        SHAFT.Properties.visuals.set()
                .screenshotParamsWhenToTakeAScreenshot(screenshots)
                .whenToTakePageSourceSnapshot(pageSource)
                .createAnimatedGif(gif)
                .videoParamsRecordVideo(video);
        SHAFT.Properties.reporting.set()
                .captureWebDriverLogs(webDriverLogs)
                .attachFullLog(fullLog)
                .diagnosticsBundleEnabled(diagnostics)
                .traceEnabled(trace)
                .traceMode(traceMode);
    }
}
