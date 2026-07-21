package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.TestNGListener;
import com.shaft.tools.io.internal.AllureManager;
import com.shaft.tools.io.internal.ProjectStructureManager;
import io.appium.java_client.AppiumBy;
import jakarta.annotation.PostConstruct;
import org.openqa.selenium.By;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

@Service
public class EngineService {
    private static final Logger logger = LoggerFactory.getLogger(EngineService.class);
    private static SHAFT.GUI.WebDriver driver;
    private static boolean engineInitialized = false;
    private static ActiveEngine activeEngine = ActiveEngine.NONE;
    private final PlaywrightService playwrightService;

    /**
     * Creates the default engine service, wiring a default {@link PlaywrightService} for direct
     * Java callers and tests that construct this class without Spring.
     */
    public EngineService() {
        this(new PlaywrightService());
    }

    @Autowired
    EngineService(PlaywrightService playwrightService) {
        this.playwrightService = playwrightService;
    }

    /**
     * Returns the MCP session's currently active automation engine.
     *
     * @return the active engine; {@link ActiveEngine#NONE} when no session has been initialized
     */
    static ActiveEngine activeEngine() {
        return activeEngine;
    }

    /**
     * Sets the MCP session's currently active automation engine. Called by {@code driver_initialize},
     * {@code driver_quit}, and the Playwright/mobile init and quit tools so engine-dispatching tools
     * (for example {@code element_click}) always route to whichever engine is really active.
     *
     * @param engine the new active engine; {@code null} normalizes to {@link ActiveEngine#NONE}
     */
    static void setActiveEngine(ActiveEngine engine) {
        activeEngine = engine == null ? ActiveEngine.NONE : engine;
    }
    // Folder values keep the engine defaults' trailing slash: several engine call sites join
    // folder + file by concatenation, and a missing slash wrote files like
    // "allure-resultsenvironment.xml" into every consumer project root.
    private static final String[][] MCP_PATH_PROPERTIES = {
            {"allureResultsFolderPath", "allure-results/"},
            {"propertiesFolderPath", "src/main/resources/properties/"},
            {"downloadsFolderPath", "target/downloadedFiles"},
            {"video.folder", "allure-results/videos"},
            {"servicesFolderPath", "src/test/resources/META-INF/services/"},
            {"dynamicObjectRepositoryPath", "src/main/resources/dynamicObjectRepository/"},
            {"testDataFolderPath", "src/test/resources/testDataFiles/"},
            {"extentReportsFolderPath", "extent-reports/"},
            {"executionSummaryReportFolderPath", "execution-summary/"},
            {"PerformanceReportFolderPath", "performanceReport/"}
    };

    /**
     * Called by Spring after this bean is constructed.
     * Applies remote WebDriver configuration from the {@code REMOTE_DRIVER_ADDRESS}
     * environment variable so that ALL WebDriver instances — including those created
     * directly by tests without going through {@link #initializeDriver} — connect to
     * the remote Selenium Server rather than trying to launch a local browser.
     */
    @PostConstruct
    void configureRemoteExecution() {
        // Set default ReportPortal property to prevent NPE in shaft-engine
        if (System.getProperty("rp.enable") == null) {
            System.setProperty("rp.enable", "false");
        }
        configureRuntimePaths();

        // Configure remote WebDriver if REMOTE_DRIVER_ADDRESS environment variable is set.
        // SHAFT Engine uses a single "executionAddress" property:
        //   - "local"          → launch a browser on this machine
        //   - "<selenium URL>" → connect to an existing Selenium/Grid server
        // Setting REMOTE_DRIVER_ADDRESS to a Selenium Grid URL (e.g.
        // http://localhost:4444/wd/hub) makes SHAFT Engine connect remotely.
        // The env var is only applied when the system property is not already set,
        // so an explicit -DexecutionAddress=... JVM flag always takes precedence.
        String remoteDriverAddress = System.getenv("REMOTE_DRIVER_ADDRESS");
        if (remoteDriverAddress != null && !remoteDriverAddress.isEmpty()
                && System.getProperty("executionAddress") == null) {
            System.setProperty("executionAddress", remoteDriverAddress);
            logger.info("Remote WebDriver configured (address redacted for security)");
        }
    }

    /**
     * Retrieves the current WebDriver instance.
     *
     * @return The current WebDriver instance.
     * @throws IllegalStateException if no active browser session is found.
     */
    static SHAFT.GUI.WebDriver getDriver() {
        if (driver == null) {
            SHAFT.GUI.WebDriver captureDriver = activeCaptureDriver();
            if (captureDriver != null) {
                return captureDriver;
            }
            logger.error("No active browser session found. Please initialize a browser session first.");
            throw new IllegalStateException("No active browser session. Start one with driver_initialize, "
                    + "or start a capture session (capture_start) whose recorded browser these "
                    + "element tools can drive directly.");
        }
        return driver;
    }

    /**
     * Bridge to the active SHAFT Capture session's browser, registered by {@link CaptureService}.
     * With it, element tools drive the recorded browser when no driver_initialize session exists —
     * the agent-performed codegen flow (capture_start, perform actions, capture_stop, generate)
     * documented in the tool guidance (issue #3429).
     */
    private static volatile java.util.function.Supplier<SHAFT.GUI.WebDriver> captureDriverBridge;

    static void registerCaptureDriverBridge(java.util.function.Supplier<SHAFT.GUI.WebDriver> bridge) {
        captureDriverBridge = bridge;
    }

    private static SHAFT.GUI.WebDriver activeCaptureDriver() {
        java.util.function.Supplier<SHAFT.GUI.WebDriver> bridge = captureDriverBridge;
        if (bridge == null) {
            return null;
        }
        try {
            return bridge.get();
        } catch (RuntimeException unavailable) {
            return null;
        }
    }

    /**
     * Bridge to {@link MobileService}'s native/web-emulation initializers, registered by
     * {@link MobileService} itself. {@code driver_initialize} routes {@code engine=MOBILE_NATIVE}/
     * {@code MOBILE_WEB} requests through this static functional bridge -- mirroring
     * {@link #captureDriverBridge} -- instead of holding a direct {@link MobileService} field, since
     * {@link MobileService} already depends on {@link EngineService} and a direct field would form a
     * constructor-injection cycle (design doc amendment A9).
     */
    private static volatile java.util.function.BiConsumer<ActiveEngine, McpMobileInitOptions> mobileInitBridge;

    static void registerMobileInitBridge(java.util.function.BiConsumer<ActiveEngine, McpMobileInitOptions> bridge) {
        mobileInitBridge = bridge;
    }

    /**
     * Finds a web element using the specified locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH, CSSSELECTOR).
     * @param locatorValue    The value used with the locator strategy to find the element.
     * @return The located web element.
     */
    static By getLocator(locatorStrategy locatorStrategy, String locatorValue) {
        return switch (locatorStrategy) {
            case ID -> By.id(locatorValue);
            case CSSSELECTOR, CSS, SELECTOR -> By.cssSelector(locatorValue);
            case XPATH -> By.xpath(locatorValue);
            case NAME -> SHAFT.GUI.Locator.hasAnyTagName().hasAttribute("name", locatorValue).build();
            case TAGNAME -> SHAFT.GUI.Locator.hasTagName(locatorValue).build();
            case CLASSNAME -> SHAFT.GUI.Locator.hasAnyTagName().hasAttribute("class", locatorValue).build();
            case ACCESSIBILITY_ID -> AppiumBy.accessibilityId(locatorValue);
            case ANDROID_UIAUTOMATOR -> AppiumBy.androidUIAutomator(locatorValue);
            case IOS_PREDICATE -> AppiumBy.iOSNsPredicateString(locatorValue);
            case IOS_CLASS_CHAIN -> AppiumBy.iOSClassChain(locatorValue);
        };
    }

    /**
     * Initializes the WebDriver for the specified browser type, or another engine session when
     * {@code engine} selects one.
     *
     * @param targetBrowser The type of browser to initialize (e.g., CHROME, FIREFOX).
     * @param engine which engine to start; blank/omitted defaults to {@link ActiveEngine#WEB}.
     *               {@link ActiveEngine#PLAYWRIGHT} starts a SHAFT Playwright session instead of a
     *               WebDriver session. {@link ActiveEngine#MOBILE_NATIVE}/{@link ActiveEngine#MOBILE_WEB}
     *               start an Appium native or Chrome/Edge mobile-emulation session using
     *               {@code mobileOptions} (design doc amendment A9).
     * @param mobileOptions optional nested request carrying the union of the former
     *                      {@code mobile_initialize_native}/{@code mobile_initialize_web_emulation}
     *                      parameters; only read when {@code engine} is a mobile engine; unset fields
     *                      resolve from {@code SHAFT.Properties} exactly as those tools did
     */
    @Tool(name = "driver_initialize", description = "launches browser, Playwright, or a mobile session; optional "
            + "engine selects web (default) | playwright | mobile_native | mobile_web; for mobile engines, "
            + "optional nested mobileOptions carries native/web-emulation parameters, absorbing "
            + "mobile_initialize_native/mobile_initialize_web_emulation")
    public void initializeDriver(
            BrowserType targetBrowser,
            @ToolParam(required = false) ActiveEngine engine,
            @ToolParam(required = false) McpMobileInitOptions mobileOptions) {
        ActiveEngine resolvedEngine = engine == null ? ActiveEngine.WEB : engine;
        if (resolvedEngine == ActiveEngine.MOBILE_NATIVE || resolvedEngine == ActiveEngine.MOBILE_WEB) {
            java.util.function.BiConsumer<ActiveEngine, McpMobileInitOptions> bridge = mobileInitBridge;
            if (bridge == null) {
                throw new IllegalStateException("driver_initialize cannot start a mobile session: no MobileService "
                        + "bridge is registered (this indicates the shaft-mcp Spring context did not construct a "
                        + "MobileService bean).");
            }
            bridge.accept(resolvedEngine, mobileOptions == null ? McpMobileInitOptions.EMPTY : mobileOptions);
            return;
        }
        if (resolvedEngine == ActiveEngine.PLAYWRIGHT) {
            playwrightService.initialize(targetBrowser == null ? null : targetBrowser.name(),
                    SHAFT.Properties.web.headlessExecution());
            return;
        }
        try {
            SHAFT.Properties.web.set().targetBrowserName(targetBrowser.name());
            initializeConfiguredDriver(targetBrowser.name(), ActiveEngine.WEB);
        } catch (Exception e) {
            // Logs `targetBrowser` itself (SLF4J's `{}` placeholder null-safely stringifies it),
            // not `targetBrowser.name()`: re-deriving the name here previously threw a second,
            // masking NullPointerException whenever `targetBrowser` was null -- the exact case this
            // catch block exists to report -- silently skipping the log line and replacing the
            // original failure with this unrelated one.
            logger.error("Failed to initialize driver for browser: {}", targetBrowser, e);
            throw e;
        }
    }

    /**
     * Java-caller convenience overload defaulting {@code engine} to {@link ActiveEngine#WEB}; not
     * an MCP tool.
     *
     * @param targetBrowser The type of browser to initialize (e.g., CHROME, FIREFOX).
     */
    public void initializeDriver(BrowserType targetBrowser) {
        initializeDriver(targetBrowser, null, null);
    }

    void initializeConfiguredDriver(String sessionName) {
        initializeConfiguredDriver(sessionName, ActiveEngine.WEB);
    }

    void initializeConfiguredDriver(String sessionName, ActiveEngine engine) {
        ensureEngineInitialized();
        driver = new SHAFT.GUI.WebDriver();
        setActiveEngine(engine);
        logger.info("Driver initialized successfully: {}", sessionName);
    }

    static void ensureEngineInitialized() {
        // Initialize engine setup only once to avoid repeated initialization warnings
        if (!engineInitialized) {
            logger.info("Initializing SHAFT Engine for AI Agent mode...");
            configureRuntimePaths();
            TestNGListener.engineSetup(ProjectStructureManager.RunType.AI_AGENT);
            engineInitialized = true;
        }
    }

    static Path configureRuntimePaths() {
        return configureRuntimePaths(McpRuntimePaths.currentRoot());
    }

    static Path configureRuntimePaths(Path runtimeRoot) {
        Path root = runtimeRoot.toAbsolutePath().normalize();
        createDirectory(root.toString(), "MCP runtime root");
        System.setProperty("user.dir", root.toString());
        setPathProperty("aiAgentWorkspaceRoot", root, root.toString());
        for (String[] pathProperty : MCP_PATH_PROPERTIES) {
            setPathProperty(pathProperty[0], root, pathProperty[1]);
        }
        for (String[] pathProperty : MCP_PATH_PROPERTIES) {
            createDirectory(System.getProperty(pathProperty[0]), pathProperty[0]);
        }
        return root;
    }

    private static void setPathProperty(String key, Path root, String defaultValue) {
        String configured = System.getProperty(key);
        String value = configured == null || configured.isBlank()
                ? defaultValue
                : configured;
        Path path = Path.of(value);
        if (!path.isAbsolute()) {
            path = root.resolve(path).normalize();
        }
        System.setProperty(key, path.toString());
    }

    private static void createDirectory(String configuredPath, String propertyKey) {
        try {
            Files.createDirectories(Path.of(configuredPath));
        } catch (IOException exception) {
            throw new IllegalStateException(
                    "Could not create MCP runtime directory for " + propertyKey + ": " + configuredPath,
                    exception);
        }
    }

    /**
     * Quits whichever engine session is currently active (web, mobile, or Playwright), closing all
     * associated browser windows.
     */
    @Tool(name = "driver_quit", description = "closes whichever engine session (web, mobile, or Playwright) is currently active")
    public void quitDriver() {
        if (activeEngine == ActiveEngine.PLAYWRIGHT) {
            playwrightService.quit();
            return;
        }
        try {
            logger.info("Active driver will be closed");
            if (driver == null) {
                setActiveEngine(ActiveEngine.NONE);
                return;
            }
            driver.quit();
            driver = null;
        } catch (Exception e) {
            logger.error("Failed to close driver.", e);
            throw e;
        } finally {
            setActiveEngine(ActiveEngine.NONE);
        }
    }

    /**
     * Generate a test report for the current session.
     * This method compiles and generates a detailed test report based on the actions performed during the session.
     * It utilizes SHAFT's reporting capabilities to create a comprehensive report that includes
     * information such as test steps, outcomes, screenshots, and logs.
     * The generated report is saved in a predefined location for easy access and review.
     * This method should be called at the end of the test session to ensure all actions are documented.
     */
    @Tool(name = "generate_test_report", description = "generates a test report for the current session")
    public void generateTestReport() {
        try {
            AllureManager.openAllureReportAfterExecution();
            logger.info("Test report generated successfully.");
        } catch (Exception e) {
            logger.error("Failed to generate test report.", e);
            throw e;
        }
    }

    /**
     * Get the source code of the current page.
     * This is a support method for the AI agent to better explore the page.
     * @return The HTML source code of the current page as a string.
     */
    public String getPageSource() {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            String pageSource = driver.browser().getPageSource();
            logger.info("Retrieved page source for direct Java caller (content redacted from logs).");
            return pageSource;
        } catch (Exception e) {
            logger.error("Failed to retrieve page source.", e);
            throw e;
        }
    }
}
