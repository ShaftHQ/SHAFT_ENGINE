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
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

@Service
public class EngineService {
    private static final Logger logger = LoggerFactory.getLogger(EngineService.class);
    private static SHAFT.GUI.WebDriver driver;
    private static boolean engineInitialized = false;
    private static final String[][] MCP_PATH_PROPERTIES = {
            {"allureResultsFolderPath", "allure-results"},
            {"propertiesFolderPath", "src/main/resources/properties"},
            {"downloadsFolderPath", "target/downloadedFiles"},
            {"video.folder", "allure-results/videos"},
            {"servicesFolderPath", "src/test/resources/META-INF/services"},
            {"dynamicObjectRepositoryPath", "src/main/resources/dynamicObjectRepository"},
            {"testDataFolderPath", "src/test/resources/testDataFiles"},
            {"extentReportsFolderPath", "extent-reports"},
            {"executionSummaryReportFolderPath", "execution-summary"},
            {"PerformanceReportFolderPath", "performanceReport"}
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
            logger.error("No active browser session found. Please initialize a browser session first.");
            throw new IllegalStateException("No active browser session");
        }
        return driver;
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
            case ID -> SHAFT.GUI.Locator.hasAnyTagName().hasId(locatorValue).build();
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
     * Initializes the WebDriver for the specified browser type.
     *
     * @param targetBrowser The type of browser to initialize (e.g., CHROME, FIREFOX).
     */
    @Tool(name = "driver_initialize", description = "launches browser")
    public void initializeDriver(BrowserType targetBrowser) {
        try {
            SHAFT.Properties.web.set().targetBrowserName(targetBrowser.name());
            initializeConfiguredDriver(targetBrowser.name());
        } catch (Exception e) {
            logger.error("Failed to initialize driver for browser: {}", targetBrowser.name(), e);
            throw e;
        }
    }

    void initializeConfiguredDriver(String sessionName) {
        ensureEngineInitialized();
        driver = new SHAFT.GUI.WebDriver();
        logger.info("Driver initialized successfully: {}", sessionName);
    }

    private static void ensureEngineInitialized() {
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
     * Quits the WebDriver, closing all associated browser windows.
     */
    @Tool(name = "driver_quit", description = "closes browser")
    public void quitDriver() {
        try {
            logger.info("Active driver will be closed");
            if (driver == null) {
                return;
            }
            driver.quit();
            driver = null;
        } catch (Exception e) {
            logger.error("Failed to close driver.", e);
            throw e;
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
