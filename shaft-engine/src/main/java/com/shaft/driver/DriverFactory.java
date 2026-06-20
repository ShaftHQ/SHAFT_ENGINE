package com.shaft.driver;

import com.shaft.api.RestActions;
import com.shaft.cli.TerminalActions;
import com.shaft.db.DatabaseActions;
import com.shaft.db.DatabaseActions.DatabaseType;
import com.shaft.driver.internal.DriverFactory.BrowserStackHelper;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.LambdaTestHelper;
import com.shaft.listeners.internal.TestNGListenerHelper;
import com.shaft.properties.internal.PropertiesHelper;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.internal.ProjectStructureManager;
import lombok.Getter;
import lombok.Setter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.Browser;

/**
 * Factory for creating and managing WebDriver instances in SHAFT.
 *
 * <p>This class handles driver initialization for local browsers, remote
 * Selenium Grid, BrowserStack, and LambdaTest. It also provides factory
 * methods for API, CLI, and database drivers.
 *
 * <p>The factory automatically reads configuration from SHAFT's properties
 * system and supports last-minute property overrides from TestNG suite XML.
 *
 * @see SHAFT.GUI.WebDriver
 * @see DriverFactory.DriverType
 */
@Setter
@SuppressWarnings({"UnusedReturnValue", "unused"})
public class DriverFactory {

    /**
     * Creates a new driver factory instance.
     */
    public DriverFactory() {
        super();
    }

    private static final Logger logger = LogManager.getLogger(DriverFactory.class);

    private DriverFactoryHelper helper;

    /**
     * override properties with test suite properties
     * read testng properties (enables modifying the test execution properties programmatically)
     * used to duplicate the tests for each browser in case of cross-browser Execution
     */
    private static void readLastMinuteUpdatedProperties() {
        reloadProperties();
        // it's null in case of Cucumber native feature file execution
        if (TestNGListenerHelper.getXmlTest() != null) {
            var allParameters = TestNGListenerHelper.getXmlTest().getAllParameters();
            allParameters.forEach(ThreadLocalPropertiesManager::setProperty);
            var testName = TestNGListenerHelper.getTestName().toLowerCase();
            if (testName.contains("firefox")
                    || testName.contains("chrome")
                    || testName.contains("safari")) {
                SHAFT.Properties.platform.set().targetPlatform(Platform.LINUX.name());
            }
        }
    }

    /**
     * Ensures SHAFT properties are initialized before driver creation.
     *
     * @return {@code true} when properties are initialized and ready for use
     */
    public static boolean reloadProperties() {
        if (!com.shaft.properties.internal.Properties.isInitialized()) {
            logger.warn("SHAFT listeners are not initialized. Starting a minimal test runtime.");
            var runType = ProjectStructureManager.identifyRunType();
            if (runType.equals(ProjectStructureManager.RunType.CUCUMBER)) {
                // stuck on minimalistic test run in case of native cucumber execution without manual plugin configuration
                logger.warn("Configure SHAFT's Cucumber plugin to enable the full listener lifecycle:");
                logger.warn("https://github.com/ShaftHQ/SHAFT_ENGINE?tab=readme-ov-file#23-cucumber");
            }
            PropertiesHelper.bootstrapEngine(runType);
        }
        return true;
    }

    /**
     * Creates a new API instance to facilitate using the Rest Actions Library
     *
     * @param serviceURI the base URI of the target web service
     * @return rest actions instance that can be used to chain and build your api request
     * @deprecated use {@link SHAFT.API#API(String)} instead.
     */
    @Deprecated(since = "10.2.20260620", forRemoval = false)
    @SuppressWarnings("deprecation")
    public static RestActions getAPIDriver(String serviceURI) {
        return new RestActions(serviceURI);
    }

    /**
     * Creates a new local Terminal instance to facilitate using the Terminal Actions Library
     *
     * @return local terminal driver instance
     */
    public static TerminalActions getTerminalDriver() {
        return new TerminalActions();
    }

    /**
     * Creates a new Database driver instance to facilitate using the Database Actions Library
     *
     * @param databaseType database type that you want to connect with:
     *                     DatabaseType.MY_SQL ,SQL_SERVER,POSTGRES_SQL.
     * @param ip           IP address that has database installation that we need to
     *                     connect to (e.g. 72.55.136.25)
     * @param port         port of database installation on the server (e.g. 3306)
     * @param name         database name that you need to connect to
     * @param username     database username
     * @param password     password of database user
     * @return new database driver instance
     */
    public static DatabaseActions getDatabaseDriver(DatabaseType databaseType, String ip, String port, String name, String username,
                                                    String password) {
        return new DatabaseActions(databaseType, ip, port, name, username, password);
    }

    /**
     * Read the target Selenium WebDriver value from the execution.properties file
     *
     * @return a new Selenium WebDriver instance
     */
    public DriverFactoryHelper getHelper() {
        if (helper == null) {
            readLastMinuteUpdatedProperties();
            if (SHAFT.Properties.platform.executionAddress().toLowerCase().contains("browserstack")) {
                return getHelper(DriverType.BROWSERSTACK, new MutableCapabilities());
            } else if (SHAFT.Properties.platform.executionAddress().toLowerCase().contains("lambdatest")) {
                return getHelper(DriverType.LAMBDATEST, new MutableCapabilities());
            } else {
                var helper = new DriverFactoryHelper();
                helper.initializeDriver();
                this.helper = helper;
            }
        }
        return helper;
    }

    /**
     * Returns the managed WebDriver instance from this factory.
     *
     * @return the active WebDriver session
     */
    public WebDriver getDriver() {
        return getHelper().getDriver();
    }

    /**
     * Creates a new Selenium WebDriver instance with custom driver type
     *
     * @param driverType one of the supported driver types
     * @return a new Selenium WebDriver instance
     */
    public DriverFactoryHelper getHelper(DriverType driverType) {
        return getHelper(driverType, new MutableCapabilities());
    }

    /**
     * Creates a new Selenium WebDriver instance with custom driver type and options
     *
     * @param driverType          one of the supported driver types
     * @param customDriverOptions the custom options that will be used to create this new driver instance, or null to use the default
     * @return a new Selenium WebDriver instance
     */
    public DriverFactoryHelper getHelper(DriverType driverType, MutableCapabilities customDriverOptions) {
        readLastMinuteUpdatedProperties();
        if (driverType.equals(DriverType.BROWSERSTACK)) {
            return BrowserStackHelper.getBrowserStackDriver(customDriverOptions);
        } else if (driverType.equals(DriverType.LAMBDATEST)) {
            return LambdaTestHelper.getLambdaTestDriver(customDriverOptions);
        } else {
            var helper = new DriverFactoryHelper();
            helper.initializeDriver(driverType, customDriverOptions);
            return helper;
        }
    }

    /**
     * Attaches the Engine to an already up and running selenium webdriver instance
     *
     * @param driver an already initialized native selenium webdriver instance
     * @return a helper instance to be used for driver manipulation
     */
    public DriverFactoryHelper getHelper(WebDriver driver) {
        readLastMinuteUpdatedProperties();
        var helper = new DriverFactoryHelper();
        helper.initializeDriver(driver);
        return helper;
    }

    /**
     * List of the supported driver types for execution
     */
    @Getter
    public enum DriverType {
        /** Sikuli desktop automation execution. */
        SIKULI("SikuliActions"),
        /** BrowserStack cloud execution. */
        BROWSERSTACK("BrowserStack"),
        /** LambdaTest cloud execution. */
        LAMBDATEST("LambdaTest"),
        /** Database actions driver. */
        DATABASE("DatabaseActions"),
        /** Terminal actions driver. */
        TERMINAL("TerminalActions"),
        /** REST API actions driver. */
        API("RestActions"),
        /** Firefox browser execution. */
        FIREFOX(Browser.FIREFOX.browserName()),
        /** Chrome browser execution. */
        CHROME(Browser.CHROME.browserName()),
        /** Safari browser execution. */
        SAFARI(Browser.SAFARI.browserName()),
        /** Internet Explorer browser execution. */
        IE(Browser.IE.browserName()),
        /** Microsoft Edge browser execution. */
        EDGE(Browser.EDGE.browserName()),
        /** Chromium browser execution. */
        CHROMIUM("Chromium"),
        /** WebKit browser execution. */
        WEBKIT("Webkit"),
        /** Appium Chrome browser execution. */
        APPIUM_CHROME("chrome"),
        /** Appium Chromium browser execution. */
        APPIUM_CHROMIUM("Chromium"),
        /** Appium generic browser execution. */
        APPIUM_BROWSER("Browser"),
        /** Appium Samsung browser execution. */
        APPIUM_SAMSUNG_BROWSER("samsung"),
        /** Appium native mobile app execution. */
        APPIUM_MOBILE_NATIVE("NativeMobileApp"),
        /** Appium Flutter app execution. */
        APPIUM_FLUTTER("Flutter");

        private final String value;

        DriverType(String type) {
            this.value = type;
        }

    }

}
