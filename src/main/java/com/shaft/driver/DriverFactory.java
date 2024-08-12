package com.shaft.driver;

import com.shaft.api.RestActions;
import com.shaft.cli.TerminalActions;
import com.shaft.db.DatabaseActions;
import com.shaft.db.DatabaseActions.DatabaseType;
import com.shaft.driver.internal.DriverFactory.BrowserStackHelper;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.LambdaTestHelper;
import com.shaft.listeners.TestNGListener;
import com.shaft.listeners.internal.TestNGListenerHelper;
import com.shaft.tools.io.internal.ProjectStructureManager;
import lombok.Getter;
import lombok.Setter;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.Browser;

@Setter
@SuppressWarnings({"UnusedReturnValue", "unused"})
public class DriverFactory {

    private DriverFactoryHelper helper;

    /**
     * override properties with test suite properties
     * read testng properties (enables modifying the test execution properties programmatically)
     * used to duplicate the tests for each browser in case of cross-browser Execution
     */
    private static void readLastMinuteUpdatedProperties() {
        reloadProperties();
        // it's null in case of Cucumber native feature file execution
        if (TestNGListener.getXmlTest() != null) {
            System.getProperties().putAll(TestNGListener.getXmlTest().getAllParameters());
            var testName = TestNGListenerHelper.getTestName().toLowerCase();
            if (testName.contains("firefox")
                    || testName.contains("chrome")
                    || testName.contains("safari")) {
                SHAFT.Properties.platform.set().targetPlatform(Platform.LINUX.name());
            }
        }
    }

    public static boolean reloadProperties() {
        if (SHAFT.Properties.platform == null) {
            System.out.println("Execution Listeners are not loaded properly... Self-Healing... Initializing minimalistic test run...");
            var runType = TestNGListener.identifyRunType();
            if (runType.equals(ProjectStructureManager.RunType.CUCUMBER)) {
                // stuck on minimalistic test run in case of native cucumber execution without manual plugin configuration
                System.out.println("To unlock the full capabilities of SHAFT kindly follow these steps to configure SHAFT's Cucumber plugin:");
                System.out.println("https://github.com/ShaftHQ/SHAFT_ENGINE#stop-reinventing-the-wheel-start-using-shaft");
            }
            TestNGListener.engineSetup(runType);
        }
        return true;
    }

    /**
     * Creates a new API instance to facilitate using the Rest Actions Library
     *
     * @param serviceURI the base URI of the target web service
     * @return rest actions instance that can be used to chain and build your api request
     */
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
        SIKULI("SikuliActions"), BROWSERSTACK("BrowserStack"), LAMBDATEST("LambdaTest"), DATABASE("DatabaseActions"), TERMINAL("TerminalActions"), API("RestActions"), FIREFOX(Browser.FIREFOX.browserName()), CHROME(Browser.CHROME.browserName()), SAFARI(Browser.SAFARI.browserName()),
        IE(Browser.IE.browserName()), EDGE(Browser.EDGE.browserName()), CHROMIUM("Chromium"), WEBKIT("Webkit"), APPIUM_CHROME("chrome"),
        APPIUM_CHROMIUM("Chromium"), APPIUM_BROWSER("Browser"), APPIUM_SAMSUNG_BROWSER("samsung"), APPIUM_MOBILE_NATIVE("NativeMobileApp");

        private final String value;

        DriverType(String type) {
            this.value = type;
        }

    }

}
