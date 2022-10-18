package com.shaft.driver;

import com.shaft.api.BrowserStack;
import com.shaft.api.RestActions;
import com.shaft.cli.TerminalActions;
import com.shaft.db.DatabaseActions;
import com.shaft.db.DatabaseActions.DatabaseType;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.listeners.InvokedMethodListener;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.WebDriver;
import org.sikuli.script.App;

public class DriverFactory {

    /**
     * Read the target Selenium WebDriver value from the execution.properties file
     *
     * @return a new Selenium WebDriver instance
     */
    public static WebDriver getDriver() {
        // override properties with test suite properties
        // read testng properties (enables modifying the test execution properties programatically)
        System.getProperties().putAll(InvokedMethodListener.getXmlTest().getAllParameters());
        if (System.getProperty("executionAddress").contains("browserstack")) {
            return getBrowserStackDriver(new MutableCapabilities());
        } else {
            DriverFactoryHelper.initializeDriver();
            return DriverFactoryHelper.getDriver().get();
        }
    }

    /**
     * Creates a new Selenium WebDriver instance with custom driver type
     *
     * @param driverType one of the supported driver types
     * @return a new Selenium WebDriver instance
     */
    public static WebDriver getDriver(DriverType driverType) {
        // override properties with test suite properties
        // read testng properties (enables modifying the test execution properties programatically)
        System.getProperties().putAll(InvokedMethodListener.getXmlTest().getAllParameters());
        if (driverType.equals(DriverType.BROWSERSTACK)) {
            return getBrowserStackDriver(new MutableCapabilities());
        } else {
            DriverFactoryHelper.initializeDriver(driverType);
            return DriverFactoryHelper.getDriver().get();
        }
    }

    /**
     * Creates a new Selenium WebDriver instance with custom driver type and options
     *
     * @param driverType          one of the supported driver types
     * @param customDriverOptions the custom options that will be used to create this new driver instance, or null to use the default
     * @return a new Selenium WebDriver instance
     */
    public static WebDriver getDriver(DriverType driverType, MutableCapabilities customDriverOptions) {
        // override properties with test suite properties
        // read testng properties (enables modifying the test execution properties programatically)
        System.getProperties().putAll(InvokedMethodListener.getXmlTest().getAllParameters());
        if (driverType.equals(DriverType.BROWSERSTACK)) {
            return getBrowserStackDriver(customDriverOptions);
        } else {
            DriverFactoryHelper.initializeDriver(driverType, customDriverOptions);
            return DriverFactoryHelper.getDriver().get();
        }
    }

    /**
     * Creates a new Selenium WebDriver instance using BrowserStack, use this to test Native Mobile apps over BrowserStack
     *
     * @param browserStackOptions custom browserstack options to be merged with the default in the browserStack.properties file
     * @return a new Selenium WebDriver instance using BrowserStack
     */
    private static WebDriver getBrowserStackDriver(MutableCapabilities browserStackOptions) {
        String appUrl = System.getProperty("browserStack.appUrl");
        if ("".equals(appUrl)) {
            // new native app OR web execution
            if ("".equals(System.getProperty("browserStack.appRelativeFilePath"))){
                // this means it's a web execution (desktop or mobile)
                browserStackOptions = BrowserStack.setupDesktopWebExecution().merge(browserStackOptions);
                // TODO: support web mobile execution
                DriverFactoryHelper.initializeDriver(browserStackOptions);
                return DriverFactoryHelper.getDriver().get();
            }else {
                // this is the new native app scenario
                //TODO: there is a bug in the merge method and it doesn't respect the capabilities at all
                browserStackOptions = BrowserStack.setupNativeAppExecution(System.getProperty("browserStack.username"), System.getProperty("browserStack.accessKey"),
                        System.getProperty("browserStack.deviceName"), System.getProperty("browserStack.platformVersion"), System.getProperty("browserStack.appRelativeFilePath"), System.getProperty("browserStack.appName")).merge(browserStackOptions);
                DriverFactoryHelper.initializeDriver(DriverType.APPIUM_MOBILE_NATIVE, browserStackOptions);
                return DriverFactoryHelper.getDriver().get();
            }
        } else {
            // this is the existing version from a native app scenario
            browserStackOptions = BrowserStack.setupNativeAppExecution(System.getProperty("browserStack.username"), System.getProperty("browserStack.accessKey"),
                    System.getProperty("browserStack.deviceName"), System.getProperty("browserStack.platformVersion"), appUrl).merge(browserStackOptions);
            DriverFactoryHelper.initializeDriver(DriverType.APPIUM_MOBILE_NATIVE, browserStackOptions);
            return DriverFactoryHelper.getDriver().get();
        }

    }

    /**
     * Attaches your SikuliActions to a specific Application instance
     *
     * @param applicationName the name or partial name of the currently opened application window that you want to attach to
     * @return a sikuli App instance that can be used to perform SikuliActions
     */
    public static App getSikuliApp(String applicationName) {
//        DriverFactoryHelper.initializeSystemProperties();
        var myapp = new App(applicationName);
        myapp.waitForWindow(Integer.parseInt(System.getProperty("browserNavigationTimeout")));
        myapp.focus();
        ReportManager.log("Opened app: [" + myapp.getName() + "]...");
        return myapp;
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
     *                     DatabaseType.MY_SQL ,SQL_SERVER,POSTGRE_SQL.
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
     * Terminates the desired sikuli app instance
     *
     * @param application a sikuli App instance that can be used to perform SikuliActions
     */
    public static void closeSikuliApp(App application) {
        ReportManager.log("Closing app: [" + application.getName() + "]...");
        application.close();
    }

    /**
     * Close all open driver instances.
     */
    public static void closeAllDrivers() {
        DriverFactoryHelper.closeDriver();
    }

    /**
     * List of the supported driver types for execution
     */
    public enum DriverType {
        SIKULI("SikuliActions"), BROWSERSTACK("BrowserStack"), DATABASE("DatabaseActions"), TERMINAL("TerminalActions"), API("RestActions"), DESKTOP_FIREFOX("MozillaFirefox"), DESKTOP_CHROME("GoogleChrome"), DESKTOP_SAFARI("Safari"),
        DESKTOP_INTERNET_EXPLORER("MicrosoftInternetExplorer"), DESKTOP_EDGE("MicrosoftEdge"), DESKTOP_CHROMIUM("Chromium"), DESKTOP_WEBKIT("Webkit"), APPIUM_CHROME("Chrome"),
        APPIUM_CHROMIUM("Chromium"), APPIUM_BROWSER("Browser"), APPIUM_MOBILE_NATIVE("NativeMobileApp");

        private final String value;

        DriverType(String type) {
            this.value = type;
        }

        public String getValue() {
            return value;
        }
    }

}
