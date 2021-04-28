package com.shaft.driver;

import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.WebDriver;
import org.sikuli.script.App;

import com.shaft.api.RestActions;
import com.shaft.cli.TerminalActions;
import com.shaft.db.DatabaseActions;
import com.shaft.db.DatabaseActions.DatabaseType;
import com.shaft.tools.io.ReportManager;

public class DriverFactory {

    private DriverFactory() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Read the target driver value from the execution.properties file
     *
     * @return a new driver instance
     */
    public static WebDriver getDriver() {
        return DriverFactoryHelper.getDriver();
    }
    
    /**
     * Creates a new driver instance with custom driver options
     *
     * @param driverType          one of the supported driver types
     * @param customDriverOptions the custom options that will be used to create this new driver instance, or null to use the default
     * @return a new driver instance
     */
    public static WebDriver getDriver(DriverType driverType, MutableCapabilities customDriverOptions) {
        return DriverFactoryHelper.getDriver(driverType, customDriverOptions);
    }

    /**
     * Attaches your SikuliActions to a specific Application instance
     *
     * @param applicationName the name or partial name of the currently opened application window that you want to attach to
     * @return a sikuli App instance that can be used to perform SikuliActions
     */
    public static App getSikuliApp(String applicationName) {
        DriverFactoryHelper.initializeSystemProperties(System.getProperty("targetBrowserName") == null);
        App myapp = new App(applicationName);
        myapp.waitForWindow(Integer.parseInt(System.getProperty("browserNavigationTimeout")));
        myapp.focus();
        ReportManager.log("Opened app: [" + myapp.getName() + "]...");
        return myapp;
    }
    
    /**
     * Creates a new API instance to facilitate using the Rest Actions Library
     * @param serviceURI the base URI of the target web service 
     * @return rest actions instance that can be used to chain and build your api request
     */
    public static RestActions getAPIDriver(String serviceURI) {
    	return new RestActions (serviceURI);
    }
    
    /**
     * Creates a new local Terminal instance to facilitate using the Terminal Actions Library
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
    public static synchronized void closeAllDrivers() {
    	DriverFactoryHelper.closeAllDrivers();
    }

    /**
     * List of the supported driver types for execution
     */
    public enum DriverType {
        SIKULI("SikuliActions"), DATABASE("DatabaseActions"), TERMINAL("TerminalActions"), API("RestActions"), SELENIUM_FIREFOX("MozillaFirefox"), SELENIUM_CHROME("GoogleChrome"), SELENIUM_SAFARI("Safari"),
        SELENIUM_INTERNET_EXPLORER("MicrosoftInternetExplorer"), SELENIUM_EDGE("MicrosoftEdge"), APPIUM_CHROME("Chrome"),
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
