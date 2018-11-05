package com.shaft.browser;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import org.openqa.selenium.NoSuchSessionException;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.edge.EdgeDriver;
import org.openqa.selenium.edge.EdgeOptions;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.firefox.FirefoxOptions;
import org.openqa.selenium.ie.InternetExplorerDriver;
import org.openqa.selenium.ie.InternetExplorerOptions;
import org.openqa.selenium.logging.LogEntry;
import org.openqa.selenium.logging.LogType;
import org.openqa.selenium.logging.LoggingPreferences;
import org.openqa.selenium.remote.CapabilityType;
import org.openqa.selenium.remote.LocalFileDetector;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.safari.SafariDriver;
import org.openqa.selenium.safari.SafariOptions;
import org.testng.Assert;

import com.shaft.element.JSWaiter;
import com.shaft.io.ExcelFileManager;
import com.shaft.io.ReportManager;

public class BrowserFactory {

    private static final Boolean AUTO_MAXIMIZE = Boolean.valueOf(System.getProperty("autoMaximizeBrowserWindow").trim());
    private static final String EXECUTION_ADDRESS = System.getProperty("executionAddress").trim();
    // local OR hub ip:port
    private static final String TARGET_HUB_URL = "http://" + EXECUTION_ADDRESS + "/wd/hub";
    private static final String TARGET_OPERATING_SYSTEM = System.getProperty("targetOperatingSystem");
    // Windows-64 | Linux-64 | Mac-64
    private static final String TARGET_BROWSER_NAME = System.getProperty("targetBrowserName");
    // Default | MozillaFirefox | MicrosoftInternetExplorer | GoogleChrome |
    // MicrosoftEdge | Safari
    private static final int PAGE_LOAD_TIMEOUT = 60;
    private static final int IMPLICIT_WAIT_TIMEOUT = 10;

    private static String driversPath;
    private static String fileExtension;
    private static Map<String, WebDriver> drivers = new HashMap<>();
    private static WebDriver driver = null;

    // logging preferences object
    private static LoggingPreferences logPrefs;

    // supported browser options
    private static ChromeOptions chOptions;
    private static FirefoxOptions ffOptions;
    private static SafariOptions sfOptions;
    private static EdgeOptions edOptions;
    private static InternetExplorerOptions ieOptions;

    // replaced with browser specific options

    private BrowserFactory() {
	throw new IllegalStateException("Utility class");
    }

    /**
     * Given that there is no test data file, read the target browser value from pom
     * configuration (overridable from jenkins), if "Default" it fails because it
     * will not be able to read the "Target Browser" cell value from the configured
     * test data file.
     * 
     * @return a singleton browser instance
     */
    public static WebDriver getBrowser() {
	if (TARGET_BROWSER_NAME.equals("Default")) {
	    Assert.fail("Unsupported Browser Type.");
	    return getBrowser(TARGET_BROWSER_NAME);
	} else {
	    return getBrowser(TARGET_BROWSER_NAME);
	}
    }

    /**
     * Read the target browser value from pom configuration (overridable from
     * jenkins), if "Default" it reads the "Target Browser" cell value from the
     * configured test data file.
     * 
     * @param testDataReader
     *            the current instance of the Excel reader used for reading test
     *            data
     * @return a singleton browser instance
     */
    public static WebDriver getBrowser(ExcelFileManager testDataReader) {
	if (TARGET_BROWSER_NAME.equals("Default")) {
	    return getBrowser(testDataReader.getCellData("Target Browser"));
	} else {
	    return getBrowser(TARGET_BROWSER_NAME);
	}
    }

    /**
     * Create and/or return an instance of the target browser (maintains a single
     * instance per browser type) and checks for cross-compatibility between the
     * selected browser and operating system
     * 
     * @param browserName
     *            the name of the browser that you want to run, currently supports
     *            'MozillaFirefox', 'MicrosoftInternetExplorer', 'GoogleChrome', and
     *            'MicrosoftEdge'
     * @return a singleton browser instance
     */
    public static WebDriver getBrowser(String browserName) {
	try {
	    if (driver != null) {
		// retrieve current instance
		driver = getActiveDriverInstance(browserName);

	    } else {
		// if driver is null set logging preferences, then set driver options and create
		// new instances
		checkBrowserOSCrossCompatibility(browserName);
		// check cross-compatibility between the selected operating system and browser
		// and report in case they are not compatible
		setDriversPath();
		// set path based on operating system
		setLoggingPrefrences();
		// set logging global preferences
		setDriverOptions(browserName);
		// set driver options with respect to the target browser name

		if (EXECUTION_ADDRESS.equals("local")) {
		    // Manage local execution
		    driver = createNewLocalDriverInstance(browserName);
		} else {
		    // Manage remote execution
		    driver = createNewRemoteDriverInstance(browserName);
		}
		driver.manage().timeouts().pageLoadTimeout(PAGE_LOAD_TIMEOUT, TimeUnit.SECONDS);
		driver.manage().timeouts().implicitlyWait(IMPLICIT_WAIT_TIMEOUT, TimeUnit.SECONDS);

		JSWaiter.setDriver(driver);
		if (AUTO_MAXIMIZE) {
		    BrowserActions.maximizeWindow(driver); // Automatically maximize driver window after opening it
		}
	    }

	} catch (NullPointerException e) {
	    ReportManager.log(e);
	    ReportManager.log("Unhandled Exception with Browser Type [" + browserName + "].");
	    Assert.fail("Unhandled Exception with Browser Type [" + browserName + "].");
	}
	return driver;
    }

    private static WebDriver getActiveDriverInstance(String browserName) {
	ReportManager.log("Switching to active browser instance on: [" + TARGET_OPERATING_SYSTEM + "], [" + browserName + "].");
	switch (browserName) {
	case "MozillaFirefox":
	    driver = drivers.get("MozillaFirefox");
	    break;
	case "MicrosoftInternetExplorer":
	    driver = drivers.get("MicrosoftInternetExplorer");
	    break;
	case "GoogleChrome":
	    driver = drivers.get("GoogleChrome");
	    break;
	case "MicrosoftEdge":
	    driver = drivers.get("MicrosoftEdge");
	    break;
	case "Safari":
	    driver = drivers.get("Safari");
	    break;
	default:
	    ReportManager.log("Unsupported Browser Type [" + browserName + "].");
	    Assert.fail("Unsupported Browser Type [" + browserName + "].");
	    break;
	}
	return driver;
    }

    /**
     * Check cross-compatibility between the selected operating system and browser
     * and report in case they are not compatible
     */
    private static void checkBrowserOSCrossCompatibility(String browserName) {
	Boolean isCompatibleBrowser = false;

	switch (TARGET_OPERATING_SYSTEM) {
	case "Windows-64":
	    if (browserName.equals("MozillaFirefox") || browserName.equals("GoogleChrome") || browserName.equals("MicrosoftInternetExplorer") || browserName.equals("MicrosoftEdge")) {
		isCompatibleBrowser = true;
	    }
	    break;
	case "Linux-64":
	    if (browserName.equals("MozillaFirefox") || browserName.equals("GoogleChrome")) {
		isCompatibleBrowser = true;
	    }
	    break;
	case "Mac-64":
	    if (browserName.equals("MozillaFirefox") || browserName.equals("GoogleChrome") || browserName.equals("Safari")) {
		isCompatibleBrowser = true;
	    }
	    break;
	default:
	    ReportManager.log("Unsupported Operating System [" + TARGET_OPERATING_SYSTEM + "].");
	    Assert.fail("Unsupported Operating System [" + TARGET_OPERATING_SYSTEM + "].");
	    break;
	}

	if (!isCompatibleBrowser) {
	    ReportManager.log("Unsupported Browser Type [" + browserName + "] for this Operating System [" + TARGET_OPERATING_SYSTEM + "].");
	    Assert.fail("Unsupported Browser Type [" + browserName + "] for this Operating System [" + TARGET_OPERATING_SYSTEM + "].");
	}

    }

    /**
     * Set the driver folder path and file extension based on the target Operating
     * System
     */
    private static void setDriversPath() {
	switch (TARGET_OPERATING_SYSTEM) {
	case "Windows-64":
	    driversPath = "src/main/resources/drivers/";
	    fileExtension = ".exe";
	    break;
	case "Linux-64":
	    driversPath = "src/main/resources/drivers/linux-64/";
	    fileExtension = "";
	    break;
	case "Mac-64":
	    driversPath = "src/main/resources/drivers/mac-64/";
	    fileExtension = "";
	    break;
	default:
	    ReportManager.log("Unsupported Operating System [" + TARGET_OPERATING_SYSTEM + "].");
	    Assert.fail("Unsupported Operating System [" + TARGET_OPERATING_SYSTEM + "].");
	    break;
	}
    }

    private static void setLoggingPrefrences() {
	logPrefs = new LoggingPreferences();
	logPrefs.enable(LogType.PERFORMANCE, Level.ALL);
	logPrefs.enable(LogType.BROWSER, Level.ALL);
	logPrefs.enable(LogType.DRIVER, Level.ALL);
    }

    private static void setDriverOptions(String browserName) {
	switch (browserName) {
	case "MozillaFirefox":
	    ffOptions = new FirefoxOptions();
	    ffOptions.setCapability("platform", getDesiredOperatingSystem());
	    // ffOptions.setCapability("marionette", true);
	    // ffOptions.setCapability("nativeEvents", true);
	    // ffOptions.setAcceptInsecureCerts(true);
	    // ffOptions.setUnhandledPromptBehaviour(UnexpectedAlertBehaviour.ACCEPT);
	    ffOptions.setCapability(CapabilityType.LOGGING_PREFS, logPrefs);
	    break;
	case "MicrosoftInternetExplorer":
	    ieOptions = new InternetExplorerOptions();
	    ieOptions.setCapability("platform", getDesiredOperatingSystem());
	    ieOptions.setCapability(CapabilityType.LOGGING_PREFS, logPrefs);
	    break;
	case "GoogleChrome":
	    chOptions = new ChromeOptions();
	    chOptions.setCapability("platform", getDesiredOperatingSystem());
	    chOptions.addArguments("--no-sandbox");
	    chOptions.addArguments("--disable-infobars"); // disable automation info bar
	    // chOptions.setAcceptInsecureCerts(true);
	    // chOptions.setUnhandledPromptBehaviour(UnexpectedAlertBehaviour.ACCEPT);
	    // chOptions.addArguments("--headless");
	    // chOptions.addArguments("--disable-gpu");

	    // chOptions.addArguments("--enable-logging --v=1");
	    chOptions.setCapability(CapabilityType.LOGGING_PREFS, logPrefs);
	    break;
	case "MicrosoftEdge":
	    edOptions = new EdgeOptions();
	    edOptions.setCapability("platform", getDesiredOperatingSystem());
	    edOptions.setCapability(CapabilityType.LOGGING_PREFS, logPrefs);
	    break;
	case "Safari":
	    sfOptions = new SafariOptions();
	    sfOptions.setCapability("platform", getDesiredOperatingSystem());
	    sfOptions.setCapability(CapabilityType.LOGGING_PREFS, logPrefs);
	    break;
	default:
	    ReportManager.log("Unsupported Browser Type [" + browserName + "].");
	    Assert.fail("Unsupported Browser Type [" + browserName + "].");
	    break;
	}
    }

    private static WebDriver createNewLocalDriverInstance(String browserName) {
	ReportManager.log("Attempting to run locally on: [" + TARGET_OPERATING_SYSTEM + "], [" + browserName + "].");
	switch (browserName) {
	case "MozillaFirefox":
	    System.setProperty("webdriver.gecko.driver", driversPath + "geckodriver" + fileExtension);
	    driver = new FirefoxDriver(ffOptions);
	    drivers.put("MozillaFirefox", driver);
	    ReportManager.log("Successfully Opened Mozilla Firefox.");

	    break;
	case "MicrosoftInternetExplorer":
	    System.setProperty("webdriver.ie.driver", driversPath + "IEDriverServer" + fileExtension);
	    driver = new InternetExplorerDriver(ieOptions);
	    drivers.put("MicrosoftInternetExplorer", driver);
	    ReportManager.log("Successfully Opened Microsoft Internet Explorer.");

	    break;
	case "GoogleChrome":
	    System.setProperty("webdriver.chrome.driver", driversPath + "chromedriver" + fileExtension);
	    driver = new ChromeDriver(chOptions);
	    drivers.put("GoogleChrome", driver);
	    ReportManager.log("Successfully Opened Google Chrome.");
	    break;
	case "MicrosoftEdge":
	    System.setProperty("webdriver.edge.driver", driversPath + "MicrosoftWebDriver" + fileExtension);
	    driver = new EdgeDriver(edOptions);
	    drivers.put("MicrosoftEdge", driver);
	    ReportManager.log("Successfully Opened Microsoft Edge.");
	    break;
	case "Safari":
	    driver = new SafariDriver(sfOptions);
	    drivers.put("Safari", driver);
	    ReportManager.log("Successfully Opened Safari.");
	    break;
	default:
	    ReportManager.log("Unsupported Browser Type [" + browserName + "].");
	    Assert.fail("Unsupported Browser Type [" + browserName + "].");
	    break;
	}
	// Set<String> logTypes = driver.manage().logs().getAvailableLogTypes();
	// logTypes.size();
	return driver;
    }

    private static WebDriver createNewRemoteDriverInstance(String browserName) {
	ReportManager.log("Attempting to run remotely on: [" + TARGET_OPERATING_SYSTEM + "], [" + browserName + "], [" + TARGET_HUB_URL + "].");
	try {
	    switch (browserName) {
	    case "MozillaFirefox":
		driver = new RemoteWebDriver(new URL(TARGET_HUB_URL), ffOptions);
		drivers.put("MozillaFirefox", driver);
		ReportManager.log("Successfully Opened Mozilla Firefox.");
		break;
	    case "MicrosoftInternetExplorer":
		driver = new RemoteWebDriver(new URL(TARGET_HUB_URL), ieOptions);
		drivers.put("MicrosoftInternetExplorer", driver);
		ReportManager.log("Successfully Opened Microsoft Internet Explorer.");
		break;
	    case "GoogleChrome":
		driver = new RemoteWebDriver(new URL(TARGET_HUB_URL), chOptions);
		drivers.put("GoogleChrome", driver);
		ReportManager.log("Successfully Opened Google Chrome.");
		break;
	    case "MicrosoftEdge":
		driver = new RemoteWebDriver(new URL(TARGET_HUB_URL), edOptions);
		drivers.put("MicrosoftEdge", driver);
		ReportManager.log("Successfully Opened Microsoft Edge.");
		break;
	    case "Safari":
		driver = new RemoteWebDriver(new URL(TARGET_HUB_URL), sfOptions);
		drivers.put("Safari", driver);
		ReportManager.log("Successfully Opened Safari.");
		break;
	    default:
		ReportManager.log("Unsupported Browser Type [" + browserName + "].");
		Assert.fail("Unsupported Browser Type [" + browserName + "].");
		break;
	    }
	    ((RemoteWebDriver) driver).setFileDetector(new LocalFileDetector());
	} catch (WebDriverException e) {
	    ReportManager.log(e);
	    if (e.getMessage().contains("Error forwarding the new session cannot find")) {
		ReportManager.log("Error forwarding the new session: Couldn't find a node that matches the desired capabilities.");
		ReportManager.log("Failed to run remotely on: [" + TARGET_OPERATING_SYSTEM + "], [" + browserName + "], [" + TARGET_HUB_URL + "].");
		Assert.fail("Error forwarding the new session: Couldn't find a node that matches the desired capabilities.");
	    } else {
		ReportManager.log("Unhandled Error.");
		ReportManager.log("Failed to run remotely on: [" + TARGET_OPERATING_SYSTEM + "], [" + browserName + "], [" + TARGET_HUB_URL + "].");
		Assert.fail("Unhandled Error.");
	    }
	} catch (MalformedURLException e) {
	    ReportManager.log(e);
	}
	return driver;
    }

    private static Platform getDesiredOperatingSystem() {
	switch (TARGET_OPERATING_SYSTEM) {
	case "Windows-64":
	    return Platform.WINDOWS;
	case "Linux-64":
	    return Platform.LINUX;
	case "Mac-64":
	    return Platform.MAC;
	default:
	    ReportManager.log("Unsupported Operating System [" + TARGET_OPERATING_SYSTEM + "], setting target platform to [ANY].");
	    return Platform.ANY;
	}
    }

    /**
     * Close all open browser instances.
     * 
     */
    public static void closeAllDrivers() {
	try {
	    for (Entry<String, WebDriver> entry : drivers.entrySet()) {
		attachBrowserLogs(entry.getKey(), entry.getValue());
		entry.getValue().close();
		entry.getValue().quit();
	    }
	} catch (NoSuchSessionException e) {
	    // browser was already closed by the .close() method
	} catch (Exception e) {
	    ReportManager.log(e);
	}
	driver = null;
	drivers.clear();
	ReportManager.log("Successfully Closed All Browsers.");
    }

    private static void attachBrowserLogs(String borwserName, WebDriver driver) {

	if (!borwserName.equals("MozillaFirefox")) {
	    // The Selenium log API isn’t supported by geckodriver.
	    // Confirmed to work with chromeDriver

	    StringBuilder logBuilder;
	    String performanceLogText = "";
	    String browserLogText = "";
	    String driverLogText = "";

	    try {
		logBuilder = new StringBuilder();
		for (LogEntry entry : driver.manage().logs().get(LogType.BROWSER)) {
		    logBuilder.append(entry.toString() + System.lineSeparator());
		}
		browserLogText = logBuilder.toString();
		ReportManager.attach("Browser Logs for [" + borwserName + "]", browserLogText);
	    } catch (WebDriverException e) {
		// exception when the defined log type is not found
		ReportManager.log(e);
	    }

	    try {
		logBuilder = new StringBuilder();
		for (LogEntry entry : driver.manage().logs().get(LogType.PERFORMANCE)) {
		    logBuilder.append(entry.toString() + System.lineSeparator());
		}
		performanceLogText = logBuilder.toString();
		ReportManager.attach("Performance Logs for [" + borwserName + "]", performanceLogText);
	    } catch (WebDriverException e) {
		// exception when the defined log type is not found
		ReportManager.log(e);
	    }

	    try {
		logBuilder = new StringBuilder();
		for (LogEntry entry : driver.manage().logs().get(LogType.DRIVER)) {
		    logBuilder.append(entry.toString() + System.lineSeparator());
		}
		driverLogText = logBuilder.toString();
		ReportManager.attach("Driver Logs for [" + borwserName + "]", driverLogText);
	    } catch (WebDriverException e) {
		// exception when the defined log type is not found
		ReportManager.log(e);
	    }
	}
    }
}
