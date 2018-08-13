package com.shaftEngine.browserActionLibrary;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.TimeUnit;

import org.openqa.selenium.Platform;
import org.openqa.selenium.SessionNotCreatedException;
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
import org.openqa.selenium.remote.DesiredCapabilities;
import org.openqa.selenium.remote.LocalFileDetector;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.safari.SafariDriver;
import org.testng.Assert;

import com.shaftEngine.elementActionLibrary.JSWaiter;
import com.shaftEngine.ioActionLibrary.ExcelFileManager;
import com.shaftEngine.ioActionLibrary.ReportManager;

public class BrowserFactory {

	private static final Boolean AUTO_MAXIMIZE = Boolean
			.valueOf(System.getProperty("autoMaximizeBrowserWindow").trim());
	private static final String EXECUTION_ADDRESS = System.getProperty("executionAddress").trim();
	// local OR hub ip:port
	private static final String TARGET_HUB_URL = "http://" + EXECUTION_ADDRESS + "/wd/hub";
	private static final String TARGET_OPERATING_SYSTEM = System.getProperty("targetOperatingSystem");
	// Windows-64 | Linux-64 | Mac-64
	private static final String TARGET_BROWSER_NAME = System.getProperty("targetBrowserName");
	// Default | MozillaFirefox | MicrosoftInternetExplorer | GoogleChrome |
	// MicrosoftEdge | Safari
	// private static final String driversPath = "src/main/resources/drivers/";
	private static String driversPath;
	private static String fileExtension;
	private static Map<String, WebDriver> drivers = new HashMap<>();
	private static WebDriver driver = null;
	private static DesiredCapabilities capabilities = null;
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
		checkBrowserOSCrossCompatibility(browserName);
		// check cross-compatibility between the selected operating system and browser
		// and report in case they are not compatible
		setDriversPath();
		// set path based on operating system
		try {
			if (driver != null) {
				// retrieve current instance
				driver = getActiveDriverInstance(browserName);

			} else {
				// if driver is null create new instances
				if (EXECUTION_ADDRESS.equals("local")) {
					// Manage local execution
					driver = createNewLocalDriverInstance(browserName);
				} else {
					// Manage remote execution
					driver = createNewRemoteDriverInstance(browserName);
				}
			}
			driver.manage().timeouts().pageLoadTimeout(60, TimeUnit.SECONDS);
			JSWaiter.setDriver(driver);
			if (AUTO_MAXIMIZE) {
				BrowserActions.maximizeWindow(driver); // Automatically maximize driver window after opening it
			}
		} catch (NullPointerException e) {
			ReportManager.log(e);
			ReportManager.log("Unhandled Exception with Browser Type [" + browserName + "].");
			Assert.fail("Unhandled Exception with Browser Type [" + browserName + "].");
		}

		return driver;
	}

	private static WebDriver getActiveDriverInstance(String browserName) {
		ReportManager.log(
				"Switching to active browser instance on: [" + TARGET_OPERATING_SYSTEM + "], [" + browserName + "].");
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

	private static WebDriver createNewLocalDriverInstance(String browserName) {
		ReportManager.log("Attempting to run locally on: [" + TARGET_OPERATING_SYSTEM + "], [" + browserName + "].");
		switch (browserName) {
		case "MozillaFirefox":
			System.setProperty("webdriver.gecko.driver", driversPath + "geckodriver" + fileExtension);
			driver = new FirefoxDriver();
			drivers.put("MozillaFirefox", driver);
			ReportManager.log("Successfully Opened Mozilla Firefox.");

			break;
		case "MicrosoftInternetExplorer":
			System.setProperty("webdriver.ie.driver", driversPath + "IEDriverServer" + fileExtension);
			driver = new InternetExplorerDriver();
			drivers.put("MicrosoftInternetExplorer", driver);
			ReportManager.log("Successfully Opened Microsoft Internet Explorer.");

			break;
		case "GoogleChrome":
			System.setProperty("webdriver.chrome.driver", driversPath + "chromedriver" + fileExtension);
			/*
			 * if (autoMaximizeBrowserWindow) { ChromeOptions options = new ChromeOptions();
			 * options.addArguments("--start-maximized"); driver = new
			 * ChromeDriver(options); } else { driver = new ChromeDriver(); }
			 */
			driver = new ChromeDriver();
			drivers.put("GoogleChrome", driver);
			ReportManager.log("Successfully Opened Google Chrome.");

			break;
		case "MicrosoftEdge":
			System.setProperty("webdriver.edge.driver", driversPath + "MicrosoftWebDriver" + fileExtension);
			driver = new EdgeDriver();
			drivers.put("MicrosoftEdge", driver);
			ReportManager.log("Successfully Opened Microsoft Edge.");

			break;
		case "Safari":
			// System.setProperty("webdriver.edge.driver", driversPath +
			// "MicrosoftWebDriver" + fileExtension);
			driver = new SafariDriver();
			drivers.put("Safari", driver);
			ReportManager.log("Successfully Opened Safari.");

			break;
		default:
			ReportManager.log("Unsupported Browser Type [" + browserName + "].");
			Assert.fail("Unsupported Browser Type [" + browserName + "].");
			break;
		}
		return driver;
	}

	private static WebDriver createNewRemoteDriverInstance(String browserName) {

		// handling this exception for remote execution
		// [org.openqa.selenium.WebDriverException: Error forwarding the new session
		// cannot find : Capabilities]
		ReportManager.log("Attempting to run remotely on: [" + TARGET_OPERATING_SYSTEM + "], [" + browserName + "], ["
				+ TARGET_HUB_URL + "].");
		try {
			switch (browserName) {
			case "MozillaFirefox":
				capabilities = DesiredCapabilities.firefox();
				capabilities.setCapability("marionette", true);
				capabilities.setCapability("nativeEvents", true);
				setDesiredOperatingSystemCapabilities(capabilities);
				// set remote driver instance
				driver = getRemoteWebDriverInstance(browserName);
				drivers.put("MozillaFirefox", driver);
				ReportManager.log("Successfully Opened Mozilla Firefox.");

				break;
			case "MicrosoftInternetExplorer":
				capabilities = DesiredCapabilities.internetExplorer();
				setDesiredOperatingSystemCapabilities(capabilities);
				// set remote driver instance
				driver = getRemoteWebDriverInstance(browserName);
				drivers.put("MicrosoftInternetExplorer", driver);
				ReportManager.log("Successfully Opened Microsoft Internet Explorer.");

				break;
			case "GoogleChrome":
				capabilities = DesiredCapabilities.chrome();
				setDesiredOperatingSystemCapabilities(capabilities);
				// set remote driver instance
				driver = getRemoteWebDriverInstance(browserName);
				drivers.put("GoogleChrome", driver);
				ReportManager.log("Successfully Opened Google Chrome.");

				break;
			case "MicrosoftEdge":
				System.setProperty("webdriver.edge.driver", driversPath + "MicrosoftWebDriver" + fileExtension);
				capabilities = DesiredCapabilities.edge();
				setDesiredOperatingSystemCapabilities(capabilities);
				// set remote driver instance
				driver = getRemoteWebDriverInstance(browserName);
				drivers.put("MicrosoftEdge", driver);
				ReportManager.log("Successfully Opened Microsoft Edge.");

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
				ReportManager.log(
						"Error forwarding the new session: Couldn't find a node that matches the desired capabilities.");
				ReportManager.log("Failed to run remotely on: [" + TARGET_OPERATING_SYSTEM + "], [" + browserName
						+ "], [" + TARGET_HUB_URL + "].");
				Assert.fail(
						"Error forwarding the new session: Couldn't find a node that matches the desired capabilities.");
			} else {
				ReportManager.log("Unhandled Error.");
				ReportManager.log("Failed to run remotely on: [" + TARGET_OPERATING_SYSTEM + "], [" + browserName
						+ "], [" + TARGET_HUB_URL + "].");
				Assert.fail("Unhandled Error.");
			}
		}
		return driver;
	}

	private static WebDriver getRemoteWebDriverInstance(String browserName) {
		try {
			switch (browserName) {
			case "MozillaFirefox":
				driver = new RemoteWebDriver(new URL(TARGET_HUB_URL), new FirefoxOptions(capabilities));
				break;
			case "MicrosoftInternetExplorer":
				driver = new RemoteWebDriver(new URL(TARGET_HUB_URL), new InternetExplorerOptions(capabilities));
				break;
			case "GoogleChrome":
				/*
				 * if (autoMaximizeBrowserWindow) { driver = new RemoteWebDriver(new
				 * URL(targetHubURL), new
				 * ChromeOptions().merge(capabilities).addArguments("--start-maximized")); }
				 * else { driver = new RemoteWebDriver(new URL(targetHubURL), new
				 * ChromeOptions().merge(capabilities)); }
				 */
				driver = new RemoteWebDriver(new URL(TARGET_HUB_URL), new ChromeOptions().merge(capabilities));
				break;
			case "MicrosoftEdge":
				driver = new RemoteWebDriver(new URL(TARGET_HUB_URL), new EdgeOptions().merge(capabilities));
				break;
			default:
				ReportManager.log("Unsupported Browser Type [" + browserName + "].");
				Assert.fail("Unsupported Browser Type [" + browserName + "].");
				break;
			}
		} catch (MalformedURLException | SessionNotCreatedException e) {
			ReportManager.log(e);
		}
		return driver;
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

	private static void setDesiredOperatingSystemCapabilities(DesiredCapabilities capabilities) {
		switch (TARGET_OPERATING_SYSTEM) {
		case "Windows-64":
			capabilities.setPlatform(Platform.WINDOWS);
			break;
		case "Linux-64":
			capabilities.setPlatform(Platform.LINUX);
			break;
		case "Mac-64":
			capabilities.setPlatform(Platform.MAC);
			break;
		default:
			ReportManager.log("Unsupported Operating System [" + TARGET_OPERATING_SYSTEM + "].");
			Assert.fail("Unsupported Operating System [" + TARGET_OPERATING_SYSTEM + "].");
			break;
		}
	}

	/**
	 * Check cross-compatibility between the selected operating system and browser
	 * and report in case they are not compatible
	 */
	private static void checkBrowserOSCrossCompatibility(String browserName) {
		switch (TARGET_OPERATING_SYSTEM) {
		case "Windows-64":
			switch (browserName) {
			case "MozillaFirefox":
				break;
			case "MicrosoftInternetExplorer":
				break;
			case "GoogleChrome":
				break;
			case "MicrosoftEdge":
				break;
			default:
				ReportManager.log("Unsupported Browser Type [" + browserName + "] for this Operating System ["
						+ TARGET_OPERATING_SYSTEM + "].");
				Assert.fail("Unsupported Browser Type [" + browserName + "] for this Operating System ["
						+ TARGET_OPERATING_SYSTEM + "].");
				break;
			}
			break;
		case "Linux-64":
			switch (browserName) {
			case "MozillaFirefox":
				break;
			case "GoogleChrome":
				break;
			default:
				ReportManager.log("Unsupported Browser Type [" + browserName + "] for this Operating System ["
						+ TARGET_OPERATING_SYSTEM + "].");
				Assert.fail("Unsupported Browser Type [" + browserName + "] for this Operating System ["
						+ TARGET_OPERATING_SYSTEM + "].");
				break;
			}
			break;
		case "Mac-64":
			switch (browserName) {
			case "MozillaFirefox":
				break;
			case "GoogleChrome":
				break;
			case "Safari":
				break;
			default:
				ReportManager.log("Unsupported Browser Type [" + browserName + "] for this Operating System ["
						+ TARGET_OPERATING_SYSTEM + "].");
				Assert.fail("Unsupported Browser Type [" + browserName + "] for this Operating System ["
						+ TARGET_OPERATING_SYSTEM + "].");
				break;
			}
			break;
		default:
			ReportManager.log("Unsupported Operating System [" + TARGET_OPERATING_SYSTEM + "].");
			Assert.fail("Unsupported Operating System [" + TARGET_OPERATING_SYSTEM + "].");
			break;
		}
	}

	/**
	 * Close all open browser instances.
	 * 
	 */
	public static void closeAllDrivers() {
		try {
			for (Entry<String, WebDriver> entry : drivers.entrySet()) {
				entry.getValue().close();
				entry.getValue().quit();
			}
		} catch (Exception e) {
			ReportManager.log(e);
		}
		drivers.clear();
		ReportManager.log("Successfully Closed All Browsers.");
	}
}
