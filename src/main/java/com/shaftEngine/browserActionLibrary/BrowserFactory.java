package com.shaftEngine.browserActionLibrary;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.junit.Assert;
import org.openqa.selenium.Platform;
import org.openqa.selenium.SessionNotCreatedException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.firefox.FirefoxOptions;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.ie.InternetExplorerDriver;
import org.openqa.selenium.ie.InternetExplorerOptions;
import org.openqa.selenium.remote.DesiredCapabilities;
import org.openqa.selenium.remote.LocalFileDetector;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.safari.SafariDriver;

import com.shaftEngine.elementActionLibrary.JSWaiter;
import com.shaftEngine.ioActionLibrary.ExcelFileManager;
import com.shaftEngine.ioActionLibrary.ReportManager;
import org.openqa.selenium.edge.EdgeDriver;
import org.openqa.selenium.edge.EdgeOptions;

public class BrowserFactory {

	private static final Boolean autoMaximizeBrowserWindow = Boolean
			.valueOf(System.getProperty("autoMaximizeBrowserWindow").trim());
	private static final String executionAddress = System.getProperty("executionAddress").trim();
	// local OR hub ip:port
	private static final String targetHubURL = "http://" + executionAddress + "/wd/hub";
	private static String targetOperatingSystem = System.getProperty("targetOperatingSystem");
	// Windows-64 | Linux-64 | Mac-64
	private static String targetBrowserName = System.getProperty("targetBrowserName");
	// Default | MozillaFirefox | MicrosoftInternetExplorer | GoogleChrome |
	// MicrosoftEdge | Safari
	// private static final String driversPath = "src/main/resources/drivers/";
	private static String driversPath, fileExtension;
	private static Map<String, WebDriver> drivers = new HashMap<String, WebDriver>();

	/**
	 * Given that there is no test data file, read the target browser value from pom
	 * configuration (overridable from jenkins), if "Default" it fails because it
	 * will not be able to read the "Target Browser" cell value from the configured
	 * test data file.
	 * 
	 * @return a singleton browser instance
	 */
	public static WebDriver getBrowser() {
		if (targetBrowserName.equals("Default")) {
			Assert.fail("Unsupported Browser Type.");
			return getBrowser(targetBrowserName);
		} else {
			return getBrowser(targetBrowserName);
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
		if (targetBrowserName.equals("Default")) {
			return getBrowser(testDataReader.getCellData("Target Browser"));
		} else {
			return getBrowser(targetBrowserName);
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
		WebDriver driver = null;
		DesiredCapabilities capabilities = null; // replaced with browser specific
		// options
		checkBrowserOSCrossCompatibility(browserName);
		// check cross-compatibility between the selected operating system and browser
		// and report in case they are not compatible
		setDriversPath();
		// set path based on operating system

		if (executionAddress.equals("local")) {
			ReportManager.log("Attempting to run locally on: [" + targetOperatingSystem + "], [" + browserName + "].");
			switch (browserName) {
			case "MozillaFirefox":
				driver = drivers.get("MozillaFirefox");
				if (driver == null) {
					System.setProperty("webdriver.gecko.driver", driversPath + "geckodriver" + fileExtension);
					driver = new FirefoxDriver();
					drivers.put("MozillaFirefox", driver);
					ReportManager.log("Successfully Opened Mozilla Firefox.");
				}
				break;
			case "MicrosoftInternetExplorer":
				driver = drivers.get("MicrosoftInternetExplorer");
				if (driver == null) {
					System.setProperty("webdriver.ie.driver", driversPath + "IEDriverServer" + fileExtension);
					driver = new InternetExplorerDriver();
					drivers.put("MicrosoftInternetExplorer", driver);
					ReportManager.log("Successfully Opened Microsoft Internet Explorer.");
				}
				break;
			case "GoogleChrome":
				driver = drivers.get("GoogleChrome");
				if (driver == null) {
					System.setProperty("webdriver.chrome.driver", driversPath + "chromedriver" + fileExtension);
					/*
					 * if (autoMaximizeBrowserWindow) { ChromeOptions options = new ChromeOptions();
					 * options.addArguments("--start-maximized"); driver = new
					 * ChromeDriver(options); } else { driver = new ChromeDriver(); }
					 */
					driver = new ChromeDriver();
					drivers.put("GoogleChrome", driver);
					ReportManager.log("Successfully Opened Google Chrome.");
				}
				break;
			case "MicrosoftEdge":
				driver = drivers.get("MicrosoftEdge");
				if (driver == null) {
					System.setProperty("webdriver.edge.driver", driversPath + "MicrosoftWebDriver" + fileExtension);
					driver = new EdgeDriver();
					drivers.put("MicrosoftEdge", driver);
					ReportManager.log("Successfully Opened Microsoft Edge.");
				}
				break;
			case "Safari":
				driver = drivers.get("Safari");
				if (driver == null) {
					// System.setProperty("webdriver.edge.driver", driversPath +
					// "MicrosoftWebDriver" + fileExtension);
					driver = new SafariDriver();
					drivers.put("Safari", driver);
					ReportManager.log("Successfully Opened Safari.");
				}
				break;
			default:
				ReportManager.log("Unsupported Browser Type [" + browserName + "].");
				Assert.fail("Unsupported Browser Type [" + browserName + "].");
				break;
			}
		} else {
			// handling this exception for remote execution
			// [org.openqa.selenium.WebDriverException: Error forwarding the new session
			// cannot find : Capabilities]
			ReportManager.log("Attempting to run remotely on: [" + targetOperatingSystem + "], [" + browserName + "], ["
					+ targetHubURL + "].");

			try {
				switch (browserName) {
				case "MozillaFirefox":
					driver = drivers.get("MozillaFirefox");
					if (driver == null) {
						capabilities = DesiredCapabilities.firefox();
						capabilities.setCapability("marionette", true);
						capabilities.setCapability("nativeEvents", true);
						setDesiredOperatingSystem(capabilities);
						try {
							driver = new RemoteWebDriver(new URL(targetHubURL), new FirefoxOptions(capabilities));
						} catch (MalformedURLException e) {
							e.printStackTrace();
							ReportManager.log(e.getMessage() + System.lineSeparator() + e.getStackTrace().toString());
						} catch (SessionNotCreatedException e) {
							e.printStackTrace();
							ReportManager.log(e.getMessage() + System.lineSeparator() + e.getStackTrace().toString());
						}
						drivers.put("MozillaFirefox", driver);
						ReportManager.log("Successfully Opened Mozilla Firefox.");
					}
					break;
				case "MicrosoftInternetExplorer":
					driver = drivers.get("MicrosoftInternetExplorer");
					if (driver == null) {
						capabilities = DesiredCapabilities.internetExplorer();
						setDesiredOperatingSystem(capabilities);
						try {
							driver = new RemoteWebDriver(new URL(targetHubURL),
									new InternetExplorerOptions(capabilities));
						} catch (MalformedURLException e) {
							e.printStackTrace();
							ReportManager.log(e.getMessage() + System.lineSeparator() + e.getStackTrace().toString());
						} catch (SessionNotCreatedException e) {
							e.printStackTrace();
							ReportManager.log(e.getMessage() + System.lineSeparator() + e.getStackTrace().toString());
						}
						drivers.put("MicrosoftInternetExplorer", driver);
						ReportManager.log("Successfully Opened Microsoft Internet Explorer.");
					}
					break;
				case "GoogleChrome":
					driver = drivers.get("GoogleChrome");
					if (driver == null) {
						capabilities = DesiredCapabilities.chrome();
						setDesiredOperatingSystem(capabilities);
						try {
							/*
							 * if (autoMaximizeBrowserWindow) { driver = new RemoteWebDriver(new
							 * URL(targetHubURL), new
							 * ChromeOptions().merge(capabilities).addArguments("--start-maximized")); }
							 * else { driver = new RemoteWebDriver(new URL(targetHubURL), new
							 * ChromeOptions().merge(capabilities)); }
							 */
							driver = new RemoteWebDriver(new URL(targetHubURL),
									new ChromeOptions().merge(capabilities));
						} catch (MalformedURLException e) {
							e.printStackTrace();
							ReportManager.log(e.getMessage() + System.lineSeparator() + e.getStackTrace().toString());
						} catch (SessionNotCreatedException e) {
							e.printStackTrace();
							ReportManager.log(e.getMessage() + System.lineSeparator() + e.getStackTrace().toString());
						}
						drivers.put("GoogleChrome", driver);
						ReportManager.log("Successfully Opened Google Chrome.");
					}
					break;
				case "MicrosoftEdge":
					driver = drivers.get("MicrosoftEdge");
					if (driver == null) {
						System.setProperty("webdriver.edge.driver", driversPath + "MicrosoftWebDriver" + fileExtension);
						capabilities = DesiredCapabilities.edge();
						setDesiredOperatingSystem(capabilities);
						try {
							driver = new RemoteWebDriver(new URL(targetHubURL), new EdgeOptions().merge(capabilities));
						} catch (MalformedURLException e) {
							e.printStackTrace();
							ReportManager.log(e.getMessage() + System.lineSeparator() + e.getStackTrace().toString());
						} catch (SessionNotCreatedException e) {
							e.printStackTrace();
							ReportManager.log(e.getMessage() + System.lineSeparator() + e.getStackTrace().toString());
						}
						drivers.put("MicrosoftEdge", driver);
						ReportManager.log("Successfully Opened Microsoft Edge.");
					}
					break;
				default:
					ReportManager.log("Unsupported Browser Type [" + browserName + "].");
					Assert.fail("Unsupported Browser Type [" + browserName + "].");
					break;
				}
				((RemoteWebDriver) driver).setFileDetector(new LocalFileDetector());
			} catch (WebDriverException ex) {
				if (ex.getMessage().contains("Error forwarding the new session cannot find")) {
					ReportManager.log(
							"Error forwarding the new session: Couldn't find a node that matches the desired capabilities.");
					ReportManager.log("Failed to run remotely on: [" + targetOperatingSystem + "], [" + browserName
							+ "], [" + targetHubURL + "].");
					Assert.fail(
							"Error forwarding the new session: Couldn't find a node that matches the desired capabilities.");
				} else {
					ReportManager.log("Unhandled Error.");
					ReportManager.log("Failed to run remotely on: [" + targetOperatingSystem + "], [" + browserName
							+ "], [" + targetHubURL + "].");
					Assert.fail("Unhandled Error.");
				}
			}
		}

		driver.manage().timeouts().pageLoadTimeout(60, TimeUnit.SECONDS);
		JSWaiter.setDriver(driver);
		if (autoMaximizeBrowserWindow) {
			BrowserActions.maximizeWindow(driver); // Automatically maximize driver window after opening it
		}
		return driver;
	}

	/**
	 * Set the driver folder path and file extension based on the target Operating
	 * System
	 */
	private static void setDriversPath() {
		switch (targetOperatingSystem) {
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
			ReportManager.log("Unsupported Operating System [" + targetOperatingSystem + "].");
			Assert.fail("Unsupported Operating System [" + targetOperatingSystem + "].");
			break;
		}
	}

	private static void setDesiredOperatingSystem(DesiredCapabilities capabilities) {
		switch (targetOperatingSystem) {
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
			ReportManager.log("Unsupported Operating System [" + targetOperatingSystem + "].");
			Assert.fail("Unsupported Operating System [" + targetOperatingSystem + "].");
			break;
		}
	}

	/**
	 * Check cross-compatibility between the selected operating system and browser
	 * and report in case they are not compatible
	 */
	private static void checkBrowserOSCrossCompatibility(String browserName) {
		switch (targetOperatingSystem) {
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
						+ targetOperatingSystem + "].");
				Assert.fail("Unsupported Browser Type [" + browserName + "] for this Operating System ["
						+ targetOperatingSystem + "].");
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
						+ targetOperatingSystem + "].");
				Assert.fail("Unsupported Browser Type [" + browserName + "] for this Operating System ["
						+ targetOperatingSystem + "].");
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
						+ targetOperatingSystem + "].");
				Assert.fail("Unsupported Browser Type [" + browserName + "] for this Operating System ["
						+ targetOperatingSystem + "].");
				break;
			}
			break;
		default:
			ReportManager.log("Unsupported Operating System [" + targetOperatingSystem + "].");
			Assert.fail("Unsupported Operating System [" + targetOperatingSystem + "].");
			break;
		}
	}

	/**
	 * Close all open browser instances.
	 * 
	 */
	public static void closeAllDrivers() {
		try {
			for (String key : drivers.keySet()) {
				drivers.get(key).close();
				drivers.get(key).quit();
			}
		} catch (Throwable t) {
			// ReportManager.log(t.getMessage());
		}
		drivers.clear();
		ReportManager.log("Successfully Closed All Browsers.");
	}
}
