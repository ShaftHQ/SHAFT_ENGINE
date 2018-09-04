package com.shaftEngine.browserActionLibrary;

import java.awt.HeadlessException;
import java.awt.Toolkit;

import org.junit.Assert;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Point;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

import com.shaftEngine.elementActionLibrary.JSWaiter;
import com.shaftEngine.ioActionLibrary.ReportManager;
import com.shaftEngine.ioActionLibrary.ScreenshotManager;

public class BrowserActions {

	/*
	 * Driver actions that should be implemented:
	 * 
	 * driver.manage().window().maximize(); driver.manage().window().fullscreen();
	 * driver.navigate().back(); driver.navigate().forward();
	 * driver.navigate().refresh(); driver.switchTo().activeElement();
	 * driver.switchTo().alert(); driver.switchTo().defaultContent();
	 * driver.switchTo().frame(arg0); driver.switchTo().parentFrame();
	 * driver.switchTo().window(arg0);
	 */

	private BrowserActions() {
		throw new IllegalStateException("Utility class");
	}

	private static void passAction(WebDriver driver, String actionName) {
		passAction(driver, actionName, null);
	}

	private static void passAction(WebDriver driver, String actionName, String testData) {
		String message = "[" + actionName + "] successfully performed.";
		if (testData != null) {
			message = message + " With the following test data [" + testData + "].";
		}
		ScreenshotManager.captureScreenShot(driver, actionName + "_performed", true);
		ReportManager.log(message);
	}

	private static void failAction(WebDriver driver, String actionName) {
		failAction(driver, actionName, null);
	}

	private static void failAction(WebDriver driver, String actionName, String testData) {
		String message = "[" + actionName + "] failed.";
		if (testData != null) {
			message = message + " With the following test data [" + testData + "].";
		}
		ScreenshotManager.captureScreenShot(driver, actionName + "_failed", false);
		ReportManager.log(message);
		Assert.fail(message);
	}

	/**
	 * Gets the current page URL and returns it as a string
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @return the URL that's currently open in the current page
	 */
	public static String getCurrentURL(WebDriver driver) {
		triggerWaitForLazyLoading(driver);
		String currentURL = "";
		try {
			currentURL = driver.getCurrentUrl();
			passAction(driver, "getCurrentURL", currentURL);
		} catch (Exception e) {
			ReportManager.log(e);
			failAction(driver, "getCurrentURL", currentURL);
		}
		return currentURL;
	}

	/**
	 * Gets the current window title and returns it as a string
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @return the title of the current window
	 */
	public static String getCurrentWindowTitle(WebDriver driver) {
		triggerWaitForLazyLoading(driver);
		String currentWindowTitle = "";
		try {
			currentWindowTitle = driver.getTitle();
			passAction(driver, "getCurrentWindowTitle", currentWindowTitle);
		} catch (Exception e) {
			ReportManager.log(e);
			failAction(driver, "getCurrentWindowTitle", currentWindowTitle);
		}
		return currentWindowTitle;
	}

	/**
	 * Gets the current page source and returns it as a string
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @return the source of the current page
	 */
	public static String getPageSource(WebDriver driver) {
		triggerWaitForLazyLoading(driver);
		String pageSource = "";
		try {
			pageSource = driver.getPageSource();
			passAction(driver, "getPageSource", pageSource);
		} catch (Exception e) {
			ReportManager.log(e);
			failAction(driver, "getPageSource", pageSource);
		}
		return pageSource;
	}

	/**
	 * Gets the current window handle and returns it as a string
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @return the window handle for the current window
	 */
	public static String getWindowHandle(WebDriver driver) {
		triggerWaitForLazyLoading(driver);
		String windowHandle = "";
		try {
			windowHandle = driver.getWindowHandle();
			passAction(driver, "getWindowHandle", windowHandle);
		} catch (Exception e) {
			ReportManager.log(e);
			failAction(driver, "getWindowHandle", windowHandle);
		}
		return windowHandle;
	}

	/**
	 * Gets the current window position and returns it as a string
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @return the position of the current window
	 */
	public static String getWindowPosition(WebDriver driver) {
		triggerWaitForLazyLoading(driver);
		String windowPosition = "";
		try {
			windowPosition = driver.manage().window().getPosition().toString();
			passAction(driver, "getWindowPosition", windowPosition);
		} catch (Exception e) {
			ReportManager.log(e);
			failAction(driver, "getWindowPosition", windowPosition);
		}
		return windowPosition;
	}

	/**
	 * Gets the current window size and returns it as a string
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @return the size of the current window
	 */
	public static String getWindowSize(WebDriver driver) {
		triggerWaitForLazyLoading(driver);
		String windowSize = "";
		try {
			windowSize = driver.manage().window().getSize().toString();
			passAction(driver, "getWindowSize", windowSize);
		} catch (Exception e) {
			ReportManager.log(e);
			failAction(driver, "getWindowSize", windowSize);
		}
		return windowSize;
	}

	/**
	 * Navigates to targetUrl in case the current URL is different, else refreshes
	 * the current page
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param targetUrl
	 *            a string that represents the URL that you wish to navigate to
	 */
	public static void navigateToURL(WebDriver driver, String targetUrl) {
		triggerWaitForLazyLoading(driver);
		String currentUrl = "";
		try {
			currentUrl = driver.getCurrentUrl();
			if (!currentUrl.equals(targetUrl)) {
				// navigate to new url
				navigateToNewURL(driver, targetUrl);
				(new WebDriverWait(driver, 30)).until(ExpectedConditions.not(ExpectedConditions.urlToBe(currentUrl)));
			} else {
				// already on the same page
				driver.navigate().refresh();
			}
			passAction(driver, "navigateToURL", targetUrl);
		} catch (Exception e) {
			ReportManager.log(e);
			failAction(driver, "navigateToURL", targetUrl);
		}
	}

	/**
	 * Navigates one step back from the browsers history
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 */
	public static void navigateBack(WebDriver driver) {
		triggerWaitForLazyLoading(driver);
		String initialURL = "";
		try {
			initialURL = driver.getCurrentUrl();
			driver.navigate().back();
			triggerWaitForLazyLoading(driver);
			(new WebDriverWait(driver, 30)).until(ExpectedConditions.not(ExpectedConditions.urlToBe(initialURL)));
			if (!initialURL.equals(driver.getCurrentUrl())) {
				passAction(driver, "navigateBack");
			} else {
				failAction(driver, "navigateBack");
			}
		} catch (Exception e) {
			ReportManager.log(e);
			failAction(driver, "navigateBack");
		}
	}

	/**
	 * Attempts to refresh the current page
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 */
	public static void refreshCurrentPage(WebDriver driver) {
		triggerWaitForLazyLoading(driver);
		String currentURL = getCurrentURL(driver);
		try {
			driver.navigate().refresh();
			passAction(driver, "refreshCurrentPage");
		} catch (Exception e) {
			if (currentURL.equals(getCurrentURL(driver))) {
				passAction(driver, "refreshCurrentPage");
			} else {
				ReportManager.log(e);
				failAction(driver, "refreshCurrentPage");
			}
		}
	}

	private static void navigateToNewURL(WebDriver driver, String targetUrl) {
		try {
			driver.navigate().to(targetUrl);
		} catch (WebDriverException e) {
			ReportManager.log(e);
			failAction(driver, "navigateToURL", targetUrl);
		}
	}

	/**
	 * Closes the current browser window
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 */
	public static void closeCurrentWindow(WebDriver driver) {
		triggerWaitForLazyLoading(driver);
		try {
			driver.close();
			passAction(driver, "closeCurrentWindow");
		} catch (WebDriverException e) {
			ReportManager.log(e);
			failAction(driver, "closeCurrentWindow");
		}
	}

	/**
	 * Maximizes current window size based on screen size minus 5%
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 */
	public static void maximizeWindow(WebDriver driver) {
		String windowSize = "";
		int width = 1920;
		int height = 1080;

		windowSize = driver.manage().window().getSize().toString();
		driver.manage().window().maximize();

		if (windowSize.equals(driver.manage().window().getSize().toString())) {
			try {
				Toolkit toolkit = Toolkit.getDefaultToolkit();
				width = (int) toolkit.getScreenSize().getWidth();
				height = (int) toolkit.getScreenSize().getHeight(); // height = height * 95 / 100; // subtracting 5% to
																	// // account for the start bar

				// ReportManager.log("Current screen size is ["+Width+"x"+Height+"].");
				driver.manage().window().setPosition(new Point(0, 0));
				driver.manage().window().setSize(new Dimension(width, height));
			} catch (HeadlessException e) {
				// happens with headless firefox browsers // remote // linux and windows
			}
		}

		if (windowSize.equals(driver.manage().window().getSize().toString())) {
			((JavascriptExecutor) driver).executeScript("window.resizeTo(" + width + ", " + height + ");");
			((JavascriptExecutor) driver).executeScript("window.moveTo(0,0);");
			((JavascriptExecutor) driver).executeScript("window.focus();");
		}

		if (windowSize.equals(driver.manage().window().getSize().toString())) {
			// failAction(driver, "maximizeWindow");
			ReportManager.log("skipping window maximization due to unknown error, marking step as passed.");
		}
		passAction(driver, "maximizeWindow");
	}

	private static void triggerWaitForLazyLoading(WebDriver driver) {
		try {
			JSWaiter.waitForLazyLoading();
		} catch (Exception e) {
			if (e.getMessage().contains("jQuery is not defined")) {
				// do nothing
			} else {
				ReportManager.log(e);
				failAction(driver, "triggerWaitForLazyLoading");
			}
		}
	}
}
