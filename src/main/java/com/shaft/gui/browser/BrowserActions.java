package com.shaft.gui.browser;

import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.time.Duration;
import java.util.Arrays;
import java.util.List;

import org.openqa.selenium.By;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchSessionException;
import org.openqa.selenium.Point;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.testng.Assert;

import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.JSWaiter;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;

public class BrowserActions {
    private static final Boolean HEADLESS_EXECUTION = Boolean.valueOf(System.getProperty("headlessExecution").trim());
    private static final Duration NAVIGATION_TIMEOUT = Duration
	    .ofSeconds(Integer.parseInt(System.getProperty("browserNavigationTimeout").trim()));

    private BrowserActions() {
	throw new IllegalStateException("Utility class");
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Reporting Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private static void passAction(String actionName) {
	passAction(null, actionName, null);
    }

    private static void passAction(WebDriver driver, String actionName) {
	passAction(driver, actionName, null);
    }

    private static void passAction(WebDriver driver, String actionName, String testData) {
	String message = "Browser Action [" + actionName + "] successfully performed.";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	if (driver != null) {
	    ReportManager.log(message, Arrays.asList(ScreenshotManager.captureScreenShot(driver, actionName, true)));
	} else {
	    ReportManager.log(message);
	}
    }

    private static void failAction(String actionName) {
	failAction(null, actionName, null);
    }

    private static void failAction(WebDriver driver, String actionName) {
	failAction(driver, actionName, null);
    }

    private static void failAction(WebDriver driver, String actionName, String testData) {
	String message = "[" + actionName + "] failed.";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	if (driver != null) {
	    ReportManager.log(message, Arrays.asList(ScreenshotManager.captureScreenShot(driver, actionName, false)));
	} else {
	    ReportManager.log(message);
	}
	Assert.fail(message);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Preparation and Support Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private static void confirmThatWebsiteIsNotDown(WebDriver driver, String targetUrl) {
	List<String> navigationErrorMessages = Arrays.asList("This site can’t be reached", "Unable to connect",
		"Safari Can’t Connect to the Server", "This page can't be displayed", "Invalid URL",
		"<head></head><body></body>");
	navigationErrorMessages.forEach(errorMessage -> {
	    if (driver.getPageSource().contains(errorMessage)) {
		failAction(driver, "navigateToURL",
			"Error message: \"" + errorMessage + "\", Target URL: \"" + targetUrl + "\"");
	    }
	});
    }

    private static void navigateToNewURL(WebDriver driver, String targetUrl, String targetUrlAfterRedirection) {
	try {
	    driver.navigate().to(targetUrl);
	    // will use contains here instead of equals, because sometimes the url after
	    // redirection contains a random token that cannot be predefined, also as a
	    // precaution against the failure in case the user tries to navigate back to the
	    // source url which already redirected him
	    (new WebDriverWait(driver, NAVIGATION_TIMEOUT))
		    .until(ExpectedConditions.urlContains(targetUrlAfterRedirection));
	} catch (WebDriverException e) {
	    ReportManager.log(e);
	    failAction(driver, "navigateToURL", targetUrl);
	}
    }

    private static Dimension attemptMaximizeUsingSeleniumWebDriver(WebDriver driver, String executionAddress,
	    String targetBrowserName, String targetOperatingSystem) {
	if ((!executionAddress.equals("local") && !targetBrowserName.equals("GoogleChrome"))
		|| (executionAddress.equals("local")
			&& !(targetBrowserName.equals("GoogleChrome") && targetOperatingSystem.equals("Mac-64")))) {
	    try {
		driver.manage().window().maximize();
		ReportManager.logDiscrete(
			"Window size after SWD Maximize: " + driver.manage().window().getSize().toString());
		return driver.manage().window().getSize();
	    } catch (WebDriverException e) {
		// org.openqa.selenium.WebDriverException: unknown error: failed to change
		// window state to maximized, current state is normal
		ReportManager.log(e);
	    }
	}
	return null;
    }

    private static Dimension attemptMazimizeUsingToolkitAndJavascript(WebDriver driver, int width, int height) {
	try {
	    Toolkit toolkit = Toolkit.getDefaultToolkit();
	    if (!HEADLESS_EXECUTION) {
		width = (int) toolkit.getScreenSize().getWidth();
		height = (int) toolkit.getScreenSize().getHeight();
	    }
	    driver.manage().window().setPosition(new Point(0, 0));
	    driver.manage().window().setSize(new Dimension(width, height));

	    ReportManager.logDiscrete("Window size after Toolkit: " + driver.manage().window().getSize().toString());
	    return driver.manage().window().getSize();
	} catch (HeadlessException e) {
	    ((JavascriptExecutor) driver).executeScript("window.focus();");
	    ((JavascriptExecutor) driver).executeScript("window.moveTo(0,0);");
	    ((JavascriptExecutor) driver).executeScript("window.resizeTo(" + width + ", " + height + ");");

	    ReportManager.logDiscrete(
		    "Window size after JavascriptExecutor: " + driver.manage().window().getSize().toString());
	    return driver.manage().window().getSize();
	}
    }

    private static Dimension attemptMaximizeUsingSeleniumWebDriverManageWindow(WebDriver driver, int width,
	    int height) {
	driver.manage().window().setPosition(new Point(0, 0));
	driver.manage().window().setSize(new Dimension(width, height));

	ReportManager.logDiscrete(
		"Window size after WebDriver.Manage.Window: " + driver.manage().window().getSize().toString());
	return driver.manage().window().getSize();
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [Public] Core Browser Factory Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * Gets the current page URL and returns it as a string
     * 
     * @param driver the current instance of Selenium webdriver
     * @return the URL that's currently open in the current page
     */
    public static String getCurrentURL(WebDriver driver) {
	JSWaiter.waitForLazyLoading();
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
     * @param driver the current instance of Selenium webdriver
     * @return the title of the current window
     */
    public static String getCurrentWindowTitle(WebDriver driver) {
	JSWaiter.waitForLazyLoading();
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
     * @param driver the current instance of Selenium webdriver
     * @return the source of the current page
     */
    public static String getPageSource(WebDriver driver) {
	JSWaiter.waitForLazyLoading();
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
     * @param driver the current instance of Selenium webdriver
     * @return the window handle for the current window
     */
    public static String getWindowHandle(WebDriver driver) {
	JSWaiter.waitForLazyLoading();
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
     * @param driver the current instance of Selenium webdriver
     * @return the position of the current window
     */
    public static String getWindowPosition(WebDriver driver) {
	JSWaiter.waitForLazyLoading();
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
     * @param driver the current instance of Selenium webdriver
     * @return the size of the current window
     */
    public static String getWindowSize(WebDriver driver) {
	JSWaiter.waitForLazyLoading();
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
     * @param driver    the current instance of Selenium webdriver
     * @param targetUrl a string that represents the URL that you wish to navigate
     *                  to
     */
    public static void navigateToURL(WebDriver driver, String targetUrl) {
	navigateToURL(driver, targetUrl, targetUrl);
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes
     * the current page. Waits for successfully navigating to the final url after
     * redirection.
     * 
     * @param driver                    the current instance of Selenium webdriver
     * @param targetUrl                 a string that represents the URL that you
     *                                  wish to navigate to
     * @param targetUrlAfterRedirection a string that represents a part of the url
     *                                  that should be present after redirection,
     *                                  this string is used to confirm successful
     *                                  navigation
     */
    public static void navigateToURL(WebDriver driver, String targetUrl, String targetUrlAfterRedirection) {
	// force stop any current navigation
	try {
	    ((JavascriptExecutor) driver).executeScript("return window.stop;");
	} catch (Exception e) {
	    ReportManager.log(e);
	    /*
	     * org.openqa.selenium.NoSuchSessionException: Session ID is null. Using
	     * WebDriver after calling quit()? Build info: version: '3.141.59', revision:
	     * 'e82be7d358', time: '2018-11-14T08:17:03' System info: host:
	     * 'gcp-test-automation-sys-187-jenkins-fullaccess', ip: '10.128.0.11', os.name:
	     * 'Linux', os.arch: 'amd64', os.version: '4.15.0-1027-gcp', java.version:
	     * '1.8.0_202' Driver info: driver.version: RemoteWebDriver
	     */
	}
	try {
	    JSWaiter.waitForLazyLoading();

	    String initialURL = "";
	    String initialSource = driver.getPageSource();
	    initialURL = driver.getCurrentUrl();
	    if (!initialURL.equals(targetUrl)) {
		// navigate to new url
		navigateToNewURL(driver, targetUrl, targetUrlAfterRedirection);
		JSWaiter.waitForLazyLoading();
		if ((ElementActions.getElementsCount(driver, By.tagName("html")) == 1)
			&& (!driver.getPageSource().equalsIgnoreCase(initialSource))) {
		    confirmThatWebsiteIsNotDown(driver, targetUrl);
		    passAction(driver, "navigateToURL", targetUrl);
		} else {
		    failAction(driver, "navigateToURL", targetUrl);
		}
	    } else {
		// already on the same page
		driver.navigate().refresh();
		JSWaiter.waitForLazyLoading();
		if (ElementActions.getElementsCount(driver, By.tagName("html")) == 1) {
		    confirmThatWebsiteIsNotDown(driver, targetUrl);
		    passAction(driver, "navigateToURL", targetUrl);
		}
	    }
	} catch (Exception e) {
	    ReportManager.log(e);
	    failAction(driver, "navigateToURL", targetUrl);
	}
    }

    /**
     * Navigates one step back from the browsers history
     * 
     * @param driver the current instance of Selenium webdriver
     */
    public static void navigateBack(WebDriver driver) {
	JSWaiter.waitForLazyLoading();
	String initialURL = "";
	try {
	    initialURL = driver.getCurrentUrl();
	    driver.navigate().back();
	    JSWaiter.waitForLazyLoading();
	    (new WebDriverWait(driver, NAVIGATION_TIMEOUT))
		    .until(ExpectedConditions.not(ExpectedConditions.urlToBe(initialURL)));
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

    public static void navigateForward(WebDriver driver) {
	JSWaiter.waitForLazyLoading();
	String initialURL = "";
	try {
	    initialURL = driver.getCurrentUrl();
	    driver.navigate().forward();
	    JSWaiter.waitForLazyLoading();
	    (new WebDriverWait(driver, NAVIGATION_TIMEOUT))
		    .until(ExpectedConditions.not(ExpectedConditions.urlToBe(initialURL)));
	    if (!initialURL.equals(driver.getCurrentUrl())) {
		passAction(driver, "navigateForward");
	    } else {
		failAction(driver, "navigateForward");
	    }
	} catch (Exception e) {
	    ReportManager.log(e);
	    failAction(driver, "navigateForward");
	}
    }

    /**
     * Attempts to refresh the current page
     * 
     * @param driver the current instance of Selenium webdriver
     */
    public static void refreshCurrentPage(WebDriver driver) {
	JSWaiter.waitForLazyLoading();
	driver.navigate().refresh();
	passAction(driver, "refreshCurrentPage");
	// removed all exception handling as there was no comments on when and why this
	// exception happens
    }

    /**
     * Closes the current browser window
     * 
     * @param driver the current instance of Selenium webdriver
     */
    public static void closeCurrentWindow(WebDriver driver) {
	JSWaiter.waitForLazyLoading();
	try {
	    driver.close();
	    driver.quit();
	    passAction("closeCurrentWindow");
	} catch (NoSuchSessionException e) {
	    // browser was already closed by the .close() method
	} catch (Exception e) {
	    ReportManager.log(e);
	    failAction("closeCurrentWindow");
	}
    }

    /**
     * Maximizes current window size based on screen size minus 5%
     * 
     * @param driver the current instance of Selenium webdriver
     */
    public static void maximizeWindow(WebDriver driver) {
	Dimension initialWindowSize;
	Dimension currentWindowSize;
	int width = 1920;
	int height = 1080;

	initialWindowSize = driver.manage().window().getSize();
	ReportManager.logDiscrete("Initial window size: " + initialWindowSize.toString());

	String targetBrowserName = System.getProperty("targetBrowserName").trim();
	String targetOperatingSystem = System.getProperty("targetOperatingSystem").trim();
	String executionAddress = System.getProperty("executionAddress").trim();

	// try selenium webdriver maximize
	currentWindowSize = attemptMaximizeUsingSeleniumWebDriver(driver, executionAddress, targetBrowserName,
		targetOperatingSystem);
	if (currentWindowSize == null) {
	    currentWindowSize = driver.manage().window().getSize();
	}

	if ((initialWindowSize.height == currentWindowSize.height)
		&& (initialWindowSize.width == currentWindowSize.width)) {
	    // attempt resize using toolkit
	    currentWindowSize = attemptMazimizeUsingToolkitAndJavascript(driver, width, height);
	}

	if ((initialWindowSize.height == currentWindowSize.height)
		&& (initialWindowSize.width == currentWindowSize.width)) {
	    // happens with headless firefox browsers // remote // linux and windows
	    // also happens with chrome/windows

	    // attempt resize using WebDriver mange window
	    currentWindowSize = attemptMaximizeUsingSeleniumWebDriverManageWindow(driver, width, height);
	}

	if ((initialWindowSize.height == currentWindowSize.height)
		&& (initialWindowSize.width == currentWindowSize.width)) {

	    // attempt setting window to fullscreen
	    fullScreenWindow(driver);

	    currentWindowSize = driver.manage().window().getSize();
	    ReportManager.logDiscrete("Window size after fullScreenWindow: " + currentWindowSize.toString());
	}

	if ((initialWindowSize.height == currentWindowSize.height)
		&& (initialWindowSize.width == currentWindowSize.width)) {
	    ReportManager.logDiscrete("skipping window maximization due to unknown error, marking step as passed.");
	}

	passAction(driver, "maximizeWindow", "New screen size is now: " + currentWindowSize.toString());
    }

    /**
     * Resizes the current window size based on the provided width and height
     * 
     * @param driver the current instance of Selenium webdriver
     * @param width  the desired new width of the target window
     * @param height the desired new height of the target window
     */
    public static void setWindowSize(WebDriver driver, int width, int height) {
	Dimension initialWindowSize;
	Dimension currentWindowSize;

	initialWindowSize = driver.manage().window().getSize();
	ReportManager.logDiscrete("Initial window size: " + initialWindowSize.toString());

	driver.manage().window().setPosition(new Point(0, 0));
	driver.manage().window().setSize(new Dimension(width + 1, height + 1));
	// apparently we need to add +1 here to ensure that the new window size matches
	// the expected window size

	currentWindowSize = driver.manage().window().getSize();
	ReportManager.logDiscrete("Window size after SWD: " + currentWindowSize.toString());

	if ((initialWindowSize.height == currentWindowSize.height)
		&& (initialWindowSize.width == currentWindowSize.width)) {
	    ((JavascriptExecutor) driver).executeScript("window.focus();");
	    ((JavascriptExecutor) driver).executeScript("window.moveTo(0,0);");
	    ((JavascriptExecutor) driver).executeScript("window.resizeTo(" + width + 1 + ", " + height + 1 + ");");

	    currentWindowSize = driver.manage().window().getSize();
	    ReportManager.logDiscrete("Window size after JavascriptExecutor: " + currentWindowSize.toString());
	}

	if ((initialWindowSize.height == currentWindowSize.height)
		&& (initialWindowSize.width == currentWindowSize.width)) {
	    ReportManager.logDiscrete("skipping window resizing due to unknown error, marking step as passed.");
	}

	passAction(driver, "setWindowSize", "New screen size is now: " + currentWindowSize.toString());
    }

    public static void fullScreenWindow(WebDriver driver) {
	Dimension initialWindowSize;
	int width = 1920;
	int height = 1080;
	Boolean heightNotChanged;
	Boolean widthNotChanged;

	initialWindowSize = driver.manage().window().getSize();
	driver.manage().window().fullscreen();

	heightNotChanged = String.valueOf(initialWindowSize.height)
		.equals(String.valueOf(driver.manage().window().getSize().height));
	widthNotChanged = String.valueOf(initialWindowSize.width)
		.equals(String.valueOf(driver.manage().window().getSize().width));

	if (heightNotChanged && widthNotChanged) {
	    ((JavascriptExecutor) driver).executeScript("window.focus();");
	    ((JavascriptExecutor) driver).executeScript("window.moveTo(0,0);");
	    ((JavascriptExecutor) driver).executeScript("window.resizeTo(" + width + ", " + height + ");");
	}

	if (heightNotChanged && widthNotChanged) {
	    ReportManager.logDiscrete(
		    "skipping switching window to full screen due to unknown error, marking step as passed.");
	}
	passAction(driver, "fullScreenWindow");
    }
}
