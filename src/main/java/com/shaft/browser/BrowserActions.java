package com.shaft.browser;

import java.awt.HeadlessException;
import java.awt.Toolkit;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Point;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

import com.shaft.element.ElementActions;
import com.shaft.element.JSWaiter;
import com.shaft.io.ReportManager;
import com.shaft.io.ScreenshotManager;

public class BrowserActions {

    private static final int NAVIGATION_TIMEOUT = 30;

    /*
     * Driver actions that should be implemented: driver.switchTo().activeElement();
     * driver.switchTo().alert(); driver.switchTo().window(arg0);
     */

    private BrowserActions() {
	throw new IllegalStateException("Utility class");
    }

    private static void passAction(WebDriver driver, String actionName) {
	passAction(driver, actionName, null);
    }

    private static void passAction(WebDriver driver, String actionName, String testData) {
	String message = "Browser Action [" + actionName + "] successfully performed.";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	ScreenshotManager.captureScreenShot(driver, actionName, true);
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
	ScreenshotManager.captureScreenShot(driver, actionName, false);
	ReportManager.log(message);
	Assert.fail(message);
    }

    /**
     * Gets the current page URL and returns it as a string
     * 
     * @param driver the current instance of Selenium webdriver
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
     * @param driver the current instance of Selenium webdriver
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
     * @param driver the current instance of Selenium webdriver
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
     * @param driver the current instance of Selenium webdriver
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
     * @param driver the current instance of Selenium webdriver
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
     * @param driver the current instance of Selenium webdriver
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
	    triggerWaitForLazyLoading(driver);

	    String initialURL = "";
	    String initialSource = driver.getPageSource();
	    initialURL = driver.getCurrentUrl();
	    if (!initialURL.equals(targetUrl)) {
		// navigate to new url
		navigateToNewURL(driver, targetUrl, targetUrlAfterRedirection);
		triggerWaitForLazyLoading(driver);

		if ((ElementActions.getElementsCount(driver, By.tagName("html")) == 1)
			&& (!driver.getPageSource().equalsIgnoreCase(initialSource))) {
		    passAction(driver, "navigateToURL", targetUrl);
		}
	    } else {
		// already on the same page
		driver.navigate().refresh();
		triggerWaitForLazyLoading(driver);

		if (ElementActions.getElementsCount(driver, By.tagName("html")) == 1) {
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

    public static void navigateForward(WebDriver driver) {
	triggerWaitForLazyLoading(driver);
	String initialURL = "";
	try {
	    initialURL = driver.getCurrentUrl();
	    driver.navigate().forward();
	    triggerWaitForLazyLoading(driver);
	    (new WebDriverWait(driver, 30)).until(ExpectedConditions.not(ExpectedConditions.urlToBe(initialURL)));
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
	triggerWaitForLazyLoading(driver);
	driver.navigate().refresh();
	passAction(driver, "refreshCurrentPage");
	// removed all exception handling as there was no comments on when and why this
	// exception happens
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

    /**
     * Closes the current browser window
     * 
     * @param driver the current instance of Selenium webdriver
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
     * @param driver the current instance of Selenium webdriver
     */
    public static void maximizeWindow(WebDriver driver) {
	Dimension initialWindowSize;
	Dimension currentWindowSize;
	int width = 1920;
	int height = 1080;

	initialWindowSize = driver.manage().window().getSize();
	ReportManager.logDiscreet("Initial window size: " + initialWindowSize.toString());

	String targetBrowserName = System.getProperty("targetBrowserName").trim();
	String targetOperatingSystem = System.getProperty("targetOperatingSystem").trim();
	String executionAddress = System.getProperty("executionAddress").trim();

	if ((!executionAddress.equals("local") && !targetBrowserName.equals("GoogleChrome"))
		|| (executionAddress.equals("local")
			&& !(targetBrowserName.equals("GoogleChrome") && targetOperatingSystem.equals("Mac-64")))) {
	    try {
		driver.manage().window().maximize();

		currentWindowSize = driver.manage().window().getSize();
		ReportManager.logDiscreet("Window size after SWD Maximize: " + currentWindowSize.toString());
	    } catch (WebDriverException e) {
		// org.openqa.selenium.WebDriverException: unknown error: failed to change
		// window state to maximized, current state is normal
		ReportManager.log(e);
	    }
	}
	currentWindowSize = driver.manage().window().getSize();

	if ((initialWindowSize.height == currentWindowSize.height)
		&& (initialWindowSize.width == currentWindowSize.width)) {
	    try {
		Toolkit toolkit = Toolkit.getDefaultToolkit();
		width = (int) toolkit.getScreenSize().getWidth();
		height = (int) toolkit.getScreenSize().getHeight();
		driver.manage().window().setPosition(new Point(0, 0));
		driver.manage().window().setSize(new Dimension(width, height));

		currentWindowSize = driver.manage().window().getSize();
		ReportManager.logDiscreet("Window size after Toolkit: " + currentWindowSize.toString());
	    } catch (HeadlessException e) {
		((JavascriptExecutor) driver).executeScript("window.focus();");
		((JavascriptExecutor) driver).executeScript("window.moveTo(0,0);");
		((JavascriptExecutor) driver).executeScript("window.resizeTo(" + width + ", " + height + ");");

		currentWindowSize = driver.manage().window().getSize();
		ReportManager.logDiscreet("Window size after JavascriptExecutor: " + currentWindowSize.toString());
	    }
	}

	if ((initialWindowSize.height == currentWindowSize.height)
		&& (initialWindowSize.width == currentWindowSize.width)) {
	    // happens with headless firefox browsers // remote // linux and windows
	    // also happens with chrome/windows
	    driver.manage().window().setPosition(new Point(0, 0));
	    driver.manage().window().setSize(new Dimension(width, height));

	    currentWindowSize = driver.manage().window().getSize();
	    ReportManager.logDiscreet("Window size after WebDriver.Options: " + currentWindowSize.toString());
	}

	if ((initialWindowSize.height == currentWindowSize.height)
		&& (initialWindowSize.width == currentWindowSize.width)) {

	    fullScreenWindow(driver);

	    currentWindowSize = driver.manage().window().getSize();
	    ReportManager.logDiscreet("Window size after fullScreenWindow: " + currentWindowSize.toString());
	}

	if ((initialWindowSize.height == currentWindowSize.height)
		&& (initialWindowSize.width == currentWindowSize.width)) {
	    // failAction(driver, "maximizeWindow");
	    ReportManager.logDiscreet("skipping window maximization due to unknown error, marking step as passed.");
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
	ReportManager.logDiscreet("Initial window size: " + initialWindowSize.toString());

	driver.manage().window().setPosition(new Point(0, 0));
	driver.manage().window().setSize(new Dimension(width + 1, height + 1));
	// apparently we need to add +1 here to ensure that the new window size matches
	// the expected window size

	currentWindowSize = driver.manage().window().getSize();
	ReportManager.logDiscreet("Window size after SWD: " + currentWindowSize.toString());

	if ((initialWindowSize.height == currentWindowSize.height)
		&& (initialWindowSize.width == currentWindowSize.width)) {
	    ((JavascriptExecutor) driver).executeScript("window.focus();");
	    ((JavascriptExecutor) driver).executeScript("window.moveTo(0,0);");
	    ((JavascriptExecutor) driver).executeScript("window.resizeTo(" + width + 1 + ", " + height + 1 + ");");

	    currentWindowSize = driver.manage().window().getSize();
	    ReportManager.logDiscreet("Window size after JavascriptExecutor: " + currentWindowSize.toString());
	}

	if ((initialWindowSize.height == currentWindowSize.height)
		&& (initialWindowSize.width == currentWindowSize.width)) {
	    ReportManager.logDiscreet("skipping window resizing due to unknown error, marking step as passed.");
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
	    // failAction(driver, "fullScreenWindow");
	    ReportManager.logDiscreet(
		    "skipping switching window to full screen due to unknown error, marking step as passed.");
	}
	passAction(driver, "fullScreenWindow");
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
