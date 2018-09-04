package com.shaftEngine.ioActionLibrary;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.nio.file.FileSystems;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Reporter;

import com.shaftEngine.elementActionLibrary.ElementActions;
import com.shaftEngine.elementActionLibrary.JSWaiter;

public class ScreenshotManager {
	private static final String SCREENSHOT_FOLDERPATH = "allure-results/screenshots/";
	private static final String SCREENSHOT_FOLDERNAME = new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date());
	private static String screenshotFileName = "Screenshot";
	private static final String SCREENSHOT_FLAG_WHENTOTAKEASCREENSHOT = System.getProperty("screenshooterFlag");
	private static final Boolean SCREENSHOT_FLAG_TAKEFULLPAGESCREENSHOT = Boolean
			.valueOf(System.getProperty("takeFullPageScreenshots"));

	private ScreenshotManager() {
		throw new IllegalStateException("Utility class");
	}

	/*
	 * A flag to determine when to take a screenshot. Always; after every browser
	 * and element action. Never; never. ValidationPointsOnly; after every assertion
	 * or verification point. FailuresOnly; after validation failures and element
	 * action failures.
	 */

	private static boolean globalPassFailStatus = false;
	/*
	 * A flag to control the highlighting of the element green for passing yellow
	 * for failing
	 */
	private static String appendedText = "";

	/**
	 * Used only on browser-based custom validation points. passFailStatus; true
	 * means pass and false means fail.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param passFailStatus
	 *            A flag to determine whether the action has passed or failed
	 */
	public static void captureScreenShot(WebDriver driver, boolean passFailStatus) {
		globalPassFailStatus = passFailStatus;
		if (passFailStatus) {
			appendedText = "passed";
		} else {
			appendedText = "failed";
		}

		if ((SCREENSHOT_FLAG_WHENTOTAKEASCREENSHOT.equals("Always"))
				|| (SCREENSHOT_FLAG_WHENTOTAKEASCREENSHOT.equals("ValidationPointsOnly"))
				|| (SCREENSHOT_FLAG_WHENTOTAKEASCREENSHOT.equals("FailuresOnly") && (!passFailStatus))) {
			internalCaptureScreenShot(driver, null, appendedText, true);
		} else {
			internalCaptureScreenShot(driver, null, appendedText, false);
		}
		// Note: Excluded the "Always" case as there will already be another screenshot
		// taken by the browser/element action // reversed this option to be able to
		// take a failure screenshot
	}

	/**
	 * Used only on assertElementExists and verifyElementExists element-based custom
	 * validation points. passFailStatus; true means pass and false means fail.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @param passFailStatus
	 *            A flag to determine whether the action has passed or failed
	 */
	public static void captureScreenShot(WebDriver driver, By elementLocator, boolean passFailStatus) {
		globalPassFailStatus = passFailStatus;
		if (passFailStatus) {
			appendedText = "passed";
		} else {
			appendedText = "failed";
		}

		if ((SCREENSHOT_FLAG_WHENTOTAKEASCREENSHOT.equals("Always"))
				|| (SCREENSHOT_FLAG_WHENTOTAKEASCREENSHOT.equals("ValidationPointsOnly"))
				|| (SCREENSHOT_FLAG_WHENTOTAKEASCREENSHOT.equals("FailuresOnly") && (!passFailStatus))) {
			internalCaptureScreenShot(driver, elementLocator, appendedText, true);
		} else {
			internalCaptureScreenShot(driver, elementLocator, appendedText, false);
		}
		// Note: Excluded the "Always" case as there will already be another screenshot
		// taken by the browser/element action // reversed this option to be able to
		// take a failure screenshot
	}

	/**
	 * Used in all browser actions, in failed element actions, in passed element
	 * actions where the element can no longer be found, and in passed
	 * switchToDefaultContent element action which requires no locator.
	 * passFailStatus; true means pass and false means fail.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param appendedText
	 *            the text that needs to be appended to the name of the screenshot
	 *            to make it more recognizable
	 * @param passFailStatus
	 *            A flag to determine whether the action has passed or failed
	 */
	public static void captureScreenShot(WebDriver driver, String appendedText, boolean passFailStatus) {
		globalPassFailStatus = passFailStatus;

		if ((SCREENSHOT_FLAG_WHENTOTAKEASCREENSHOT.equals("Always"))
				|| (SCREENSHOT_FLAG_WHENTOTAKEASCREENSHOT.equals("ValidationPointsOnly") && (!passFailStatus))
				|| ((SCREENSHOT_FLAG_WHENTOTAKEASCREENSHOT.equals("FailuresOnly")) && (!passFailStatus))) {
			internalCaptureScreenShot(driver, null, appendedText, true);
		} else {
			internalCaptureScreenShot(driver, null, appendedText, false);
		}
	}

	/**
	 * Used only in passed element actions. Appended Text is added to the screenshot
	 * name to signal why it was taken.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @param appendedText
	 *            the text that needs to be appended to the name of the screenshot
	 *            to make it more recognizable
	 */
	public static void captureScreenShot(WebDriver driver, By elementLocator, String appendedText,
			boolean passFailStatus) {
		globalPassFailStatus = passFailStatus;

		if (SCREENSHOT_FLAG_WHENTOTAKEASCREENSHOT.equals("Always")) {
			internalCaptureScreenShot(driver, elementLocator, appendedText, true);
		} else {
			internalCaptureScreenShot(driver, elementLocator, appendedText, false);
		}
	}

	/**
	 * Internal use only. Considers the screenshooterFlag parameter.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @param appendedText
	 *            the text that needs to be appended to the name of the screenshot
	 *            to make it more recognizable
	 * @param takeScreenshot
	 *            determines whether or not to take a screenshot given the
	 *            screenshooterFlag parameter from the pom.xml file
	 */
	private static void internalCaptureScreenShot(WebDriver driver, By elementLocator, String appendedText,
			boolean takeScreenshot) {
		new File(SCREENSHOT_FOLDERPATH).mkdirs();
		if (takeScreenshot) {
			/**
			 * Force screenshot link to be shown in the results as a link not text
			 */
			System.setProperty("org.uncommons.reportng.escape-output", "false");

			/**
			 * Declare regularElementStyle, the WebElemnt, and Javascript Executor to
			 * highlight and unhighlight the WebElement
			 */
			String regularElementStyle = "";
			JavascriptExecutor js = null;
			WebElement element = null;

			/**
			 * If an elementLocator was passed, store regularElementStyle and highlight that
			 * element before taking the screenshot
			 */
			if (elementLocator != null && ElementActions.internalCanFindUniqueElement(driver, elementLocator)) {
				element = driver.findElement(elementLocator);
				js = (JavascriptExecutor) driver;
				regularElementStyle = highlightElementAndReturnDefaultStyle(element, js, setHighlightedElementStyle());
			}

			/**
			 * Take the screenshot and store it as a file
			 */
			File src;

			/**
			 * Attempt to take a full page screenshot, take a regular screenshot upon
			 * failure
			 */
			if (SCREENSHOT_FLAG_TAKEFULLPAGESCREENSHOT) {
				try {
					src = ScreenshotUtils.makeFullScreenshot(driver);
				} catch (Exception e) {
					src = ((TakesScreenshot) driver).getScreenshotAs(OutputType.FILE);
					ReportManager.log(e);
				}
			} else {
				src = ((TakesScreenshot) driver).getScreenshotAs(OutputType.FILE);
			}

			/**
			 * Declare screenshot file name
			 */
			String testCaseName = Reporter.getCurrentTestResult().getTestClass().getRealClass().getName();
			screenshotFileName = System.currentTimeMillis() + "_" + testCaseName;
			if (!appendedText.equals("")) {
				screenshotFileName = screenshotFileName + "_" + appendedText;
			}

			/**
			 * If an elementLocator was passed, unhighlight that element after taking the
			 * screenshot
			 * 
			 */
			if (js != null) {
				js.executeScript("arguments[0].setAttribute('style', arguments[1]);", element, regularElementStyle);
			}

			/**
			 * Copy the screenshot to desired path, and append the appropriate filename.
			 * 
			 */
			FileManager.copyFile(src.getAbsolutePath(), SCREENSHOT_FOLDERPATH + SCREENSHOT_FOLDERNAME
					+ FileSystems.getDefault().getSeparator() + screenshotFileName + ".png");

			/*
			 * File screenshotFile = new File(SCREENSHOT_FOLDERPATH + SCREENSHOT_FOLDERNAME
			 * + FileSystems.getDefault().getSeparator() + screenshotFileName + ".png"); try
			 * { FileUtils.copyFile(src, screenshotFile); } catch (IOException e) {
			 * ReportManager.log(e); }
			 */

			addScreenshotToReport(src);
		}
	}

	private static void addScreenshotToReport(File screenshotFile) {
		/**
		 * Adding Screenshot to the Report.
		 * 
		 */
		try {
			ReportManager.attach("Screenshot", screenshotFileName, new FileInputStream(screenshotFile));

		} catch (FileNotFoundException e) {
			ReportManager.log(e);
		}
	}

	private static String highlightElementAndReturnDefaultStyle(WebElement element, JavascriptExecutor js,
			String highlightedElementStyle) {
		String regularElementStyle = element.getAttribute("style");
		if (regularElementStyle != null && !regularElementStyle.equals("")) {
			js.executeScript("arguments[0].style.cssText = arguments[1];", element,
					regularElementStyle + highlightedElementStyle);
		} else {
			js.executeScript("arguments[0].setAttribute('style', arguments[1]);", element, highlightedElementStyle);
		}

		try {
			JSWaiter.waitForLazyLoading();
		} catch (Exception e) {
			ReportManager.log(e);
		}
		return regularElementStyle;
	}

	private static String setHighlightedElementStyle() {
		String highlightedElementStyle = "";
		if (globalPassFailStatus) {
			highlightedElementStyle = "outline-offset:-3px !important; outline:3px solid #808080 !important; background:#46aad2 !important; background-color:#A5D2A5 !important; color:#000000 !important; -webkit-transition: none !important; -moz-transition: none !important; -o-transition: none !important; transition: none !important;";
			// [incorta-blue: #46aad2] background-color:#A5D2A5
		} else {
			highlightedElementStyle = "outline-offset:-3px !important; outline:3px solid #808080 !important; background:#FFFF99 !important; background-color:#FFFF99 !important; color:#000000 !important; -webkit-transition: none !important; -moz-transition: none !important; -o-transition: none !important; transition: none !important;";
			// background-color:#ffff66
		}
		return highlightedElementStyle;
	}
}
