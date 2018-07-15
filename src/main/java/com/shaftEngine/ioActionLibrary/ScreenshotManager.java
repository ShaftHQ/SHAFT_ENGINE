package com.shaftEngine.ioActionLibrary;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.commons.io.FileUtils;
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
	private static final String screenshotFolderPath = "allure-results/screenshots/";
	private static final String screenshotFolderName = new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date());
	private static String screenshotFileName = "Screenshot";
	private static final String screenshooterFlag = System.getProperty("screenshooterFlag");
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

		if ((screenshooterFlag.equals("Always")) || (screenshooterFlag.equals("ValidationPointsOnly"))
				|| ((screenshooterFlag.equals("FailuresOnly") && (!passFailStatus)))) {
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

		if ((screenshooterFlag.equals("Always")) || (screenshooterFlag.equals("ValidationPointsOnly"))
				|| ((screenshooterFlag.equals("FailuresOnly") && (!passFailStatus)))) {
			internalCaptureScreenShot(driver, elementLocator, appendedText, true);
		} else {
			internalCaptureScreenShot(driver, elementLocator, appendedText, false);
		}
		// Note: Excluded the "Always" case as there will already be another screenshot
		// taken by the browser/element action // reversed this option to be able to
		// take a failure screenshot
	}

	/**
	 * Used in all browser actions, in failed element actions, and in passed element
	 * actions where the element can no longer be found. passFailStatus; true means
	 * pass and false means fail.
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

		if ((screenshooterFlag.equals("Always"))
				|| (screenshooterFlag.equals("ValidationPointsOnly") && (!passFailStatus))
				|| ((screenshooterFlag.equals("FailuresOnly")) && (!passFailStatus))) {
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

		if (screenshooterFlag.equals("Always")) {
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
		new File(screenshotFolderPath).mkdirs();
		if (takeScreenshot) {
			/**
			 * Force screenshot link to be shown in the results as a link not text
			 */
			System.setProperty("org.uncommons.reportng.escape-output", "false");

			/**
			 * Declare styles and Javascript Executor to highlight and unhighlight elements
			 */

			String highlightedElementStyle;
			if (globalPassFailStatus == true) {
				highlightedElementStyle = "outline-offset:-3px !important; outline:3px solid #808080 !important; background:#46aad2 !important; background-color:#A5D2A5 !important; color:#000000 !important; -webkit-transition: none !important; -moz-transition: none !important; -o-transition: none !important; transition: none !important;";
				// [incorta-blue: #46aad2] background-color:#A5D2A5
			} else {
				highlightedElementStyle = "outline-offset:-3px !important; outline:3px solid #808080 !important; background:#FFFF99 !important; background-color:#FFFF99 !important; color:#000000 !important; -webkit-transition: none !important; -moz-transition: none !important; -o-transition: none !important; transition: none !important;";
				// background-color:#ffff66
			}

			String regularElementStyle = "";
			JavascriptExecutor js = null;
			WebElement element = null;

			/**
			 * If an elementLocator was passed, store current element style and highlight
			 * that element before taking the screenshot
			 */
			if (elementLocator != null && ElementActions.internalCanFindUniqueElement(driver, elementLocator)) {
				element = driver.findElement(elementLocator);
				js = (JavascriptExecutor) driver;
				regularElementStyle = element.getAttribute("style");
				if (regularElementStyle != null && !regularElementStyle.equals("")) {
					js.executeScript("arguments[0].style.cssText = arguments[1];", element,
							regularElementStyle + highlightedElementStyle);
				} else {
					js.executeScript("arguments[0].setAttribute('style', arguments[1]);", element,
							highlightedElementStyle);
				}
			}

			try {
				JSWaiter.waitForLazyLoading();
			} catch (Exception e) {
				e.printStackTrace();
				ReportManager.log(e.getMessage());
			}

			/**
			 * Take the screenshot and store it as a file
			 */
			File src;

			/**
			 * Attempt to take a full page screenshot, take a regular screenshot upon
			 * failure
			 */
			try {
				src = ScreenshotUtils.makeFullScreenshot(driver);
			} catch (IOException e2) {
				src = ((TakesScreenshot) driver).getScreenshotAs(OutputType.FILE);
				// TODO Auto-generated catch block
				e2.printStackTrace();
			} catch (InterruptedException e2) {
				src = ((TakesScreenshot) driver).getScreenshotAs(OutputType.FILE);
				// TODO Auto-generated catch block
				e2.printStackTrace();
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
			File screenshotFile = new File(
					screenshotFolderPath + screenshotFolderName + "/" + screenshotFileName + ".png");
			try {
				FileUtils.copyFile(src, screenshotFile);
			} catch (IOException e) {
				// e.printStackTrace();
				ReportManager.log(e.getMessage());
			}

			/**
			 * Adding Screenshot to the Report.
			 * 
			 */
			try {
				ReportManager.attach("Screenshot", screenshotFileName, new FileInputStream(src));

			} catch (FileNotFoundException e1) {
				// e1.printStackTrace();
				ReportManager.log(e1.getMessage());
			}
		}
	}
}
