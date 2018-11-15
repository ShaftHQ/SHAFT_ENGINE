package com.shaft.io;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.imageio.ImageIO;
import javax.imageio.stream.FileImageOutputStream;
import javax.imageio.stream.ImageOutputStream;

import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Reporter;

import com.shaft.browser.BrowserFactory;
import com.shaft.element.ElementActions;
import com.shaft.element.JSWaiter;

public class ScreenshotManager {
    private static final String SCREENSHOT_FOLDERPATH = "allure-results/screenshots/";
    private static final String SCREENSHOT_FOLDERNAME = new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date());
    private static String screenshotFileName = "Screenshot";
    private static final String SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT = System
	    .getProperty("screenshotParams_whenToTakeAScreenshot");
    private static final Boolean SCREENSHOT_PARAMS_HIGHLIGHTELEMENTS = Boolean
	    .valueOf(System.getProperty("screenshotParams_highlightElements"));
    private static final String SCREENSHOT_PARAMS_SCREENSHOTTYPE = System
	    .getProperty("screenshotParams_screenshotType");
    private static final String SCREENSHOT_PARAMS_SKIPPEDELEMENTSFROMSCREENSHOT = System
	    .getProperty("screenshotParams_skippedElementsFromScreenshot");
    private static By targetElementLocator;

    private static final int CUSTOMELEMENTIDENTIFICATIONTIMEOUT = 1;
    private static final int RETRIESBEFORETHROWINGELEMENTNOTFOUNDEXCEPTION = 1;

    private static final Boolean CREATE_GIF = Boolean.valueOf(System.getProperty("createAnimatedGif").trim());
    private static final int GIF_FRAME_DELAY = Integer.valueOf(System.getProperty("animatedGif_frameDelay").trim());
    // default is 500

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

    private static WebDriver gifDriver = null;
    // private static ImageWriter gifWriter = null;
    private static String gifFileName = "";
    private static ImageOutputStream gifOutputStream = null;
    private static GifSequenceWriter gifWriter = null;
    private static String lastGifName = "FirstGif";

    private ScreenshotManager() {
	throw new IllegalStateException("Utility class");
    }

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

	internalCaptureScreenShot(driver, null, appendedText,
		(SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("Always"))
			|| (SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("ValidationPointsOnly"))
			|| (SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("FailuresOnly") && (!passFailStatus)));

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
	targetElementLocator = elementLocator;

	if (passFailStatus) {
	    appendedText = "passed";
	} else {
	    appendedText = "failed";
	}

	internalCaptureScreenShot(driver, elementLocator, appendedText,
		(SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("Always"))
			|| (SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("ValidationPointsOnly"))
			|| (SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("FailuresOnly") && (!passFailStatus)));
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

	internalCaptureScreenShot(driver, null, appendedText,
		(SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("Always"))
			|| (SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("ValidationPointsOnly") && (!passFailStatus))
			|| ((SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("FailuresOnly")) && (!passFailStatus)));
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
	targetElementLocator = elementLocator;

	internalCaptureScreenShot(driver, elementLocator, appendedText,
		SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("Always"));
    }

    /**
     * Internal use only. Considers the screenshotParams_whenToTakeAScreenshot
     * parameter.
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
     *            screenshotParams_whenToTakeAScreenshot parameter from the pom.xml
     *            file
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
	    if (SCREENSHOT_PARAMS_HIGHLIGHTELEMENTS && elementLocator != null
		    && (ElementActions.getElementsCount(driver, elementLocator, CUSTOMELEMENTIDENTIFICATIONTIMEOUT,
			    RETRIESBEFORETHROWINGELEMENTNOTFOUNDEXCEPTION) == 1)) {
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
	    src = takeScreenshot(driver);

	    /**
	     * Declare screenshot file name
	     */
	    // String testCaseName =
	    // Reporter.getCurrentTestResult().getTestClass().getRealClass().getName();
	    String testCaseName = Reporter.getCurrentTestResult().getMethod().getMethodName();
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
	    appendToAnimatedGif(src);
	} else {
	    appendToAnimatedGif();
	}
    }

    private static File takeScreenshot(WebDriver driver) {
	switch (SCREENSHOT_PARAMS_SCREENSHOTTYPE.toLowerCase().trim()) {
	case "regular":
	    return ((TakesScreenshot) driver).getScreenshotAs(OutputType.FILE);
	case "fullpage":
	    return takeFullPageScreenshot(driver);
	case "element":
	    return takeElementScreenshot(driver);
	default:
	    return ((TakesScreenshot) driver).getScreenshotAs(OutputType.FILE);
	}
    }

    private static File takeFullPageScreenshot(WebDriver driver) {
	try {
	    if (SCREENSHOT_PARAMS_SKIPPEDELEMENTSFROMSCREENSHOT.length() > 0) {
		List<WebElement> skippedElementsList = new ArrayList<>();
		String[] skippedElementLocators = SCREENSHOT_PARAMS_SKIPPEDELEMENTSFROMSCREENSHOT.split(";");
		for (String locator : skippedElementLocators) {
		    if (ElementActions.getElementsCount(driver, By.xpath(locator), CUSTOMELEMENTIDENTIFICATIONTIMEOUT,
			    RETRIESBEFORETHROWINGELEMENTNOTFOUNDEXCEPTION) == 1) {
			skippedElementsList.add(driver.findElement(By.xpath(locator)));
		    }
		}

		WebElement[] skippedElementsArray = new WebElement[skippedElementsList.size()];
		skippedElementsArray = skippedElementsList.toArray(skippedElementsArray);

		return ScreenshotUtils.makeFullScreenshot(driver, skippedElementsArray);
	    } else {
		return ScreenshotUtils.makeFullScreenshot(driver);
	    }
	} catch (Exception e) {
	    ReportManager.log(e);
	    return ((TakesScreenshot) driver).getScreenshotAs(OutputType.FILE);
	}
    }

    private static File takeElementScreenshot(WebDriver driver) {
	try {
	    if (targetElementLocator != null && ElementActions.getElementsCount(driver, targetElementLocator,
		    CUSTOMELEMENTIDENTIFICATIONTIMEOUT, RETRIESBEFORETHROWINGELEMENTNOTFOUNDEXCEPTION) == 1) {
		return ScreenshotUtils.makeElementScreenshot(driver, targetElementLocator);
	    } else {
		return ((TakesScreenshot) driver).getScreenshotAs(OutputType.FILE);
	    }
	} catch (Exception e) {
	    ReportManager.log(e);
	    return ((TakesScreenshot) driver).getScreenshotAs(OutputType.FILE);
	}
    }

    private static void addScreenshotToReport(File screenshotFile) {
	/**
	 * Adding Screenshot to the Report.
	 * 
	 */
	try {
	    ReportManager.attachAsStep("Screenshot", screenshotFileName, new FileInputStream(screenshotFile));

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

    public static void startAnimatedGif(WebDriver driver) {
	if (CREATE_GIF && gifDriver == null && !lastGifName.equals(screenshotFileName)) {
	    gifDriver = driver;

	    String testCaseName = Reporter.getCurrentTestResult().getMethod().getMethodName();
	    gifFileName = SCREENSHOT_FOLDERPATH + SCREENSHOT_FOLDERNAME + FileSystems.getDefault().getSeparator()
		    + System.currentTimeMillis() + "_" + testCaseName + ".gif";

	    File src = ((TakesScreenshot) gifDriver).getScreenshotAs(OutputType.FILE); // takes first screenshot
	    FileManager.copyFile(src.getAbsolutePath(), gifFileName);
	    try {
		// grab the output image type from the first image in the sequence
		BufferedImage firstImage = ImageIO.read(new File(gifFileName));

		// create a new BufferedOutputStream
		gifOutputStream = new FileImageOutputStream(new File(gifFileName));

		// create a gif sequence with the type of the first image, 500 milliseconds
		// between frames, which loops infinitely
		gifWriter = new GifSequenceWriter(gifOutputStream, firstImage.getType(), GIF_FRAME_DELAY, true);

		// write out a blank image to the sequence...
		BufferedImage blankImage = new BufferedImage(firstImage.getWidth(), firstImage.getHeight(),
			firstImage.getType());
		Graphics2D blankImageGraphics = blankImage.createGraphics();
		blankImageGraphics.setBackground(Color.WHITE);
		blankImageGraphics.clearRect(0, 0, firstImage.getWidth(), firstImage.getHeight());
		gifWriter.writeToSequence(blankImage);

		// write out the first image to the sequence...
		gifWriter.writeToSequence(firstImage);
	    } catch (IOException e) {
		ReportManager.log(e);
	    }
	}
    }

    private static void appendToAnimatedGif(File... screenshot) {
	// ensure that animatedGif is started, else force start it
	if (CREATE_GIF && gifDriver == null && !lastGifName.equals(screenshotFileName)) {
	    BrowserFactory.startAnimatedGif();
	}

	// screenshot to be appended to animated gif
	if (CREATE_GIF && gifDriver != null) {
	    try {
		BufferedImage image;
		if (screenshot.length == 1) {
		    image = ImageIO.read(screenshot[0]);
		} else {
		    image = ImageIO.read(((TakesScreenshot) gifDriver).getScreenshotAs(OutputType.FILE));
		}
		gifWriter.writeToSequence(image);
	    } catch (IOException | IllegalStateException | NullPointerException e) {
		ReportManager.log(e);
	    }
	}
    }

    public static void attachAnimatedGif() {
	// stop and attach
	if (CREATE_GIF && gifDriver != null && gifOutputStream != null && gifWriter != null) {
	    try {
		gifWriter.close();
		gifOutputStream.close();

		gifOutputStream = null;
		gifWriter = null;
		gifDriver = null;
		ReportManager.attach("Animated Gif", screenshotFileName, new FileInputStream(gifFileName));
		lastGifName = screenshotFileName;
	    } catch (IOException | NullPointerException | IllegalStateException e) {
		ReportManager.log(e);
	    }
	}
    }
}
