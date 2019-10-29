package com.shaft.gui.image;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.file.FileSystems;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import javax.imageio.ImageIO;
import javax.imageio.stream.FileImageOutputStream;
import javax.imageio.stream.ImageOutputStream;

import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchSessionException;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.testng.Reporter;

import com.shaft.cli.FileActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.JSWaiter;
import com.shaft.tools.io.ReportManager;

public class ScreenshotManager {
    private static final String SCREENSHOT_FOLDERPATH = System.getProperty("allureResultsFolderPath").trim()
	    + "/screenshots/";
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
    private static final Boolean SCREENSHOT_PARAMS_WATERMARK = Boolean
	    .valueOf(System.getProperty("screenshotParams_watermark").trim());
    private static final Float SCREENSHOT_PARAMS_WATERMARKOPACITY = Float
	    .valueOf(System.getProperty("screenshotParams_watermarkOpacity").trim());
    private static final Boolean AI_SUPPORTED_ELEMENT_IDENTIFICATION = Boolean
	    .valueOf(System.getProperty("aiPoweredElementIdentification").trim());

    public static Boolean getAiSupportedElementIdentification() {
	return AI_SUPPORTED_ELEMENT_IDENTIFICATION;
    }

    private static By aiGeneratedElementLocator = null;

    public static void setAiGeneratedElementLocator(By aiGeneratedElementLocator) {
	ScreenshotManager.aiGeneratedElementLocator = aiGeneratedElementLocator;
    }

    private static final String WATERMARK_DEFAULT_PATH = "/images/shaft.png";
    private static By targetElementLocator;

    private static final int RETRIESBEFORETHROWINGELEMENTNOTFOUNDEXCEPTION = 1;

    private static final Boolean CREATE_GIF = Boolean.valueOf(System.getProperty("createAnimatedGif").trim());
    private static final int GIF_FRAME_DELAY = Integer.parseInt(System.getProperty("animatedGif_frameDelay").trim());
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
    private static String globalPassFailAppendedText = "";

    private static WebDriver gifDriver = null;
    private static String testCaseName = "";
    private static String gifFilePath = "";
    private static ImageOutputStream gifOutputStream = null;
    private static GifSequenceWriter gifWriter = null;

    private static final String AI_AIDED_ELEMENT_IDENTIFICATION_FOLDERPATH = "src/test/resources/DynamicObjectRepository/";

    public static String getAiAidedElementIdentificationFolderpath() {
	return AI_AIDED_ELEMENT_IDENTIFICATION_FOLDERPATH;
    }

    private ScreenshotManager() {
	throw new IllegalStateException("Utility class");
    }

    /**
     * Used if there is no element locator. passFailStatus; true means pass and
     * false means fail.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param actionName     the name of the triggering action
     * @param passFailStatus A flag to determine whether the action has passed or
     *                       failed
     */
    public static List<Object> captureScreenShot(WebDriver driver, String actionName, boolean passFailStatus) {
	globalPassFailStatus = passFailStatus;
	if (passFailStatus) {
	    globalPassFailAppendedText = "passed";
	} else {
	    globalPassFailAppendedText = "failed";
	}

	return internalCaptureScreenShot(driver, null, actionName, globalPassFailAppendedText,
		(SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("Always"))
			|| (SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("ValidationPointsOnly")
				&& (actionName.toLowerCase().contains("assert")
					|| actionName.toLowerCase().contains("verify")))
			|| (SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("FailuresOnly") && (!passFailStatus))
			|| !passFailStatus);

	// Note: Excluded the "Always" case as there will already be another screenshot
	// taken by the browser/element action // reversed this option to be able to
	// take a failure screenshot
    }

    /**
     * Used if there is an element locator. passFailStatus; true means pass and
     * false means fail.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param actionName     the name of the triggering action
     * @param passFailStatus A flag to determine whether the action has passed or
     *                       failed
     */
    public static List<Object> captureScreenShot(WebDriver driver, By elementLocator, String actionName,
	    boolean passFailStatus) {

	// Override current locator with the aiGeneratedElementLocator
	if (AI_SUPPORTED_ELEMENT_IDENTIFICATION && aiGeneratedElementLocator != null && elementLocator != null) {
	    elementLocator = aiGeneratedElementLocator;
	}

	globalPassFailStatus = passFailStatus;
	targetElementLocator = elementLocator;

	if (passFailStatus) {
	    globalPassFailAppendedText = "passed";
	} else {
	    globalPassFailAppendedText = "failed";
	}

	return internalCaptureScreenShot(driver, elementLocator, actionName, globalPassFailAppendedText,
		(SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("Always"))
			|| (SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("ValidationPointsOnly")
				&& (actionName.toLowerCase().contains("assert")
					|| actionName.toLowerCase().contains("verify")))
			|| (SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("FailuresOnly") && (!passFailStatus))
			|| !passFailStatus);
	// Note: Excluded the "Always" case as there will already be another screenshot
	// taken by the browser/element action // reversed this option to be able to
	// take a failure screenshot
    }

    /**
     * Internal use only. Considers the screenshotParams_whenToTakeAScreenshot
     * parameter.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param appendedText   the text that needs to be appended to the name of the
     *                       screenshot to make it more recognizable
     * @param takeScreenshot determines whether or not to take a screenshot given
     *                       the screenshotParams_whenToTakeAScreenshot parameter
     *                       from the pom.xml file
     * @return
     */
    private static List<Object> internalCaptureScreenShot(WebDriver driver, By elementLocator, String actionName,
	    String appendedText, boolean takeScreenshot) {

	// Override current locator with the aiGeneratedElementLocator
	if (AI_SUPPORTED_ELEMENT_IDENTIFICATION && aiGeneratedElementLocator != null && elementLocator != null) {
	    elementLocator = aiGeneratedElementLocator;
	}

	FileActions.createFolder(SCREENSHOT_FOLDERPATH);

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

	    try {
		/**
		 * If an elementLocator was passed, store regularElementStyle and highlight that
		 * element before taking the screenshot
		 */
		if (SCREENSHOT_PARAMS_HIGHLIGHTELEMENTS && elementLocator != null
			&& (ElementActions.getElementsCount(driver, elementLocator,
				RETRIESBEFORETHROWINGELEMENTNOTFOUNDEXCEPTION) == 1)) {
		    element = driver.findElement(elementLocator);
		    js = (JavascriptExecutor) driver;
		    regularElementStyle = highlightElementAndReturnDefaultStyle(element, js,
			    setHighlightedElementStyle());
		}
	    } catch (StaleElementReferenceException e) {
		// this happens when WebDriver fails to capture the elements initial style or
		// fails to highlight the element for some reason
		ReportManager.log(e);
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
		src = takeScreenshot(driver);

		/**
		 * Declare screenshot file name
		 */
		testCaseName = Reporter.getCurrentTestResult().getMethod().getMethodName();
		screenshotFileName = System.currentTimeMillis() + "_" + testCaseName + "_" + actionName;
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
		FileActions.copyFile(src.getAbsolutePath(), SCREENSHOT_FOLDERPATH + SCREENSHOT_FOLDERNAME
			+ FileSystems.getDefault().getSeparator() + screenshotFileName + ".png");

		appendToAnimatedGif(src);
		return addScreenshotToReport(src);
	    } catch (WebDriverException e) {
		// this happens when a browser session crashes mid-execution, or the docker is
		// unregistered
		ReportManager.log(e);
	    }
	} else {
	    appendToAnimatedGif();
	}
	return null;
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

    public static File takeFullPageScreenshot(WebDriver driver) {
	try {
	    if (SCREENSHOT_PARAMS_SKIPPEDELEMENTSFROMSCREENSHOT.length() > 0) {
		List<WebElement> skippedElementsList = new ArrayList<>();
		String[] skippedElementLocators = SCREENSHOT_PARAMS_SKIPPEDELEMENTSFROMSCREENSHOT.split(";");
		for (String locator : skippedElementLocators) {
		    if (ElementActions.getElementsCount(driver, By.xpath(locator),
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
		    RETRIESBEFORETHROWINGELEMENTNOTFOUNDEXCEPTION) == 1) {
		return driver.findElement(targetElementLocator).getScreenshotAs(OutputType.FILE);
	    } else {
		return ((TakesScreenshot) driver).getScreenshotAs(OutputType.FILE);
	    }
	} catch (Exception e) {
	    ReportManager.log(e);
	    return ((TakesScreenshot) driver).getScreenshotAs(OutputType.FILE);
	}
    }

    private static List<Object> addScreenshotToReport(File screenshotFile) {
	/**
	 * Adding Screenshot to the Report.
	 * 
	 */
	try {
	    // add SHAFT_Engine logo overlay
	    BufferedImage screenshotImage = ImageIO.read(screenshotFile);
	    screenshotImage = overlayShaftEngineLogo(screenshotImage);
	    ByteArrayOutputStream screenshotOutputStream = new ByteArrayOutputStream();
	    ImageIO.write(screenshotImage, "png", screenshotOutputStream);
	    return Arrays.asList("Screenshot", screenshotFileName,
		    new ByteArrayInputStream(screenshotOutputStream.toByteArray()));
	} catch (IOException e) {
	    ReportManager.log(e);
	    return null;
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
	if (CREATE_GIF) {
	    gifDriver = driver;
	    try {
		testCaseName = Reporter.getCurrentTestResult().getMethod().getMethodName();
		gifFilePath = SCREENSHOT_FOLDERPATH + SCREENSHOT_FOLDERNAME + FileSystems.getDefault().getSeparator()
			+ System.currentTimeMillis() + "_" + testCaseName + ".gif";
		File src = ((TakesScreenshot) gifDriver).getScreenshotAs(OutputType.FILE); // takes first screenshot
		FileActions.copyFile(src.getAbsolutePath(), gifFilePath);

		// grab the output image type from the first image in the sequence
		BufferedImage firstImage = ImageIO.read(new File(gifFilePath));

		// create a new BufferedOutputStream
		gifOutputStream = new FileImageOutputStream(new File(gifFilePath));

		// create a gif sequence with the type of the first image, 500 milliseconds
		// between frames, which loops infinitely
		gifWriter = new GifSequenceWriter(gifOutputStream, firstImage.getType(), GIF_FRAME_DELAY, true);

		// draw initial blank image to set the size of the GIF...
		BufferedImage initialImage = new BufferedImage(firstImage.getWidth(), firstImage.getHeight(),
			firstImage.getType());
		Graphics2D initialImageGraphics = initialImage.createGraphics();
		initialImageGraphics.setBackground(Color.WHITE);
		initialImageGraphics.clearRect(0, 0, firstImage.getWidth(), firstImage.getHeight());

		// write out initialImage to the sequence...
		gifWriter.writeToSequence(overlayShaftEngineLogo(initialImage));
		initialImageGraphics.dispose();
		// write out first image to the sequence...
		gifWriter.writeToSequence(overlayShaftEngineLogo(firstImage));
	    } catch (IOException | WebDriverException e) {
		ReportManager.log(e);
	    } catch (NullPointerException e) {
		// this happens in case the start animated Gif is triggered in a none-test
		// method
	    }
	}
    }

    private static BufferedImage overlayShaftEngineLogo(BufferedImage screenshot) {
	if (SCREENSHOT_PARAMS_WATERMARK) {
	    try {
		// create graphics object
		Graphics2D screenshotGraphics = screenshot.createGraphics();
		screenshotGraphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

		screenshotGraphics.drawImage(screenshot, 0, 0, null);
		screenshotGraphics.setComposite(
			AlphaComposite.getInstance(AlphaComposite.SRC_OVER, SCREENSHOT_PARAMS_WATERMARKOPACITY));

		BufferedImage shaftLogo;
		if (System.getProperty("watermarkImagePath").trim().equals(WATERMARK_DEFAULT_PATH)) {
		    // read from tool resources
		    URL resourcesImageURL = ScreenshotManager.class
			    .getResource(System.getProperty("watermarkImagePath").trim());
		    // overlay SHAFT_Engine logo to the initial image...
		    shaftLogo = ImageIO.read(resourcesImageURL);
		} else {
		    // read from custom location
		    shaftLogo = ImageIO.read(new File(System.getProperty("watermarkImagePath").trim()));
		}
		shaftLogo = toBufferedImage(
			shaftLogo.getScaledInstance(screenshot.getWidth() / 8, -1, Image.SCALE_SMOOTH));
		screenshotGraphics.drawImage(shaftLogo, screenshot.getWidth() - shaftLogo.getWidth(),
			screenshot.getHeight() - shaftLogo.getHeight(), null);
		screenshotGraphics.dispose();
	    } catch (IOException e) {
		// do nothing and proceed to return the original screenshot
	    }
	}
	return screenshot;
    }

    private static BufferedImage toBufferedImage(Image img) {
	if (img instanceof BufferedImage) {
	    return (BufferedImage) img;
	}

	// Create a buffered image with transparency
	BufferedImage bimage = new BufferedImage(img.getWidth(null), img.getHeight(null), BufferedImage.TYPE_INT_ARGB);

	// Draw the image on to the buffered image
	Graphics2D bGr = bimage.createGraphics();
	bGr.drawImage(img, 0, 0, null);
	bGr.dispose();

	// Return the buffered image
	return bimage;
    }

    private static void appendToAnimatedGif(File... screenshot) {
	// ensure that animatedGif is started, else force start it
	if (CREATE_GIF) {
	    if (gifDriver == null || gifWriter == null) {
		BrowserFactory.startAnimatedGif();
	    } else {
		try {
		    BufferedImage image;
		    if (screenshot.length == 1) {
			image = ImageIO.read(screenshot[0]);
		    } else {
			image = ImageIO.read(((TakesScreenshot) gifDriver).getScreenshotAs(OutputType.FILE));
		    }
		    gifWriter.writeToSequence(overlayShaftEngineLogo(image));

		} catch (NoSuchSessionException e) {
		    // this happens when attempting to append to a non existing gif, expected
		    // solution is to recreate the gif
		    BrowserFactory.startAnimatedGif();
		} catch (WebDriverException e) {
		    if (e.getMessage().contains("was terminated due to BROWSER_TIMEOUT")) {
			// this happens when attempting to append to a gif from an already terminated
			// browser session
			BrowserFactory.startAnimatedGif();
		    } else {
			ReportManager.log(e);
		    }
		} catch (IOException | IllegalStateException | IllegalArgumentException | NullPointerException e) {
		    ReportManager.log(e);
		}
	    }
	}
    }

    public static void attachAnimatedGif() {
	// stop and attach
	if (CREATE_GIF && gifDriver != null && !gifFilePath.equals("")) {
	    try {
		appendToAnimatedGif();
	    } catch (Exception e) {
		ReportManager.log(e);
	    }
	    try {
		gifWriter.close();
		gifOutputStream.close();

		gifOutputStream = null;
		gifWriter = null;
		gifDriver = null;
		ReportManager.attach("Animated Gif", testCaseName, new FileInputStream(gifFilePath));
		gifFilePath = "";
	    } catch (IOException | NullPointerException | IllegalStateException e) {
		ReportManager.log(e);
	    }
	}
    }

    public static void storeElementScreenshotForAISupportedElementIdentification(WebDriver driver, By elementLocator) {
	// Override current locator with the aiGeneratedElementLocator
	if (AI_SUPPORTED_ELEMENT_IDENTIFICATION && aiGeneratedElementLocator != null && elementLocator != null) {
	    elementLocator = aiGeneratedElementLocator;
	}

	if (AI_SUPPORTED_ELEMENT_IDENTIFICATION) {
	    FileActions.createFolder(AI_AIDED_ELEMENT_IDENTIFICATION_FOLDERPATH);
	    WebElement targetElement = driver.findElement(elementLocator);
	    File screenshotFile = null;
	    try {
		screenshotFile = targetElement.getScreenshotAs(OutputType.FILE);
	    } catch (JavascriptException e) {
		// do nothing
	    }
	    if (screenshotFile != null) {
		String elementFileName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);
		if (!targetElement.getTagName().equalsIgnoreCase("input")) {
		    FileActions.copyFile(screenshotFile.getAbsolutePath(),
			    AI_AIDED_ELEMENT_IDENTIFICATION_FOLDERPATH + elementFileName + ".png");
		} else {
		    if (!FileActions.doesFileExist(AI_AIDED_ELEMENT_IDENTIFICATION_FOLDERPATH, elementFileName + ".png",
			    2)) {
			FileActions.copyFile(screenshotFile.getAbsolutePath(),
				AI_AIDED_ELEMENT_IDENTIFICATION_FOLDERPATH + elementFileName + ".png");
		    }
		}
	    }
	}
    }
}
