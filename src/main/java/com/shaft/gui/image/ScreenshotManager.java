package com.shaft.gui.image;

import com.microsoft.playwright.ElementHandle;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Page.ScreenshotOptions;
import com.shaft.cli.FileActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.JavaScriptWaitManager;
import com.shaft.gui.element.PlayWrightElementActions;
import com.shaft.tools.io.PropertyFileManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import org.imgscalr.Scalr;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.*;
import org.sikuli.script.App;
import org.sikuli.script.Pattern;
import org.sikuli.script.Screen;

import javax.imageio.ImageIO;
import javax.imageio.stream.FileImageOutputStream;
import javax.imageio.stream.ImageOutputStream;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;
import java.nio.file.FileSystems;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.*;

public class ScreenshotManager {
    private static final String SCREENSHOT_FOLDERPATH = System.getProperty("allureResultsFolderPath").trim()
            + "/screenshots/";
    private static final String SCREENSHOT_FOLDERNAME = new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date());
    private static final String SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT = System
            .getProperty("screenshotParams_whenToTakeAScreenshot");
    private static final Boolean SCREENSHOT_PARAMS_HIGHLIGHTELEMENTS = Boolean
            .valueOf(System.getProperty("screenshotParams_highlightElements"));
    private static final String SCREENSHOT_PARAMS_SCREENSHOTTYPE = System
            .getProperty("screenshotParams_screenshotType");
    private static final String SCREENSHOT_PARAMS_HIGHLIGHTMETHOD = System
            .getProperty("screenshotParams_highlightMethod");
    private static final String SCREENSHOT_PARAMS_SKIPPEDELEMENTSFROMSCREENSHOT = System
            .getProperty("screenshotParams_skippedElementsFromScreenshot");
    private static final Boolean SCREENSHOT_PARAMS_WATERMARK = Boolean
            .valueOf(System.getProperty("screenshotParams_watermark").trim());
    private static final Float SCREENSHOT_PARAMS_WATERMARKOPACITY = Float
            .valueOf(System.getProperty("screenshotParams_watermarkOpacity").trim());
    private static final int RETRIESBEFORETHROWINGELEMENTNOTFOUNDEXCEPTION = 1;
    private static final Boolean CREATE_GIF = Boolean.valueOf(System.getProperty("createAnimatedGif").trim());
    private static final int GIF_FRAME_DELAY = Integer.parseInt(System.getProperty("animatedGif_frameDelay").trim());
    private static final int GIF_SIZE = 1280;
    // TODO: parameterize the detailed gif value
    private static final Boolean DETAILED_GIF = true;
    private static final String DETAILED_GIF_REGEX = "(verify.*)|(assert.*)|(click.*)|(tap.*)|(key.*)|(navigate.*)";
    private static final String AI_AIDED_ELEMENT_IDENTIFICATION_FOLDERPATH = System.getProperty("dynamicObjectRepositoryPath").trim();
    private static Boolean AI_SUPPORTED_ELEMENT_IDENTIFICATION = Boolean
            .valueOf(System.getProperty("aiPoweredSelfHealingElementIdentification").trim());
    private static String screenshotFileName = "Screenshot";
    private static By aiGeneratedElementLocator = null;
    private static By targetElementLocator;
    private static boolean globalPassFailStatus = false;
    private static String globalPassFailAppendedText = "";
    private static String testCaseName = "";
    private static String gifRelativePathWithFileName = "";
    private static ThreadLocal<ImageOutputStream> gifOutputStream = new ThreadLocal<>();
    private static ThreadLocal<AnimatedGifManager> gifWriter = new ThreadLocal<>();

    private ScreenshotManager() {
        throw new IllegalStateException("Utility class");
    }

    public static Boolean getAiSupportedElementIdentification() {
        return AI_SUPPORTED_ELEMENT_IDENTIFICATION;
    }

    public static void setAiSupportedElementIdentification(Boolean isEnabled) {
        AI_SUPPORTED_ELEMENT_IDENTIFICATION = isEnabled;
    }

    public static void setAiGeneratedElementLocator(By aiGeneratedElementLocator) {
        ScreenshotManager.aiGeneratedElementLocator = aiGeneratedElementLocator;
    }

    public static String getAiAidedElementIdentificationFolderpath() {
        return AI_AIDED_ELEMENT_IDENTIFICATION_FOLDERPATH;
    }

    /**
     * Used if there is no element locator. passFailStatus; true means pass and
     * false means fail.
     *
     * @param driver         the current instance of Selenium webdriver
     * @param actionName     the name of the triggering action
     * @param passFailStatus A flag to determine whether the action has passed or
     *                       failed
     * @return a screenshot object
     */
    public static List<Object> captureScreenShot(WebDriver driver, String actionName, boolean passFailStatus) {
        globalPassFailStatus = passFailStatus;
        if (passFailStatus) {
            globalPassFailAppendedText = "passed";
        } else {
            globalPassFailAppendedText = "failed";
        }

        return internalCaptureScreenShot(driver, null, actionName, globalPassFailAppendedText,
                takeScreenshot(actionName, passFailStatus));
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
     * @return a screenshot object
     */
    public static List<Object> captureScreenShot(WebDriver driver, By elementLocator, String actionName,
                                                 boolean passFailStatus) {
        By internalElementLocator = elementLocator;
        // Override current locator with the aiGeneratedElementLocator
        if (Boolean.TRUE.equals(AI_SUPPORTED_ELEMENT_IDENTIFICATION) && aiGeneratedElementLocator != null
                && internalElementLocator != null) {
            internalElementLocator = aiGeneratedElementLocator;
        }

        globalPassFailStatus = passFailStatus;
        targetElementLocator = internalElementLocator;

        if (passFailStatus) {
            globalPassFailAppendedText = "passed";
        } else {
            globalPassFailAppendedText = "failed";
        }

        return internalCaptureScreenShot(driver, internalElementLocator, actionName, globalPassFailAppendedText,
                takeScreenshot(actionName, passFailStatus));
    }


    public static List<Object> captureScreenShot(Page page, String elementLocator, String actionName, boolean passFailStatus) {
        globalPassFailStatus = passFailStatus;

        if (passFailStatus) {
            globalPassFailAppendedText = "passed";
        } else {
            globalPassFailAppendedText = "failed";
        }

        return internalCaptureScreenShot(page, elementLocator, actionName, globalPassFailAppendedText,
                takeScreenshot(actionName, passFailStatus));
    }

    private static boolean takeScreenshot(String actionName, boolean passFailStatus) {
        return (SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("Always"))
                || (SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("ValidationPointsOnly")
                && (actionName.toLowerCase().contains("assert")
                || actionName.toLowerCase().contains("verify")
                || actionName.toLowerCase().contains("validate")))
                || (SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT.equals("FailuresOnly") && (!passFailStatus))
                || (!passFailStatus);
        // take screen shot if set to always,
        //OR if set to validation points only and actionName contains verify or assert
        //OR if set to failures only and the test failed
    }

    public static synchronized List<Object> captureScreenShotUsingSikuliX(Screen screen, App applicationWindow, Pattern element, String actionName,
                                                                          boolean passFailStatus) {

        globalPassFailStatus = passFailStatus;
        if (passFailStatus) {
            globalPassFailAppendedText = "passed";
        } else {
            globalPassFailAppendedText = "failed";
        }

        boolean takeScreenshot = "Always".equals(SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT)
                || ("ValidationPointsOnly".equals(SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT)
                && (actionName.toLowerCase().contains("assert")
                || actionName.toLowerCase().contains("verify")))
//                || ("FailuresOnly".equals(SCREENSHOT_PARAMS_WHENTOTAKEASCREENSHOT) && (!passFailStatus))
                || !passFailStatus;

        if (takeScreenshot || (CREATE_GIF && (DETAILED_GIF || actionName.matches(DETAILED_GIF_REGEX)))) {
            /*
             * Force screenshot link to be shown in the results as a link not text
             */
            System.setProperty("org.uncommons.reportng.escape-output", "false");

            /*
             * Take the screenshot and store it as a file
             */
            byte[] src = null;
            try {
                ByteArrayOutputStream baos = new ByteArrayOutputStream();
                switch (SCREENSHOT_PARAMS_SCREENSHOTTYPE.toLowerCase().trim()) {
                    case "element":
                        if (element != null) {
                            try {
                                ImageIO.write(screen.capture(screen.wait(element).getRect()).getImage(), "png", baos);
                                src = baos.toByteArray();
                                break;
                            } catch (org.sikuli.script.FindFailed e) {
                                //do nothing and fall into the next type of screenshot
                            }
                        }
                    case "regular":
                        if (applicationWindow != null) {
                            ImageIO.write(screen.capture(applicationWindow.waitForWindow()).getImage(), "png", baos);
                            src = baos.toByteArray();
                            break;
                        }
                    case "fullpage":
                        ImageIO.write(screen.capture().getImage(), "png", baos);
                        src = baos.toByteArray();
                        break;
                    default:
                        break;
                }
            } catch (IOException e) {
                ReportManager.logDiscrete("Failed to create attachment.");
                ReportManagerHelper.log(e);
            }

            /*
             * Declare screenshot file name
             */
            testCaseName = ReportManagerHelper.getTestMethodName();
            screenshotFileName = System.currentTimeMillis() + "_" + testCaseName + "_" + actionName;
            if (!"".equals(globalPassFailAppendedText)) {
                screenshotFileName = screenshotFileName + "_" + globalPassFailAppendedText;
            }

            startOrAppendToAnimatedGif(src);
            if (takeScreenshot) {
                return addScreenshotToReport(src);
            } else {
                return null;
            }
        }
        return null;
    }

    public static byte[] takeFullPageScreenshot(WebDriver driver) {
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

                return ScreenshotHelper.makeFullScreenshot(driver, skippedElementsArray);
            } else {
                return ScreenshotHelper.makeFullScreenshot(driver);
            }
        } catch (Exception e) {
            ReportManagerHelper.log(e);
            return ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
        }
    }
    
    public static byte[] takeFullPageScreenshot(Page page) {
        try {
            return ScreenshotHelper.makeFullScreenshot(page);
        } catch (Exception e) {
            ReportManagerHelper.log(e);
            return page.screenshot();
        }
    }

    public static byte[] takeElementScreenshot(WebDriver driver, By targetElementLocator) {
        return takeElementScreenshot(driver, targetElementLocator, false);
    }
    
    public static byte[] takeElementScreenshot(Page page, String targetElementLocator) {
        return takeElementScreenshot(page, targetElementLocator, false);
    }

    public static synchronized void attachAnimatedGif() {
        // stop and attach
        if (Boolean.TRUE.equals(CREATE_GIF) && !"".equals(gifRelativePathWithFileName)) {
            try {
                ReportManagerHelper.attach("Animated Gif", testCaseName, new FileInputStream(gifRelativePathWithFileName));
                if (!gifWriter.equals(new ThreadLocal<>())) {
                    gifWriter.get().close();
                }
                if (!gifOutputStream.equals(new ThreadLocal<>())) {
                    gifOutputStream.get().close();
                }

                gifOutputStream = new ThreadLocal<>();
                gifWriter = new ThreadLocal<>();
                gifRelativePathWithFileName = "";
            } catch (FileNotFoundException e) {
                // this happens when the gif fails to start, maybe the browser window was
                // already closed
            } catch (IOException | NullPointerException | IllegalStateException e) {
                ReportManagerHelper.log(e);
            }
        }
    }

    public static void storeElementScreenshotForAISupportedElementIdentification(WebDriver driver, By
            elementLocator) {
        By internalElementLocator = elementLocator;
        // Override current locator with the aiGeneratedElementLocator
        if (Boolean.TRUE.equals(AI_SUPPORTED_ELEMENT_IDENTIFICATION) && aiGeneratedElementLocator != null
                && internalElementLocator != null) {
            internalElementLocator = aiGeneratedElementLocator;
        }

        if (Boolean.TRUE.equals(AI_SUPPORTED_ELEMENT_IDENTIFICATION)) {
            FileActions.createFolder(AI_AIDED_ELEMENT_IDENTIFICATION_FOLDERPATH);
            WebElement targetElement = driver.findElement(internalElementLocator);
            File screenshotFile = null;
            try {
                screenshotFile = targetElement.getScreenshotAs(OutputType.FILE);
            } catch (JavascriptException e) {
                // do nothing
            }
            if (screenshotFile != null) {
                String elementFileName = ImageProcessingActions.formatElementLocatorToImagePath(Objects.requireNonNull(internalElementLocator));
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
     * @return screenshot list object
     */
    private static synchronized List<Object> internalCaptureScreenShot(WebDriver driver, By elementLocator,
                                                                       String actionName, String appendedText, boolean takeScreenshot) {
        By internalElementLocator = elementLocator;
        // Override current locator with the aiGeneratedElementLocator
        if (Boolean.TRUE.equals(AI_SUPPORTED_ELEMENT_IDENTIFICATION) && aiGeneratedElementLocator != null
                && internalElementLocator != null) {
            internalElementLocator = aiGeneratedElementLocator;
        }

        // Suggested: add to animated gif only in case of click, navigation, or validation actions.
        if (takeScreenshot || (CREATE_GIF && (DETAILED_GIF || actionName.matches(DETAILED_GIF_REGEX)))) {
            /*
             * Force screenshot link to be shown in the results as a link not text
             */
            System.setProperty("org.uncommons.reportng.escape-output", "false");

            /*
             * Declare regularElementStyle, the WebElemnt, and Javascript Executor to
             * highlight and unhighlight the WebElement
             */
            String regularElementStyle = "";
            JavascriptExecutor js = null;
            WebElement element = null;
            Rectangle elementLocation = null;

            try {
                /*
                 * If an elementLocator was passed, store regularElementStyle and highlight that
                 * element before taking the screenshot
                 */
                if (takeScreenshot && Boolean.TRUE.equals(SCREENSHOT_PARAMS_HIGHLIGHTELEMENTS) && internalElementLocator != null
                        && (ElementActions.getElementsCount(driver, internalElementLocator,
                        RETRIESBEFORETHROWINGELEMENTNOTFOUNDEXCEPTION) >= 1)) {

                    if ("JavaScript".equals(SCREENSHOT_PARAMS_HIGHLIGHTMETHOD)) {
                        element = driver.findElement(internalElementLocator);
                        js = (JavascriptExecutor) driver;
                        regularElementStyle = highlightElementAndReturnDefaultStyle(element, js,
                                setHighlightedElementStyle());
                    } else {
                        // default to using AI
                        elementLocation = driver.findElement(internalElementLocator).getRect();
                    }
                }
            } catch (StaleElementReferenceException e) {
                // this happens when WebDriver fails to capture the elements initial style or
                // fails to highlight the element for some reason
                ReportManagerHelper.log(e);
            }

            /*
             * Take the screenshot and store it as a file
             */
            byte[] src;

            /*
             * Attempt to take a full page screenshot, take a regular screenshot upon
             * failure
             */
            try {
                src = takeScreenshot(driver);

                /*
                 * Declare screenshot file name
                 */
                testCaseName = ReportManagerHelper.getTestMethodName();
                screenshotFileName = System.currentTimeMillis() + "_" + testCaseName + "_" + actionName;
                if (!"".equals(appendedText)) {
                    screenshotFileName = screenshotFileName + "_" + appendedText;
                }

                /*
                 * If an elementLocator was passed, unhighlight that element after taking the
                 * screenshot
                 *
                 */
                if (takeScreenshot && SCREENSHOT_PARAMS_HIGHLIGHTMETHOD.equals("JavaScript") && js != null) {
                    js.executeScript("arguments[0].setAttribute('style', arguments[1]);", element, regularElementStyle);
                }

                if (takeScreenshot && !SCREENSHOT_PARAMS_HIGHLIGHTMETHOD.equals("JavaScript") && elementLocation != null) {
                    Color color;
                    if (globalPassFailStatus) {
                        color = new Color(165, 210, 165); // green
                    } else {
                        color = new Color(255, 255, 153); // yellow
                    }
                    src = ImageProcessingActions.highlightElementInScreenshot(src, elementLocation, color);
                }
                startOrAppendToAnimatedGif(src);
                if (takeScreenshot) {
                    return addScreenshotToReport(src);
                } else {
                    return null;
                }
            } catch (WebDriverException e) {
                // this happens when a browser session crashes mid-execution, or the docker is
                // unregistered
                ReportManagerHelper.log(e);
            }
        }
        return null;
    }
    

    private static synchronized List<Object> internalCaptureScreenShot(Page page, String elementLocator, String actionName,
    		String appendedText, boolean takeScreenshot) {
        // Suggested: add to animated gif only in case of click, navigation, or validation actions.
        if (takeScreenshot || (CREATE_GIF && (DETAILED_GIF || actionName.matches(DETAILED_GIF_REGEX)))) {
            /*
             * Force screenshot link to be shown in the results as a link not text
             */
            System.setProperty("org.uncommons.reportng.escape-output", "false");

            /*
             * Prepare element style using javascript
             */
            ElementHandle elementHandle = null;
            if (elementLocator != null && !"".equals(elementLocator) && PlayWrightElementActions.getElementsCount(page, elementLocator) > 0) {
                elementHandle = page.querySelector(elementLocator).asElement();
            }
            String regularElementStyle = null;

            if (elementHandle != null && SCREENSHOT_PARAMS_HIGHLIGHTMETHOD.equals("JavaScript")) {
                regularElementStyle = elementHandle.getAttribute("style");

                if (regularElementStyle != null && !regularElementStyle.equals("")) {
                    page.evaluate("arguments[0].style.cssText = arguments[1];", Arrays.asList(elementHandle, regularElementStyle + setHighlightedElementStyle()));
                } else {
                    page.evaluate("arguments[0].setAttribute('style', arguments[1]);", Arrays.asList(elementHandle, setHighlightedElementStyle()));
                }
            }

            /*
             * Take the screenshot and store it as a file
             */
            byte[] src = takeScreenshot(page, elementLocator);

            /*
             * set screenshot name
             */
            testCaseName = ReportManagerHelper.getTestMethodName();
            screenshotFileName = System.currentTimeMillis() + "_" + testCaseName + "_" + actionName;
            if (!"".equals(appendedText)) {
                screenshotFileName = screenshotFileName + "_" + appendedText;
            }

            /*
             * If an elementLocator was passed, unhighlight that element after taking the
             * screenshot
             *
             */
            if (SCREENSHOT_PARAMS_HIGHLIGHTMETHOD.equals("JavaScript") && elementHandle != null) {
                page.evaluate("arguments[0].setAttribute('style', arguments[1]);", Arrays.asList(elementHandle, regularElementStyle));
            }

            if (takeScreenshot && !SCREENSHOT_PARAMS_HIGHLIGHTMETHOD.equals("JavaScript") && elementHandle != null) {
            	var boundingBox = elementHandle.boundingBox();
            	var elementLocation = new Rectangle((int)boundingBox.x, (int)boundingBox.y, (int)boundingBox.height, (int)boundingBox.width);
                Color color;
                if (globalPassFailStatus) {
                    color = new Color(165, 210, 165); // green
                } else {
                    color = new Color(255, 255, 153); // yellow
                }
                src = ImageProcessingActions.highlightElementInScreenshot(src, elementLocation, color);
            }
            startOrAppendToAnimatedGif(src);
            if (takeScreenshot) {
                return addScreenshotToReport(src);
            } else {
                return null;
            }
        }
        return null;
	}

    private static byte[] takeScreenshot(WebDriver driver) {
        return switch (SCREENSHOT_PARAMS_SCREENSHOTTYPE.toLowerCase().trim()) {
            case "fullpage" -> takeFullPageScreenshot(driver);
            case "element" -> takeElementScreenshot(driver, targetElementLocator, true);
            default -> ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
        };
    }
    
    private static byte[] takeScreenshot(Page page, String elementLocator) {
        return switch (SCREENSHOT_PARAMS_SCREENSHOTTYPE.toLowerCase().trim()) {
            case "fullpage" -> page.screenshot(new ScreenshotOptions().setFullPage(true));
            case "element" -> page.querySelector(elementLocator).screenshot(new ElementHandle.ScreenshotOptions());
            default -> page.screenshot(new ScreenshotOptions());
        };
    }

    private static byte[] takeElementScreenshot(WebDriver driver, By targetElementLocator, Boolean
            returnRegularScreenshotInCaseOfFailure) {
        try {
            if (targetElementLocator != null && ElementActions.getElementsCount(driver, targetElementLocator,
                    RETRIESBEFORETHROWINGELEMENTNOTFOUNDEXCEPTION) == 1) {
                return driver.findElement(targetElementLocator).getScreenshotAs(OutputType.BYTES);
            } else {
                if (returnRegularScreenshotInCaseOfFailure) {
                    return ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
                } else {
                    return new byte[]{};
                }
            }
        } catch (Exception e) {
            ReportManagerHelper.log(e);
            if (returnRegularScreenshotInCaseOfFailure) {
                return ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
            } else {
                return new byte[]{};
            }
        }
    }
    
    private static byte[] takeElementScreenshot(Page page, String targetElementLocator, Boolean
            returnRegularScreenshotInCaseOfFailure) {
        try {
            if (targetElementLocator != null && ElementActions.performElementAction(page).getElementsCount(targetElementLocator) == 1) {
                return page.querySelector(targetElementLocator).screenshot();                		
            } else {
                if (returnRegularScreenshotInCaseOfFailure) {
                    return page.screenshot();
                } else {
                    return new byte[]{};
                }
            }
        } catch (Exception e) {
            ReportManagerHelper.log(e);
            if (returnRegularScreenshotInCaseOfFailure) {
                return page.screenshot();
            } else {
                return new byte[]{};
            }
        }
    }

    private static List<Object> addScreenshotToReport(byte[] screenshotFile) {
        /*
         * Adding Screenshot to the Report.
         *
         */
        try {
            // add SHAFT_Engine logo overlay
            InputStream in = new ByteArrayInputStream(screenshotFile);
            BufferedImage screenshotImage = ImageIO.read(in);
            overlayShaftEngineLogo(screenshotImage);

            ByteArrayOutputStream screenshotOutputStream = new ByteArrayOutputStream();
            ImageIO.write(screenshotImage, "png", screenshotOutputStream);
            return Arrays.asList("Screenshot", screenshotFileName,
                    new ByteArrayInputStream(screenshotOutputStream.toByteArray()));
        } catch (IOException e) {
            ReportManagerHelper.log(e);
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
            JavaScriptWaitManager.waitForLazyLoading();
        } catch (Exception e) {
            ReportManagerHelper.log(e);
        }
        return regularElementStyle;
    }

    private static String setHighlightedElementStyle() {
        String backgroud;
        String backgroundColor;

        if (globalPassFailStatus) {
            backgroud = "#46aad2";
            backgroundColor = "#A5D2A5";
        } else {
            backgroud = "#FFFF99";
            backgroundColor = "#FFFF99";
        }
        return "outline-offset:-3px !important; outline:3px solid #808080 !important; background:" + backgroud
                + " !important; background-color:" + backgroundColor
                + " !important; color:#000000 !important; -webkit-transition: none !important; -moz-transition: none !important; -o-transition: none !important; transition: none !important;";

    }

    private static synchronized void startAnimatedGif(byte[] screenshot) {
        // TODO: refactor performance to reduce severe drop when enabling this option
        if (Boolean.TRUE.equals(CREATE_GIF) && screenshot != null) {
            try {
                testCaseName = ReportManagerHelper.getTestMethodName();
                String gifFileName = FileSystems.getDefault().getSeparator() + System.currentTimeMillis() + "_"
                        + testCaseName + ".gif";
                gifRelativePathWithFileName = SCREENSHOT_FOLDERPATH + SCREENSHOT_FOLDERNAME + gifFileName;

                // grab the output image type from the first image in the sequence
                BufferedImage firstImage = ImageIO.read(new ByteArrayInputStream(screenshot));
                
                //scaling it down
                firstImage = Scalr.resize(firstImage, Scalr.Method.BALANCED, GIF_SIZE);
                
                // create a new BufferedOutputStream
                FileActions.createFile(SCREENSHOT_FOLDERPATH + SCREENSHOT_FOLDERNAME, gifFileName);
                gifOutputStream.set(new FileImageOutputStream(new File(gifRelativePathWithFileName)));

                // create a gif sequence with the type of the first image, 500 milliseconds
                // between frames, which loops infinitely
                gifWriter.set(
                        new AnimatedGifManager(gifOutputStream.get(), firstImage.getType(), GIF_FRAME_DELAY));

                // draw initial blank image to set the size of the GIF...
                BufferedImage initialImage = new BufferedImage(firstImage.getWidth(), firstImage.getHeight(),
                        firstImage.getType());
                Graphics2D initialImageGraphics = initialImage.createGraphics();
                initialImageGraphics.setBackground(Color.WHITE);
                initialImageGraphics.clearRect(0, 0, firstImage.getWidth(), firstImage.getHeight());

                // write out initialImage to the sequence...
                gifWriter.get().writeToSequence(overlayShaftEngineLogo(initialImage));
                initialImageGraphics.dispose();
                // write out first image to the sequence...
                gifWriter.get().writeToSequence(overlayShaftEngineLogo(firstImage));
            } catch (NullPointerException | NoSuchSessionException e) {
                // this happens in case the start animated Gif is triggered in a none-test
                // method
                // or this happens when the window is already closed
            } catch (IOException | WebDriverException e) {
                ReportManagerHelper.log(e);
            }
        }
    }

    private static BufferedImage overlayShaftEngineLogo(BufferedImage screenshot) {
        if (Boolean.TRUE.equals(SCREENSHOT_PARAMS_WATERMARK)) {
            try {
                // create graphics object
                Graphics2D screenshotGraphics = screenshot.createGraphics();
                screenshotGraphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                screenshotGraphics.drawImage(screenshot, 0, 0, null);
                screenshotGraphics.setComposite(
                        AlphaComposite.getInstance(AlphaComposite.SRC_OVER, SCREENSHOT_PARAMS_WATERMARKOPACITY));

                BufferedImage shaftLogo;
                // read from custom location
                String watermarkImagePath = PropertyFileManager.getDefaultPropertiesFolderPath().replace("defaultProperties/", System.getProperty("watermarkImagePath"));
                shaftLogo = ImageIO.read(new File(watermarkImagePath));
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

    private static synchronized void startOrAppendToAnimatedGif(byte[] screenshot) {
        // ensure that animatedGif is started, else force start it
        if (Boolean.TRUE.equals(CREATE_GIF)) {
            if ("".equals(gifRelativePathWithFileName)) {
                startAnimatedGif(screenshot);
            } else {
                appendToAnimatedGif(screenshot);
            }
        }
    }

    private static synchronized void appendToAnimatedGif(byte[] screenshot) {
        try {
            BufferedImage image;
            if (screenshot != null) {                
                image = ImageIO.read(new ByteArrayInputStream(screenshot));
            	//scaling it down
                image = Scalr.resize(image, Scalr.Method.BALANCED, GIF_SIZE);
                gifWriter.get().writeToSequence(overlayShaftEngineLogo(image));
            }
        } catch (NoSuchSessionException e) {
            // this happens when attempting to append to a non existing gif, expected
            // solution is to recreate the gif
            // removed the old solution, the new fix is to ignore this exception, this will
            // leave the gif intact and will attach it even after failing to append to it
        } catch (WebDriverException | IOException | IllegalStateException | IllegalArgumentException | NullPointerException e) {
            ReportManagerHelper.log(e);
        }
    }
}