package com.shaft.gui.internal.image;

import com.epam.healenium.SelfHealingDriver;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactoryHelper;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.gui.element.internal.ElementInformation;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.imgscalr.Scalr;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.*;
import org.openqa.selenium.support.locators.RelativeLocator;
import org.sikuli.script.App;
import org.sikuli.script.Pattern;
import org.sikuli.script.Screen;

import javax.imageio.ImageIO;
import javax.imageio.stream.FileImageOutputStream;
import javax.imageio.stream.ImageOutputStream;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;
import java.net.URI;
import java.nio.file.FileSystems;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.*;

public class ScreenshotManager {
    private static final int RETRIES_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION = 1;

    private static Screenshots setScreenshotType() {
        switch (SHAFT.Properties.visuals.screenshotParamsScreenshotType().toLowerCase()) {
            case "element" -> {
                return Screenshots.ELEMENT;
            }
            case "regular" -> {
                return Screenshots.VIEWPORT;
            }
            case "fullpage" -> {
                return Screenshots.FULL;
            }
            default -> {
                return null;
            }
        }
    }
    private static final int GIF_SIZE = 1280;
    // TODO: parameterize the detailed gif value
    private static final Boolean DETAILED_GIF = true;
    private static final String DETAILED_GIF_REGEX = "(verify.*)|(assert.*)|(click.*)|(tap.*)|(key.*)|(navigate.*)";
    private static String AI_AIDED_ELEMENT_IDENTIFICATION_FOLDER_PATH = "";
    private static String screenshotFileName = "Screenshot";
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

    public static String getAiAidedElementIdentificationFolderPath() {
        if (AI_AIDED_ELEMENT_IDENTIFICATION_FOLDER_PATH.isEmpty()) {
            // fixes https://github.com/ShaftHQ/SHAFT_ENGINE/issues/808 by respecting OS/Platform information for mobile native
            AI_AIDED_ELEMENT_IDENTIFICATION_FOLDER_PATH = Properties.paths.dynamicObjectRepository()
                    + Properties.platform.targetPlatform() + "/";
            if (DriverFactoryHelper.isMobileNativeExecution()) {
                if (!Properties.mobile.platformVersion().isEmpty()) {
                    AI_AIDED_ELEMENT_IDENTIFICATION_FOLDER_PATH += Properties.mobile.platformVersion() + "/";
                }
            } else {
                //mobile web, or desktop web
                AI_AIDED_ELEMENT_IDENTIFICATION_FOLDER_PATH += Properties.web.targetBrowserName() + "/";
            }
            return AI_AIDED_ELEMENT_IDENTIFICATION_FOLDER_PATH.replace(".", "_").replace(" ", "_");
        } else {
            return AI_AIDED_ELEMENT_IDENTIFICATION_FOLDER_PATH;
        }
    }

    /**
     * Used if there is no element locator. passFailStatus; true means pass and
     * false means fail.
     *
     * @param driver         the current instance of Selenium WebDriver
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
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param actionName     the name of the triggering action
     * @param passFailStatus A flag to determine whether the action has passed or
     *                       failed
     * @return a screenshot object
     */
    public static List<Object> captureScreenShot(WebDriver driver, By elementLocator, String actionName,
                                                 boolean passFailStatus) {
        globalPassFailStatus = passFailStatus;
        targetElementLocator = elementLocator;

        if (passFailStatus) {
            globalPassFailAppendedText = "passed";
        } else {
            globalPassFailAppendedText = "failed";
        }

        return internalCaptureScreenShot(driver, targetElementLocator, actionName, globalPassFailAppendedText,
                takeScreenshot(actionName, passFailStatus));
    }

    private static boolean takeScreenshot(String actionName, boolean passFailStatus) {
        return (SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot().equals("Always"))
                || (SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot().equals("ValidationPointsOnly")
                && (actionName.toLowerCase().contains("assert")
                || actionName.toLowerCase().contains("verify")
                || actionName.toLowerCase().contains("validate")))
                || (SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot().equals("FailuresOnly") && (!passFailStatus))
                || (!passFailStatus);
        // take screenshot if set to always,
        //OR if set to validation points only and actionName contains verify or assert
        //OR if set to failures only and the test failed
    }

    public static List<Object> captureScreenShotUsingSikuliX(Screen screen, App applicationWindow, Pattern element, String actionName,
                                                                          boolean passFailStatus) {

        globalPassFailStatus = passFailStatus;
        if (passFailStatus) {
            globalPassFailAppendedText = "passed";
        } else {
            globalPassFailAppendedText = "failed";
        }

        boolean takeScreenshot = "Always".equals(SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot())
                || ("ValidationPointsOnly".equals(SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot())
                && (actionName.toLowerCase().contains("assert")
                || actionName.toLowerCase().contains("verify")))
                || !passFailStatus;

        if (takeScreenshot || (SHAFT.Properties.visuals.createAnimatedGif() && (DETAILED_GIF || actionName.matches(DETAILED_GIF_REGEX)))) {
            /*
             * Force screenshot link to be shown in the results as a link not text
             */
            System.setProperty("org.uncommons.reportng.escape-output", "false");

            /*
             * Take the screenshot and store it as a file
             */
            byte[] src = null;
            try {
                ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                switch (Objects.requireNonNull(setScreenshotType())) {
                    case ELEMENT:
                        if (element != null) {
                            try {
                                ImageIO.write(screen.capture(screen.wait(element).getRect()).getImage(), "png", byteArrayOutputStream);
                                src = byteArrayOutputStream.toByteArray();
                                break;
                            } catch (org.sikuli.script.FindFailed e) {
                                //do nothing and fall into the next type of screenshot
                            }
                        }
                    case VIEWPORT:
                        if (applicationWindow != null) {
                            ImageIO.write(screen.capture(applicationWindow.waitForWindow()).getImage(), "png", byteArrayOutputStream);
                            src = byteArrayOutputStream.toByteArray();
                            break;
                        }
                    case FULL:
                        ImageIO.write(screen.capture().getImage(), "png", byteArrayOutputStream);
                        src = byteArrayOutputStream.toByteArray();
                        break;
                    default:
                        break;
                }
            } catch (IOException e) {
                ReportManager.logDiscrete("Failed to create attachment.");
                ReportManagerHelper.logDiscrete(e);
            }

            startOrAppendToAnimatedGif(src);
            if (takeScreenshot) {
                return prepareImageForReport(src, actionName);
            } else {
                return null;
            }
        }
        return null;
    }

    public static byte[] takeViewportScreenshot(WebDriver driver) {
        return ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
    }

    public static byte[] takeFullPageScreenshot(WebDriver driver) {
        try {
            if (!SHAFT.Properties.testNG.parallel().equals("NONE")) {
                //in case of parallel execution, force regular screenshots
                return takeViewportScreenshot(driver);
            } else if (!SHAFT.Properties.visuals.screenshotParamsSkippedElementsFromScreenshot().isEmpty()) {
                List<WebElement> skippedElementsList = new ArrayList<>();
                String[] skippedElementLocators =SHAFT.Properties.visuals.screenshotParamsSkippedElementsFromScreenshot().split(";");
                for (String locator : skippedElementLocators) {
                    if (ElementActionsHelper.getElementsCount(driver, By.xpath(locator),
                            RETRIES_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION) == 1) {
                        skippedElementsList.add(((WebElement) ElementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, By.xpath(locator)).get(1)));
                    }
                }

                WebElement[] skippedElementsArray = new WebElement[skippedElementsList.size()];
                skippedElementsArray = skippedElementsList.toArray(skippedElementsArray);

                return ScreenshotHelper.makeFullScreenshot(driver, skippedElementsArray);
            } else {
                return ScreenshotHelper.makeFullScreenshot(driver);
            }
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            return ScreenshotManager.takeViewportScreenshot(driver);
        }
    }

    public static byte[] takeElementScreenshot(WebDriver driver, By targetElementLocator) {
        return takeElementScreenshot(driver, targetElementLocator, false);
    }

    public static String attachAnimatedGif() {
        // stop and attach
        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.createAnimatedGif()) && !"".equals(gifRelativePathWithFileName)) {
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
                String gifRelativePath = gifRelativePathWithFileName;
                gifRelativePathWithFileName = "";
                return gifRelativePath;
            } catch (FileNotFoundException e) {
                // this happens when the gif fails to start, maybe the browser window was
                // already closed
            } catch (IOException | NullPointerException | IllegalStateException e) {
                ReportManagerHelper.logDiscrete(e);
            }
        }
        return "";
    }

    /**
     * Internal use only. Considers the screenshotParams_whenToTakeAScreenshot
     * parameter.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param appendedText   the text that needs to be appended to the name of the
     *                       screenshot to make it more recognizable
     * @param takeScreenshot determines whether to take a screenshot given
     *                       the screenshotParams_whenToTakeAScreenshot parameter
     *                       from the pom.xml file
     * @return screenshot list object
     */
    private static List<Object> internalCaptureScreenShot(WebDriver driver, By elementLocator,
                                                                       String actionName, String appendedText, boolean takeScreenshot) {
//        if (!actionName.toLowerCase().contains("get")) {
        // Suggested: add to animated gif only in case of click, navigation, or validation actions.
            if (takeScreenshot || (SHAFT.Properties.visuals.createAnimatedGif() && (DETAILED_GIF || actionName.matches(DETAILED_GIF_REGEX)))) {
                /*
                 * Force screenshot link to be shown in the results as a link not text
                 */
                System.setProperty("org.uncommons.reportng.escape-output", "false");

                /*
                 * Declare regularElementStyle, the WebElement, and Javascript Executor to
                 * highlight and unhighlight the WebElement
                 */
                String regularElementStyle = "";
                JavascriptExecutor js = null;
                WebElement element = null;
                Rectangle elementLocation = null;

                /*
                 * If an elementLocator was passed, store regularElementStyle and highlight that
                 * element before taking the screenshot
                 */
                if (takeScreenshot && Boolean.TRUE.equals(SHAFT.Properties.visuals.screenshotParamsHighlightElements()) && elementLocator != null) {
                    int elementCount = ElementActionsHelper.getElementsCount(driver, elementLocator, RETRIES_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION);
                    boolean isRelativeLocator = elementLocator instanceof RelativeLocator.RelativeBy;
                    if ((!isRelativeLocator && elementCount == 1) || (isRelativeLocator && elementCount >= 1)) {
                        if ("JavaScript".equals(SHAFT.Properties.visuals.screenshotParamsHighlightMethod())) {
                            element = ((WebElement) ElementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1));
                            js = (JavascriptExecutor) driver;
                            regularElementStyle = highlightElementAndReturnDefaultStyle(element, js,
                                    setHighlightedElementStyle());
                        } else {
                            // default to using AI
                            elementLocation = ElementInformation.fromList(ElementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator)).getElementRect();
                        }
                    }
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
                    if (takeScreenshot && SHAFT.Properties.visuals.screenshotParamsHighlightMethod().equals("JavaScript") && js != null) {
                        js.executeScript("arguments[0].setAttribute('style', arguments[1]);", element, regularElementStyle);
                    }

                    if (takeScreenshot && !SHAFT.Properties.visuals.screenshotParamsHighlightMethod().equals("JavaScript") && elementLocation != null) {
                        Color color;
                        if (globalPassFailStatus) {
                            color = new Color(67, 176, 42); // selenium-green
                        } else {
                            color = new Color(255, 255, 153); // yellow
                        }
                        src = ImageProcessingActions.highlightElementInScreenshot(src, elementLocation, color);
                    }
                    startOrAppendToAnimatedGif(src);
                    if (takeScreenshot) {
                        return prepareImageForReport(src, actionName);
                    } else {
                        return new ArrayList<>();
                    }
                } catch (WebDriverException e) {
                    // this happens when a browser session crashes mid-execution, or the docker is
                    // unregistered
                    ReportManagerHelper.logDiscrete(e);
                }
            }
//        }
        return new ArrayList<>();
    }

    private static byte[] takeScreenshot(WebDriver driver) {
        if (driver instanceof SelfHealingDriver selfHealingDriver) {
            driver = selfHealingDriver.getDelegate();
        }

        if (DriverFactoryHelper.isWebExecution()) {
            return switch (Objects.requireNonNull(setScreenshotType())) {
                case FULL -> {
                    try {
                        yield takeFullPageScreenshot(driver);
                    } catch (Exception throwable) {
                        ReportManagerHelper.logDiscrete(throwable);
                        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("Regular");
                        yield takeScreenshot(driver);
                    }
                }
                case ELEMENT -> takeElementScreenshot(driver, targetElementLocator, true);
                default -> ScreenshotManager.takeViewportScreenshot(driver);
            };
        }else {
            if (Objects.requireNonNull(setScreenshotType()).equals(Screenshots.ELEMENT)) {
                return takeElementScreenshot(driver, targetElementLocator, true);
            } else {
                return ScreenshotManager.takeViewportScreenshot(driver);
            }
        }
    }

    private static byte[] takeElementScreenshot(WebDriver driver, By targetElementLocator, Boolean
            returnRegularScreenshotInCaseOfFailure) {
        try {
            if (targetElementLocator != null && ElementActionsHelper.getElementsCount(driver, targetElementLocator,
                    RETRIES_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION) == 1) {
                return ((WebElement) ElementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, targetElementLocator).get(1)).getScreenshotAs(OutputType.BYTES);
            } else {
                if (returnRegularScreenshotInCaseOfFailure) {
                    return ScreenshotManager.takeViewportScreenshot(driver);
                } else {
                    return new byte[]{};
                }
            }
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            if (returnRegularScreenshotInCaseOfFailure) {
                return ScreenshotManager.takeViewportScreenshot(driver);
            } else {
                return new byte[]{};
            }
        }
    }

    public static String generateAttachmentFileName(String actionName) {
        testCaseName = ReportManagerHelper.getTestMethodName();
        var fileName = System.currentTimeMillis() + "_" + testCaseName + "_" + actionName;
        if (!"".equals(globalPassFailAppendedText)) {
            fileName = fileName + "_" + globalPassFailAppendedText;
        }
        return fileName;
    }

    public static List<Object> prepareImageForReport(byte[] image, String actionName) {
        if (image != null && image.length > 0) {
            /*
             * Declare screenshot file name
             */
            screenshotFileName = generateAttachmentFileName(actionName);

            /*
             * Adding Screenshot to the Report.
             *
             */
            try {
                // add SHAFT_Engine logo overlay
                InputStream in = new ByteArrayInputStream(image);
                BufferedImage screenshotImage = ImageIO.read(in);
                overlayShaftEngineLogo(screenshotImage);

                ByteArrayOutputStream screenshotOutputStream = new ByteArrayOutputStream();
                ImageIO.write(screenshotImage, "png", screenshotOutputStream);
                return Arrays.asList("Screenshot", screenshotFileName,
                        new ByteArrayInputStream(screenshotOutputStream.toByteArray()));
            } catch (IOException e) {
                ReportManagerHelper.logDiscrete(e);
                return null;
            }
        } else{
            //empty image byte array
            return null;
        }
    }

    private static String highlightElementAndReturnDefaultStyle(WebElement element, JavascriptExecutor js,
                                                                String highlightedElementStyle) {
        String regularElementStyle = element.getAttribute("style");
        if (regularElementStyle != null && !regularElementStyle.isEmpty()) {
            js.executeScript("arguments[0].style.cssText = arguments[1];", element,
                    regularElementStyle + highlightedElementStyle);
        } else {
            js.executeScript("arguments[0].setAttribute('style', arguments[1]);", element, highlightedElementStyle);
        }

        try {
            JavaScriptWaitManager.waitForLazyLoading();
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
        }
        return regularElementStyle;
    }

    private static String setHighlightedElementStyle() {
        String background;
        String backgroundColor;

        if (globalPassFailStatus) {
            background = "#46aad2";
            backgroundColor = "#A5D2A5";
        } else {
            background = "#FFFF99";
            backgroundColor = "#FFFF99";
        }
        return "outline-offset:-3px !important; outline:3px solid #808080 !important; background:" + background
                + " !important; background-color:" + backgroundColor
                + " !important; color:#000000 !important; -webkit-transition: none !important; -moz-transition: none !important; -o-transition: none !important; transition: none !important;";

    }

    private static void startAnimatedGif(byte[] screenshot) {
        // TODO: refactor performance to reduce severe drop when enabling this option
        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.createAnimatedGif()) && screenshot != null) {
            try {
                testCaseName = ReportManagerHelper.getTestMethodName();
                String gifFileName = FileSystems.getDefault().getSeparator() + System.currentTimeMillis() + "_"
                        + testCaseName + ".gif";
                gifRelativePathWithFileName = SHAFT.Properties.paths.allureResults() + "/screenshots/" + new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date()) + gifFileName;

                // get the width and height of the current window of the browser
                var height = DriverFactoryHelper.getTARGET_WINDOW_SIZE().getHeight();
                var width = DriverFactoryHelper.getTARGET_WINDOW_SIZE().getWidth();

                // grab the output image type from the first image in the sequence
                BufferedImage firstImage = ImageIO.read(new ByteArrayInputStream(screenshot));

                //scaling it down
                firstImage = Scalr.resize(firstImage, Scalr.Method.BALANCED, GIF_SIZE);

                // create a new BufferedOutputStream
                FileActions.getInstance().createFile(SHAFT.Properties.paths.allureResults() + "/screenshots/" + new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date()), gifFileName);
                gifOutputStream.set(new FileImageOutputStream(new File(gifRelativePathWithFileName)));

                // create a gif sequence with the type of the first image, 500 milliseconds
                // between frames, which loops infinitely
                gifWriter.set(
                        new AnimatedGifManager(gifOutputStream.get(), firstImage.getType(),SHAFT.Properties.visuals.animatedGifFrameDelay()));

                // draw initial blank image to set the size of the GIF...
                BufferedImage initialImage = new BufferedImage(width, height, firstImage.getType());
                Graphics2D initialImageGraphics = initialImage.createGraphics();
                initialImageGraphics.setBackground(Color.WHITE);
                initialImageGraphics.setColor(Color.WHITE);
                initialImageGraphics.clearRect(0, 0, width, height);

                // write out initialImage to the sequence...
                gifWriter.get().writeToSequence(initialImage);
                initialImageGraphics.dispose();

                // write out first image to the sequence...
                gifWriter.get().writeToSequence(overlayShaftEngineLogo(toBufferedImage(firstImage)));
            } catch (NullPointerException | NoSuchSessionException e) {
                // this happens in case the start animated Gif is triggered in a none-test
                // method
                // or this happens when the window is already closed
            } catch (IOException | WebDriverException e) {
                ReportManagerHelper.logDiscrete(e);
            }
        }
    }

    static BufferedImage shaftLogo = null;
    private static BufferedImage overlayShaftEngineLogo(BufferedImage screenshot) {
        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.screenshotParamsWatermark())) {
            try {
                // create graphics object
                Graphics2D screenshotGraphics = screenshot.createGraphics();
                screenshotGraphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                screenshotGraphics.drawImage(screenshot, 0, 0, null);
                screenshotGraphics.setComposite(
                        AlphaComposite.getInstance(AlphaComposite.SRC_OVER, SHAFT.Properties.visuals.screenshotParamsWatermarkOpacity()));

                if (shaftLogo == null) {
                    // read from custom location
                    String watermarkImagePath = Properties.internal.watermarkImagePath();
                    shaftLogo = ImageIO.read(URI.create(watermarkImagePath).toURL());
                    shaftLogo = toBufferedImage(
                            shaftLogo.getScaledInstance(screenshot.getWidth() / 8, -1, Image.SCALE_SMOOTH));
                }

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
        BufferedImage bufferedImage = new BufferedImage(img.getWidth(null), img.getHeight(null), BufferedImage.TYPE_INT_ARGB);

        // Draw the image on to the buffered image
        Graphics2D bGr = bufferedImage.createGraphics();
        bGr.drawImage(img, 0, 0, null);
        bGr.dispose();

        // Return the buffered image
        return bufferedImage;
    }

    private static void startOrAppendToAnimatedGif(byte[] screenshot) {
        // ensure that animatedGif is started, else force start it
        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.createAnimatedGif())) {
            if (gifRelativePathWithFileName.isEmpty()) {
                startAnimatedGif(screenshot);
            } else {
                appendToAnimatedGif(screenshot);
            }
        }
    }

    private static void appendToAnimatedGif(byte[] screenshot) {
        try {
            BufferedImage image;
            if (screenshot != null && gifWriter.get() != null) {
                image = ImageIO.read(new ByteArrayInputStream(screenshot));
                //scaling it down
                image = Scalr.resize(image, Scalr.Method.BALANCED, GIF_SIZE);
                gifWriter.get().writeToSequence(overlayShaftEngineLogo(image));
            }
        } catch (NoSuchSessionException e) {
            // this happens when attempting to append to a non-existing gif, expected
            // solution is to recreate the gif
            // removed the old solution, the new fix is to ignore this exception, this will
            // leave the gif intact and will attach it even after failing to append to it
        } catch (WebDriverException | IOException | IllegalStateException | IllegalArgumentException | NullPointerException e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }
}