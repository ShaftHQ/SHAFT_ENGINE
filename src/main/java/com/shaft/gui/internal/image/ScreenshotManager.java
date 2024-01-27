package com.shaft.gui.internal.image;

import com.epam.healenium.SelfHealingDriver;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.gui.element.internal.ElementInformation;
import com.shaft.tools.io.internal.ReportManagerHelper;
import lombok.SneakyThrows;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.*;
import org.openqa.selenium.support.locators.RelativeLocator;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ScreenshotManager {
    private static final int RETRIES_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION = 1;
    private static String screenshotFileName = "shot";
    private static By targetElementLocator;
    private static boolean globalPassFailStatus = false;
    private static String globalPassFailAppendedText = "";
    private ScreenshotManager() {
        throw new IllegalStateException("Utility class");
    }

    public static List<Object> takeScreenshot(WebDriver driver, By elementLocator, String actionName,
                                              boolean passFailStatus) {
        globalPassFailStatus = passFailStatus;
        targetElementLocator = elementLocator;
        if (passFailStatus) {
            globalPassFailAppendedText = "passed";
        } else {
            globalPassFailAppendedText = "failed";
        }
        return internalCaptureScreenShot(driver, targetElementLocator, actionName, globalPassFailAppendedText,
                shouldTakeScreenshot(actionName, passFailStatus));
    }

    public static byte[] takeScreenshot(WebDriver driver, By targetElementLocator, Screenshots type) {
        return switch (type) {
            case ELEMENT -> takeElementScreenshot(driver, targetElementLocator, false);
            case VIEWPORT -> takeViewportScreenshot(driver);
            case FULL -> takeFullPageScreenshot(driver);
        };
    }

    public static byte[] takeScreenshot(WebDriver driver) {
        if (driver instanceof SelfHealingDriver selfHealingDriver) {
            driver = selfHealingDriver.getDelegate();
        }

        if (DriverFactoryHelper.isWebExecution()) {
            return switch (Screenshots.getType()) {
                case FULL -> {
                    try {
                        yield takeFullPageScreenshot(driver);
                    } catch (Throwable throwable) {
                        ReportManagerHelper.logDiscrete(throwable);
                        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
                        yield takeScreenshot(driver);
                    }
                }
                case ELEMENT -> takeElementScreenshot(driver, targetElementLocator, true);
                default -> ScreenshotManager.takeViewportScreenshot(driver);
            };
        } else {
            if (Screenshots.getType().equals(Screenshots.ELEMENT)) {
                return takeElementScreenshot(driver, targetElementLocator, true);
            } else {
                return ScreenshotManager.takeViewportScreenshot(driver);
            }
        }
    }

    public static String generateAttachmentFileName(String actionName, String appendedText) {
        if (appendedText != null && !appendedText.isBlank())
            return actionName + "_" + appendedText + "_" + System.currentTimeMillis();
        return actionName + "_" + System.currentTimeMillis();
    }

    public static List<Object> prepareImageForReport(byte[] image, String actionName) {
        if (image != null && image.length > 0) {
            /*
             * Declare screenshot file name
             */
            screenshotFileName = generateAttachmentFileName(actionName, globalPassFailAppendedText);

            /*
             * Adding Screenshot to the Report.
             *
             */
            try {
                // add SHAFT_Engine logo overlay
                BufferedImage screenshotImage = ImageIO.read(new ByteArrayInputStream(image));
                ScreenshotHelper.overlayShaftEngineLogo(screenshotImage);
                ByteArrayOutputStream screenshotOutputStream = new ByteArrayOutputStream();
                ImageIO.write(screenshotImage, "png", screenshotOutputStream);
                return Arrays.asList("Screenshot", screenshotFileName,
                        new ByteArrayInputStream(screenshotOutputStream.toByteArray()));
            } catch (IOException e) {
                ReportManagerHelper.logDiscrete(e);
                return null;
            }
        } else {
            //empty image byte array
            return null;
        }
    }

    private static boolean shouldTakeScreenshot(String actionName, boolean passFailStatus) {
        return (SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot().equals("Always"))
                || (SHAFT.Properties.visuals.createAnimatedGif()
                && (AnimatedGifManager.DETAILED_GIF
                || actionName.matches(AnimatedGifManager.DETAILED_GIF_REGEX)))
                || (SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot().equals("ValidationPointsOnly")
                && (actionName.toLowerCase().contains("assert")
                || actionName.toLowerCase().contains("verify")
                || actionName.toLowerCase().contains("validate")))
                || (!passFailStatus);
        //take screenshot if set to always,
        //OR if needed for an animated GIF,
        //OR if set to validation points only and actionName contains verify, assert or validate
        //OR if test failed
    }

    private static byte[] takeViewportScreenshot(WebDriver driver) {
        return ScreenshotHelper.takeViewportScreenshot(driver, 6);
    }

    @SneakyThrows
    private static byte[] takeFullPageScreenshot(WebDriver driver) {
        if (!SHAFT.Properties.testNG.parallel().equals("NONE")) {
            //in case of parallel execution, force regular screenshots
            return takeViewportScreenshot(driver);
        } else if (!SHAFT.Properties.visuals.screenshotParamsSkippedElementsFromScreenshot().isEmpty()) {
            List<WebElement> skippedElementsList = new ArrayList<>();
            String[] skippedElementLocators = SHAFT.Properties.visuals.screenshotParamsSkippedElementsFromScreenshot().split(";");
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

    private static List<Object> internalCaptureScreenShot(WebDriver driver, By elementLocator,
                                                          String actionName, String appendedText, boolean shouldCaptureScreenshot) {
        byte[] src = new byte[0];
        if (shouldCaptureScreenshot
                || (SHAFT.Properties.visuals.createAnimatedGif() && (AnimatedGifManager.DETAILED_GIF || actionName.matches(AnimatedGifManager.DETAILED_GIF_REGEX)))) {
            if ("JavaScript".equals(SHAFT.Properties.visuals.screenshotParamsHighlightMethod())) {
                src = takeJavaScriptHighlightedScreenshot(driver, elementLocator, actionName, appendedText);
            } else {
                src = takeAIHighlightedScreenshot(driver, elementLocator, actionName, appendedText);
            }
        }
        //return screenshot to be attached only if needed, else do nothing as it was already added to the GIF
        if (shouldCaptureScreenshot)
            return prepareImageForReport(src, actionName);
        return new ArrayList<>();
    }

    private static byte[] takeAIHighlightedScreenshot(WebDriver driver, By elementLocator,
                                                      String actionName, String appendedText) {
        Rectangle elementLocation = null;
        // getElementLocation
        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.screenshotParamsHighlightElements()) && elementLocator != null) {
            int elementCount = ElementActionsHelper.getElementsCount(driver, elementLocator, RETRIES_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION);
            boolean isRelativeLocator = elementLocator instanceof RelativeLocator.RelativeBy;
            if ((!isRelativeLocator && elementCount == 1) || (isRelativeLocator && elementCount >= 1)) {
                elementLocation = ElementInformation.fromList(ElementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator)).getElementRect();
            }
        }
        try {
            //takeScreenshot
            byte[] src = takeScreenshot(driver);
            screenshotFileName = actionName + "_" + appendedText + "_" + System.currentTimeMillis();
            //highlightElement using OpenCV
            if (elementLocation != null) {
                Color color;
                if (globalPassFailStatus) {
                    color = new Color(67, 176, 42); // selenium-green
                } else {
                    color = new Color(255, 255, 153); // yellow
                }
                src = ImageProcessingActions.highlightElementInScreenshot(src, elementLocation, color);
            }
            //append highlighted element to GIF
            AnimatedGifManager.startOrAppendToAnimatedGif(src);
            return src;
        } catch (WebDriverException e) {
            // in case we failed to take a screenshot
            ReportManagerHelper.logDiscrete(e);
        }
        //return an empty byteArray if no screenshot was needed or if we failed to take it
        return new byte[0];
    }

    private static byte[] takeJavaScriptHighlightedScreenshot(WebDriver driver, By elementLocator,
                                                              String actionName, String appendedText) {
        String regularElementStyle = "";
        JavascriptExecutor js = null;
        WebElement element = null;
        // get & highlight Element
        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.screenshotParamsHighlightElements()) && elementLocator != null) {
            int elementCount = ElementActionsHelper.getElementsCount(driver, elementLocator, RETRIES_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION);
            boolean isRelativeLocator = elementLocator instanceof RelativeLocator.RelativeBy;
            if ((!isRelativeLocator && elementCount == 1) || (isRelativeLocator && elementCount >= 1)) {
                //getElement
                element = ((WebElement) ElementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1));
                //highlightElement
                js = (JavascriptExecutor) driver;
                regularElementStyle = highlightElementAndReturnDefaultStyle(driver, element, js,
                        setHighlightedElementStyle());
            }
        }
        try {
            byte[] src = takeScreenshot(driver);
            screenshotFileName = actionName + "_" + appendedText + "_" + System.currentTimeMillis();
            //append highlighted element to GIF
            AnimatedGifManager.startOrAppendToAnimatedGif(src);
            //resetElementStyle
            if (SHAFT.Properties.visuals.screenshotParamsHighlightMethod().equals("JavaScript") && js != null) {
                js.executeScript("arguments[0].setAttribute('style', arguments[1]);", element, regularElementStyle);
            }
            return src;
        } catch (WebDriverException e) {
            // in case we failed to take a screenshot
            ReportManagerHelper.logDiscrete(e);
        }
        //return an empty byteArray if no screenshot was needed or if we failed to take it
        return new byte[0];
    }
    private static String highlightElementAndReturnDefaultStyle(WebDriver driver, WebElement element, JavascriptExecutor js,
                                                                String highlightedElementStyle) {
        String regularElementStyle = element.getAttribute("style");
        if (regularElementStyle != null && !regularElementStyle.isEmpty()) {
            js.executeScript("arguments[0].style.cssText = arguments[1];", element,
                    regularElementStyle + highlightedElementStyle);
        } else {
            js.executeScript("arguments[0].setAttribute('style', arguments[1]);", element, highlightedElementStyle);
        }

        try {
            JavaScriptWaitManager.waitForLazyLoading(driver);
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
}