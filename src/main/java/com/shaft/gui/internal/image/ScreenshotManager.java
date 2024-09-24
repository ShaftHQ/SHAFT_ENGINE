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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

public class ScreenshotManager {
    private static final String VALIDATION_ACTION_REGEX = "(.*validation.*)|(.*verify.*)|(.*assert.*)";
    private final ElementActionsHelper elementActionsHelper;

    public ScreenshotManager() {
        elementActionsHelper = new ElementActionsHelper(false);
    }

    public List<Object> takeScreenshot(WebDriver driver, By elementLocator, String actionName,
                                       boolean passFailStatus) {
        return internalCaptureScreenShot(driver, elementLocator, actionName, shouldTakeScreenshot(actionName, passFailStatus), passFailStatus);
    }

    protected byte[] takeScreenshot(WebDriver driver, By targetElementLocator, Screenshots type) {
        return switch (type) {
            case ELEMENT -> takeElementScreenshot(driver, targetElementLocator, false);
            case VIEWPORT -> takeViewportScreenshot(driver);
            case FULL -> takeFullPageScreenshot(driver);
        };
    }

    public byte[] takeScreenshot(WebDriver driver, By targetElementLocator) {
        if (driver instanceof SelfHealingDriver selfHealingDriver) {
            driver = selfHealingDriver.getDelegate();
        }

        byte[] screenshot;
        if (DriverFactoryHelper.isNotMobileExecution()) {
            screenshot = switch (Screenshots.getType()) {
                case FULL -> {
                    try {
                        yield takeFullPageScreenshot(driver);
                    } catch (Throwable throwable) {
                        ReportManagerHelper.logDiscrete(throwable);
                        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
                        yield takeScreenshot(driver, null);
                    }
                }
                case ELEMENT -> takeElementScreenshot(driver, targetElementLocator, true);
                default -> this.takeViewportScreenshot(driver);
            };
        } else {
            if (Screenshots.getType().equals(Screenshots.ELEMENT)) {
                screenshot = takeElementScreenshot(driver, targetElementLocator, true);
            } else {
                screenshot = this.takeViewportScreenshot(driver);
            }
        }
        return screenshot;
    }

    public String generateAttachmentFileName(String actionName) {
        return actionName + "_" + new SimpleDateFormat("HH-mm-ss-SSS_ddMMyyyy").format(new Date());
    }

    public List<Object> prepareImageForReport(byte[] image, String actionName) {
        if (image != null && image.length > 0) {
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
                return Arrays.asList("Screenshot", generateAttachmentFileName(actionName),
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

    private boolean shouldTakeScreenshot(String actionName, boolean passFailStatus) {
        var whenToTakeAScreenshot = SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot();
        return (
                !passFailStatus
                        || (actionName.toLowerCase().matches(VALIDATION_ACTION_REGEX) && !whenToTakeAScreenshot.equals("Never"))
                        || (SHAFT.Properties.visuals.createAnimatedGif() && (AnimatedGifManager.DETAILED_GIF || actionName.toLowerCase().matches(AnimatedGifManager.LIGHTWEIGHT_GIF_REGEX)))
                        || whenToTakeAScreenshot.equals("Always")
        );
        // if action failed => most common case
        // validation action & not set to never => second most common case
        // OR animated GIF && (detailed gif OR actionName matches lightweight gif regex)
        // OR set to always
    }

    private byte[] takeViewportScreenshot(WebDriver driver) {
        return ScreenshotHelper.takeViewportScreenshot(driver, 6);
    }

    @SneakyThrows
    private byte[] takeFullPageScreenshot(WebDriver driver) {
        if (!SHAFT.Properties.testNG.parallel().equals("NONE")) {
            //in case of parallel execution, force regular screenshots
            return takeViewportScreenshot(driver);
        } else if (!SHAFT.Properties.visuals.screenshotParamsSkippedElementsFromScreenshot().isEmpty()) {
            List<WebElement> skippedElementsList = new ArrayList<>();
            String[] skippedElementLocators = SHAFT.Properties.visuals.screenshotParamsSkippedElementsFromScreenshot().split(";");
            for (String locator : skippedElementLocators) {
                if (elementActionsHelper.getElementsCount(driver, By.xpath(locator)) == 1) {
                    skippedElementsList.add(((WebElement) elementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, By.xpath(locator)).get(1)));
                }
            }
            WebElement[] skippedElementsArray = new WebElement[skippedElementsList.size()];
            skippedElementsArray = skippedElementsList.toArray(skippedElementsArray);
            return ScreenshotHelper.makeFullScreenshot(driver, skippedElementsArray);
        } else {
            return ScreenshotHelper.makeFullScreenshot(driver);
        }
    }

    public byte[] takeElementScreenshot(WebDriver driver, By targetElementLocator){
        return takeElementScreenshot(driver, targetElementLocator, false);
    }

    private byte[] takeElementScreenshot(WebDriver driver, By targetElementLocator, Boolean
            returnRegularScreenshotInCaseOfFailure) {
        try {
            if (targetElementLocator != null && elementActionsHelper.getElementsCount(driver, targetElementLocator) == 1) {
                return ((WebElement) elementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, targetElementLocator).get(1)).getScreenshotAs(OutputType.BYTES);
            } else {
                if (returnRegularScreenshotInCaseOfFailure) {
                    return this.takeViewportScreenshot(driver);
                } else {
                    return new byte[]{};
                }
            }
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            if (returnRegularScreenshotInCaseOfFailure) {
                return this.takeViewportScreenshot(driver);
            } else {
                return new byte[]{};
            }
        }
    }

    private List<Object> internalCaptureScreenShot(WebDriver driver, By elementLocator, String actionName, boolean shouldCaptureScreenshot, boolean isPass) {
        if (shouldCaptureScreenshot) {
            byte[] src = internalCaptureScreenshot(driver, elementLocator, isPass);
            return prepareImageForReport(src, actionName);
        }
        //return screenshot to be attached only if needed, else do nothing as it was already added to the GIF
        return new ArrayList<>();
    }

    public byte[] internalCaptureScreenshot(WebDriver driver, By elementLocator, boolean isPass){
        if ("JavaScript".equals(SHAFT.Properties.visuals.screenshotParamsHighlightMethod())) {
            return takeJavaScriptHighlightedScreenshot(driver, elementLocator, isPass);
        } else {
            return takeAIHighlightedScreenshot(driver, elementLocator, isPass);
        }
    }

    private byte[] takeAIHighlightedScreenshot(WebDriver driver, By elementLocator, boolean isPass) {
        Rectangle elementLocation = null;
        // getElementLocation
        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.screenshotParamsHighlightElements()) && elementLocator != null) {
            var elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator));
            int elementCount = elementInformation.getNumberOfFoundElements();
            boolean isRelativeLocator = elementLocator instanceof RelativeLocator.RelativeBy;
            if ((!isRelativeLocator && elementCount == 1) || (isRelativeLocator && elementCount >= 1)) {
                elementLocation = elementInformation.getElementRect();
            }
        }
        try {
            //takeScreenshot
            byte[] src = takeScreenshot(driver, elementLocator);
            //highlightElement using OpenCV
            if (elementLocation != null) {
                Color color;
                if (isPass) {
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

    private byte[] takeJavaScriptHighlightedScreenshot(WebDriver driver, By elementLocator, boolean isPass) {
        String regularElementStyle = "";
        JavascriptExecutor js = null;
        WebElement element = null;
        // get & highlight Element
        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.screenshotParamsHighlightElements()) && elementLocator != null) {
            var elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator));
            int elementCount = elementInformation.getNumberOfFoundElements();
            boolean isRelativeLocator = elementLocator instanceof RelativeLocator.RelativeBy;
            if ((!isRelativeLocator && elementCount == 1) || (isRelativeLocator && elementCount >= 1)) {
                //getElement
                element = elementInformation.getFirstElement();
                //highlightElement
                js = (JavascriptExecutor) driver;
                regularElementStyle = highlightElementAndReturnDefaultStyle(driver, element, js,
                        setHighlightedElementStyle(isPass));
            }
        }
        try {
            byte[] src = takeScreenshot(driver, elementLocator);
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

    private String highlightElementAndReturnDefaultStyle(WebDriver driver, WebElement element, JavascriptExecutor js,
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

    private String setHighlightedElementStyle(boolean isPass) {
        String background;
        String backgroundColor;

        if (isPass) {
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