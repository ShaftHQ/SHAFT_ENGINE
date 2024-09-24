package com.shaft.gui.internal.image;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.enums.internal.Screenshots;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertiesHelper;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.commons.io.output.ByteArrayOutputStream;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.*;
import org.openqa.selenium.chromium.HasCdp;
import org.openqa.selenium.firefox.FirefoxDriver;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;

public class ScreenshotHelper {
    private static final String JS_RETRIEVE_DEVICE_PIXEL_RATIO = "var pr = window.devicePixelRatio; if (pr != undefined && pr != null)return pr; else return 1.0;";
    private static String AI_AIDED_ELEMENT_IDENTIFICATION_FOLDER_PATH = "";
    private static BufferedImage shaftLogo = null;

    private ScreenshotHelper() {
        throw new IllegalStateException("Utility class");
    }

    public static String getAiAidedElementIdentificationFolderPath() {
        if (AI_AIDED_ELEMENT_IDENTIFICATION_FOLDER_PATH.isEmpty()) {
            // fixes https://github.com/ShaftHQ/SHAFT_ENGINE/issues/808 by respecting OS/Platform information for mobile native
            if (Properties.paths == null)
                PropertiesHelper.initialize();
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

    public static BufferedImage overlayShaftEngineLogo(BufferedImage screenshot) {
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

    protected static BufferedImage toBufferedImage(Image img) {
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

    public static byte[] makeFullScreenshot(WebDriver driver, WebElement... skipElements) throws IOException {
        if (driver instanceof FirefoxDriver firefoxDriver) {
            return firefoxDriver.getFullPageScreenshotAs(OutputType.BYTES);
        } else if (driver instanceof HasCdp cdpDriver) {
            return takeFullPageScreenshotUsingCDP(driver, cdpDriver);
        } else {
            return takeFullPageScreenshotManually(driver, skipElements);
        }
    }

    protected static byte[] takeViewportScreenshot(WebDriver driver, int retryAttempts) {
        try {
            return ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
        } catch (RuntimeException exception) {
            if (retryAttempts <= 0) {
                ReportManagerHelper.logDiscrete(exception, Level.WARN);
                ReportManagerHelper.logDiscrete("Failed to take a screenshot after 5 attempts.", Level.WARN);
                return null;
            } else
                // java.lang.RuntimeException: Unexpected result for screenshot command: com.google.common.collect.Maps$TransformedEntriesMap instance
                if (exception.getMessage().contains("Permission denied to access property \"pageXOffset\" on cross-origin object")
                        || exception.getMessage().contains("not connected to DevTools")
                        || exception.getMessage().contains("unhandled inspector error: {\"code\":-32000,\"message\":\"Unable to capture screenshot\"}")) {
                    // Ubuntu_Firefox_Grid
                    // org.openqa.selenium.WebDriverException: SecurityError: Permission denied to access property "pageXOffset" on cross-origin object
                    // MacOSX_Chrome_Local
                    // org.openqa.selenium.WebDriverException: disconnected: not connected to DevTools
                    // Ubuntu_Edge_Grid, Ubuntu_Chrome_Grid
                    // org.openqa.selenium.WebDriverException: unknown error: unhandled inspector error: {"code":-32000,"message":"Unable to capture screenshot"}
                    driver.switchTo().defaultContent();
                    return takeViewportScreenshot(driver, retryAttempts - 1);
                } else {
                    FailureReporter.fail(ScreenshotManager.class, "Failed to capture a screenshot", exception);
                    return null;
                }
        }
    }

    @SuppressWarnings("unchecked")
    private static byte[] takeFullPageScreenshotUsingCDP(WebDriver driver, HasCdp cdpDriver) throws IOException {
        try {
            Map<String, Object> page_rect = cdpDriver.executeCdpCommand("Page.getLayoutMetrics", new HashMap<>());
            Map<String, Object> contentSize = (Map<String, Object>) page_rect.get("contentSize");
            Number contentWidth = (Number) contentSize.get("width");
            Number contentHeight = (Number) contentSize.get("height");
            Map<String, Object> clip = new HashMap<>();
            clip.put("width", (long) contentWidth / SHAFT.Properties.visuals.screenshotParamsScalingFactor());
            clip.put("height", (long) contentHeight / SHAFT.Properties.visuals.screenshotParamsScalingFactor());
            clip.put("x", 0);
            clip.put("y", 0);
            clip.put("scale", 1);
            Map<String, Object> screenshot_config = new HashMap<>();
            screenshot_config.put("optimizeForSpeed", true);
            screenshot_config.put("captureBeyondViewport", true);
            screenshot_config.put("fromSurface", true);
            screenshot_config.put("clip", clip);
            var result = cdpDriver.executeCdpCommand("Page.captureScreenshot", screenshot_config);
            String base64EncodedPng = (String) ((Map<String, ?>) result).get("data");
            return OutputType.BYTES.convertFromBase64Png(base64EncodedPng);
        } catch (org.openqa.selenium.TimeoutException timeoutException) {
                /* Error:  org.openqa.selenium.TimeoutException: java.net.http.HttpTimeoutException: request timed out
                    Build info: version: '4.16.1', revision: '9b4c83354e'
                    System info: os.name: 'Mac OS X', os.arch: 'x86_64', os.version: '12.7.1', java.version: '21.0.1'
                    Driver info: org.openqa.selenium.chrome.ChromeDriver
                    Command: [xxx, executeCdpCommand {cmd=Page.captureScreenshot, params={fromSurface=true, optimizeForSpeed=true, captureBeyondViewport=true, clip={width=1905, x=0, y=0, scale=1, height=2555}}}]
                 */
            // in some cases it was noticed that the full page screenshot Cdp command can cause a timeout, and therefore a workaround should be implemented
            return takeFullPageScreenshotManually(driver);
        }
    }

    private static byte[] takeFullPageScreenshotManually(WebDriver driver, WebElement... skipElements) throws IOException {
        // scroll up first to start taking screenshots
        scrollVerticallyTo(driver, 0);
        hideScroll(driver);
        // No need to hide elements for first attempt
        byte[] bytes = new ScreenshotManager().takeScreenshot(driver, null, Screenshots.VIEWPORT);

        showHideElements(driver, true, skipElements);
        long longScrollHeight = (Long) ((JavascriptExecutor) driver)
                .executeScript("return Math.max(" + "document.body.scrollHeight, document.documentElement.scrollHeight,"
                        + "document.body.offsetHeight, document.documentElement.offsetHeight,"
                        + "document.body.clientHeight, document.documentElement.clientHeight);");

        BufferedImage image = ImageIO.read(new ByteArrayInputStream(bytes));
        int capturedWidth = image.getWidth();
        int capturedHeight = image.getHeight();

        double devicePixelRatio = ((Number) ((JavascriptExecutor) driver).executeScript(JS_RETRIEVE_DEVICE_PIXEL_RATIO))
                .doubleValue();

        int scrollHeight = (int) longScrollHeight;

        int adaptedCapturedHeight = (int) (((double) capturedHeight) / devicePixelRatio);

        BufferedImage resultingImage;

        if (Math.abs(adaptedCapturedHeight - scrollHeight) > 40) {
            int times = scrollHeight / adaptedCapturedHeight;
            int leftover = scrollHeight % adaptedCapturedHeight;

            final BufferedImage tiledImage = new BufferedImage(capturedWidth,
                    (int) (((double) scrollHeight) * devicePixelRatio), BufferedImage.TYPE_INT_RGB);
            Graphics2D g2dTile = tiledImage.createGraphics();
            g2dTile.drawImage(image, 0, 0, null);

            int scroll = 0;
            for (int i = 0; i < times - 1; i++) {
                scroll += adaptedCapturedHeight;
                scrollVerticallyTo(driver, scroll);
                BufferedImage nextImage = ImageIO.read(new ByteArrayInputStream(new ScreenshotManager().takeScreenshot(driver, null, Screenshots.VIEWPORT)));
                g2dTile.drawImage(nextImage, 0, (i + 1) * capturedHeight, null);
            }
            if (leftover > 0) {
                scroll += adaptedCapturedHeight;
                scrollVerticallyTo(driver, scroll);
                BufferedImage nextImage = ImageIO.read(new ByteArrayInputStream(new ScreenshotManager().takeScreenshot(driver, null, Screenshots.VIEWPORT)));
                BufferedImage lastPart = nextImage.getSubimage(0,
                        nextImage.getHeight() - (int) (((double) leftover) * devicePixelRatio), nextImage.getWidth(),
                        leftover);
                g2dTile.drawImage(lastPart, 0, times * capturedHeight, null);
            }

            scrollVerticallyTo(driver, 0);

            resultingImage = tiledImage;
        } else {
            resultingImage = image;
        }
        showScroll(driver);
        showHideElements(driver, false, skipElements);

        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        ImageIO.write(resultingImage, "png", byteArrayOutputStream);
        return byteArrayOutputStream.toByteArray();
    }

    private static void hideScroll(WebDriver driver) {
        ((JavascriptExecutor) driver).executeScript("document.documentElement.style.overflow = 'hidden';");
    }

    private static void showScroll(WebDriver driver) {
        ((JavascriptExecutor) driver).executeScript("document.documentElement.style.overflow = 'visible';");
    }

    private static void showHideElements(WebDriver driver, Boolean hide, WebElement... skipElements) {
        String display;
        if (Boolean.TRUE.equals(hide)) {
            display = "none";
        } else {
            display = "block";
        }
        if (skipElements != null) {
            for (WebElement skipElement : skipElements) {
                ((JavascriptExecutor) driver).executeScript("arguments[0].style.display = arguments[1];", skipElement, display);
            }
        }
    }

    private static void scrollVerticallyTo(WebDriver driver, int scroll) {
        ((JavascriptExecutor) driver).executeScript("window.scrollTo(0, \"arguments[0]\");", scroll);
        try {
            waitUntilItIsScrolledToPosition(driver, scroll);
        } catch (Exception e) {
            // this used to throw InterruptedException e, but was changed to the generic
            // exception to resolve the sonar lint comment
        }
    }

    private static void waitUntilItIsScrolledToPosition(WebDriver driver, int scrollPosition)
            throws InterruptedException {
        int time = 250;// SCREENSHOT_FULL_PAGE_SCROLL_TIMEOUT
        boolean isScrolledToPosition = false;
        while (time >= 0 && !isScrolledToPosition) {
            Thread.sleep(50);
            time -= 50;
            isScrolledToPosition = Math.abs(obtainVerticalScrollPosition(driver) - scrollPosition) < 3;
        }
    }

    private static int obtainVerticalScrollPosition(WebDriver driver) {
        Long scrollLong = (Long) ((JavascriptExecutor) driver).executeScript(
                "return (window.pageYOffset !== undefined) ? window.pageYOffset : (document.documentElement || document.body.parentNode || document.body).scrollTop;");
        return scrollLong.intValue();
    }
}