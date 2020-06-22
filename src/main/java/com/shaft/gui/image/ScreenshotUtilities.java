package com.shaft.gui.image;

import org.apache.commons.io.output.ByteArrayOutputStream;
import org.openqa.selenium.*;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;

public class ScreenshotUtilities {

    private static final String JS_RETRIEVE_DEVICE_PIXEL_RATIO = "var pr = window.devicePixelRatio; if (pr != undefined && pr != null)return pr; else return 1.0;";

    private ScreenshotUtilities() {
        throw new IllegalStateException("Utility class");
    }

    protected static byte[] makeFullScreenshot(WebDriver driver, WebElement... skipElements) throws IOException {

        // scroll up first to start taking screenshots
        scrollVerticallyTo(driver, 0);
        hideScroll(driver);
        // No need to hide elements for first attempt
        byte[] bytes = getScreenShot(driver);

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
                BufferedImage nextImage = ImageIO.read(new ByteArrayInputStream(getScreenShot(driver)));
                g2dTile.drawImage(nextImage, 0, (i + 1) * capturedHeight, null);
            }
            if (leftover > 0) {
                scroll += adaptedCapturedHeight;
                scrollVerticallyTo(driver, scroll);
                BufferedImage nextImage = ImageIO.read(new ByteArrayInputStream(getScreenShot(driver)));
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

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ImageIO.write(resultingImage, "png", baos);
        return baos.toByteArray();
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
                ((JavascriptExecutor) driver).executeScript("arguments[0].style.display = '" + display + "';",
                        skipElement);
            }
        }
    }

    private static byte[] getScreenShot(WebDriver driver) {
        return ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
    }

    private static void scrollVerticallyTo(WebDriver driver, int scroll) {
        ((JavascriptExecutor) driver).executeScript("window.scrollTo(0, " + scroll + ");");
        try {
            waitUntilItIsScrolledToPosition(driver, scroll);
        } catch (Exception e) {
            // this used to throw InterruptedException e, but was changed to the generic
            // exception to resolve the sonar lint comment
        }
    }

    private static void waitUntilItIsScrolledToPosition(WebDriver driver, int scrollPosition)
            throws InterruptedException {
        int time = 250;// SCREENSHOT_FULLPAGE_SCROLLTIMEOUT
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