package com.shaft.gui.internal.image;

import org.openqa.selenium.By;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Issue #7175: a secret typed into a plain text field must not appear in the screenshot pixels SHAFT
 * captures, while ordinary typed text stays visible. Runs in a real headless Chrome.
 */
public class SecretScreenshotAcceptanceTest {
    private static final String PAGE = "<!doctype html><body style='margin:0;background:#fff'>"
            + "<input id='key' style='font:48px monospace;color:#000;background:#fff;border:0;outline:0;"
            + "width:900px;height:80px;padding:0;margin:20px' autocomplete='off'></body>";

    @AfterMethod(alwaysRun = true)
    public void clear() {
        SecretScreenMask.clear();
    }

    @Test(groups = "trace-viewer-browser-acceptance")
    public void typedSecretIsHiddenInViewportPixelsAndOrdinaryTextIsNot() throws IOException {
        String chrome = System.getProperty("shaft.trace.viewer.chrome", "/usr/bin/google-chrome");
        if (!Files.isExecutable(Path.of(chrome))) {
            throw new SkipException("Chrome is required for the secret screenshot acceptance test: " + chrome);
        }
        ChromeDriver driver = new ChromeDriver(new ChromeOptions().setBinary(chrome)
                .addArguments("--headless=new", "--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage",
                        "--window-size=1000,300", "--force-device-scale-factor=1"));
        try {
            driver.get("data:text/html," + URLEncoder.encode(PAGE, StandardCharsets.UTF_8).replace("+", "%20"));
            WebElement field = driver.findElement(By.id("key"));
            field.sendKeys("SECRET-ACCESS-KEY-0123456789");
            field.click();
            Rectangle rect = field.getRect();

            long visible = darkPixels(ScreenshotHelper.takeViewportScreenshot(driver, 0), rect);
            Assert.assertTrue(visible > 200, "control: typed text is visible before registration, dark pixels=" + visible);

            SecretScreenMask.register(driver, field);
            long masked = darkPixels(ScreenshotHelper.takeViewportScreenshot(driver, 0), rect);
            Assert.assertEquals(masked, 0L, "a registered secret leaves no dark pixels in its field");
            Assert.assertEquals(field.getDomAttribute("style"), driver.findElement(By.id("key")).getDomAttribute("style"));
            Assert.assertFalse(String.valueOf(field.getDomAttribute("style")).contains("transparent"),
                    "the field's own style is restored after the capture");
            Assert.assertEquals(field.getDomProperty("value"), "SECRET-ACCESS-KEY-0123456789",
                    "masking never changes the typed value");
        } finally {
            driver.quit();
        }
    }

    private static long darkPixels(byte[] png, Rectangle rect) throws IOException {
        BufferedImage image = ImageIO.read(new ByteArrayInputStream(png));
        long dark = 0;
        for (int x = rect.getX(); x < Math.min(image.getWidth(), rect.getX() + rect.getWidth()); x++) {
            for (int y = rect.getY(); y < Math.min(image.getHeight(), rect.getY() + rect.getHeight()); y++) {
                int rgb = image.getRGB(x, y);
                int luminance = ((rgb >> 16) & 0xff) + ((rgb >> 8) & 0xff) + (rgb & 0xff);
                if (luminance < 3 * 128) {
                    dark++;
                }
            }
        }
        return dark;
    }
}
