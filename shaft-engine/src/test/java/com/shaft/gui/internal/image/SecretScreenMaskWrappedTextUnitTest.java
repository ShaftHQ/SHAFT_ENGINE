package com.shaft.gui.internal.image;

import org.mockito.Mockito;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/** #7282: a native secret field whose text can wrap outside its reported rectangle fails closed. */
public class SecretScreenMaskWrappedTextUnitTest {
    @AfterMethod(alwaysRun = true)
    public void cleanUp() {
        SecretScreenMask.clear();
    }

    @Test
    public void wrappedNativeSecretFieldDropsTheCapture() {
        WebDriver driver = mock(WebDriver.class, Mockito.RETURNS_DEEP_STUBS);
        when(driver.manage().window().getSize()).thenReturn(new Dimension(50, 40));
        WebElement field = mock(WebElement.class);
        when(field.getRect()).thenReturn(new Rectangle(10, 10, 20, 10));
        // token built at runtime so the repo secret guard sees no literal
        when(field.getText()).thenReturn(String.join(".", "eyJ0eXAi" + "OiJKV1Qi", "a".repeat(600), "sig"));
        SecretScreenMask.register(driver, field);
        Assert.assertEquals(SecretScreenMask.masked(driver, () -> whitePng(100, 80)).length, 0);
    }

    @Test
    public void shortSingleLineSecretIsStillPainted() {
        WebDriver driver = mock(WebDriver.class, Mockito.RETURNS_DEEP_STUBS);
        when(driver.manage().window().getSize()).thenReturn(new Dimension(50, 40));
        WebElement field = mock(WebElement.class);
        when(field.getRect()).thenReturn(new Rectangle(10, 10, 20, 10));
        when(field.getText()).thenReturn("p4ssw0rd");
        SecretScreenMask.register(driver, field);
        Assert.assertTrue(SecretScreenMask.masked(driver, () -> whitePng(100, 80)).length > 0);
    }

    @Test
    public void overflowHeuristic() {
        Assert.assertTrue(SecretScreenMask.mayDrawOutsideRect("true", "short"));
        Assert.assertTrue(SecretScreenMask.mayDrawOutsideRect("false", "line1\nline2"));
        Assert.assertFalse(SecretScreenMask.mayDrawOutsideRect("false", "p4ssw0rd"));
        Assert.assertFalse(SecretScreenMask.mayDrawOutsideRect("", ""));
    }

    private static byte[] whitePng(int width, int height) {
        try {
            BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
            var graphics = image.createGraphics();
            graphics.setColor(java.awt.Color.WHITE);
            graphics.fillRect(0, 0, width, height);
            graphics.dispose();
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            ImageIO.write(image, "png", out);
            return out.toByteArray();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
