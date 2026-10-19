package com.shaft.gui.internal.image;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.ReportSecrets;
import org.mockito.InOrder;
import org.mockito.Mockito;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.WebDriverException;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class SecretScreenMaskTest {
    private static final byte[] IMAGE = {1, 2, 3};
    private static final String JWT = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.sig";

    interface JsDriver extends WebDriver, JavascriptExecutor {
    }

    @BeforeMethod
    public void reset() {
        SecretScreenMask.clear();
        ReportSecrets.clear();
    }

    @AfterMethod(alwaysRun = true)
    public void cleanUp() {
        SecretScreenMask.clear();
        ReportSecrets.clear();
        Properties.clearForCurrentThread();
    }

    @Test
    public void secretTextIsARegisteredValueOrAToken() {
        ReportSecrets.registerValue("s3cr3t-value");
        Assert.assertTrue(SecretScreenMask.isSecret("prefix ", "s3cr3t-value"));
        Assert.assertTrue(SecretScreenMask.isSecret(JWT));
        Assert.assertFalse(SecretScreenMask.isSecret("search term"));
        Assert.assertFalse(SecretScreenMask.isSecret((CharSequence[]) null));
    }

    @Test
    public void withoutSecretElementsTheCaptureRunsUntouched() {
        JsDriver driver = mock(JsDriver.class);
        Assert.assertSame(SecretScreenMask.masked(driver, () -> IMAGE), IMAGE);
        verify(driver, never()).executeScript(anyString(), any());
    }

    @Test
    public void liveSecretElementIsHiddenOnlyDuringTheCapture() {
        JsDriver driver = mock(JsDriver.class);
        WebElement field = mock(WebElement.class);
        SecretScreenMask.register(driver, field);
        SecretScreenMask.register(driver, field);
        AtomicInteger captures = new AtomicInteger();

        byte[] image = SecretScreenMask.masked(driver, () -> {
            captures.incrementAndGet();
            return IMAGE;
        });

        Assert.assertSame(image, IMAGE);
        Assert.assertEquals(captures.get(), 1);
        InOrder order = Mockito.inOrder(driver);
        order.verify(driver).executeScript(contains("transparent"), Mockito.eq(List.of(field)));
        order.verify(driver).executeScript(contains("removeAttribute"), Mockito.eq(List.of(field)), any());
    }

    @Test
    public void nestedCapturesMaskOnce() {
        JsDriver driver = mock(JsDriver.class);
        SecretScreenMask.register(driver, mock(WebElement.class));
        SecretScreenMask.masked(driver, () -> SecretScreenMask.masked(driver, () -> IMAGE));
        verify(driver, times(1)).executeScript(contains("transparent"), any());
    }

    @Test
    public void secretsBelongToTheirDriverSessionAndAreForgottenOnClose() {
        JsDriver owner = mock(JsDriver.class);
        JsDriver other = mock(JsDriver.class);
        SecretScreenMask.register(owner, mock(WebElement.class));
        SecretScreenMask.register(null, mock(WebElement.class));
        SecretScreenMask.masked(other, () -> IMAGE);
        verify(other, never()).executeScript(anyString(), any());
        SecretScreenMask.forget(owner);
        SecretScreenMask.forget(null);
        SecretScreenMask.masked(owner, () -> IMAGE);
        verify(owner, never()).executeScript(anyString(), any());
    }

    @Test
    public void staleSecretElementsArePruned() {
        JsDriver driver = mock(JsDriver.class);
        WebElement gone = mock(WebElement.class);
        when(gone.isEnabled()).thenThrow(new StaleElementReferenceException("gone"));
        SecretScreenMask.register(driver, gone);
        Assert.assertSame(SecretScreenMask.masked(driver, () -> IMAGE), IMAGE);
        verify(driver, never()).executeScript(anyString(), any());
    }

    @Test
    public void captureIsDroppedWhenALiveSecretCannotBeHidden() {
        JsDriver driver = mock(JsDriver.class);
        when(driver.executeScript(contains("transparent"), any())).thenThrow(new WebDriverException("no js"));
        SecretScreenMask.register(driver, mock(WebElement.class));
        AtomicInteger captures = new AtomicInteger();
        byte[] image = SecretScreenMask.masked(driver, () -> {
            captures.incrementAndGet();
            return IMAGE;
        });
        Assert.assertEquals(image.length, 0);
        Assert.assertEquals(captures.get(), 0, "nothing is captured while the secret is visible");
    }

    @Test
    public void withoutJavaScriptTheSecretRectangleIsPaintedBlack() throws IOException {
        WebDriver driver = mock(WebDriver.class, Mockito.RETURNS_DEEP_STUBS);
        when(driver.manage().window().getSize()).thenReturn(new Dimension(50, 40));
        WebElement field = mock(WebElement.class);
        when(field.getRect()).thenReturn(new Rectangle(10, 10, 20, 10));
        SecretScreenMask.register(driver, field);

        byte[] image = SecretScreenMask.masked(driver, () -> whitePng(100, 80));

        BufferedImage picture = ImageIO.read(new ByteArrayInputStream(image));
        Assert.assertEquals(new Color(picture.getRGB(30, 25)), Color.BLACK, "scaled rectangle is covered");
        Assert.assertEquals(new Color(picture.getRGB(5, 5)), Color.WHITE, "the rest stays visible");
        Assert.assertEquals(SecretScreenMask.maskedElement(driver, () -> IMAGE).length, 0,
                "an element screenshot cannot be painted reliably, so it is dropped");
    }

    @Test
    public void playwrightMaskKeepsOnlyLocatorsOfTheCapturedPage() {
        Page page = mock(Page.class);
        Page other = mock(Page.class);
        Locator here = mock(Locator.class);
        Locator elsewhere = mock(Locator.class);
        when(here.page()).thenReturn(page);
        when(elsewhere.page()).thenReturn(other);
        SecretScreenMask.register(here);
        SecretScreenMask.register(elsewhere);
        Assert.assertEquals(SecretScreenMask.playwrightMask(page), List.of(here));
    }

    @Test
    public void registeringWhileRecordingVideoStillRegisters() {
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);
        JsDriver driver = mock(JsDriver.class);
        SecretScreenMask.register(driver, mock(WebElement.class));
        SecretScreenMask.register(driver, mock(WebElement.class));
        SecretScreenMask.masked(driver, () -> IMAGE);
        verify(driver).executeScript(contains("transparent"), any());
    }

    private static byte[] whitePng(int width, int height) {
        BufferedImage picture = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        java.awt.Graphics2D graphics = picture.createGraphics();
        graphics.setColor(Color.WHITE);
        graphics.fillRect(0, 0, width, height);
        graphics.dispose();
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try {
            ImageIO.write(picture, "png", out);
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
        return out.toByteArray();
    }

    @Test
    public void edgeInputsAreIgnored() {
        Assert.assertFalse(SecretScreenMask.isSecret((CharSequence[]) null));
        Assert.assertFalse(SecretScreenMask.isSecret("plain", null));
        Assert.assertFalse(com.shaft.tools.io.internal.ReportSecrets.containsSecret(null));
        Assert.assertFalse(com.shaft.tools.io.internal.ReportSecrets.containsSecret(""));
        JsDriver driver = mock(JsDriver.class);
        SecretScreenMask.register(driver, null);
        SecretScreenMask.register((com.microsoft.playwright.Locator) null);
        Assert.assertSame(SecretScreenMask.masked(driver, () -> IMAGE), IMAGE);
        Assert.assertSame(SecretScreenMask.masked(null, () -> IMAGE), IMAGE);
    }

    @Test
    public void wrappedDriversShareTheSecretsOfTheirSession() {
        JsDriver inner = mock(JsDriver.class);
        WrappingDriver outer = mock(WrappingDriver.class);
        when(outer.getWrappedDriver()).thenReturn(inner);
        SecretScreenMask.register(outer, mock(WebElement.class));
        SecretScreenMask.masked(inner, () -> IMAGE);
        verify(inner).executeScript(contains("transparent"), any());
        SecretScreenMask.forget(outer);
        SecretScreenMask.masked(inner, () -> IMAGE);
        verify(inner, times(1)).executeScript(contains("transparent"), any());
    }

    @Test
    public void elementsOfAClosedSessionOrUnknownStateAreHandled() {
        JsDriver driver = mock(JsDriver.class);
        WebElement closed = mock(WebElement.class);
        when(closed.isEnabled()).thenThrow(new org.openqa.selenium.NoSuchSessionException("gone"));
        WebElement odd = mock(WebElement.class);
        when(odd.isEnabled()).thenThrow(new IllegalStateException("odd"));
        SecretScreenMask.register(driver, closed);
        SecretScreenMask.register(driver, odd);
        when(driver.executeScript(contains("removeAttribute"), any(), any())).thenThrow(new WebDriverException("restore"));
        Assert.assertSame(SecretScreenMask.masked(driver, () -> IMAGE), IMAGE);
        verify(driver).executeScript(contains("transparent"), any());
    }

    @Test
    public void paintingOverAnUnreadableImageDropsIt() {
        WebDriver driver = mock(WebDriver.class, org.mockito.Answers.RETURNS_DEEP_STUBS);
        SecretScreenMask.register(driver, mock(WebElement.class));
        Assert.assertEquals(SecretScreenMask.masked(driver, () -> new byte[]{1, 2, 3}).length, 0);
        Assert.assertEquals(SecretScreenMask.masked(driver, () -> new byte[0]).length, 0);
    }

    interface WrappingDriver extends WebDriver, org.openqa.selenium.WrapsDriver {
    }
}
