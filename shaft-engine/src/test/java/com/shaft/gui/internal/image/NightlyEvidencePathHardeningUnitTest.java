package com.shaft.gui.internal.image;

import com.shaft.driver.internal.DriverFactory.EmulatedDeviceProfiles;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotHelper;
import org.mockito.Mockito;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.Rectangle;
import org.testng.Assert;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class NightlyEvidencePathHardeningUnitTest {

    @Test
    public void makeFullScreenshotWithNonJavascriptExecutorMockDoesNotClassCast() throws Exception {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        byte[] pngBytes = pngBytes(8, 8, Color.BLUE);
        Mockito.when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(pngBytes);
        byte[] out = ScreenshotHelper.makeFullScreenshot(driver);
        Assert.assertNotNull(out);
    }

    @Test
    public void highlightElementInScreenshotReturnsOriginalBytesWhenUndecodable() {
        byte[] junk = {1, 2, 3, 4, 5};
        byte[] out = ImageProcessingActions.highlightElementInScreenshot(junk, new Rectangle(0, 0, 2, 2), Color.RED);
        Assert.assertEquals(out, junk);
    }

    @Test
    public void emulatedDeviceProfilesPinsHistoricalPixel2AndPixel5() {
        Assert.assertTrue(EmulatedDeviceProfiles.of("Pixel 2").isPresent());
        Assert.assertTrue(EmulatedDeviceProfiles.of("Pixel 5").isPresent());
        Assert.assertEquals(EmulatedDeviceProfiles.of("pixel 2").orElseThrow().deviceName(), "Pixel 2");
    }

    private static byte[] pngBytes(int w, int h, Color c) throws IOException {
        BufferedImage image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics2D = image.createGraphics();
        graphics2D.setColor(c);
        graphics2D.fillRect(0, 0, w, h);
        graphics2D.dispose();
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        ImageIO.write(image, "png", byteArrayOutputStream);
        return byteArrayOutputStream.toByteArray();
    }
}
