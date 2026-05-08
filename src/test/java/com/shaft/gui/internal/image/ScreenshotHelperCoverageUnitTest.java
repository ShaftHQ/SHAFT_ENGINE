package com.shaft.gui.internal.image;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chromium.HasCdp;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Base64;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class ScreenshotHelperCoverageUnitTest {
    private static final String AI_PATH_FIELD = "AI_AIDED_ELEMENT_IDENTIFICATION_FOLDER_PATH";
    private static final String SHAFT_LOGO_FIELD = "shaftLogo";

    @BeforeMethod(alwaysRun = true)
    public void setUp() throws Exception {
        Properties.clearForCurrentThread();
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false).screenshotParamsWatermarkOpacity(1.0f);
        SHAFT.Properties.platform.set().targetPlatform("LINUX");
        SHAFT.Properties.web.set().targetBrowserName("chrome");
        SHAFT.Properties.mobile.set().browserName("chrome").platformVersion("");
        setStaticField(AI_PATH_FIELD, "");
        setStaticField(SHAFT_LOGO_FIELD, null);
        System.clearProperty("watermarkImagePath");
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() throws Exception {
        Properties.clearForCurrentThread();
        setStaticField(AI_PATH_FIELD, "");
        setStaticField(SHAFT_LOGO_FIELD, null);
        System.clearProperty("watermarkImagePath");
    }

    @Test
    public void constructorShouldThrowIllegalStateException() throws Exception {
        Constructor<ScreenshotHelper> constructor = ScreenshotHelper.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        InvocationTargetException exception = Assert.expectThrows(InvocationTargetException.class, constructor::newInstance);
        Assert.assertTrue(exception.getCause() instanceof IllegalStateException);
    }

    @Test
    public void aiAidedFolderPathShouldHandleWebMobileAndCachedPaths() throws Exception {
        setStaticField(AI_PATH_FIELD, "");
        SHAFT.Properties.platform.set().targetPlatform("LINUX");
        SHAFT.Properties.web.set().targetBrowserName("Google Chrome");
        SHAFT.Properties.mobile.set().browserName("chrome").platformVersion("");

        String webPath = ScreenshotHelper.getAiAidedElementIdentificationFolderPath();
        Assert.assertTrue(webPath.endsWith("LINUX/Google_Chrome/"));
        Assert.assertTrue(ScreenshotHelper.getAiAidedElementIdentificationFolderPath().endsWith("LINUX/Google Chrome/"));

        setStaticField(AI_PATH_FIELD, "");
        SHAFT.Properties.platform.set().targetPlatform("ANDROID");
        SHAFT.Properties.mobile.set().browserName("").platformVersion("13.0");
        String mobilePath = ScreenshotHelper.getAiAidedElementIdentificationFolderPath();
        Assert.assertTrue(mobilePath.endsWith("ANDROID/13_0/"));
    }

    @Test
    public void overlayAndToBufferedImageShouldCoverWatermarkAndConversionBranches() throws Exception {
        BufferedImage screenshot = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
        Graphics2D graphics = screenshot.createGraphics();
        graphics.setColor(Color.WHITE);
        graphics.fillRect(0, 0, 100, 100);
        graphics.dispose();

        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false);
        Assert.assertSame(ScreenshotHelper.overlayShaftEngineLogo(screenshot), screenshot);

        SHAFT.Properties.visuals.set().screenshotParamsWatermark(true).screenshotParamsWatermarkOpacity(1.0f);
        setStaticField(SHAFT_LOGO_FIELD, new BufferedImage(10, 10, BufferedImage.TYPE_INT_ARGB));
        BufferedImage watermarked = ScreenshotHelper.overlayShaftEngineLogo(screenshot);
        Assert.assertNotNull(watermarked);

        setStaticField(SHAFT_LOGO_FIELD, null);
        System.setProperty("watermarkImagePath", Path.of("target", "temp", "missing-watermark.png").toUri().toString());
        Assert.assertSame(ScreenshotHelper.overlayShaftEngineLogo(screenshot), screenshot);

        BufferedImage alreadyBuffered = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB);
        Assert.assertSame(ScreenshotHelper.toBufferedImage(alreadyBuffered), alreadyBuffered);

        Image scaledImage = alreadyBuffered.getScaledInstance(6, 6, Image.SCALE_DEFAULT);
        BufferedImage converted = ScreenshotHelper.toBufferedImage(scaledImage);
        Assert.assertEquals(converted.getWidth(), 6);
        Assert.assertEquals(converted.getHeight(), 6);
    }

    @Test
    public void takeViewportScreenshotShouldCoverRetryAndFailureBranches() {
        WebDriver retryDriver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        WebDriver.TargetLocator targetLocator = Mockito.mock(WebDriver.TargetLocator.class);
        when(retryDriver.switchTo()).thenReturn(targetLocator);
        when(targetLocator.defaultContent()).thenReturn(retryDriver);
        when(((TakesScreenshot) retryDriver).getScreenshotAs(OutputType.BYTES))
                .thenThrow(new RuntimeException("not connected to DevTools"))
                .thenReturn("ok".getBytes());
        Assert.assertNotNull(ScreenshotHelper.takeViewportScreenshot(retryDriver, 1));

        WebDriver exhaustedDriver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        when(((TakesScreenshot) exhaustedDriver).getScreenshotAs(OutputType.BYTES))
                .thenThrow(new RuntimeException("Permission denied to access property \"pageXOffset\" on cross-origin object"));
        Assert.assertNull(ScreenshotHelper.takeViewportScreenshot(exhaustedDriver, 0));

        WebDriver fatalDriver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        when(((TakesScreenshot) fatalDriver).getScreenshotAs(OutputType.BYTES))
                .thenThrow(new RuntimeException("unexpected screenshot failure"));
        Assert.assertThrows(RuntimeException.class, () -> ScreenshotHelper.takeViewportScreenshot(fatalDriver, 1));
    }

    @Test
    public void makeFullScreenshotShouldCoverMobileCdpAndManualPaths() throws Exception {
        WebDriver mobileDriver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        when(((TakesScreenshot) mobileDriver).getScreenshotAs(OutputType.BYTES)).thenReturn("mobile".getBytes());
        SHAFT.Properties.platform.set().targetPlatform("ANDROID");
        SHAFT.Properties.mobile.set().browserName("").platformVersion("14");
        Assert.assertNotNull(ScreenshotHelper.makeFullScreenshot(mobileDriver));

        SHAFT.Properties.platform.set().targetPlatform("LINUX");
        SHAFT.Properties.mobile.set().browserName("chrome");
        byte[] cdpScreenshot = createPngBytes(20, 20, Color.BLUE);
        String base64 = Base64.getEncoder().encodeToString(cdpScreenshot);
        WebDriver cdpWebDriver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasCdp.class));
        HasCdp cdpDriver = (HasCdp) cdpWebDriver;
        when(cdpDriver.executeCdpCommand(anyString(), any())).thenAnswer(invocation -> {
            String cmd = invocation.getArgument(0);
            if ("Page.getLayoutMetrics".equals(cmd)) {
                return Map.of("contentSize", Map.of("width", 100L, "height", 120L));
            }
            if ("Page.captureScreenshot".equals(cmd)) {
                return Map.of("data", base64);
            }
            return Map.of();
        });
        Assert.assertTrue(Arrays.equals(ScreenshotHelper.makeFullScreenshot(cdpWebDriver), cdpScreenshot));

        WebDriver manualDriver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        WebElement skipElement = Mockito.mock(WebElement.class);
        when(((JavascriptExecutor) manualDriver).executeScript(Mockito.contains("Math.max("))).thenReturn(120L);
        when(((JavascriptExecutor) manualDriver).executeScript(Mockito.contains("window.devicePixelRatio"))).thenReturn(1.0);
        when(((JavascriptExecutor) manualDriver).executeScript(Mockito.contains("window.pageYOffset")))
                .thenReturn(0L, 50L, 100L, 0L);
        when(((JavascriptExecutor) manualDriver).executeScript(Mockito.contains("window.scrollTo"), any())).thenReturn(null);
        when(((JavascriptExecutor) manualDriver).executeScript(Mockito.contains("document.documentElement.style.overflow = 'hidden';"))).thenReturn(null);
        when(((JavascriptExecutor) manualDriver).executeScript(Mockito.contains("document.documentElement.style.overflow = 'visible';"))).thenReturn(null);
        when(((JavascriptExecutor) manualDriver).executeScript(Mockito.contains("arguments[0].style.display = arguments[1];"), any(), any())).thenReturn(null);

        byte[] viewport1 = createPngBytes(100, 50, Color.RED);
        byte[] viewport2 = createPngBytes(100, 50, Color.GREEN);
        byte[] viewport3 = createPngBytes(100, 50, Color.YELLOW);
        try (MockedConstruction<ScreenshotManager> mocked = Mockito.mockConstruction(ScreenshotManager.class,
                (mock, context) -> when(mock.takeScreenshot(any(), any(), any()))
                        .thenReturn(viewport1, viewport2, viewport3))) {
            byte[] full = ScreenshotHelper.makeFullScreenshot(manualDriver, skipElement);
            BufferedImage fullImage = ImageIO.read(new java.io.ByteArrayInputStream(full));
            Assert.assertNotNull(fullImage);
            Assert.assertEquals(fullImage.getWidth(), 100);
            Assert.assertEquals(fullImage.getHeight(), 120);
        }
    }

    private static void setStaticField(String fieldName, Object value) throws Exception {
        Field field = ScreenshotHelper.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(null, value);
    }

    private static byte[] createPngBytes(int width, int height, Color color) throws Exception {
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        graphics.setColor(color);
        graphics.fillRect(0, 0, width, height);
        graphics.dispose();
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ImageIO.write(image, "png", outputStream);
        return outputStream.toByteArray();
    }
}
