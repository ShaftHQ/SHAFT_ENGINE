package com.shaft.gui.internal.image;

import com.epam.healenium.SelfHealingDriver;
import com.shaft.driver.SHAFT;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.internal.ElementActionsHelper;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.locators.RelativeLocator;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ScreenshotManagerCoverageUnitTest {
    private String whenToTakeScreenshot;
    private String screenshotType;
    private String skippedElementsFromScreenshot;
    private boolean highlightElements;
    private String highlightMethod;
    private boolean createAnimatedGif;
    private boolean watermark;

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() {
        whenToTakeScreenshot = SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot();
        screenshotType = SHAFT.Properties.visuals.screenshotParamsScreenshotType();
        skippedElementsFromScreenshot = SHAFT.Properties.visuals.screenshotParamsSkippedElementsFromScreenshot();
        highlightElements = SHAFT.Properties.visuals.screenshotParamsHighlightElements();
        highlightMethod = SHAFT.Properties.visuals.screenshotParamsHighlightMethod();
        createAnimatedGif = SHAFT.Properties.visuals.createAnimatedGif();
        watermark = SHAFT.Properties.visuals.screenshotParamsWatermark();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot(whenToTakeScreenshot);
        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(screenshotType);
        SHAFT.Properties.visuals.set().screenshotParamsSkippedElementsFromScreenshot(skippedElementsFromScreenshot);
        SHAFT.Properties.visuals.set().screenshotParamsHighlightElements(highlightElements);
        SHAFT.Properties.visuals.set().screenshotParamsHighlightMethod(highlightMethod);
        SHAFT.Properties.visuals.set().createAnimatedGif(createAnimatedGif);
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(watermark);
    }

    @Test
    public void takeScreenshotAndPrepareImagePathsShouldBeCovered() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Always");
        SHAFT.Properties.visuals.set().screenshotParamsHighlightMethod("AI");
        SHAFT.Properties.visuals.set().screenshotParamsHighlightElements(false);
        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("VIEWPORT");
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false);
        SHAFT.Properties.visuals.set().createAnimatedGif(false);

        WebDriver driver = mock(WebDriver.class);

        try (MockedConstruction<ElementActionsHelper> ignored = Mockito.mockConstruction(ElementActionsHelper.class);
             MockedStatic<ScreenshotHelper> screenshotHelperMocked = Mockito.mockStatic(ScreenshotHelper.class);
             MockedStatic<AnimatedGifManager> animatedGifMocked = Mockito.mockStatic(AnimatedGifManager.class)) {
            byte[] png = createPng(10, 10, Color.BLUE);
            screenshotHelperMocked.when(() -> ScreenshotHelper.takeViewportScreenshot(any(WebDriver.class), anyInt()))
                    .thenReturn(png);
            animatedGifMocked.when(() -> AnimatedGifManager.startOrAppendToAnimatedGif(any(byte[].class))).thenAnswer(i -> null);

            ScreenshotManager manager = new ScreenshotManager();

            List<Object> report = manager.takeScreenshot(driver, null, "click", true);
            Assert.assertEquals(report.get(0), "Screenshot");
            Assert.assertTrue(((String) report.get(1)).startsWith("click_"));

            List<Object> notNeeded = manager.takeScreenshot(driver, null, "navigate", true);
            Assert.assertFalse(notNeeded.isEmpty());

            SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
            notNeeded = manager.takeScreenshot(driver, null, "navigate", true);
            Assert.assertTrue(notNeeded.isEmpty());

            Assert.assertNull(manager.prepareImageForReport(new byte[0], "empty"));
            Assert.assertNull(manager.prepareImageForReport(null, "null"));
        }
    }

    @Test
    public void takeScreenshotFallbackAndElementBranchesShouldBeCovered() throws Exception {
        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("FULL");
        SHAFT.Properties.visuals.set().screenshotParamsHighlightMethod("AI");
        SHAFT.Properties.visuals.set().screenshotParamsHighlightElements(false);
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false);

        WebDriver delegateDriver = mock(WebDriver.class);
        SelfHealingDriver healingDriver = mock(SelfHealingDriver.class);
        when(healingDriver.getDelegate()).thenReturn(delegateDriver);

        WebElement element = mock(WebElement.class);
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(new byte[]{9});

        try (MockedConstruction<ElementActionsHelper> helperMocked = Mockito.mockConstruction(ElementActionsHelper.class,
                (mock, context) -> {
                    when(mock.getElementsCount(any(WebDriver.class), any(By.class))).thenReturn(1, 0);
                    when(mock.identifyUniqueElementIgnoringVisibility(any(WebDriver.class), any(By.class)))
                            .thenReturn(List.of(1, element, By.id("x"), "", "", "", "", new Rectangle(1, 2, 3, 4)));
                });
             MockedStatic<ScreenshotHelper> screenshotHelperMocked = Mockito.mockStatic(ScreenshotHelper.class);
             MockedStatic<AnimatedGifManager> animatedGifMocked = Mockito.mockStatic(AnimatedGifManager.class)) {
            byte[] viewport = createPng(20, 20, Color.RED);
            screenshotHelperMocked.when(() -> ScreenshotHelper.makeFullScreenshot(any(WebDriver.class), any(WebElement[].class)))
                    .thenThrow(new RuntimeException("force full failure"));
            screenshotHelperMocked.when(() -> ScreenshotHelper.takeViewportScreenshot(any(WebDriver.class), anyInt()))
                    .thenReturn(viewport);
            animatedGifMocked.when(() -> AnimatedGifManager.startOrAppendToAnimatedGif(any(byte[].class))).thenAnswer(i -> null);

            ScreenshotManager manager = new ScreenshotManager();
            byte[] screenshot = manager.takeScreenshot(healingDriver, By.id("x"));
            Assert.assertTrue(Arrays.equals(screenshot, viewport));
            Assert.assertEquals(SHAFT.Properties.visuals.screenshotParamsScreenshotType(), "VIEWPORT");

            byte[] elementShot = manager.takeElementScreenshot(delegateDriver, By.id("x"));
            Assert.assertTrue(Arrays.equals(elementShot, new byte[]{9}));

            Method elementPrivate = ScreenshotManager.class.getDeclaredMethod("takeElementScreenshot", WebDriver.class, By.class, Boolean.class);
            elementPrivate.setAccessible(true);
            byte[] fallbackElement = (byte[]) elementPrivate.invoke(manager, delegateDriver, By.id("x"), true);
            Assert.assertTrue(Arrays.equals(fallbackElement, viewport));

            ElementActionsHelper helper = helperMocked.constructed().getFirst();
            when(helper.getElementsCount(any(WebDriver.class), any(By.class))).thenThrow(new RuntimeException("boom"));
            byte[] noFallback = (byte[]) elementPrivate.invoke(manager, delegateDriver, By.id("x"), false);
            Assert.assertEquals(noFallback.length, 0);
        }
    }

    @Test
    public void fullPageAndHighlightMethodsShouldBeCovered() throws Exception {
        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("VIEWPORT");
        SHAFT.Properties.visuals.set().screenshotParamsSkippedElementsFromScreenshot("//div[@id='hide']");
        SHAFT.Properties.visuals.set().screenshotParamsHighlightElements(true);
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false);
        SHAFT.Properties.visuals.set().createAnimatedGif(false);

        WebDriver driver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        WebElement element = mock(WebElement.class);
        when(element.getDomProperty(anyString())).thenReturn("display:block;");
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(createPng(12, 12, Color.BLACK));
        when(((JavascriptExecutor) driver).executeScript(anyString(), any(), any())).thenReturn(null);

        try (MockedConstruction<ElementActionsHelper> helperMocked = Mockito.mockConstruction(ElementActionsHelper.class,
                (mock, context) -> {
                    when(mock.getElementsCount(any(WebDriver.class), any(By.class))).thenReturn(1);
                    when(mock.identifyUniqueElementIgnoringVisibility(any(WebDriver.class), any(By.class)))
                            .thenReturn(List.of(1, element, By.id("x"), "", "", "", "", new Rectangle(10, 10, 25, 25)));
                });
             MockedStatic<ScreenshotHelper> screenshotHelperMocked = Mockito.mockStatic(ScreenshotHelper.class);
             MockedStatic<AnimatedGifManager> animatedGifMocked = Mockito.mockStatic(AnimatedGifManager.class);
             MockedStatic<ImageProcessingActions> imageProcessingMocked = Mockito.mockStatic(ImageProcessingActions.class);
             MockedStatic<JavaScriptWaitManager> javaScriptWaitManagerMocked = Mockito.mockStatic(JavaScriptWaitManager.class)) {
            byte[] viewport = createPng(30, 30, Color.GREEN);
            screenshotHelperMocked.when(() -> ScreenshotHelper.takeViewportScreenshot(any(WebDriver.class), anyInt()))
                    .thenReturn(viewport);
            screenshotHelperMocked.when(() -> ScreenshotHelper.makeFullScreenshot(any(WebDriver.class), any(WebElement[].class)))
                    .thenReturn(createPng(40, 40, Color.ORANGE));
            screenshotHelperMocked.when(() -> ScreenshotHelper.makeFullScreenshot(any(WebDriver.class)))
                    .thenReturn(createPng(50, 50, Color.CYAN));
            animatedGifMocked.when(() -> AnimatedGifManager.startOrAppendToAnimatedGif(any(byte[].class))).thenAnswer(i -> null);
            imageProcessingMocked.when(() -> ImageProcessingActions.highlightElementInScreenshot(any(byte[].class), any(Rectangle.class), any(Color.class)))
                    .thenReturn(createPng(10, 10, Color.MAGENTA));
            javaScriptWaitManagerMocked.when(() -> JavaScriptWaitManager.waitForLazyLoading(any(WebDriver.class)))
                    .thenThrow(new RuntimeException("ignore wait failure"));

            ScreenshotManager manager = new ScreenshotManager();

            Method full = ScreenshotManager.class.getDeclaredMethod("takeFullPageScreenshot", WebDriver.class);
            full.setAccessible(true);
            Method typed = ScreenshotManager.class.getDeclaredMethod("takeScreenshot", WebDriver.class, By.class, Screenshots.class);
            typed.setAccessible(true);

            byte[] skippedElements = (byte[]) full.invoke(manager, driver);
            Assert.assertNotNull(skippedElements);

            SHAFT.Properties.visuals.set().screenshotParamsSkippedElementsFromScreenshot("");
            byte[] directFull = (byte[]) full.invoke(manager, driver);
            Assert.assertNotNull(directFull);

            byte[] typedViewport = (byte[]) typed.invoke(manager, driver, By.id("x"), Screenshots.VIEWPORT);
            byte[] typedElement = (byte[]) typed.invoke(manager, driver, By.id("x"), Screenshots.ELEMENT);
            byte[] typedFull = (byte[]) typed.invoke(manager, driver, By.id("x"), Screenshots.FULL);
            Assert.assertTrue(typedViewport.length > 0);
            Assert.assertTrue(typedElement.length > 0);
            Assert.assertTrue(typedFull.length > 0);

            SHAFT.Properties.visuals.set().screenshotParamsHighlightMethod("AI");
            byte[] aiPass = manager.internalCaptureScreenshot(driver, By.id("x"), true);
            byte[] aiFail = manager.internalCaptureScreenshot(driver, RelativeLocator.with(By.tagName("div")).above(By.id("x")), false);
            Assert.assertTrue(aiPass.length > 0);
            Assert.assertTrue(aiFail.length > 0);

            SHAFT.Properties.visuals.set().screenshotParamsHighlightMethod("JavaScript");
            byte[] jsShot = manager.internalCaptureScreenshot(driver, By.id("x"), true);
            Assert.assertTrue(jsShot.length > 0);

            screenshotHelperMocked.when(() -> ScreenshotHelper.takeViewportScreenshot(any(WebDriver.class), anyInt()))
                    .thenThrow(new WebDriverException("cannot capture"));
            byte[] failed = manager.internalCaptureScreenshot(driver, By.id("x"), true);
            Assert.assertEquals(failed.length, 0);

            Assert.assertTrue(helperMocked.constructed().size() >= 1);
        }
    }

    private byte[] createPng(int width, int height, Color color) {
        try {
            BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
            Graphics2D graphics = image.createGraphics();
            graphics.setColor(color);
            graphics.fillRect(0, 0, width, height);
            graphics.dispose();

            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            ImageIO.write(image, "png", outputStream);
            return outputStream.toByteArray();
        } catch (Exception e) {
            throw new AssertionError(e);
        }
    }
}
