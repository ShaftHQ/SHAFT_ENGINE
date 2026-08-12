package com.shaft.gui.internal.ocr;

import com.microsoft.playwright.Mouse;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.options.ViewportSize;
import com.shaft.gui.driver.ElementActionsContract;
import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrRectangle;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTarget;
import com.shaft.gui.ocr.OcrTextBlock;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.interactions.Interactive;
import org.openqa.selenium.interactions.Sequence;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import io.appium.java_client.AppiumDriver;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class OcrGuiActionContractTest {
    private byte[] screenshot;

    @BeforeMethod
    public void installProvider() throws Exception {
        BufferedImage image = new BufferedImage(2000, 1000, BufferedImage.TYPE_INT_RGB);
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        ImageIO.write(image, "png", output);
        screenshot = output.toByteArray();
        OcrProcessingProviderRegistry.setProvidersForTesting(List.of(new OcrProcessingProvider() {
            @Override
            public OcrResult recognize(byte[] image, OcrOptions options) {
                return new OcrResult("Pay now", List.of(new OcrTextBlock(
                        "Pay now", new OcrRectangle(800, 400, 400, 200), 0.96, OcrBlockLevel.LINE)));
            }
        }));
    }

    @AfterMethod
    public void clearProvider() {
        OcrProcessingProviderRegistry.clearProviderForTesting();
    }

    @Test
    public void publicElementContractExposesCoordinateSafeOcrActions() throws Exception {
        Assert.assertNotNull(ElementActionsContract.class.getMethod("click", OcrTarget.class));
        Assert.assertNotNull(ElementActionsContract.class.getMethod("doubleClick", OcrTarget.class));
        Assert.assertNotNull(ElementActionsContract.class.getMethod("hover", OcrTarget.class));
    }

    @Test
    @SuppressWarnings("unchecked")
    public void webDriverActionDispatchesW3cPointerAtScaledMatchCenter() {
        WebDriver driver = mock(WebDriver.class, Mockito.withSettings()
                .extraInterfaces(TakesScreenshot.class, Interactive.class, JavascriptExecutor.class));
        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(screenshot);
        when(((JavascriptExecutor) driver).executeScript("return [window.innerWidth, window.innerHeight];"))
                .thenReturn(List.of(1000L, 500L));

        OcrWebDriverPointerActions.click(driver, OcrTarget.exact("Pay now"));

        ArgumentCaptor<Collection<Sequence>> captor = ArgumentCaptor.forClass(Collection.class);
        verify((Interactive) driver).perform(captor.capture());
        List<Map<String, Object>> actions = (List<Map<String, Object>>) captor.getValue().iterator().next().encode().get("actions");
        Map<String, Object> move = actions.getFirst();
        Assert.assertEquals(((Number) move.get("x")).intValue(), 500);
        Assert.assertEquals(((Number) move.get("y")).intValue(), 250);
    }

    @Test
    public void playwrightElementActionClicksScaledMatchCenter() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        Mouse mouse = mock(Mouse.class);
        when(session.page()).thenReturn(page);
        when(page.screenshot()).thenReturn(screenshot);
        when(page.viewportSize()).thenReturn(new ViewportSize(1000, 500));
        when(page.mouse()).thenReturn(mouse);

        new com.shaft.gui.playwright.element.ElementActions(session).click(OcrTarget.exact("Pay now"));

        verify(mouse).click(anyDouble(), anyDouble());
        verify(mouse).click(500.0, 250.0);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void appiumActionUsesScreenshotCoordinatesAndTouchInputWithoutBrowserJavascript() {
        AppiumDriver driver = mock(AppiumDriver.class);
        when(driver.getScreenshotAs(OutputType.BYTES)).thenReturn(screenshot);

        OcrWebDriverPointerActions.click(driver, OcrTarget.exact("Pay now"));

        ArgumentCaptor<Collection<Sequence>> captor = ArgumentCaptor.forClass(Collection.class);
        verify(driver).perform(captor.capture());
        List<Map<String, Object>> actions = (List<Map<String, Object>>) captor.getValue().iterator().next().encode().get("actions");
        Assert.assertEquals(actions.getFirst().get("x"), 1000);
        Assert.assertEquals(actions.getFirst().get("y"), 500);
        Assert.assertEquals(captor.getValue().iterator().next().encode().get("type"), "pointer");
        Map<String, Object> parameters = (Map<String, Object>) captor.getValue().iterator().next().encode().get("parameters");
        Assert.assertEquals(parameters.get("pointerType"), "touch");
        verify(driver, Mockito.never()).executeScript(Mockito.anyString(), Mockito.any(Object[].class));
    }
}
