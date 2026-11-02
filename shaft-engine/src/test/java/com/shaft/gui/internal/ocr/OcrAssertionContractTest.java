package com.shaft.gui.internal.ocr;

import com.microsoft.playwright.Locator;
import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrRectangle;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTextBlock;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.playwright.validation.PlaywrightElementValidationsBuilder;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.Validations;
import com.shaft.validation.internal.WebDriverElementValidationsBuilder;
import org.mockito.Mockito;
import org.mockito.MockedStatic;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.By;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.io.InputStream;

import static org.mockito.Mockito.when;

public class OcrAssertionContractTest {
    private static final byte[] IMAGE = {1, 2, 3};

    @BeforeMethod
    public void installProvider() {
        OcrProcessingProviderRegistry.setProvidersForTesting(List.of(new OcrProcessingProvider() {
            @Override
            public OcrResult recognize(byte[] image, OcrOptions options) {
                return new OcrResult("Checkout complete", List.of(new OcrTextBlock(
                        "Checkout complete", new OcrRectangle(1, 2, 30, 10), 0.95, OcrBlockLevel.LINE)));
            }
        }));
    }

    @AfterMethod
    public void clearProvider() {
        OcrProcessingProviderRegistry.clearProviderForTesting();
    }

    @Test
    public void standaloneImageAssertionUsesExistingNativeContainsSyntax() {
        Validations.assertThat().image(IMAGE).ocrText().contains("Checkout");
        Validations.verifyThat().image(IMAGE).ocrText().contains("complete");
    }

    @Test
    public void recognitionAndSelectionAttachDiagnosticEvidence() {
        try (MockedStatic<ReportManagerHelper> reporting = Mockito.mockStatic(ReportManagerHelper.class)) {
            OcrProcessingActions.find(IMAGE, com.shaft.gui.ocr.OcrTarget.containing("Checkout"));

            reporting.verify(() -> ReportManagerHelper.attach(Mockito.eq("screenshot"),
                    Mockito.eq("SHAFT OCR source image"), Mockito.any(InputStream.class)));
            reporting.verify(() -> ReportManagerHelper.attach(Mockito.eq("text"),
                    Mockito.eq("SHAFT OCR recognition details"), Mockito.contains("Checkout complete")));
            reporting.verify(() -> ReportManagerHelper.attach(Mockito.eq("text"),
                    Mockito.eq("SHAFT OCR selected match"), Mockito.contains("Selected match")));
        }
    }

    @Test
    public void webDriverElementAssertionRecognizesElementScreenshot() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        WebElement element = Mockito.mock(WebElement.class);
        when(driver.findElement(By.id("receipt"))).thenReturn(element);
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(IMAGE);

        new WebDriverElementValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                driver, By.id("receipt"), new StringBuilder("the element "))
                .ocrText().contains("Checkout");
    }

    @Test
    public void playwrightElementAssertionRecognizesLocatorScreenshot() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Locator locator = Mockito.mock(Locator.class);
        when(locator.screenshot()).thenReturn(IMAGE);

        new PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                session, locator, "#receipt").ocrText().contains("complete");
    }
}
