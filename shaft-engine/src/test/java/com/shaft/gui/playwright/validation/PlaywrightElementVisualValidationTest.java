package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Locator;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.ValidationsHelper;
import io.qameta.allure.Allure;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class PlaywrightElementVisualValidationTest {
    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        ValidationsHelper.resetVerificationStateAfterFailing();
        Properties.clearForCurrentThread();
    }

    @Test
    public void matchesReferenceImageShouldCaptureLocatorScreenshotAndUseByteBasedProviderPath() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        byte[] screenshot = new byte[]{1, 2, 3};
        when(locator.screenshot()).thenReturn(screenshot);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");

        try (MockedStatic<ImageProcessingActions> imageProcessingActions = Mockito.mockStatic(ImageProcessingActions.class)) {
            imageProcessingActions.when(() -> ImageProcessingActions.getReferenceImage(anyString()))
                    .thenReturn(null);
            imageProcessingActions.when(() -> ImageProcessingActions.compareAgainstBaseline("CSS:#login", screenshot,
                            ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV))
                    .thenReturn(true);

            new PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                    session, locator, "CSS:#login").matchesReferenceImage();

            verify(locator).screenshot();
            imageProcessingActions.verify(() -> ImageProcessingActions.compareAgainstBaseline("CSS:#login", screenshot,
                    ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV));
        }
    }

    @Test
    public void matchesReferenceImageShouldNotReportVisualEngineInStepTitle() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        byte[] screenshot = new byte[]{1, 2, 3};
        when(locator.screenshot()).thenReturn(screenshot);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");

        try (MockedStatic<ImageProcessingActions> imageProcessingActions = Mockito.mockStatic(ImageProcessingActions.class);
             MockedStatic<ReportManager> reportManager = Mockito.mockStatic(ReportManager.class)) {
            imageProcessingActions.when(() -> ImageProcessingActions.getReferenceImage(anyString()))
                    .thenReturn(null);
            imageProcessingActions.when(() -> ImageProcessingActions.compareAgainstBaseline("Smart Locator: \"Login\"", screenshot,
                            ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV))
                    .thenReturn(true);

            new PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                    session, locator, "Smart Locator: \"Login\"").matchesReferenceImage();

            reportManager.verify(() -> ReportManager.logDiscrete(
                    "Assert that the element \"Login\" matches the reference image."));
        }
    }

    @Test
    public void matchesReferenceImageShouldAttachOnlyVisualComparisonWhenReferenceExists() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        byte[] referenceScreenshot = new byte[]{1, 2, 3};
        byte[] actualScreenshot = new byte[]{1, 2, 4};
        when(locator.screenshot()).thenReturn(actualScreenshot);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");

        try (MockedStatic<ImageProcessingActions> imageProcessingActions = Mockito.mockStatic(ImageProcessingActions.class);
             MockedStatic<Allure> allureMocked = Mockito.mockStatic(Allure.class);
             MockedStatic<ReportManagerHelper> reportManagerHelperMocked = Mockito.mockStatic(ReportManagerHelper.class)) {
            allureMocked.when(Allure::getLifecycle).thenCallRealMethod();
            imageProcessingActions.when(() -> ImageProcessingActions.getReferenceImage("CSS:#login"))
                    .thenReturn(referenceScreenshot);
            imageProcessingActions.when(() -> ImageProcessingActions.compareAgainstBaseline("CSS:#login", actualScreenshot,
                            ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV))
                    .thenReturn(true);
            @SuppressWarnings({"rawtypes", "unchecked"})
            ArgumentCaptor<List<List<Object>>> attachmentsCaptor = (ArgumentCaptor) ArgumentCaptor.forClass(List.class);

            new PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                    session, locator, "CSS:#login").matchesReferenceImage();

            reportManagerHelperMocked.verify(() -> ReportManagerHelper.attach(attachmentsCaptor.capture()));
            Assert.assertTrue(attachmentsCaptor.getValue().isEmpty());
            allureMocked.verify(() -> Allure.addAttachment(eq("Visual Comparison"),
                    eq("application/vnd.allure.image.diff"), anyString()));
        }
    }
}
