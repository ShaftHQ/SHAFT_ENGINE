package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Locator;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.properties.internal.Properties;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.ValidationsHelper;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import static org.mockito.ArgumentMatchers.anyString;
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
}
