package com.shaft.gui.internal.image;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.Collections;
import java.util.List;

public class ImageProcessingActionsPlaywrightVisualTest {
    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        VisualProcessingProviderRegistry.resetProviderForTesting();
    }

    @Test
    public void byteBasedVisualComparisonShouldRouteThroughVisualProvider() {
        CapturingProvider provider = new CapturingProvider(true);
        byte[] screenshot = new byte[]{1, 2, 3};
        VisualProcessingProviderRegistry.setProviderForTesting(provider);

        Boolean result = ImageProcessingActions.compareAgainstBaseline("CSS:#login", screenshot,
                ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV);

        Assert.assertTrue(result);
        Assert.assertEquals(provider.elementLocatorName, "CSS:#login");
        Assert.assertSame(provider.elementScreenshot, screenshot);
        Assert.assertEquals(provider.visualValidationEngine, ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV);
        Assert.assertTrue(provider.referenceImagePath.endsWith(".png"));
        Assert.assertTrue(provider.differencesImagePath.endsWith("_shutterbug"));
        Assert.assertEquals(provider.referenceImagePath.replace(".png", ""),
                provider.differencesImagePath.replace("_shutterbug", ""));
    }

    @Test
    public void byteBasedVisualComparisonShouldKeepMissingProviderMessage() {
        VisualProcessingProviderRegistry.setProviderForTesting(null);

        IllegalStateException exception = Assert.expectThrows(IllegalStateException.class,
                () -> ImageProcessingActions.compareAgainstBaseline("CSS:#login", new byte[]{1},
                        ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV));

        Assert.assertEquals(exception.getMessage(), VisualProcessingProviderRegistry.MISSING_PROVIDER_MESSAGE);
    }

    private static class CapturingProvider implements VisualProcessingProvider {
        private final Boolean result;
        private String elementLocatorName;
        private byte[] elementScreenshot;
        private ImageProcessingActions.VisualValidationEngine visualValidationEngine;
        private String referenceImagePath;
        private String differencesImagePath;

        CapturingProvider(Boolean result) {
            this.result = result;
        }

        @Override
        public List<Integer> findImageWithinCurrentPage(String referenceImagePath, byte[] currentPageScreenshot) {
            return Collections.emptyList();
        }

        @Override
        public Boolean compareAgainstBaseline(WebDriver driver, By elementLocator, byte[] elementScreenshot,
                                              ImageProcessingActions.VisualValidationEngine visualValidationEngine,
                                              String referenceImagePath, String differencesImagePath) {
            return false;
        }

        @Override
        public Boolean compareAgainstBaseline(String elementLocatorName, byte[] elementScreenshot,
                                              ImageProcessingActions.VisualValidationEngine visualValidationEngine,
                                              String referenceImagePath, String differencesImagePath) {
            this.elementLocatorName = elementLocatorName;
            this.elementScreenshot = elementScreenshot;
            this.visualValidationEngine = visualValidationEngine;
            this.referenceImagePath = referenceImagePath;
            this.differencesImagePath = differencesImagePath;
            return result;
        }

        @Override
        public void load() {
            // No-op test provider.
        }
    }
}
