package com.shaft.gui.internal.image;

import com.shaft.gui.image.ImageTarget;
import com.shaft.gui.image.ImageRectangle;
import com.shaft.gui.image.ImageMatchingMode;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.Collections;
import java.util.List;
import java.nio.file.Path;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class VisualProcessingProviderRegistryTest {
    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        VisualProcessingProviderRegistry.resetProviderForTesting();
    }

    @Test
    public void selectProviderShouldReturnEmptyForNoProviders() {
        Assert.assertTrue(VisualProcessingProviderRegistry.selectProvider(Collections.emptyList()).isEmpty());
    }

    @Test
    public void selectProviderShouldReturnTheSingleProvider() {
        VisualProcessingProvider provider = mock(VisualProcessingProvider.class);

        Assert.assertSame(VisualProcessingProviderRegistry.selectProvider(List.of(provider)).orElseThrow(), provider);
    }

    @Test
    public void selectProviderShouldFailWithProvidersSortedByClassName() {
        VisualProcessingProvider zuluProvider = new ZuluProvider();
        VisualProcessingProvider alphaProvider = new AlphaProvider();

        IllegalStateException exception = Assert.expectThrows(IllegalStateException.class,
                () -> VisualProcessingProviderRegistry.selectProvider(List.of(zuluProvider, alphaProvider)));

        String message = exception.getMessage();
        Assert.assertTrue(message.contains("Multiple visual processing providers were found"));
        Assert.assertTrue(message.indexOf(alphaProvider.getClass().getName())
                < message.indexOf(zuluProvider.getClass().getName()));
    }

    @Test
    public void optionalPreloadShouldRemainQuietWhenProviderIsMissing() {
        VisualProcessingProviderRegistry.setProviderForTesting(null);

        ImageProcessingActions.loadOpenCVIfAvailable();
    }

    @Test
    public void optionalPreloadShouldLoadDiscoveredProvider() {
        VisualProcessingProvider provider = mock(VisualProcessingProvider.class);
        VisualProcessingProviderRegistry.setProviderForTesting(provider);

        ImageProcessingActions.loadOpenCVIfAvailable();

        verify(provider).load();
    }

    @Test
    public void legacyProviderShouldFailClosedForTypedConstraints() {
        ImageTarget target = ImageTarget.fromPath(
                Path.of("src", "test", "resources", "testDataFiles", "youtube.png"));
        AlphaProvider provider = new AlphaProvider();

        Assert.expectThrows(UnsupportedOperationException.class,
                () -> provider.findImageMatches(target.minimumConfidence(0.92), new byte[]{1}));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> provider.findImageMatches(target.within(new ImageRectangle(0, 0, 10, 10)), new byte[]{1}));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> provider.findImageMatches(target.matchingMode(ImageMatchingMode.FEATURE), new byte[]{1}));
        Assert.assertTrue(provider.findImageMatches(target, new byte[]{1}).isEmpty());
    }

    private static class AlphaProvider implements VisualProcessingProvider {
        @Override
        public List<Integer> findImageWithinCurrentPage(String referenceImagePath, byte[] currentPageScreenshot) {
            return Collections.emptyList();
        }

        @Override
        public Boolean compareAgainstBaseline(WebDriver driver, By elementLocator, byte[] elementScreenshot,
                                              ImageProcessingActions.VisualValidationEngine visualValidationEngine,
                                              String referenceImagePath, String differencesImagePath) {
            return true;
        }

        @Override
        public void load() {
            // No-op test provider.
        }
    }

    private static class ZuluProvider implements VisualProcessingProvider {
        @Override
        public List<Integer> findImageWithinCurrentPage(String referenceImagePath, byte[] currentPageScreenshot) {
            return Collections.emptyList();
        }

        @Override
        public Boolean compareAgainstBaseline(WebDriver driver, By elementLocator, byte[] elementScreenshot,
                                              ImageProcessingActions.VisualValidationEngine visualValidationEngine,
                                              String referenceImagePath, String differencesImagePath) {
            return true;
        }

        @Override
        public void load() {
            // No-op test provider.
        }
    }
}
