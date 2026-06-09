package com.shaft.gui.internal.image;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.Collections;
import java.util.List;

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
