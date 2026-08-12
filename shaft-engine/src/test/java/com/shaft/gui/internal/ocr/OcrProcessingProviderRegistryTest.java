package com.shaft.gui.internal.ocr;

import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrResult;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;

public class OcrProcessingProviderRegistryTest {
    @AfterMethod
    public void clearOverride() {
        OcrProcessingProviderRegistry.clearProviderForTesting();
    }

    @Test
    public void selectsHighestPriorityProviderDeterministically() {
        OcrProcessingProvider low = provider("low", 1);
        OcrProcessingProvider high = provider("high", 100);

        Assert.assertSame(OcrProcessingProviderRegistry.selectProvider(List.of(low, high)).orElseThrow(), high);
    }

    @Test
    public void rejectsDuplicateHighestPriorityProviders() {
        IllegalStateException error = Assert.expectThrows(IllegalStateException.class,
                () -> OcrProcessingProviderRegistry.selectProvider(List.of(provider("one", 10), provider("two", 10))));

        Assert.assertTrue(error.getMessage().contains("one"));
        Assert.assertTrue(error.getMessage().contains("two"));
    }

    @Test
    public void missingProviderExplainsOptionalDependency() {
        OcrProcessingProviderRegistry.setProvidersForTesting(List.of());

        IllegalStateException error = Assert.expectThrows(IllegalStateException.class,
                OcrProcessingProviderRegistry::requireProvider);

        Assert.assertTrue(error.getMessage().contains("io.github.shafthq:shaft-ocr"));
    }

    private static OcrProcessingProvider provider(String name, int priority) {
        return new OcrProcessingProvider() {
            @Override
            public OcrResult recognize(byte[] image, OcrOptions options) {
                return new OcrResult("", List.of());
            }

            @Override
            public String name() {
                return name;
            }

            @Override
            public int priority() {
                return priority;
            }
        };
    }
}
