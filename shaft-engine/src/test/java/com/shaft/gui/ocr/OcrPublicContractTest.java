package com.shaft.gui.ocr;

import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class OcrPublicContractTest {
    @Test
    public void locatorFactoriesCreateExactAndPartialTargets() {
        OcrTarget exact = SHAFT.GUI.Locator.hasOcrText("Checkout");
        OcrTarget partial = SHAFT.GUI.Locator.containsOcrText("Check");

        Assert.assertEquals(exact.expectedText(), "Checkout");
        Assert.assertEquals(exact.matchMode(), OcrMatchMode.EXACT);
        Assert.assertEquals(partial.expectedText(), "Check");
        Assert.assertEquals(partial.matchMode(), OcrMatchMode.CONTAINS);
        Assert.assertTrue(exact.requireUniqueMatch());
    }

    @Test
    public void targetTuningIsImmutableAndValidatesOccurrence() {
        OcrTarget original = SHAFT.GUI.Locator.hasOcrText("Checkout");
        OcrTarget tuned = original
                .caseSensitive()
                .minimumConfidence(0.82)
                .languages("English", "Arabic")
                .within(new OcrRectangle(10, 20, 300, 100))
                .occurrence(1);

        Assert.assertFalse(original.options().caseSensitive());
        Assert.assertTrue(tuned.options().caseSensitive());
        Assert.assertEquals(tuned.options().minimumConfidence(), 0.82);
        Assert.assertEquals(tuned.options().languages(), List.of("English", "Arabic"));
        Assert.assertEquals(tuned.options().region(), new OcrRectangle(10, 20, 300, 100));
        Assert.assertEquals(tuned.occurrence(), 1);
        Assert.assertFalse(tuned.requireUniqueMatch());
        Assert.expectThrows(IllegalArgumentException.class, () -> original.occurrence(-1));
    }

    @Test
    public void dataContractsRejectInvalidGeometryAndConfidence() {
        Assert.expectThrows(IllegalArgumentException.class, () -> new OcrRectangle(0, 0, 0, 1));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new OcrTextBlock("text", new OcrRectangle(0, 0, 10, 10), 1.01, OcrBlockLevel.WORD));

        OcrTextBlock block = new OcrTextBlock(
                "Checkout", new OcrRectangle(4, 8, 40, 12), 0.91, OcrBlockLevel.LINE);
        OcrResult result = new OcrResult("Checkout", List.of(block));

        Assert.assertEquals(result.fullText(), "Checkout");
        Assert.assertEquals(result.blocks(), List.of(block));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> result.blocks().add(block));
    }

    @Test
    public void blankTargetsAndInvalidOptionsFailFast() {
        Assert.expectThrows(IllegalArgumentException.class, () -> SHAFT.GUI.Locator.hasOcrText("  "));
        Assert.expectThrows(IllegalArgumentException.class, () -> SHAFT.GUI.Locator.containsOcrText(""));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> OcrOptions.defaults().withMinimumConfidence(-0.01));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> OcrOptions.defaults().withLanguages("English", " "));
    }
}
