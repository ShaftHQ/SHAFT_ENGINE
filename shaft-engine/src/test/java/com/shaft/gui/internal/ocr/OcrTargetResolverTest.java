package com.shaft.gui.internal.ocr;

import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrMatch;
import com.shaft.gui.ocr.OcrRectangle;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTarget;
import com.shaft.gui.ocr.OcrTextBlock;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class OcrTargetResolverTest {
    @Test
    public void resolvesExactTextAfterWhitespaceAndCaseNormalization() {
        OcrResult result = result(
                line("  CHECKOUT   NOW ", 10, 20, 140, 24, 0.94),
                word("CHECKOUT", 10, 20, 90, 24, 0.96),
                word("NOW", 110, 20, 40, 24, 0.92));

        OcrMatch match = OcrTargetResolver.resolve(result, OcrTarget.exact("checkout now"));

        Assert.assertEquals(match.text(), "CHECKOUT NOW");
        Assert.assertEquals(match.bounds(), new OcrRectangle(10, 20, 140, 24));
        Assert.assertEquals(match.confidence(), 0.94);
    }

    @Test
    public void partialTextUsesSmallestConsecutiveWordUnion() {
        OcrResult result = result(
                line("Proceed to checkout now", 5, 10, 230, 30, 0.90),
                word("Proceed", 5, 10, 60, 30, 0.95),
                word("to", 70, 10, 18, 30, 0.94),
                word("checkout", 95, 10, 90, 30, 0.93),
                word("now", 195, 10, 40, 30, 0.92));

        OcrMatch match = OcrTargetResolver.resolve(result, OcrTarget.containing("checkout no"));

        Assert.assertEquals(match.bounds(), new OcrRectangle(95, 10, 140, 30));
        Assert.assertEquals(match.text(), "checkout now");
        Assert.assertEquals(match.confidence(), 0.92);
    }

    @Test
    public void ambiguityRequiresExplicitOccurrenceInReadingOrder() {
        OcrResult result = result(
                line("Save", 200, 100, 50, 20, 0.96),
                line("Save", 10, 20, 50, 20, 0.97));

        IllegalStateException ambiguity = Assert.expectThrows(IllegalStateException.class,
                () -> OcrTargetResolver.resolve(result, OcrTarget.exact("Save")));
        Assert.assertTrue(ambiguity.getMessage().contains("2 OCR matches"));

        OcrMatch second = OcrTargetResolver.resolve(result, OcrTarget.exact("Save").occurrence(1));
        Assert.assertEquals(second.bounds(), new OcrRectangle(200, 100, 50, 20));
    }

    @Test
    public void duplicatePartialTextOnOneLineRequiresExplicitOccurrence() {
        OcrResult result = result(
                line("Save Save", 10, 20, 120, 20, 0.96),
                word("Save", 10, 20, 45, 20, 0.97),
                word("Save", 80, 20, 45, 20, 0.95));

        IllegalStateException ambiguity = Assert.expectThrows(IllegalStateException.class,
                () -> OcrTargetResolver.resolve(result, OcrTarget.containing("Save")));
        Assert.assertTrue(ambiguity.getMessage().contains("2 OCR matches"));
        Assert.assertEquals(OcrTargetResolver.resolve(result, OcrTarget.containing("Save").occurrence(1)).bounds(),
                new OcrRectangle(80, 20, 45, 20));
    }

    @Test
    public void excludesLowConfidenceAndReportsThreshold() {
        OcrResult result = result(line("Pay now", 10, 20, 80, 20, 0.55));

        IllegalStateException notFound = Assert.expectThrows(IllegalStateException.class,
                () -> OcrTargetResolver.resolve(result, OcrTarget.exact("Pay now").minimumConfidence(0.80)));

        Assert.assertTrue(notFound.getMessage().contains("0.8"));
        Assert.assertTrue(notFound.getMessage().contains("Pay now"));
    }

    private static OcrResult result(OcrTextBlock... blocks) {
        return new OcrResult("", List.of(blocks));
    }

    private static OcrTextBlock line(String text, int x, int y, int width, int height, double confidence) {
        return new OcrTextBlock(text, new OcrRectangle(x, y, width, height), confidence, OcrBlockLevel.LINE);
    }

    private static OcrTextBlock word(String text, int x, int y, int width, int height, double confidence) {
        return new OcrTextBlock(text, new OcrRectangle(x, y, width, height), confidence, OcrBlockLevel.WORD);
    }
}
