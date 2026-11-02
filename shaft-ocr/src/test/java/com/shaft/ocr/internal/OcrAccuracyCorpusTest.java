package com.shaft.ocr.internal;

import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrRectangle;
import com.shaft.gui.ocr.OcrResult;
import org.testng.Assert;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;

/** Deterministic accuracy and latency gate for OCR preprocessing plus the bundled Tesseract backend. */
public class OcrAccuracyCorpusTest {
    private static final double MINIMUM_PRECISION = 1.0;
    private static final double MINIMUM_RECALL = 0.80;
    private static final long MAX_P95_MILLIS = 15_000;

    @Test
    public void calibratedCorpusShouldMeetAccuracyBoundingBoxAndRuntimeBudgets() throws Exception {
        TesseractOcrProvider provider = new TesseractOcrProvider();
        OcrOptions options = OcrOptions.defaults().withLanguages("English").withMinimumConfidence(0);
        List<CorpusCase> corpus = List.of(
                positive("standard", render("SHAFT OCR 42", 68, Color.BLACK, Color.WHITE, 1.0f), "SHAFT OCR 42",
                        new OcrRectangle(37, 55, 483, 51)),
                positive("small-text", render("MOBILE TARGET", 30, Color.BLACK, Color.WHITE, 1.0f), "MOBILE TARGET",
                        new OcrRectangle(37, 83, 242, 22)),
                positive("dark-mode", render("DARK CONTROL", 56, Color.WHITE, new Color(28, 32, 38), 1.0f),
                        "DARK CONTROL", new OcrRectangle(35, 55, 290, 55)),
                positive("semi-transparent", render("ALPHA LABEL", 56, Color.BLACK, Color.WHITE, 0.62f),
                        "ALPHA LABEL", new OcrRectangle(35, 55, 275, 55)),
                negative("blank", render("", 56, Color.BLACK, Color.WHITE, 1.0f))
        );

        // Exclude model provisioning/native initialization from per-image matching latency.
        provider.recognize(render("WARMUP", 48, Color.BLACK, Color.WHITE, 1.0f), options);

        int truePositive = 0;
        int falsePositive = 0;
        int falseNegative = 0;
        List<Long> elapsedMillis = new ArrayList<>();
        List<String> failures = new ArrayList<>();
        for (CorpusCase sample : corpus) {
            long started = System.nanoTime();
            OcrResult result = provider.recognize(sample.image(), options);
            elapsedMillis.add((System.nanoTime() - started) / 1_000_000);
            String normalizedActual = normalize(result.fullText());
            boolean expectedTextFound = sample.expectedText() != null && normalizedActual.equals(sample.expectedText());
            boolean expectedWordBounds = sample.expectedText() != null && result.blocks().stream()
                    .filter(block -> block.level() == OcrBlockLevel.WORD)
                    .filter(block -> sample.expectedText().contains(normalize(block.text())))
                    .map(block -> block.bounds())
                    .reduce(OcrRectangle::union)
                    .filter(bounds -> intersectionOverUnion(bounds, sample.expectedBounds()) >= 0.50)
                    .isPresent();
            if (sample.expectedText() != null && expectedTextFound && expectedWordBounds) {
                truePositive++;
            } else if (sample.expectedText() != null) {
                falseNegative++;
                failures.add(sample.name() + "=false-negative:'" + result.fullText().strip() + "' blocks="
                        + result.blocks().stream().filter(block -> block.level() == OcrBlockLevel.WORD).toList());
                if (!normalizedActual.isBlank() && !normalizedActual.equals(sample.expectedText())) {
                    falsePositive++;
                }
            } else if (!result.fullText().isBlank()) {
                falsePositive++;
                failures.add(sample.name() + "=false-positive:'" + result.fullText().strip() + "'");
            }
        }

        double precision = ratio(truePositive, truePositive + falsePositive);
        double recall = ratio(truePositive, truePositive + falseNegative);
        elapsedMillis.sort(Comparator.naturalOrder());
        long p95Millis = elapsedMillis.get((int) Math.ceil(elapsedMillis.size() * 0.95) - 1);
        String metrics = String.format(Locale.ROOT,
                "precision=%.3f recall=%.3f tp=%d fp=%d fn=%d p95=%dms cases=%s",
                precision, recall, truePositive, falsePositive, falseNegative, p95Millis, failures);
        System.out.println("OCR accuracy corpus: " + metrics);

        Assert.assertTrue(precision >= MINIMUM_PRECISION, metrics);
        Assert.assertTrue(recall >= MINIMUM_RECALL, metrics);
        Assert.assertTrue(p95Millis <= MAX_P95_MILLIS, metrics);
    }

    private static CorpusCase positive(String name, byte[] image, String expectedText, OcrRectangle expectedBounds) {
        return new CorpusCase(name, image, expectedText, expectedBounds);
    }

    private static CorpusCase negative(String name, byte[] image) {
        return new CorpusCase(name, image, null, null);
    }

    private static double ratio(int numerator, int denominator) {
        return denominator == 0 ? 1 : (double) numerator / denominator;
    }

    private static String normalize(String text) {
        return text.toUpperCase(Locale.ROOT).replaceAll("\\s+", " ").strip();
    }

    private static double intersectionOverUnion(OcrRectangle actual, OcrRectangle expected) {
        int left = Math.max(actual.x(), expected.x());
        int top = Math.max(actual.y(), expected.y());
        int right = Math.min(actual.right(), expected.right());
        int bottom = Math.min(actual.bottom(), expected.bottom());
        long intersection = (long) Math.max(0, right - left) * Math.max(0, bottom - top);
        long union = (long) actual.width() * actual.height() + (long) expected.width() * expected.height() - intersection;
        return union == 0 ? 0 : (double) intersection / union;
    }

    private static byte[] render(String text, int fontSize, Color foreground, Color background, float opacity)
            throws Exception {
        BufferedImage image = new BufferedImage(760, 160, BufferedImage.TYPE_INT_ARGB);
        Graphics2D graphics = image.createGraphics();
        try {
            graphics.setColor(background);
            graphics.fillRect(0, 0, image.getWidth(), image.getHeight());
            graphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity));
            graphics.setColor(foreground);
            graphics.setFont(new Font(Font.SANS_SERIF, Font.BOLD, fontSize));
            graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
            graphics.drawString(text, 35, 105);
        } finally {
            graphics.dispose();
        }
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        ImageIO.write(image, "png", output);
        return output.toByteArray();
    }

    private record CorpusCase(String name, byte[] image, String expectedText, OcrRectangle expectedBounds) {
    }
}
