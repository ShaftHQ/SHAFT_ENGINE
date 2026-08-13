package com.shaft.ocr.internal;

import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrOptions;
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
                positive("standard", render("SHAFT OCR 42", 68, Color.BLACK, Color.WHITE, 1.0f), "SHAFT"),
                positive("small-text", render("MOBILE TARGET", 30, Color.BLACK, Color.WHITE, 1.0f), "MOBILE"),
                positive("dark-mode", render("DARK CONTROL", 56, Color.WHITE, new Color(28, 32, 38), 1.0f),
                        "DARK"),
                positive("semi-transparent", render("ALPHA LABEL", 56, Color.BLACK, Color.WHITE, 0.62f),
                        "ALPHA"),
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
            boolean expectedTextFound = sample.expectedFragment() != null
                    && result.fullText().toUpperCase(Locale.ROOT).contains(sample.expectedFragment());
            boolean validWordBounds = result.blocks().stream()
                    .filter(block -> block.level() == OcrBlockLevel.WORD)
                    .anyMatch(block -> block.bounds().width() > 0 && block.bounds().height() > 0);
            if (sample.expectedFragment() != null && expectedTextFound && validWordBounds) {
                truePositive++;
            } else if (sample.expectedFragment() != null) {
                falseNegative++;
                failures.add(sample.name() + "=false-negative:'" + result.fullText().strip() + "'");
                if (!result.fullText().isBlank()) {
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

    private static CorpusCase positive(String name, byte[] image, String expectedFragment) {
        return new CorpusCase(name, image, expectedFragment);
    }

    private static CorpusCase negative(String name, byte[] image) {
        return new CorpusCase(name, image, null);
    }

    private static double ratio(int numerator, int denominator) {
        return denominator == 0 ? 1 : (double) numerator / denominator;
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

    private record CorpusCase(String name, byte[] image, String expectedFragment) {
    }
}
