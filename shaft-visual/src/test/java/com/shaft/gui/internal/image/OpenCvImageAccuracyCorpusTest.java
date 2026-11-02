package com.shaft.gui.internal.image;

import com.shaft.gui.image.ImageMatch;
import com.shaft.gui.image.ImageMatchingMode;
import com.shaft.gui.image.ImageRectangle;
import com.shaft.gui.image.ImageTarget;
import org.testng.Assert;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;

/** Deterministic precision/recall and latency gate for the supported OpenCV matching modes. */
public class OpenCvImageAccuracyCorpusTest {
    private static final double MINIMUM_PRECISION = 1.0;
    private static final double MINIMUM_RECALL = 1.0;
    private static final long MAX_P95_MILLIS = 5_000;
    private static final double MINIMUM_INTERSECTION_OVER_UNION = 0.50;

    @Test
    public void calibratedCorpusShouldMeetAccuracyAndRuntimeBudgets() throws Exception {
        OpenCvVisualProcessingProvider provider = new OpenCvVisualProcessingProvider();
        provider.load();
        BufferedImage detailed = detailedTarget(100, 80);
        BufferedImage flat = solid(18, 14, Color.BLACK);
        BufferedImage androidReference = repositoryImage("content_local.png");
        BufferedImage androidCurrent = repositoryImage("content.png");
        BufferedImage androidReferenceText = androidReference.getSubimage(20, 18, 190, 70);
        BufferedImage androidCurrentText = androidCurrent.getSubimage(32, 12, 205, 72);
        List<CorpusCase> corpus = List.of(
                positive("exact-color", detailed, placed(detailed, 1.0, 0), ImageMatchingMode.TEMPLATE,
                        new ImageRectangle(90, 70, 100, 80)),
                positive("scaled-down", detailed, placed(detailed, 0.75, 0), ImageMatchingMode.TEMPLATE,
                        new ImageRectangle(102, 80, 76, 60)),
                positive("scaled-up", detailed, placed(detailed, 1.25, 0), ImageMatchingMode.TEMPLATE,
                        new ImageRectangle(77, 60, 126, 100)),
                positive("rotated-feature", detailed, placed(detailed, 1.0, 18), ImageMatchingMode.AUTO,
                        new ImageRectangle(80, 56, 120, 108)),
                positive("flat-template", flat, placed(flat, 1.0, 0), ImageMatchingMode.TEMPLATE,
                        new ImageRectangle(131, 103, 18, 14)),
                positive("real-android-cross-capture", androidReferenceText,
                        placedAt(androidCurrentText, 120, 130, 520, 340), ImageMatchingMode.AUTO,
                        new ImageRectangle(120, 130, androidCurrentText.getWidth(), androidCurrentText.getHeight())),
                positive("similar-distractor", detailed, placedWithDistractor(detailed), ImageMatchingMode.TEMPLATE,
                        new ImageRectangle(35, 70, 100, 80)),
                negative("absent", detailed, solid(280, 220, Color.WHITE), ImageMatchingMode.AUTO),
                negative("wrong-flat-color", solid(18, 14, Color.RED), placed(flat, 1.0, 0),
                        ImageMatchingMode.TEMPLATE),
                negative("repeated-local-fragment", detailed, repeatedFragments(detailed), ImageMatchingMode.FEATURE)
        );

        int truePositive = 0;
        int falsePositive = 0;
        int falseNegative = 0;
        List<Long> elapsedMillis = new ArrayList<>();
        List<String> failures = new ArrayList<>();
        for (CorpusCase sample : corpus) {
            long started = System.nanoTime();
            List<ImageMatch> matches = provider.findImageMatches(
                    ImageTarget.fromBytes(png(sample.target())).matchingMode(sample.mode()), png(sample.screenshot()));
            elapsedMillis.add((System.nanoTime() - started) / 1_000_000);
            List<ImageMatch> correctMatches = matches.stream()
                    .filter(match -> sample.expectedBounds() != null
                            && intersectionOverUnion(match.bounds(), sample.expectedBounds())
                            >= MINIMUM_INTERSECTION_OVER_UNION)
                    .toList();
            boolean correctMatchFound = !correctMatches.isEmpty();
            if (sample.expectedMatch() && correctMatchFound) {
                truePositive++;
                int unmatchedDetections = matches.size() - 1;
                falsePositive += unmatchedDetections;
                if (unmatchedDetections > 0) {
                    failures.add(sample.name() + "=extra-detections:" + summarize(matches));
                }
            } else if (sample.expectedMatch()) {
                falseNegative++;
                failures.add(sample.name() + "=false-negative:" + summarize(matches));
                if (!matches.isEmpty()) {
                    falsePositive++;
                }
            } else if (!matches.isEmpty()) {
                falsePositive++;
                failures.add(sample.name() + "=false-positive:" + summarize(matches));
            }
        }

        double precision = ratio(truePositive, truePositive + falsePositive);
        double recall = ratio(truePositive, truePositive + falseNegative);
        elapsedMillis.sort(Comparator.naturalOrder());
        long p95Millis = elapsedMillis.get((int) Math.ceil(elapsedMillis.size() * 0.95) - 1);
        String metrics = String.format(Locale.ROOT,
                "precision=%.3f recall=%.3f tp=%d fp=%d fn=%d p95=%dms cases=%s",
                precision, recall, truePositive, falsePositive, falseNegative, p95Millis, failures);
        System.out.println("OpenCV accuracy corpus: " + metrics);

        Assert.assertTrue(precision >= MINIMUM_PRECISION, metrics);
        Assert.assertTrue(recall >= MINIMUM_RECALL, metrics);
        Assert.assertTrue(p95Millis <= MAX_P95_MILLIS, metrics);
    }

    private static CorpusCase positive(String name, BufferedImage target, BufferedImage screenshot,
                                       ImageMatchingMode mode, ImageRectangle expectedBounds) {
        return new CorpusCase(name, target, screenshot, mode, true, expectedBounds);
    }

    private static CorpusCase negative(String name, BufferedImage target, BufferedImage screenshot,
                                       ImageMatchingMode mode) {
        return new CorpusCase(name, target, screenshot, mode, false, null);
    }

    private static double ratio(int numerator, int denominator) {
        return denominator == 0 ? 1 : (double) numerator / denominator;
    }

    private static double intersectionOverUnion(ImageRectangle actual, ImageRectangle expected) {
        int left = Math.max(actual.x(), expected.x());
        int top = Math.max(actual.y(), expected.y());
        int right = Math.min(actual.x() + actual.width(), expected.x() + expected.width());
        int bottom = Math.min(actual.y() + actual.height(), expected.y() + expected.height());
        long intersection = (long) Math.max(0, right - left) * Math.max(0, bottom - top);
        long union = (long) actual.width() * actual.height()
                + (long) expected.width() * expected.height() - intersection;
        return union == 0 ? 0 : (double) intersection / union;
    }

    private static String summarize(List<ImageMatch> matches) {
        return matches.stream()
                .limit(3)
                .map(match -> String.format(Locale.ROOT, "(%d,%d @ %.3f)",
                        match.centerX(), match.centerY(), match.confidence()))
                .toList()
                .toString();
    }

    private static BufferedImage placed(BufferedImage target, double scale, double degrees) {
        BufferedImage screenshot = solid(280, 220, Color.WHITE);
        Graphics2D graphics = screenshot.createGraphics();
        try {
            graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
            AffineTransform transform = new AffineTransform();
            transform.translate(140, 110);
            transform.rotate(Math.toRadians(degrees));
            transform.scale(scale, scale);
            transform.translate(-target.getWidth() / 2.0, -target.getHeight() / 2.0);
            graphics.drawImage(target, transform, null);
        } finally {
            graphics.dispose();
        }
        return screenshot;
    }

    private static BufferedImage placedAt(BufferedImage target, int x, int y, int width, int height) {
        BufferedImage screenshot = solid(width, height, Color.WHITE);
        Graphics2D graphics = screenshot.createGraphics();
        try {
            graphics.drawImage(target, x, y, null);
        } finally {
            graphics.dispose();
        }
        return screenshot;
    }

    private static BufferedImage placedWithDistractor(BufferedImage target) {
        BufferedImage screenshot = solid(300, 220, Color.WHITE);
        BufferedImage distractor = detailedTarget(target.getWidth(), target.getHeight());
        Graphics2D distractorGraphics = distractor.createGraphics();
        try {
            distractorGraphics.setColor(new Color(190, 25, 45));
            distractorGraphics.fillOval(8, 8, 52, 52);
            distractorGraphics.setColor(Color.BLACK);
            distractorGraphics.fillRect(60, 8, 32, 62);
        } finally {
            distractorGraphics.dispose();
        }
        Graphics2D graphics = screenshot.createGraphics();
        try {
            graphics.drawImage(target, 35, 70, null);
            graphics.drawImage(distractor, 165, 70, null);
        } finally {
            graphics.dispose();
        }
        return screenshot;
    }

    private static BufferedImage repositoryImage(String fileName) throws Exception {
        Path workingDirectory = Path.of(System.getProperty("user.dir"));
        Path repositoryRoot = Files.isDirectory(workingDirectory.resolve("shaft-engine"))
                ? workingDirectory : workingDirectory.getParent();
        Path imagePath = repositoryRoot.resolve(Path.of("shaft-engine", "src", "main", "resources",
                "dynamicObjectRepository", "Android", fileName));
        Assert.assertTrue(Files.isRegularFile(imagePath), "Missing corpus resource " + imagePath);
        BufferedImage image = ImageIO.read(imagePath.toFile());
        Assert.assertNotNull(image, "Unreadable corpus resource " + imagePath);
        return image;
    }

    private static BufferedImage repeatedFragments(BufferedImage target) {
        BufferedImage screenshot = solid(280, 220, Color.WHITE);
        BufferedImage fragment = target.getSubimage(35, 25, 25, 20);
        Graphics2D graphics = screenshot.createGraphics();
        try {
            for (int y = 20; y < 190; y += 40) {
                for (int x = 20; x < 250; x += 45) {
                    graphics.drawImage(fragment, x, y, null);
                }
            }
        } finally {
            graphics.dispose();
        }
        return screenshot;
    }

    private static BufferedImage detailedTarget(int width, int height) {
        BufferedImage image = solid(width, height, new Color(245, 248, 252));
        Graphics2D graphics = image.createGraphics();
        try {
            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            graphics.setColor(new Color(16, 89, 166));
            graphics.fillRoundRect(3, 3, width - 6, height - 6, 14, 14);
            graphics.setColor(Color.WHITE);
            graphics.setStroke(new BasicStroke(3));
            graphics.drawLine(12, 15, width - 15, height - 15);
            graphics.drawOval(15, 18, 30, 30);
            graphics.setColor(new Color(255, 196, 0));
            graphics.fillRect(width - 38, 14, 22, 31);
            graphics.setColor(Color.BLACK);
            graphics.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 15));
            graphics.drawString("S42", 28, height - 13);
        } finally {
            graphics.dispose();
        }
        return image;
    }

    private static BufferedImage solid(int width, int height, Color color) {
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        try {
            graphics.setColor(color);
            graphics.fillRect(0, 0, width, height);
        } finally {
            graphics.dispose();
        }
        return image;
    }

    private static byte[] png(BufferedImage image) throws Exception {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        ImageIO.write(image, "png", output);
        return output.toByteArray();
    }

    private record CorpusCase(String name, BufferedImage target, BufferedImage screenshot,
                              ImageMatchingMode mode, boolean expectedMatch, ImageRectangle expectedBounds) {
    }
}
