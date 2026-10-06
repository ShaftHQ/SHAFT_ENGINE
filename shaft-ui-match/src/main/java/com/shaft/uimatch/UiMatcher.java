package com.shaft.uimatch;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Properties;

/**
 * Scores two captured UI documents. Capture stays outside this library.
 */
public final class UiMatcher {
    /** Default assertion and heal confidence. */
    public static final double DEFAULT_CONFIDENCE = 0.90d;

    /** Heal confidence is read only from this property. */
    public static final String HEAL_CONFIDENCE_PROPERTY = "shaft.uiMatch.healConfidence";

    private UiMatcher() {
    }

    /**
     * Compare two documents and write one JSON file.
     *
     * @param assertionConfidence {@code null} uses {@link #DEFAULT_CONFIDENCE}
     */
    public static MatchResult compare(
            UiDocument expected,
            UiDocument actual,
            MatchMode mode,
            Double assertionConfidence,
            Path jsonOutput) {
        Objects.requireNonNull(expected, "expected");
        Objects.requireNonNull(actual, "actual");
        Objects.requireNonNull(mode, "mode");
        Objects.requireNonNull(jsonOutput, "jsonOutput");
        double required = assertionConfidence == null ? DEFAULT_CONFIDENCE : assertionConfidence;
        return finish(expected, actual, mode, required, jsonOutput);
    }

    /**
     * Heal comparison. Confidence comes from {@code properties} or the default.
     * There is no call-site override.
     */
    public static MatchResult heal(UiDocument expected, UiDocument actual, Properties properties, Path jsonOutput) {
        double required = DEFAULT_CONFIDENCE;
        if (properties != null) {
            String configured = properties.getProperty(HEAL_CONFIDENCE_PROPERTY);
            if (configured != null && !configured.isBlank()) {
                try {
                    required = Double.parseDouble(configured);
                } catch (NumberFormatException exception) {
                    throw new IllegalArgumentException(
                            HEAL_CONFIDENCE_PROPERTY + " must be a number", exception);
                }
            }
        }
        return compare(expected, actual, MatchMode.STRICT, required, jsonOutput);
    }

    private static MatchResult finish(
            UiDocument expected, UiDocument actual, MatchMode mode, double required, Path jsonOutput) {
        Score score = score(expected, actual, mode);
        boolean matched = score.confidence >= required && score.compared > 0;
        Integer x = matched ? score.bestX : null;
        Integer y = matched ? score.bestY : null;
        String hint = matched ? score.bestHint : null;
        String summary = matched ? "UI matched in " + mode + "." : "";
        String difference = matched ? "" : score.difference;
        MatchResult result = new MatchResult(score.confidence, matched, mode, x, y, hint, summary, difference);
        write(jsonOutput, result);
        return result;
    }

    private static Score score(UiDocument expected, UiDocument actual, MatchMode mode) {
        Tally tally = new Tally();
        compareUrl(expected, actual, mode, tally);
        compareElements(expected, actual, fields(mode), tally);
        if (tally.difference.isEmpty()) {
            tally.difference = "Documents differ.";
        }
        double confidence = tally.compared == 0 ? 0d : (double) tally.hits / tally.compared;
        return new Score(confidence, tally.compared, tally.bestX, tally.bestY, tally.bestHint, tally.difference);
    }

    private static void compareUrl(UiDocument expected, UiDocument actual, MatchMode mode, Tally tally) {
        if (mode == MatchMode.LAYOUT) {
            return;
        }
        tally.compared++;
        if (Objects.equals(expected.url(), actual.url())) {
            tally.hits++;
            return;
        }
        tally.note("Expected URL " + expected.url() + " but found " + actual.url() + ".");
    }

    private static void compareElements(UiDocument expected, UiDocument actual, List<String> fields, Tally tally) {
        int count = Math.max(expected.elements().size(), actual.elements().size());
        for (int index = 0; index < count; index++) {
            UiElement left = index < expected.elements().size() ? expected.elements().get(index) : null;
            UiElement right = index < actual.elements().size() ? actual.elements().get(index) : null;
            scoreElement(left, right, fields, tally);
        }
    }

    private static void scoreElement(UiElement left, UiElement right, List<String> fields, Tally tally) {
        int elementHits = 0;
        for (String field : fields) {
            tally.compared++;
            if (fieldMatches(field, left, right)) {
                tally.hits++;
                elementHits++;
            } else {
                tally.note("Expected " + field + " " + value(field, left) + " but found " + value(field, right) + ".");
            }
        }
        tally.consider(right, elementHits, fields.size());
    }

    private static boolean fieldMatches(String field, UiElement left, UiElement right) {
        return left != null && right != null && equal(field, left, right);
    }

    private static List<String> fields(MatchMode mode) {
        List<String> fields = new ArrayList<>();
        fields.add("role");
        if (mode == MatchMode.STRICT || mode == MatchMode.IGNORE_IMAGE) {
            fields.add("text");
            fields.add("accessibleName");
        }
        if (mode == MatchMode.STRICT || mode == MatchMode.IGNORE_TEXT) {
            fields.add("imageDigest");
        }
        fields.add("x");
        fields.add("y");
        fields.add("width");
        fields.add("height");
        return fields;
    }

    private static boolean equal(String field, UiElement left, UiElement right) {
        return Objects.equals(value(field, left), value(field, right));
    }

    private static String value(String field, UiElement element) {
        if (element == null) {
            return "missing";
        }
        return switch (field) {
            case "role" -> element.role();
            case "text" -> element.text();
            case "accessibleName" -> element.accessibleName();
            case "imageDigest" -> element.imageDigest();
            case "x" -> Integer.toString(element.x());
            case "y" -> Integer.toString(element.y());
            case "width" -> Integer.toString(element.width());
            case "height" -> Integer.toString(element.height());
            default -> "";
        };
    }

    private static void write(Path jsonOutput, MatchResult result) {
        String json = "{"
                + "\"confidence\":" + result.confidence()
                + ",\"matched\":" + result.matched()
                + ",\"mode\":\"" + result.mode() + "\""
                + ",\"x\":" + jsonNumber(result.x())
                + ",\"y\":" + jsonNumber(result.y())
                + ",\"locatorHint\":" + jsonString(result.locatorHint())
                + ",\"summary\":" + jsonString(result.summary())
                + ",\"difference\":" + jsonString(result.difference())
                + "}";
        try {
            Path parent = jsonOutput.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            Files.writeString(jsonOutput, json + System.lineSeparator(), StandardCharsets.UTF_8);
        } catch (IOException exception) {
            throw new IllegalStateException("Could not write UI match JSON", exception);
        }
    }

    private static String jsonNumber(Integer value) {
        return value == null ? "null" : Integer.toString(value);
    }

    private static String jsonString(String value) {
        if (value == null) {
            return "null";
        }
        return "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
    }

    private static final class Tally {
        private int compared;
        private int hits;
        private String difference = "";
        private int bestHits = -1;
        private int bestFields = 1;
        private Integer bestX;
        private Integer bestY;
        private String bestHint;

        private void note(String text) {
            if (difference.isEmpty()) {
                difference = text;
            }
        }

        private void consider(UiElement right, int elementHits, int fieldCount) {
            if (right == null || elementHits * bestFields < bestHits * fieldCount) {
                return;
            }
            bestHits = elementHits;
            bestFields = fieldCount;
            bestX = right.x() + (right.width() / 2);
            bestY = right.y() + (right.height() / 2);
            bestHint = right.locatorHint();
        }
    }

    private record Score(
            double confidence,
            int compared,
            Integer bestX,
            Integer bestY,
            String bestHint,
            String difference) {
    }

    /** One match decision. */
    public record MatchResult(
            double confidence,
            boolean matched,
            MatchMode mode,
            Integer x,
            Integer y,
            String locatorHint,
            String summary,
            String difference) {
    }

    /** Captured UI. The matcher does not collect it. */
    public record UiDocument(String url, List<UiElement> elements) {
        public UiDocument {
            elements = List.copyOf(elements);
        }
    }

    /** One captured element, including the locator hint from the capture step. */
    public record UiElement(
            String role,
            String text,
            String accessibleName,
            String imageDigest,
            int x,
            int y,
            int width,
            int height,
            String locatorHint) {
    }

    /** Comparison modes. */
    public enum MatchMode {
        /** Compare URL, text, accessible name, image digest, and bounds. */
        STRICT,
        /** Drop text and accessible name. */
        IGNORE_TEXT,
        /** Drop the image digest. */
        IGNORE_IMAGE,
        /** Compare role and bounds only. */
        LAYOUT
    }
}
