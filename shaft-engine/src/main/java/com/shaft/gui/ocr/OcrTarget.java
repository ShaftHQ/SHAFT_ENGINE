package com.shaft.gui.ocr;

import java.util.Objects;

/** Visible text target resolved from an application screenshot by an OCR provider. */
public record OcrTarget(String expectedText,
                        OcrMatchMode matchMode,
                        OcrOptions options,
                        Integer occurrence) {
    public OcrTarget {
        if (expectedText == null || expectedText.isBlank()) {
            throw new IllegalArgumentException("OCR target text cannot be null or blank.");
        }
        expectedText = expectedText.trim();
        matchMode = Objects.requireNonNull(matchMode, "matchMode");
        options = Objects.requireNonNull(options, "options");
        if (occurrence != null && occurrence < 0) {
            throw new IllegalArgumentException("OCR target occurrence cannot be negative.");
        }
    }

    /**
     * Creates a target that matches the exact text.
     */
    public static OcrTarget exact(String text) {
        return new OcrTarget(text, OcrMatchMode.EXACT, OcrOptions.defaults(), null);
    }

    /**
     * Creates a target that matches text containing the given value.
     */
    public static OcrTarget containing(String text) {
        return new OcrTarget(text, OcrMatchMode.CONTAINS, OcrOptions.defaults(), null);
    }

    /**
     * Returns whether exactly one match is required.
     */
    public boolean requireUniqueMatch() {
        return occurrence == null;
    }

    /**
     * Returns a copy that picks the match at the given index.
     */
    public OcrTarget occurrence(int index) {
        return new OcrTarget(expectedText, matchMode, options, index);
    }

    /**
     * Returns a copy that matches case-sensitively.
     */
    public OcrTarget caseSensitive() {
        return new OcrTarget(expectedText, matchMode, options.withCaseSensitive(true), occurrence);
    }

    /**
     * Returns a copy that only accepts text at or above the given confidence.
     */
    public OcrTarget minimumConfidence(double confidence) {
        return new OcrTarget(expectedText, matchMode, options.withMinimumConfidence(confidence), occurrence);
    }

    /**
     * Returns a copy that recognizes the given languages.
     */
    public OcrTarget languages(String... languages) {
        return new OcrTarget(expectedText, matchMode, options.withLanguages(languages), occurrence);
    }

    /**
     * Returns a copy that only reads text inside the given screen region.
     */
    public OcrTarget within(OcrRectangle region) {
        return new OcrTarget(expectedText, matchMode, options.within(region), occurrence);
    }

    /**
     * Returns a copy that uses the given page segmentation mode.
     */
    public OcrTarget pageSegmentationMode(OcrPageSegmentationMode mode) {
        return new OcrTarget(expectedText, matchMode, options.withPageSegmentationMode(mode), occurrence);
    }

    /**
     * Returns a copy that uses the given image preprocessing mode.
     */
    public OcrTarget preprocessing(OcrPreprocessingMode mode) {
        return new OcrTarget(expectedText, matchMode, options.withPreprocessingMode(mode), occurrence);
    }
}
