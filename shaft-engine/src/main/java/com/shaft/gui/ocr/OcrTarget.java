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

    public static OcrTarget exact(String text) {
        return new OcrTarget(text, OcrMatchMode.EXACT, OcrOptions.defaults(), null);
    }

    public static OcrTarget containing(String text) {
        return new OcrTarget(text, OcrMatchMode.CONTAINS, OcrOptions.defaults(), null);
    }

    public boolean requireUniqueMatch() {
        return occurrence == null;
    }

    public OcrTarget occurrence(int index) {
        return new OcrTarget(expectedText, matchMode, options, index);
    }

    public OcrTarget caseSensitive() {
        return new OcrTarget(expectedText, matchMode, options.withCaseSensitive(true), occurrence);
    }

    public OcrTarget minimumConfidence(double confidence) {
        return new OcrTarget(expectedText, matchMode, options.withMinimumConfidence(confidence), occurrence);
    }

    public OcrTarget languages(String... languages) {
        return new OcrTarget(expectedText, matchMode, options.withLanguages(languages), occurrence);
    }

    public OcrTarget within(OcrRectangle region) {
        return new OcrTarget(expectedText, matchMode, options.within(region), occurrence);
    }

    public OcrTarget pageSegmentationMode(OcrPageSegmentationMode mode) {
        return new OcrTarget(expectedText, matchMode, options.withPageSegmentationMode(mode), occurrence);
    }

    public OcrTarget preprocessing(OcrPreprocessingMode mode) {
        return new OcrTarget(expectedText, matchMode, options.withPreprocessingMode(mode), occurrence);
    }
}
