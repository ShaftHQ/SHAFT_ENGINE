package com.shaft.gui.ocr;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;

/** Immutable recognition and matching options shared by all SHAFT OCR providers. */
public record OcrOptions(List<String> languages,
                         double minimumConfidence,
                         boolean caseSensitive,
                         boolean normalizeWhitespace,
                         OcrRectangle region,
                         OcrPageSegmentationMode pageSegmentationMode,
                         OcrPreprocessingMode preprocessingMode) {
    private static final double DEFAULT_MINIMUM_CONFIDENCE = 0.60;

    public OcrOptions {
        languages = normalizeLanguages(languages);
        if (!Double.isFinite(minimumConfidence) || minimumConfidence < 0 || minimumConfidence > 1) {
            throw new IllegalArgumentException("OCR minimum confidence must be between 0 and 1.");
        }
        pageSegmentationMode = Objects.requireNonNull(pageSegmentationMode, "pageSegmentationMode");
        preprocessingMode = Objects.requireNonNull(preprocessingMode, "preprocessingMode");
    }

    /**
     * Returns OCR options built from the configured OCR properties.
     */
    public static OcrOptions defaults() {
        return new OcrOptions(List.of(), DEFAULT_MINIMUM_CONFIDENCE, false, true, null,
                OcrPageSegmentationMode.AUTO, OcrPreprocessingMode.AUTO);
    }

    /**
     * Returns a copy that recognizes the given languages.
     */
    public OcrOptions withLanguages(String... configuredLanguages) {
        Objects.requireNonNull(configuredLanguages, "configuredLanguages");
        return new OcrOptions(Arrays.asList(configuredLanguages), minimumConfidence, caseSensitive,
                normalizeWhitespace, region, pageSegmentationMode, preprocessingMode);
    }

    /**
     * Returns a copy that only accepts text at or above the given confidence.
     */
    public OcrOptions withMinimumConfidence(double configuredMinimumConfidence) {
        return new OcrOptions(languages, configuredMinimumConfidence, caseSensitive, normalizeWhitespace,
                region, pageSegmentationMode, preprocessingMode);
    }

    /**
     * Returns a copy with case-sensitive matching turned on or off.
     */
    public OcrOptions withCaseSensitive(boolean configuredCaseSensitive) {
        return new OcrOptions(languages, minimumConfidence, configuredCaseSensitive, normalizeWhitespace,
                region, pageSegmentationMode, preprocessingMode);
    }

    /**
     * Returns a copy with whitespace normalization turned on or off.
     */
    public OcrOptions withWhitespaceNormalization(boolean configuredNormalization) {
        return new OcrOptions(languages, minimumConfidence, caseSensitive, configuredNormalization,
                region, pageSegmentationMode, preprocessingMode);
    }

    /**
     * Returns a copy that only reads text inside the given screen region.
     */
    public OcrOptions within(OcrRectangle configuredRegion) {
        return new OcrOptions(languages, minimumConfidence, caseSensitive, normalizeWhitespace,
                Objects.requireNonNull(configuredRegion, "configuredRegion"), pageSegmentationMode,
                preprocessingMode);
    }

    /**
     * Returns a copy that uses the given page segmentation mode.
     */
    public OcrOptions withPageSegmentationMode(OcrPageSegmentationMode configuredMode) {
        return new OcrOptions(languages, minimumConfidence, caseSensitive, normalizeWhitespace, region,
                configuredMode, preprocessingMode);
    }

    /**
     * Returns a copy that uses the given image preprocessing mode.
     */
    public OcrOptions withPreprocessingMode(OcrPreprocessingMode configuredMode) {
        return new OcrOptions(languages, minimumConfidence, caseSensitive, normalizeWhitespace, region,
                pageSegmentationMode, configuredMode);
    }

    private static List<String> normalizeLanguages(List<String> configuredLanguages) {
        Objects.requireNonNull(configuredLanguages, "languages");
        LinkedHashSet<String> normalized = new LinkedHashSet<>();
        for (String language : configuredLanguages) {
            if (language == null || language.isBlank()) {
                throw new IllegalArgumentException("OCR language names cannot be null or blank.");
            }
            normalized.add(language.trim());
        }
        return List.copyOf(normalized);
    }
}
