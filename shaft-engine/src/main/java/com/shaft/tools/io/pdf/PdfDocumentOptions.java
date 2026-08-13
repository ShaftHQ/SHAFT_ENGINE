package com.shaft.tools.io.pdf;

import com.shaft.gui.ocr.OcrOptions;
import com.shaft.properties.internal.Properties;

import java.time.Duration;
import java.util.Objects;

/** Immutable limits and recognition options for a PDF document. */
public record PdfDocumentOptions(OcrOptions ocrOptions, int renderDpi, int nativeTextMinimumCharacters,
                                 double imageCoverageThreshold, long maximumInputBytes, int maximumPages,
                                 long maximumPixelsPerPage, Duration pageTimeout, double tableConfidenceThreshold,
                                 long maximumAllureArtifactBytes, boolean detectOrientation, boolean deskew,
                                 boolean attachAllureEvidence) {
    public PdfDocumentOptions {
        ocrOptions = Objects.requireNonNull(ocrOptions, "ocrOptions");
        pageTimeout = Objects.requireNonNull(pageTimeout, "pageTimeout");
        if (renderDpi < 72 || renderDpi > 600 || nativeTextMinimumCharacters < 0 || maximumInputBytes < 1
                || maximumPages < 1 || maximumPixelsPerPage < 1 || maximumAllureArtifactBytes < 1
                || pageTimeout.isNegative() || pageTimeout.isZero() || pageTimeout.toMillis() < 1) {
            throw new IllegalArgumentException("PDF document OCR limits are invalid.");
        }
        if (!unitInterval(imageCoverageThreshold) || !unitInterval(tableConfidenceThreshold)) {
            throw new IllegalArgumentException("PDF document OCR thresholds must be between 0 and 1.");
        }
    }

    public static PdfDocumentOptions defaults() {
        var configured = Properties.ocr;
        return new PdfDocumentOptions(OcrOptions.defaults(), configured.documentRenderDpi(), 16, 0.25,
                configured.documentMaximumInputBytes(), configured.documentMaximumPages(),
                configured.documentMaximumPixelsPerPage(), Duration.ofSeconds(configured.documentPageTimeoutSeconds()),
                0.65, configured.documentMaximumAllureArtifactBytes(), true, true, true);
    }

    public PdfDocumentOptions withOcrOptions(OcrOptions value) {
        return copy(Objects.requireNonNull(value), renderDpi, maximumInputBytes, maximumPages, maximumPixelsPerPage);
    }

    public PdfDocumentOptions withRenderDpi(int value) {
        return copy(ocrOptions, value, maximumInputBytes, maximumPages, maximumPixelsPerPage);
    }

    public PdfDocumentOptions withResourceLimits(long inputBytes, int pages, long pixelsPerPage) {
        return copy(ocrOptions, renderDpi, inputBytes, pages, pixelsPerPage);
    }

    public PdfDocumentOptions withPageTimeout(Duration value) {
        return new PdfDocumentOptions(ocrOptions, renderDpi, nativeTextMinimumCharacters, imageCoverageThreshold,
                maximumInputBytes, maximumPages, maximumPixelsPerPage, value, tableConfidenceThreshold,
                maximumAllureArtifactBytes, detectOrientation, deskew, attachAllureEvidence);
    }

    public PdfDocumentOptions withAllureEvidence(boolean value) {
        return new PdfDocumentOptions(ocrOptions, renderDpi, nativeTextMinimumCharacters, imageCoverageThreshold,
                maximumInputBytes, maximumPages, maximumPixelsPerPage, pageTimeout, tableConfidenceThreshold,
                maximumAllureArtifactBytes, detectOrientation, deskew, value);
    }

    private PdfDocumentOptions copy(OcrOptions options, int dpi, long bytes, int pages, long pixels) {
        return new PdfDocumentOptions(options, dpi, nativeTextMinimumCharacters, imageCoverageThreshold, bytes, pages,
                pixels, pageTimeout, tableConfidenceThreshold, maximumAllureArtifactBytes, detectOrientation, deskew,
                attachAllureEvidence);
    }

    private static boolean unitInterval(double value) {
        return Double.isFinite(value) && value >= 0 && value <= 1;
    }
}
