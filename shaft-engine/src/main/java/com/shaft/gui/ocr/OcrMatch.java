package com.shaft.gui.ocr;

import java.util.Objects;

/** A selected visible-text OCR match and its source-image coordinates. */
public record OcrMatch(String text, OcrRectangle bounds, double confidence) {
    public OcrMatch {
        text = Objects.requireNonNull(text, "text");
        bounds = Objects.requireNonNull(bounds, "bounds");
        if (!Double.isFinite(confidence) || confidence < 0 || confidence > 1) {
            throw new IllegalArgumentException("OCR match confidence must be between 0 and 1.");
        }
    }
}
