package com.shaft.gui.ocr;

import java.util.Objects;

/** Recognized text and its pixel-space geometry. */
public record OcrTextBlock(String text, OcrRectangle bounds, double confidence, OcrBlockLevel level) {
    public OcrTextBlock {
        text = Objects.requireNonNull(text, "text");
        bounds = Objects.requireNonNull(bounds, "bounds");
        level = Objects.requireNonNull(level, "level");
        if (!Double.isFinite(confidence) || confidence < 0 || confidence > 1) {
            throw new IllegalArgumentException("OCR confidence must be between 0 and 1.");
        }
    }
}
