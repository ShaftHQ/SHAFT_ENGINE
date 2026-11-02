package com.shaft.tools.io.pdf;

import com.shaft.gui.ocr.OcrRectangle;

import java.util.Objects;

/** One inferred PDF table cell. */
public record PdfTableCell(String text, OcrRectangle bounds, int rowSpan, int columnSpan, double confidence) {
    public PdfTableCell {
        text = Objects.requireNonNull(text, "text");
        bounds = Objects.requireNonNull(bounds, "bounds");
        if (rowSpan < 1 || columnSpan < 1) {
            throw new IllegalArgumentException("PDF table cell spans must be positive.");
        }
        if (!Double.isFinite(confidence) || confidence < 0 || confidence > 1) {
            throw new IllegalArgumentException("PDF table cell confidence must be between 0 and 1.");
        }
    }
}
