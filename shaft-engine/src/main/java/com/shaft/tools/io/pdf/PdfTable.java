package com.shaft.tools.io.pdf;

import com.shaft.gui.ocr.OcrRectangle;

import java.util.List;
import java.util.Objects;

/** Geometry-derived table with an explicit structural confidence. */
public record PdfTable(OcrRectangle bounds, List<PdfTableRow> rows, double confidence) {
    public PdfTable {
        bounds = Objects.requireNonNull(bounds, "bounds");
        rows = List.copyOf(Objects.requireNonNull(rows, "rows"));
        if (!Double.isFinite(confidence) || confidence < 0 || confidence > 1) {
            throw new IllegalArgumentException("PDF table confidence must be between 0 and 1.");
        }
    }
}
