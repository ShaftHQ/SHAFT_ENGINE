package com.shaft.tools.io.pdf;

import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTextBlock;

import java.util.List;
import java.util.Objects;

/** Immutable native/OCR result for one one-based PDF page. */
public record PdfPageResult(int pageNumber, OcrResult recognition, PdfTextSource source, double confidence,
                            int rotationDegrees, double deskewDegrees, List<OcrTextBlock> searchableOverlay,
                            List<PdfTable> tables, List<String> warnings) {
    public PdfPageResult {
        recognition = Objects.requireNonNull(recognition, "recognition");
        source = Objects.requireNonNull(source, "source");
        searchableOverlay = List.copyOf(Objects.requireNonNull(searchableOverlay, "searchableOverlay"));
        tables = List.copyOf(Objects.requireNonNull(tables, "tables"));
        warnings = List.copyOf(Objects.requireNonNull(warnings, "warnings"));
        if (pageNumber < 1 || !Double.isFinite(confidence) || confidence < 0 || confidence > 1) {
            throw new IllegalArgumentException("PDF page result values are invalid.");
        }
    }
}
