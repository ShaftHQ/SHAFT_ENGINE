package com.shaft.gui.internal.ocr;

import com.shaft.gui.ocr.OcrResult;

import java.util.List;
import java.util.Objects;

/** Provider-neutral OCR analysis metadata for a rendered document page. */
public record OcrDocumentPageAnalysis(OcrResult result, int rotationDegrees, double deskewDegrees,
                                      List<String> warnings) {
    public OcrDocumentPageAnalysis {
        result = Objects.requireNonNull(result, "result");
        warnings = List.copyOf(Objects.requireNonNull(warnings, "warnings"));
    }

    public static OcrDocumentPageAnalysis uncorrected(OcrResult result) {
        return new OcrDocumentPageAnalysis(result, 0, 0, List.of());
    }
}
