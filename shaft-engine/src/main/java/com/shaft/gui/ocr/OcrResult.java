package com.shaft.gui.ocr;

import java.util.List;
import java.util.Objects;

/** Immutable OCR text and structured recognition result. */
public record OcrResult(String fullText, List<OcrTextBlock> blocks) {
    public OcrResult {
        fullText = Objects.requireNonNull(fullText, "fullText");
        blocks = List.copyOf(Objects.requireNonNull(blocks, "blocks"));
    }
}
