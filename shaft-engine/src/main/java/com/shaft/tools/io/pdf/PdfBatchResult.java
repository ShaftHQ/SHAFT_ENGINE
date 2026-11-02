package com.shaft.tools.io.pdf;

import java.util.List;
import java.util.Objects;

/** Ordered immutable result for a PDF batch. */
public record PdfBatchResult(List<PdfBatchItemResult> items) {
    public PdfBatchResult {
        items = List.copyOf(Objects.requireNonNull(items, "items"));
    }
}
