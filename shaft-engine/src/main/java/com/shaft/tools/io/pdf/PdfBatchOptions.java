package com.shaft.tools.io.pdf;

import com.shaft.properties.internal.Properties;

/** Bounded PDF batch scheduling options. Fail-fast mode is serial to prevent later export side effects. */
public record PdfBatchOptions(int parallelism, long maximumInFlightRasterBytes, boolean failFast) {
    public PdfBatchOptions {
        if (parallelism < 1 || parallelism > 64 || maximumInFlightRasterBytes < 1) {
            throw new IllegalArgumentException("PDF batch limits are invalid.");
        }
    }

    public static PdfBatchOptions defaults() {
        return new PdfBatchOptions(Properties.ocr.documentBatchParallelism(),
                Properties.ocr.documentMaximumInFlightRasterBytes(), false);
    }
}
