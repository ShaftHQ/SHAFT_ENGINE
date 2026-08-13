package com.shaft.tools.io.pdf;

import java.nio.file.Path;
import java.util.Objects;

/** Success or failure for one ordered PDF batch item. */
public record PdfBatchItemResult(Path source, PdfDocumentResult result, String failure) {
    public PdfBatchItemResult {
        source = Objects.requireNonNull(source, "source").toAbsolutePath().normalize();
        if ((result == null) == (failure == null)) {
            throw new IllegalArgumentException("A PDF batch item must contain exactly one result or failure.");
        }
    }

    public boolean successful() {
        return result != null;
    }
}
