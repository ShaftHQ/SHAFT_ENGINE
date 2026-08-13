package com.shaft.tools.io.pdf;

import java.nio.file.Path;
import java.util.Objects;

/** Completed document export receipt. */
public record PdfExportResult(PdfExportFormat format, Path output, long sizeBytes, String sha256) {
    public PdfExportResult {
        format = Objects.requireNonNull(format, "format");
        output = Objects.requireNonNull(output, "output").toAbsolutePath().normalize();
        sha256 = Objects.requireNonNull(sha256, "sha256");
        if (sizeBytes < 0) {
            throw new IllegalArgumentException("PDF export size cannot be negative.");
        }
    }
}
