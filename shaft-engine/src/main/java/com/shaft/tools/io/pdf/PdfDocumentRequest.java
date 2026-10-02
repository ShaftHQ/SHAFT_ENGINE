package com.shaft.tools.io.pdf;

import java.nio.file.Path;
import java.util.List;
import java.util.Objects;

/** One ordered PDF batch request. */
public record PdfDocumentRequest(Path source, PdfDocumentOptions options, List<PdfExportRequest> exports) {
    public PdfDocumentRequest {
        source = Objects.requireNonNull(source, "source").toAbsolutePath().normalize();
        options = Objects.requireNonNull(options, "options");
        exports = List.copyOf(Objects.requireNonNull(exports, "exports"));
    }

    /**
     * Creates a request to read the PDF document at the given path.
     */
    public static PdfDocumentRequest of(Path source) {
        return new PdfDocumentRequest(source, PdfDocumentOptions.defaults(), List.of());
    }
}
