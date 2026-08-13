package com.shaft.tools.io.pdf;

import java.nio.file.Path;
import java.util.Objects;

/** Explicit destination and replacement policy for one document export. */
public record PdfExportRequest(PdfExportFormat format, Path output, boolean replaceExisting,
                               boolean allowSignatureInvalidation) {
    public PdfExportRequest {
        format = Objects.requireNonNull(format, "format");
        output = Objects.requireNonNull(output, "output").toAbsolutePath().normalize();
    }

    public static PdfExportRequest to(PdfExportFormat format, Path output) {
        return new PdfExportRequest(format, output, false, false);
    }

    public PdfExportRequest replacingExisting() {
        return new PdfExportRequest(format, output, true, allowSignatureInvalidation);
    }

    public PdfExportRequest allowingSignatureInvalidation() {
        return new PdfExportRequest(format, output, replaceExisting, true);
    }
}
