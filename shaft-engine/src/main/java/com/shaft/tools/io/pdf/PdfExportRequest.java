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

    /**
     * Creates a request to export a PDF in the given format to the output path.
     */
    public static PdfExportRequest to(PdfExportFormat format, Path output) {
        return new PdfExportRequest(format, output, false, false);
    }

    /**
     * Returns a copy that overwrites an existing output file.
     */
    public PdfExportRequest replacingExisting() {
        return new PdfExportRequest(format, output, true, allowSignatureInvalidation);
    }

    /**
     * Returns a copy that allows an export that invalidates existing digital signatures.
     */
    public PdfExportRequest allowingSignatureInvalidation() {
        return new PdfExportRequest(format, output, replaceExisting, true);
    }
}
