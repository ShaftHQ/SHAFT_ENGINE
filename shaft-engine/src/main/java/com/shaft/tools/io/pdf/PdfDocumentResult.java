package com.shaft.tools.io.pdf;

import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/** Immutable result and export receipts for one PDF document. */
public record PdfDocumentResult(Path source, List<PdfPageResult> pages, List<PdfExportResult> exports,
                                List<String> warnings) {
    public PdfDocumentResult {
        source = Objects.requireNonNull(source, "source").toAbsolutePath().normalize();
        pages = List.copyOf(Objects.requireNonNull(pages, "pages"));
        exports = List.copyOf(Objects.requireNonNull(exports, "exports"));
        warnings = List.copyOf(Objects.requireNonNull(warnings, "warnings"));
    }

    /** Text from all pages separated by form-feed characters. */
    public String fullText() {
        return pages.stream().map(page -> page.recognition().fullText()).collect(Collectors.joining("\f"));
    }
}
