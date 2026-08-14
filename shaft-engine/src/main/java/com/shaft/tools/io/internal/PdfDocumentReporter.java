package com.shaft.tools.io.internal;

import com.shaft.tools.io.pdf.PdfDocumentResult;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.nio.file.Files;

final class PdfDocumentReporter {
    private static final ObjectMapper JSON = new ObjectMapper();

    private PdfDocumentReporter() {
        throw new IllegalStateException("Utility class");
    }

    static void attach(PdfDocumentResult result, long maximumArtifactBytes) {
        String summary;
        try {
            summary = JSON.writerWithDefaultPrettyPrinter().writeValueAsString(java.util.Map.of(
                    "source", result.source().toString(),
                    "pages", result.pages().size(),
                    "sources", result.pages().stream().map(page -> page.source().name()).toList(),
                    "warnings", result.pages().stream().flatMap(page -> page.warnings().stream()).toList()));
        } catch (JsonProcessingException exception) {
            throw new IllegalStateException("Could not serialize PDF OCR evidence summary.", exception);
        }
        ReportManagerHelper.attach("json", "SHAFT PDF OCR summary", summary);
        for (var page : result.pages()) {
            try {
                ReportManagerHelper.attach("json", "SHAFT PDF OCR page " + page.pageNumber(),
                        JSON.writerWithDefaultPrettyPrinter().writeValueAsString(java.util.Map.of(
                                "pageNumber", page.pageNumber(), "source", page.source(),
                                "confidence", page.confidence(), "rotationDegrees", page.rotationDegrees(),
                                "deskewDegrees", page.deskewDegrees(), "recognition", page.recognition(),
                                "tables", page.tables(), "warnings", page.warnings())));
            } catch (JsonProcessingException exception) {
                throw new IllegalStateException("Could not serialize PDF OCR page evidence.", exception);
            }
        }
        for (var export : result.exports()) {
            if (export.sizeBytes() > maximumArtifactBytes) {
                ReportManagerHelper.attach("text", "SHAFT PDF OCR artifact manifest",
                        export.output() + System.lineSeparator() + export.sizeBytes() + " bytes" + System.lineSeparator()
                                + "sha256=" + export.sha256());
                continue;
            }
            try (var input = Files.newInputStream(export.output())) {
                ReportManagerHelper.attach(export.format().name().toLowerCase(), export.output().getFileName().toString(), input);
            } catch (IOException exception) {
                throw new IllegalStateException("Could not attach PDF OCR export '" + export.output() + "'.", exception);
            }
        }
    }
}
