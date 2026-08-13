package com.shaft.tools.io.internal;

import com.shaft.tools.io.pdf.PdfBatchItemResult;
import com.shaft.tools.io.pdf.PdfBatchOptions;
import com.shaft.tools.io.pdf.PdfBatchResult;
import com.shaft.tools.io.pdf.PdfDocumentRequest;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/** Internal bounded ordered PDF batch scheduler. */
public final class PdfBatchProcessor {
    private PdfBatchProcessor() {
        throw new IllegalStateException("Utility class");
    }

    public static PdfBatchResult process(List<PdfDocumentRequest> requests, PdfBatchOptions options) {
        List<PdfDocumentRequest> immutableRequests = List.copyOf(requests);
        preflight(immutableRequests);
        PdfRasterBudget rasterBudget = new PdfRasterBudget(options.maximumInFlightRasterBytes());
        if (options.failFast()) {
            return processFailFast(immutableRequests, rasterBudget);
        }
        try (ExecutorService executor = Executors.newFixedThreadPool(options.parallelism(),
                Thread.ofVirtual().name("shaft-pdf-batch-", 0).factory())) {
            List<Future<PdfBatchItemResult>> futures = immutableRequests.stream().map(request -> executor.submit(() -> {
                try {
                    return new PdfBatchItemResult(request.source(), PdfDocumentProcessor.process(request.source(),
                            request.options(), request.exports(), rasterBudget), null);
                } catch (RuntimeException exception) {
                    return new PdfBatchItemResult(request.source(), null, failure(exception));
                }
            })).toList();
            List<PdfBatchItemResult> results = new ArrayList<>();
            for (Future<PdfBatchItemResult> future : futures) {
                try {
                    PdfBatchItemResult item = future.get();
                    results.add(item);
                } catch (InterruptedException exception) {
                    Thread.currentThread().interrupt();
                    futures.forEach(pending -> pending.cancel(true));
                    throw new IllegalStateException("PDF batch processing was interrupted.", exception);
                } catch (ExecutionException exception) {
                    throw new IllegalStateException("PDF batch scheduler failed.", exception.getCause());
                }
            }
            return new PdfBatchResult(results);
        }
    }

    private static PdfBatchResult processFailFast(List<PdfDocumentRequest> requests, PdfRasterBudget rasterBudget) {
        List<PdfBatchItemResult> results = new ArrayList<>();
        boolean failed = false;
        for (PdfDocumentRequest request : requests) {
            if (failed) {
                results.add(new PdfBatchItemResult(request.source(), null,
                        "Cancelled: an earlier PDF batch item failed."));
                continue;
            }
            try {
                results.add(new PdfBatchItemResult(request.source(), PdfDocumentProcessor.process(request.source(),
                        request.options(), request.exports(), rasterBudget), null));
            } catch (RuntimeException exception) {
                results.add(new PdfBatchItemResult(request.source(), null, failure(exception)));
                failed = true;
            }
        }
        return new PdfBatchResult(results);
    }

    private static void preflight(List<PdfDocumentRequest> requests) {
        Set<Path> sources = new HashSet<>();
        Set<Path> outputs = new HashSet<>();
        for (PdfDocumentRequest request : requests) {
            if (!sources.add(request.source())) {
                throw new IllegalArgumentException("Duplicate PDF batch input: " + request.source());
            }
            for (var export : request.exports()) {
                if (sources.contains(export.output()) || export.output().equals(request.source())
                        || !outputs.add(export.output())) {
                    throw new IllegalArgumentException("PDF batch input/output collision: " + export.output());
                }
            }
        }
        for (Path source : sources) {
            if (outputs.contains(source)) {
                throw new IllegalArgumentException("PDF batch output collides with an input: " + source);
            }
        }
    }

    private static String failure(RuntimeException exception) {
        String message = exception.getMessage();
        return exception.getClass().getSimpleName() + (message == null || message.isBlank() ? "" : ": " + message);
    }
}
