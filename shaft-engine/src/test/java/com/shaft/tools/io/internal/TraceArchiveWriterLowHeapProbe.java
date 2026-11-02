package com.shaft.tools.io.internal;

import java.io.BufferedOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Random;
import java.util.zip.ZipFile;

/** Runs outside the test JVM so the archive writer's bounded-memory contract can be proven. */
public final class TraceArchiveWriterLowHeapProbe {
    private TraceArchiveWriterLowHeapProbe() {
    }

    public static void main(String[] args) throws Exception {
        Path directory = Path.of(args[0]);
        Path source = directory.resolve("incompressible.bin");
        byte[] chunk = new byte[16 * 1024];
        Random random = new Random(4691L);
        try (var output = new BufferedOutputStream(Files.newOutputStream(source))) {
            for (int written = 0; written < 48 * 1024 * 1024; written += chunk.length) {
                random.nextBytes(chunk);
                output.write(chunk);
            }
        }
        Path target = directory.resolve("shaft-trace.zip");
        FailureTraceReporter.renderTraceZip(target, "{}", "<html></html>", "[]", java.util.Map.of(), source,
                64 * 1024 * 1024, "omitted");
        try (ZipFile zip = new ZipFile(target.toFile())) {
            if (zip.getEntry(source.getFileName().toString()) == null
                    || zip.getEntry(source.getFileName().toString()).getSize() != Files.size(source)) {
                throw new IllegalStateException("Large streamed entry was not preserved.");
            }
        }
    }
}
