package com.shaft.tools.io.internal;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

final class PlaywrightTraceTestFixtures {
    private PlaywrightTraceTestFixtures() {
    }

    static Path writeTrace(String trace) throws IOException {
        Path archive = Files.createTempFile("playwright-import", ".zip");
        try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(archive))) {
            output.putNextEntry(new ZipEntry("0-trace.trace"));
            output.write(trace.getBytes(StandardCharsets.UTF_8));
            output.closeEntry();
        }
        return archive;
    }
}
