package com.shaft.commandline;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * {@code ShaftCli.main} is not exercised here: it unconditionally calls {@code System.exit},
 * which would terminate the test JVM if invoked in-process (JDK 25 has no SecurityManager hook to
 * intercept it). That entry point is covered out-of-process instead, by
 * {@code ShaftCliEndToEndStdioIT} launching the real packaged CLI. This class covers the two
 * pieces of {@link ShaftCli} that ARE safely unit-testable in-process: the {@code --version}
 * provider and the UTF-8 writer factory {@code main} builds its streams from.
 */
class ShaftCliTest {

    @Test
    void manifestVersionProviderFallsBackToDevelopmentWhenNoImplementationVersion() {
        // Running from the test classpath (not a packaged jar), the package has no
        // Implementation-Version manifest entry, so the provider must fall back to "(development)".
        String[] version = new ShaftCli.ManifestVersionProvider().getVersion();

        assertArrayEquals(new String[]{"shaft-cli (development)"}, version);
    }

    @Test
    void utf8WriterEncodesNonAsciiTextAsUtf8(@TempDir Path temp) throws Exception {
        Path file = temp.resolve("utf8-writer-output.txt");
        Method utf8Writer = ShaftCli.class.getDeclaredMethod("utf8Writer", FileDescriptor.class);
        utf8Writer.setAccessible(true);

        try (FileOutputStream fileOutputStream = new FileOutputStream(file.toFile())) {
            PrintWriter writer = (PrintWriter) utf8Writer.invoke(null, fileOutputStream.getFD());
            writer.print("héllo wörld 日本語");
            writer.flush();
        }

        String written;
        try (FileInputStream in = new FileInputStream(file.toFile())) {
            written = new String(in.readAllBytes(), StandardCharsets.UTF_8);
        }
        assertEquals("héllo wörld 日本語", written);
        assertArrayEquals(Files.readAllBytes(file), "héllo wörld 日本語".getBytes(StandardCharsets.UTF_8));
    }
}
