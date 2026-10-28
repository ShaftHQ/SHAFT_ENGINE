package com.shaft.ai.local;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Supervisor argument and path checks. {@code main} calls {@code System.exit} only for a short
 * argument list, so that case runs in a child JVM. Validation runs in-process and throws before
 * a child is launched.
 */
class ManagedLocalAiProcessSupervisorTest {
    @TempDir
    Path temp;

    @Test
    void shortArgumentListExitsTwoWithoutLaunching() throws Exception {
        Path java = Path.of(System.getProperty("java.home"), "bin", "java");
        Process process = new ProcessBuilder(
                java.toString(),
                "-cp",
                System.getProperty("java.class.path"),
                ManagedLocalAiProcessSupervisor.class.getName(),
                "1",
                Instant.EPOCH.toString()).start();

        assertTrue(process.waitFor(10, TimeUnit.SECONDS));
        assertEquals(2, process.exitValue());
    }

    @Test
    void rejectsAParentInstantThatDoesNotParse() throws Exception {
        assertThrows(java.time.format.DateTimeParseException.class,
                () -> ManagedLocalAiProcessSupervisor.main(new String[] {
                        Long.toString(ProcessHandle.current().pid()),
                        "not-an-instant",
                        temp.toAbsolutePath().toString(),
                        temp.resolve("child").toString()
                }));
    }

    @Test
    void ignoresALivePidWhoseStartInstantDoesNotMatch() throws Exception {
        ManagedLocalAiProcessSupervisor.main(new String[] {
                Long.toString(ProcessHandle.current().pid()),
                Instant.EPOCH.toString(),
                temp.toAbsolutePath().toString(),
                temp.resolve("must-not-start").toString()
        });
        assertTrue(Files.notExists(temp.resolve("must-not-start")));
    }

    @Test
    void rejectsRelativeAndUnownedChildPathsBeforeLaunch() throws Exception {
        String[] parent = currentParent();
        IllegalArgumentException relative = assertThrows(IllegalArgumentException.class,
                () -> ManagedLocalAiProcessSupervisor.main(new String[] {
                        parent[0], parent[1], "relative-dir", "relative-dir/child"
                }));
        assertTrue(relative.getMessage().contains("absolute"));

        Path executable = Files.writeString(temp.resolve("child.bin"), "not-used");
        IllegalArgumentException unowned = assertThrows(IllegalArgumentException.class,
                () -> ManagedLocalAiProcessSupervisor.main(new String[] {
                        parent[0], parent[1],
                        temp.resolve("other").toAbsolutePath().toString(),
                        executable.toAbsolutePath().toString()
                }));
        assertTrue(unowned.getMessage().contains("executable-owned"));
    }

    @Test
    void rejectsMissingAndLinkedChildPaths() throws Exception {
        String[] parent = currentParent();
        Path directory = Files.createDirectories(temp.resolve("ordinary"));
        Path missing = directory.resolve("missing.bin");
        assertThrows(IOException.class, () -> ManagedLocalAiProcessSupervisor.main(new String[] {
                parent[0], parent[1], directory.toAbsolutePath().toString(), missing.toAbsolutePath().toString()
        }));

        Path real = Files.writeString(temp.resolve("real.bin"), "binary");
        Path link = directory.resolve("linked.bin");
        try {
            Files.createSymbolicLink(link, real);
        } catch (IOException | UnsupportedOperationException exception) {
            org.junit.jupiter.api.Assumptions.assumeTrue(false, "Symbolic links are required for this check");
            return;
        }
        IllegalArgumentException linked = assertThrows(IllegalArgumentException.class,
                () -> ManagedLocalAiProcessSupervisor.main(new String[] {
                        parent[0], parent[1], directory.toAbsolutePath().toString(), link.toAbsolutePath().toString()
                }));
        assertTrue(linked.getMessage().contains("not ordinary"));
    }

    private static String[] currentParent() {
        ProcessHandle current = ProcessHandle.current();
        return new String[] {
                Long.toString(current.pid()),
                current.info().startInstant().orElseThrow().toString()
        };
    }
}
