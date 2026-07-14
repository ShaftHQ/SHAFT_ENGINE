package com.shaft.capture.generate;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression coverage for the orphaned-browser defect: a timed-out replay used to call only
 * {@code Process.destroyForcibly()} on the forked TestNG JVM, which does not kill descendants
 * (ChromeDriver/Chrome). {@code Process.destroyForcibly()} makes no cross-platform promise about
 * descendants -- on POSIX it never touches them, and even where the OS incidentally cleans up
 * simple child chains, real-world browser processes are documented to escape that cleanup. The
 * only portable fix is to enumerate and kill the process tree explicitly, which is what
 * {@link GeneratedTestValidator#destroyProcessTree(Process)} does.
 */
class GeneratedTestValidatorProcessTreeTest {

    @TempDir
    Path temp;

    @Test
    void destroyProcessTreeTerminatesEveryDescendantAcrossMultipleGenerations() throws Exception {
        Path classesDir = temp.resolve("classes");
        Files.createDirectories(classesDir);
        compile(classesDir, "Grandchild", GRANDCHILD_SOURCE);
        compile(classesDir, "Child", CHILD_SOURCE);
        compile(classesDir, "Top", TOP_SOURCE);
        String javaExe = ProcessHandle.current().info().command().orElse("java");

        Process top = new ProcessBuilder(javaExe, "-cp", classesDir.toString(), "Top", classesDir.toString())
                .redirectErrorStream(true)
                .redirectOutput(ProcessBuilder.Redirect.DISCARD)
                .start();
        List<ProcessHandle> descendants = awaitDescendants(top, 2);
        assertTrue(top.isAlive(), "the top-level process should still be alive before teardown");
        assertTrue(descendants.stream().allMatch(ProcessHandle::isAlive),
                "both spawned descendants should be alive before teardown");

        GeneratedTestValidator.destroyProcessTree(top);

        assertFalse(top.isAlive(), "destroyProcessTree() must kill the top-level process");
        assertFalse(descendants.stream().anyMatch(ProcessHandle::isAlive),
                "destroyProcessTree() must kill every descendant, not just the top-level process");
    }

    /**
     * Waits until the process reports at least {@code minimumCount} descendants (a child and its
     * own child both register once spawned), which can take a moment on a loaded machine.
     */
    private static List<ProcessHandle> awaitDescendants(Process process, int minimumCount) throws InterruptedException {
        for (int attempt = 0; attempt < 100; attempt++) {
            List<ProcessHandle> descendants = process.descendants().toList();
            if (descendants.size() >= minimumCount) {
                return descendants;
            }
            Thread.sleep(100);
        }
        throw new IllegalStateException("The process tree never grew to " + minimumCount + " descendants.");
    }

    private static void compile(Path classesDir, String className, String source) throws Exception {
        Path sourceFile = classesDir.resolve(className + ".java");
        Files.writeString(sourceFile, source, StandardCharsets.UTF_8);
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        int result = compiler.run(null, null, null,
                "-d", classesDir.toString(), sourceFile.toString());
        if (result != 0) {
            throw new IllegalStateException("Failed to compile " + className + " for the process-tree test.");
        }
    }

    private static final String GRANDCHILD_SOURCE = """
            public class Grandchild {
                public static void main(String[] args) throws Exception {
                    Thread.sleep(60_000);
                }
            }
            """;

    // Child spawns Grandchild, so the top-level process has two generations of descendants beneath
    // it -- mirroring the forked TestNG JVM -> ChromeDriver -> Chrome shape in production.
    private static final String CHILD_SOURCE = """
            public class Child {
                public static void main(String[] args) throws Exception {
                    String javaHome = System.getProperty("java.home");
                    String javaExe = javaHome + java.io.File.separator + "bin" + java.io.File.separator + "java";
                    ProcessBuilder builder = new ProcessBuilder(javaExe, "-cp", args[0], "Grandchild");
                    builder.inheritIO();
                    builder.start();
                    Thread.sleep(60_000);
                }
            }
            """;

    private static final String TOP_SOURCE = """
            public class Top {
                public static void main(String[] args) throws Exception {
                    String javaHome = System.getProperty("java.home");
                    String javaExe = javaHome + java.io.File.separator + "bin" + java.io.File.separator + "java";
                    ProcessBuilder builder = new ProcessBuilder(javaExe, "-cp", args[0], "Child", args[0]);
                    builder.inheritIO();
                    builder.start();
                    Thread.sleep(60_000);
                }
            }
            """;
}
