package com.shaft.capture.generate;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.TimeUnit;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.stream.Stream;

/**
 * Compiles generated Java and optionally replays it in an isolated TestNG process.
 */
public class GeneratedTestValidator {
    private static final ObjectMapper JSON = new ObjectMapper();

    /**
     * Compiles one generated test class against the current SHAFT runtime.
     *
     * @param source generated Java source
     * @param classesDirectory isolated class output
     * @return compilation result
     */
    public CaptureGenerationReport.Validation compile(Path source, Path classesDirectory) {
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            return failed("A full Java 25 JDK is required to compile generated tests.");
        }
        DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
        try {
            Files.createDirectories(classesDirectory);
            try (StandardJavaFileManager files = compiler.getStandardFileManager(
                    diagnostics, Locale.ROOT, StandardCharsets.UTF_8)) {
                Iterable<? extends JavaFileObject> units =
                        files.getJavaFileObjectsFromPaths(List.of(source));
                List<String> options = List.of(
                        "--release", "25",
                        "-classpath", resolvedClasspath(classesDirectory),
                        "-d", classesDirectory.toString(),
                        "-encoding", "UTF-8");
                boolean success = Boolean.TRUE.equals(compiler.getTask(
                        null, files, diagnostics, options, null, units).call());
                List<String> safeDiagnostics = diagnostics.getDiagnostics().stream()
                        .map(GeneratedTestValidator::diagnostic)
                        .limit(20)
                        .toList();
                return new CaptureGenerationReport.Validation(
                        success
                                ? CaptureGenerationReport.Validation.ValidationStatus.PASSED
                                : CaptureGenerationReport.Validation.ValidationStatus.FAILED,
                        safeDiagnostics,
                        0);
            }
        } catch (IOException exception) {
            return failed("Generated source compilation could not be initialized.");
        }
    }

    /**
     * Replays a compiled TestNG class and requires populated, passing Allure results.
     *
     * @param fullyQualifiedClassName generated class
     * @param classesDirectory compiled classes
     * @param resourcesDirectory generated test resources
     * @param workDirectory isolated replay directory
     * @param timeout maximum replay duration
     * @return replay result
     */
    public CaptureGenerationReport.Validation replay(
            String fullyQualifiedClassName,
            Path classesDirectory,
            Path resourcesDirectory,
            Path workDirectory,
            Duration timeout) {
        Path replayDirectory = workDirectory.resolve("target/shaft-capture/replay");
        Path allureResults = replayDirectory.resolve("allure-results");
        Path testngOutput = replayDirectory.resolve("testng-output");
        Path outputLog = replayDirectory.resolve("replay.log");
        try {
            Files.createDirectories(allureResults);
            Files.createDirectories(testngOutput);
            List<String> command = new ArrayList<>();
            command.add(javaCommand());
            command.add("-Dallure.results.directory=" + allureResults.toAbsolutePath().normalize());
            command.add("-DheadlessExecution="
                    + System.getProperty("headlessExecution", "true"));
            command.add("-cp");
            command.add(classesDirectory.toAbsolutePath().normalize()
                    + File.pathSeparator
                    + resourcesDirectory.toAbsolutePath().normalize()
                    + File.pathSeparator
                    + resolvedClasspath(classesDirectory));
            command.add("org.testng.TestNG");
            command.add("-usedefaultlisteners");
            command.add("false");
            command.add("-d");
            command.add(testngOutput.toAbsolutePath().normalize().toString());
            command.add("-testclass");
            command.add(fullyQualifiedClassName);
            Process process = new ProcessBuilder(command)
                    .directory(workDirectory.toAbsolutePath().normalize().toFile())
                    .redirectErrorStream(true)
                    .redirectOutput(outputLog.toFile())
                    .start();
            boolean finished = process.waitFor(timeout.toMillis(), TimeUnit.MILLISECONDS);
            if (!finished) {
                process.destroyForcibly();
                return failed("Generated test replay timed out.");
            }
            AllureSummary allure = allure(allureResults);
            List<String> diagnostics = new ArrayList<>();
            if (process.exitValue() != 0) {
                diagnostics.add("TestNG replay exited with code " + process.exitValue() + ".");
            }
            diagnostics.addAll(allure.diagnostics());
            boolean passed = process.exitValue() == 0 && allure.count() > 0 && allure.failed() == 0;
            return new CaptureGenerationReport.Validation(
                    passed
                            ? CaptureGenerationReport.Validation.ValidationStatus.PASSED
                            : CaptureGenerationReport.Validation.ValidationStatus.FAILED,
                    List.copyOf(diagnostics),
                    allure.count());
        } catch (IOException exception) {
            return failed("Generated test replay could not be launched.");
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            return failed("Generated test replay was interrupted.");
        }
    }

    private static AllureSummary allure(Path directory) throws IOException {
        int count = 0;
        int failed = 0;
        try (Stream<Path> files = Files.list(directory)) {
            for (Path file : files.filter(path -> path.getFileName().toString().endsWith("-result.json")).toList()) {
                count++;
                JsonNode result = JSON.readTree(Files.readString(file, StandardCharsets.UTF_8));
                if (!"passed".equalsIgnoreCase(result.path("status").asText())) {
                    failed++;
                }
            }
        }
        List<String> diagnostics = count == 0
                ? List.of("Replay produced no populated Allure result files.")
                : failed == 0
                ? List.of("Replay produced " + count + " passing Allure result file(s).")
                : List.of("Replay produced " + failed + " non-passing Allure result file(s).");
        return new AllureSummary(count, failed, diagnostics);
    }

    private static String diagnostic(Diagnostic<? extends JavaFileObject> diagnostic) {
        return "line " + diagnostic.getLineNumber() + ": "
                + diagnostic.getMessage(Locale.ROOT).replaceAll("\\s+", " ").trim();
    }

    private static CaptureGenerationReport.Validation failed(String diagnostic) {
        return new CaptureGenerationReport.Validation(
                CaptureGenerationReport.Validation.ValidationStatus.FAILED,
                List.of(diagnostic),
                0);
    }

    private static String javaCommand() {
        return ProcessHandle.current().info().command().orElse("java");
    }

    private static String resolvedClasspath(Path classesDirectory) {
        List<String> resolved = new ArrayList<>();
        for (String entry : System.getProperty("java.class.path", "").split(
                java.util.regex.Pattern.quote(File.pathSeparator))) {
            if (entry.isBlank()) {
                continue;
            }
            Path path = Path.of(entry).toAbsolutePath().normalize();
            if (isSpringBootArchive(path)) {
                resolved.addAll(extractSpringBootClasspath(
                        path,
                        classesDirectory.toAbsolutePath().normalize().getParent()
                                .resolve("runtime-classpath")));
            } else {
                resolved.add(path.toString());
            }
        }
        return String.join(File.pathSeparator, resolved);
    }

    private static boolean isSpringBootArchive(Path path) {
        if (!Files.isRegularFile(path) || !path.getFileName().toString().endsWith(".jar")) {
            return false;
        }
        try (JarFile jar = new JarFile(path.toFile())) {
            return jar.getEntry("BOOT-INF/lib/") != null || jar.stream()
                    .anyMatch(entry -> entry.getName().startsWith("BOOT-INF/lib/"));
        } catch (IOException exception) {
            return false;
        }
    }

    private static synchronized List<String> extractSpringBootClasspath(Path archive, Path destination) {
        Path classes = destination.resolve("classes").toAbsolutePath().normalize();
        Path libraries = destination.resolve("lib").toAbsolutePath().normalize();
        try {
            Files.createDirectories(classes);
            Files.createDirectories(libraries);
            try (JarFile jar = new JarFile(archive.toFile())) {
                for (JarEntry entry : java.util.Collections.list(jar.entries())) {
                    if (entry.isDirectory()) {
                        continue;
                    }
                    if (entry.getName().startsWith("BOOT-INF/classes/")) {
                        Path output = classes.resolve(
                                entry.getName().substring("BOOT-INF/classes/".length())).normalize();
                        copyJarEntry(jar, entry, output, classes);
                    } else if (entry.getName().startsWith("BOOT-INF/lib/")
                            && entry.getName().endsWith(".jar")) {
                        Path output = libraries.resolve(
                                entry.getName().substring("BOOT-INF/lib/".length())).normalize();
                        copyJarEntry(jar, entry, output, libraries);
                    }
                }
            }
            List<String> classpath = new ArrayList<>();
            classpath.add(classes.toString());
            try (Stream<Path> files = Files.list(libraries)) {
                files.filter(Files::isRegularFile)
                        .sorted()
                        .map(Path::toString)
                        .forEach(classpath::add);
            }
            return List.copyOf(classpath);
        } catch (IOException exception) {
            throw new IllegalStateException("Spring Boot runtime classpath could not be extracted.", exception);
        }
    }

    private static void copyJarEntry(
            JarFile jar,
            JarEntry entry,
            Path output,
            Path allowedRoot) throws IOException {
        if (!output.toAbsolutePath().normalize().startsWith(allowedRoot)) {
            throw new IOException("Spring Boot archive entry escapes the validation directory.");
        }
        Files.createDirectories(output.getParent());
        try (var input = jar.getInputStream(entry)) {
            Files.copy(input, output, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        }
    }

    private record AllureSummary(int count, int failed, List<String> diagnostics) {
    }
}
