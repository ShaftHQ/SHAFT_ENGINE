package com.shaft.capture.generate;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.stream.Stream;

/**
 * Compiles generated Java and optionally replays it in an isolated TestNG process.
 */
public class GeneratedTestValidator {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final Duration PROCESS_TREE_KILL_GRACE = Duration.ofSeconds(5);

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
        Path chromeDriverLog = replayDirectory.resolve("chromedriver.log");
        Process process = null;
        try {
            Files.createDirectories(allureResults);
            Files.createDirectories(testngOutput);
            seedHeadlessCustomProperties(workDirectory);
            String classpath = classesDirectory.toAbsolutePath().normalize()
                    + File.pathSeparator
                    + resourcesDirectory.toAbsolutePath().normalize()
                    + File.pathSeparator
                    + resolvedClasspath(classesDirectory);
            // The JVM options and the full runtime classpath (hundreds of jar paths) go into a
            // Java @argfile: passed inline they exceed Windows' ~32K CreateProcess command-line
            // limit, which made every replay die at launch with a bare IOException.
            Path argsFile = replayDirectory.resolve("replay.args");
            StringBuilder argsContent = new StringBuilder();
            argsContent.append(argFileQuote(
                    "-Dallure.results.directory=" + allureResults.toAbsolutePath().normalize())).append('\n');
            // Always headless regardless of the outer process's own headlessExecution setting
            // (e.g. shaft-capture's custom.properties defaults to false for local interactive
            // development) -- this replay is a non-interactive validation run.
            argsContent.append("-DheadlessExecution=true").append('\n');
            argsContent.append("-Dwebdriver.chrome.verboseLogging=true").append('\n');
            argsContent.append(argFileQuote(
                    "-Dwebdriver.chrome.logfile=" + chromeDriverLog.toAbsolutePath().normalize())).append('\n');
            argsContent.append("-cp").append('\n');
            argsContent.append(argFileQuote(classpath)).append('\n');
            argsContent.append("org.testng.TestNG").append('\n');
            Files.writeString(argsFile, argsContent.toString());
            List<String> command = new ArrayList<>();
            command.add(javaCommand());
            command.add("@" + argsFile.toAbsolutePath().normalize());
            command.add("-usedefaultlisteners");
            command.add("false");
            command.add("-d");
            command.add(testngOutput.toAbsolutePath().normalize().toString());
            command.add("-testclass");
            command.add(fullyQualifiedClassName);
            process = new ProcessBuilder(command)
                    .directory(workDirectory.toAbsolutePath().normalize().toFile())
                    .redirectErrorStream(true)
                    .redirectOutput(outputLog.toFile())
                    .start();
            boolean finished = process.waitFor(timeout.toMillis(), TimeUnit.MILLISECONDS);
            if (!finished) {
                // destroyForcibly() only kills the forked TestNG JVM itself -- it leaves
                // ChromeDriver/Chrome (its descendants) running as orphans, especially on Windows.
                destroyProcessTree(process);
                return failed("Generated test replay timed out.");
            }
            AllureSummary allure = allure(allureResults);
            List<String> diagnostics = new ArrayList<>();
            if (process.exitValue() != 0) {
                diagnostics.add("TestNG replay exited with code " + process.exitValue() + ".");
            }
            diagnostics.addAll(allure.diagnostics());
            boolean passed = process.exitValue() == 0 && allure.count() > 0 && allure.failed() == 0;
            if (!passed) {
                diagnostics.add(replayProcessOutputTail(outputLog));
                if (Files.isRegularFile(chromeDriverLog)) {
                    diagnostics.add(boundedFileTail("ChromeDriver log", chromeDriverLog));
                }
            }
            return new CaptureGenerationReport.Validation(
                    passed
                            ? CaptureGenerationReport.Validation.ValidationStatus.PASSED
                            : CaptureGenerationReport.Validation.ValidationStatus.FAILED,
                    List.copyOf(diagnostics),
                    allure.count());
        } catch (IOException exception) {
            if (process != null) {
                destroyProcessTree(process);
            }
            return failed("Generated test replay could not be launched: " + exception.getMessage());
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            if (process != null) {
                destroyProcessTree(process);
            }
            return failed("Generated test replay was interrupted.");
        }
    }

    /**
     * Forcibly kills a process and every descendant it spawned (e.g. ChromeDriver and the Chrome
     * it launches beneath the forked TestNG JVM). {@link Process#destroyForcibly()} alone only
     * terminates the top-level process -- descendants are reparented and survive as orphans,
     * which on Windows leaves a visible browser window behind after a failed/timed-out replay.
     *
     * <p>Descendants must be snapshotted before the process is destroyed: {@link
     * Process#descendants()} reports nothing once the parent has already exited.
     *
     * @param process the process (and its process tree) to terminate
     */
    static void destroyProcessTree(Process process) {
        List<ProcessHandle> descendants = process.descendants().toList();
        process.destroyForcibly();
        descendants.forEach(ProcessHandle::destroyForcibly);
        // The top-level process already has a live OS handle from ProcessBuilder#start(), so
        // Process#waitFor() is the authoritative wait for it. Descendants are only known as
        // ProcessHandle snapshots (there is no Process object for them), so ProcessHandle#onExit()
        // is the only option there -- re-deriving a ProcessHandle for the top-level process via
        // Process#toHandle() after it has already been destroyed is avoided on purpose, since a
        // freshly looked-up handle can race a reused PID immediately after exit.
        try {
            process.waitFor(PROCESS_TREE_KILL_GRACE.toMillis(), TimeUnit.MILLISECONDS);
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
        }
        List<CompletableFuture<ProcessHandle>> descendantExits =
                descendants.stream().map(ProcessHandle::onExit).toList();
        try {
            CompletableFuture.allOf(descendantExits.toArray(new CompletableFuture[0]))
                    .get(PROCESS_TREE_KILL_GRACE.toMillis(), TimeUnit.MILLISECONDS);
        } catch (Exception exception) {
            // Best-effort grace period; destroyForcibly() was already issued to every handle above,
            // so a slow or already-gone process here does not change the outcome.
        }
    }

    /**
     * Quotes one Java @argfile line: backslashes become forward slashes (the argfile parser
     * treats backslashes as escapes) and embedded quotes are escaped.
     */
    private static String argFileQuote(String value) {
        return '"' + value.replace("\\", "/").replace("\"", "\\\"") + '"';
    }

    /**
     * Pre-seeds a {@code custom.properties} at the path SHAFT's engine bootstrap looks for one
     * relative to the replay's working directory, forcing headless/CI-safe Chrome flags.
     *
     * <p>Without this, the engine's own "create custom.properties from the bundled template if
     * missing" bootstrap step fills that same file with the template's {@code headlessExecution=false}
     * default and reloads it into system properties, silently discarding any headless override
     * passed to the replay subprocess on the command line — Chrome then launches without
     * {@code --headless} and crashes on runners with no display.
     *
     * @param workDirectory the replay subprocess's working directory
     */
    private static void seedHeadlessCustomProperties(Path workDirectory) throws IOException {
        Path customProperties = workDirectory.resolve("src/main/resources/properties/custom.properties");
        Files.createDirectories(customProperties.getParent());
        Files.writeString(customProperties,
                "headlessExecution=true" + System.lineSeparator()
                        + "automaticallyAddRecommendedChromeOptions=true" + System.lineSeparator(),
                StandardCharsets.UTF_8);
    }

    private static AllureSummary allure(Path directory) throws IOException {
        int count = 0;
        int failed = 0;
        List<String> failureMessages = new ArrayList<>();
        try (Stream<Path> files = Files.list(directory)) {
            for (Path file : files.filter(path -> path.getFileName().toString().endsWith("-result.json")).toList()) {
                count++;
                JsonNode result = JSON.readTree(Files.readString(file, StandardCharsets.UTF_8));
                if (!"passed".equalsIgnoreCase(result.path("status").asText())) {
                    failed++;
                    String message = result.path("statusDetails").path("message").asText("");
                    if (!message.isBlank() && failureMessages.size() < 5) {
                        failureMessages.add(boundedFailureMessage(result.path("name").asText(""), message));
                    }
                }
            }
        }
        List<String> diagnostics = new ArrayList<>();
        if (count == 0) {
            diagnostics.add("Replay produced no populated Allure result files.");
        } else if (failed == 0) {
            diagnostics.add("Replay produced " + count + " passing Allure result file(s).");
        } else {
            diagnostics.add("Replay produced " + failed + " non-passing Allure result file(s).");
            diagnostics.addAll(failureMessages);
        }
        return new AllureSummary(count, failed, List.copyOf(diagnostics));
    }

    private static final java.util.regex.Pattern RELEVANT_REPLAY_LOG_LINE = java.util.regex.Pattern.compile(
            "(?i)attempt|webdriver|chrome|session|exception|driver factory|caused by");

    private static String replayProcessOutputTail(Path outputLog) {
        try {
            List<String> lines = Files.readAllLines(outputLog, StandardCharsets.UTF_8);
            List<String> relevant = lines.stream()
                    .filter(line -> RELEVANT_REPLAY_LOG_LINE.matcher(line).find())
                    .map(line -> line.replaceAll("\\s+", " ").trim())
                    .toList();
            String joined = String.join(" | ", relevant.isEmpty() ? lines : relevant);
            String bounded = joined.length() > 3_000 ? joined.substring(0, 3_000) + "..." : joined;
            return "Replay process output (filtered): " + bounded;
        } catch (IOException exception) {
            return "Replay process output could not be read.";
        }
    }

    private static String boundedFileTail(String label, Path file) {
        try {
            List<String> lines = Files.readAllLines(file, StandardCharsets.UTF_8);
            String joined = String.join(" | ", lines).replaceAll("\\s+", " ").trim();
            String tail = joined.length() > 3_000 ? joined.substring(joined.length() - 3_000) : joined;
            return label + " (tail): " + tail;
        } catch (IOException exception) {
            return label + " could not be read.";
        }
    }

    private static String boundedFailureMessage(String name, String message) {
        String oneLine = message.replaceAll("\\s+", " ").trim();
        String bounded = oneLine.length() > 2_000 ? oneLine.substring(0, 2_000) + "..." : oneLine;
        return name.isBlank() ? bounded : name + ": " + bounded;
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
