package com.shaft.capture.cli;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.shaft.capture.control.CaptureControlClient;
import com.shaft.capture.control.CaptureControlFiles;
import com.shaft.capture.control.CaptureControlServer;
import com.shaft.capture.generate.CaptureGenerationRequest;
import com.shaft.capture.generate.CaptureGenerationResult;
import com.shaft.capture.generate.CaptureGenerator;
import com.shaft.capture.generate.CodegenFeatureCatalog;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.privacy.CapturePrivacyClassifier;
import com.shaft.capture.runtime.CaptureBrowser;
import com.shaft.capture.runtime.CaptureManager;
import com.shaft.capture.runtime.CaptureStartRequest;
import com.shaft.capture.runtime.CaptureStartOptions;
import com.shaft.capture.runtime.CaptureStatus;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.shaft.pilot.ai.EvidenceCategory;

import java.io.File;
import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.lang.management.ManagementFactory;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.SecureRandom;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Executable local CLI for managed SHAFT Capture recording.
 */
public final class CaptureCli {
    private static final Duration START_TIMEOUT = Duration.ofSeconds(60);
    private static final Path DEFAULT_RUNTIME = Path.of("target", "shaft-capture");
    private static final DateTimeFormatter FILE_TIME = DateTimeFormatter
            .ofPattern("yyyyMMdd-HHmmss")
            .withZone(ZoneOffset.UTC);
    private static final ObjectMapper MAPPER = new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    private static final PrintWriter OUTPUT = new PrintWriter(
            new OutputStreamWriter(new FileOutputStream(FileDescriptor.out), StandardCharsets.UTF_8), true);
    private static final PrintWriter ERROR = new PrintWriter(
            new OutputStreamWriter(new FileOutputStream(FileDescriptor.err), StandardCharsets.UTF_8), true);

    private CaptureCli() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * CLI entry point.
     *
     * @param args capture command arguments
     */
    public static void main(String[] args) {
        int exitCode = run(args, defaultLaunchPrefix());
        if (exitCode != 0) {
            System.exit(exitCode);
        }
    }

    /**
     * Runs a command with an explicit daemon launch prefix.
     *
     * @param args capture command arguments
     * @param launchPrefix command used to relaunch this CLI before the daemon subcommand
     * @return process-style exit code
     */
    public static int run(String[] args, List<String> launchPrefix) {
        try {
            if (args == null || args.length == 0) {
                throw new IllegalArgumentException(usage());
            }
            String command = args[0].trim().toLowerCase(Locale.ROOT);
            Arguments options = Arguments.parse(Arrays.copyOfRange(args, 1, args.length));
            return switch (command) {
                case "start" -> start(options, launchPrefix);
                case "status" -> status(options);
                case "stop" -> stop(options);
                case "checkpoint" -> checkpoint(options);
                case "generate" -> generate(options);
                case "features" -> features();
                case "daemon" -> daemon(options);
                default -> throw new IllegalArgumentException(usage());
            };
        } catch (RuntimeException exception) {
            ERROR.println("SHAFT Capture command failed: " + safeMessage(exception));
            return 1;
        }
    }

    private static int start(Arguments options, List<String> launchPrefix) {
        Path runtime = options.path("runtime-dir", DEFAULT_RUNTIME);
        CaptureControlFiles files = new CaptureControlFiles(runtime);
        CaptureControlClient client = new CaptureControlClient(runtime);
        if (files.hasActiveControl() && isRunning(client.status().state())) {
            throw new IllegalStateException("A SHAFT Capture session is already active.");
        }
        String url = options.required("url");
        CaptureBrowser browser = CaptureBrowser.parse(options.value("browser", "chrome"));
        Path output = options.path(
                "output",
                Path.of("recordings", "capture-" + FILE_TIME.format(Instant.now()) + ".json"));
        CaptureStartRequest request = new CaptureStartRequest(
                url,
                browser,
                output,
                runtime,
                options.flag("headless"),
                startOptions(options));
        var safeUrl = new CapturePrivacyClassifier().sanitizeUrl(url).value();
        files.writeStatus(new CaptureStatus(
                CaptureStatus.State.STARTING,
                "",
                browser.name().toLowerCase(),
                safeUrl,
                0,
                List.of(),
                request.outputPath().toString(),
                false,
                0,
                Instant.now()));
        Path requestFile = files.writeLaunchRequest(request);
        Process process = launchDaemon(launchPrefix, runtime, requestFile);
        CaptureStatus started = awaitStart(client, files, process);
        print(started);
        return started.state() == CaptureStatus.State.ACTIVE ? 0 : 1;
    }

    private static int status(Arguments options) {
        CaptureStatus status = new CaptureControlClient(
                options.path("runtime-dir", DEFAULT_RUNTIME)).status();
        print(status);
        return 0;
    }

    private static int stop(Arguments options) {
        CaptureControlClient client = new CaptureControlClient(
                options.path("runtime-dir", DEFAULT_RUNTIME));
        CaptureStatus status = client.status();
        if (!isRunning(status.state())) {
            print(status);
            return 0;
        }
        print(client.stop(options.flag("discard")));
        return 0;
    }

    private static int checkpoint(Arguments options) {
        CaptureControlClient client = new CaptureControlClient(
                options.path("runtime-dir", DEFAULT_RUNTIME));
        Checkpoint.CheckpointKind kind;
        try {
            kind = Checkpoint.CheckpointKind.valueOf(
                    options.value("kind", "USER_MARKER").toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException exception) {
            throw new IllegalArgumentException("Unsupported checkpoint kind.", exception);
        }
        print(client.checkpoint(options.required("description"), kind));
        return 0;
    }

    private static int generate(Arguments options) {
        CaptureGenerationRequest.EnrichmentMode enrichmentMode =
                options.flag("ai-preview")
                        ? CaptureGenerationRequest.EnrichmentMode.PREVIEW
                        : options.values().containsKey("apply-enrichment")
                        ? CaptureGenerationRequest.EnrichmentMode.APPLY
                        : CaptureGenerationRequest.EnrichmentMode.NONE;
        Path output = options.path("output-dir", Path.of("generated-tests"));
        Path preview = enrichmentMode == CaptureGenerationRequest.EnrichmentMode.APPLY
                ? options.pathRequired("apply-enrichment")
                : options.path("enrichment-preview",
                output.resolve("target/shaft-capture/enrichment-preview.json"));
        ApprovalPolicy approval = new ApprovalPolicy(
                options.flag("allow-local-ai"),
                options.flag("allow-remote-ai"),
                EnumSet.of(EvidenceCategory.TEXT));
        long timeoutSeconds = parsePositiveLong(
                options.value("replay-timeout-seconds", "300"),
                "replay-timeout-seconds");
        CaptureGenerationResult result = new CaptureGenerator().generate(new CaptureGenerationRequest(
                options.pathRequired("session"),
                output,
                options.value("package", "generated.capture"),
                options.value("class-name", ""),
                options.flag("overwrite"),
                !options.flag("skip-compile"),
                options.flag("replay"),
                Duration.ofSeconds(timeoutSeconds),
                enrichmentMode,
                preview,
                options.flag("approve-enrichment"),
                approval));
        print(result);
        return result.successful() ? 0 : 1;
    }

    private static int features() {
        printJson(CodegenFeatureCatalog.features(), "Capture feature inventory could not be serialized.");
        return 0;
    }

    private static int daemon(Arguments options) {
        Path runtime = options.path("runtime-dir", DEFAULT_RUNTIME);
        CaptureControlFiles files = new CaptureControlFiles(runtime);
        CaptureStartRequest request = files.consumeLaunchRequest(options.pathRequired("request-file"));
        if (!request.runtimeDirectory().equals(runtime.toAbsolutePath().normalize())) {
            throw new IllegalArgumentException("Capture launch request runtime directory does not match.");
        }
        String token = token();
        CountDownLatch stopped = new CountDownLatch(1);
        CaptureManager manager = new CaptureManager();
        CaptureControlServer server = new CaptureControlServer(manager, files, token, stopped::countDown);
        Thread shutdownHook = new Thread(() -> {
            manager.close();
            files.writeStatus(manager.status());
            files.clearActiveControl();
        }, "shaft-capture-shutdown");
        Runtime.getRuntime().addShutdownHook(shutdownHook);
        try {
            int port = server.start();
            files.writeToken(token);
            files.writeDescriptor(new CaptureControlFiles.ControlDescriptor(
                    port, ProcessHandle.current().pid()));
            CaptureStatus active = manager.start(request);
            files.writeStatus(active);
            while (!stopped.await(500, TimeUnit.MILLISECONDS)) {
                CaptureStatus current = manager.status();
                files.writeStatus(current);
                if (!isRunning(current.state())) {
                    break;
                }
            }
            files.writeStatus(manager.status());
            return 0;
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            manager.close();
            files.writeStatus(manager.status());
            return 1;
        } catch (RuntimeException exception) {
            CaptureStatus failed = new CaptureStatus(
                    CaptureStatus.State.FAILED,
                    manager.status().sessionId(),
                    request.browser().name().toLowerCase(),
                    new CapturePrivacyClassifier().sanitizeUrl(request.targetUrl()).value(),
                    manager.status().eventCount(),
                    List.of("Managed browser startup failed."),
                    request.outputPath().toString(),
                    false,
                    ProcessHandle.current().pid(),
                    Instant.now());
            files.writeStatus(failed);
            return 1;
        } finally {
            server.close();
            manager.close();
            files.clearActiveControl();
            try {
                Runtime.getRuntime().removeShutdownHook(shutdownHook);
            } catch (IllegalStateException ignored) {
                // JVM shutdown is already running the same cleanup.
            }
        }
    }

    private static Process launchDaemon(List<String> launchPrefix, Path runtime, Path requestFile) {
        if (launchPrefix == null || launchPrefix.isEmpty()) {
            throw new IllegalArgumentException("Capture daemon launch command is unavailable.");
        }
        List<String> command = new ArrayList<>(launchPrefix);
        command.add("daemon");
        command.add("--runtime-dir");
        command.add(runtime.toAbsolutePath().normalize().toString());
        command.add("--request-file");
        command.add(requestFile.toAbsolutePath().normalize().toString());
        try {
            Path normalizedRuntime = runtime.toAbsolutePath().normalize();
            Files.createDirectories(normalizedRuntime);
            Path log = normalizedRuntime.resolve("daemon.log");
            return new ProcessBuilder(command)
                    .directory(normalizedRuntime.toFile())
                    .redirectOutput(ProcessBuilder.Redirect.appendTo(log.toFile()))
                    .redirectError(ProcessBuilder.Redirect.appendTo(log.toFile()))
                    .start();
        } catch (java.io.IOException exception) {
            throw new IllegalStateException("SHAFT Capture daemon process could not be launched.", exception);
        }
    }

    private static CaptureStatus awaitStart(
            CaptureControlClient client,
            CaptureControlFiles files,
            Process process) {
        Instant deadline = Instant.now().plus(START_TIMEOUT);
        while (Instant.now().isBefore(deadline)) {
            if (!files.hasActiveControl() && process.isAlive()) {
                sleepForStart();
                continue;
            }
            CaptureStatus status = client.status();
            if (status.state() == CaptureStatus.State.ACTIVE
                    || status.state() == CaptureStatus.State.FAILED
                    || status.state() == CaptureStatus.State.INCOMPLETE) {
                return status;
            }
            if (!process.isAlive()) {
                return files.readStatus();
            }
            sleepForStart();
        }
        process.destroy();
        return new CaptureStatus(
                CaptureStatus.State.FAILED,
                "",
                "",
                "",
                0,
                List.of("Managed browser startup timed out."),
                "",
                false,
                process.pid(),
                null);
    }

    private static void sleepForStart() {
        try {
            Thread.sleep(200);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("Interrupted while starting SHAFT Capture.", exception);
        }
    }

    private static List<String> defaultLaunchPrefix() {
        String javaCommand = ProcessHandle.current().info().command().orElse("java");
        return List.of(
                javaCommand,
                "-cp",
                absoluteClassPath(ManagementFactory.getRuntimeMXBean().getClassPath()),
                CaptureCli.class.getName());
    }

    private static String absoluteClassPath(String classPath) {
        return Arrays.stream(classPath.split(Pattern.quote(File.pathSeparator)))
                .map(entry -> Path.of(entry).toAbsolutePath().normalize().toString())
                .collect(Collectors.joining(File.pathSeparator));
    }

    private static String token() {
        byte[] bytes = new byte[32];
        new SecureRandom().nextBytes(bytes);
        return Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
    }

    private static void print(CaptureStatus status) {
        printJson(status, "Capture status could not be serialized.");
    }

    private static void print(CaptureGenerationResult result) {
        printJson(result, "Capture generation result could not be serialized.");
    }

    private static void printJson(Object value, String failureMessage) {
        try {
            OUTPUT.println(MAPPER.writeValueAsString(value));
        } catch (com.fasterxml.jackson.core.JsonProcessingException exception) {
            throw new IllegalStateException(failureMessage, exception);
        }
    }

    private static boolean isRunning(CaptureStatus.State state) {
        return state == CaptureStatus.State.STARTING
                || state == CaptureStatus.State.ACTIVE
                || state == CaptureStatus.State.STOPPING;
    }

    private static String safeMessage(RuntimeException exception) {
        String message = exception.getMessage();
        return message == null || message.isBlank() ? "operation failed." : message;
    }

    private static String usage() {
        return "Usage: capture start --url <url> [--browser chrome|chromium|edge] [--output <path>] "
                + "[--runtime-dir <path>] [--headless] [--viewport-size <width,height>] "
                + "[--test-id-attribute <name>] [--user-agent <value>] [--user-data-dir <path>] "
                + "[--lang <locale>] [--proxy-server <url>] [--proxy-bypass <list>] "
                + "[--ignore-https-errors] | status | stop [--discard] | "
                + "checkpoint --description <text> [--kind USER_MARKER|ASSERTION|PAGE_TRANSITION|RECOVERY] | "
                + "generate --session <capture.json> [--output-dir <path>] [--package <name>] "
                + "[--class-name <name>] [--overwrite] [--skip-compile] [--replay] "
                + "[--replay-timeout-seconds <seconds>] [--ai-preview --allow-local-ai|--allow-remote-ai] "
                + "[--enrichment-preview <path>] "
                + "[--apply-enrichment <path> --approve-enrichment] | features";
    }

    private static CaptureStartOptions startOptions(Arguments options) {
        return new CaptureStartOptions(
                options.value("target", ""),
                options.value("test-id-attribute", ""),
                options.value("channel", ""),
                options.value("device", ""),
                options.value("viewport-size", ""),
                options.value("color-scheme", ""),
                options.value("geolocation", ""),
                options.flag("ignore-https-errors"),
                options.flag("block-service-workers"),
                options.value("load-storage", ""),
                options.value("save-storage", ""),
                options.value("lang", ""),
                options.value("timezone", ""),
                options.value("proxy-server", ""),
                options.value("proxy-bypass", ""),
                options.value("save-har", ""),
                options.value("save-har-glob", ""),
                options.values().containsKey("timeout")
                        ? Duration.ofMillis(parsePositiveLong(options.value("timeout", "0"), "timeout"))
                        : Duration.ZERO,
                options.value("user-agent", ""),
                options.values().containsKey("user-data-dir")
                        ? Path.of(options.value("user-data-dir", ""))
                        : null);
    }

    private static long parsePositiveLong(String value, String option) {
        try {
            long parsed = Long.parseLong(value);
            if (parsed <= 0) {
                throw new NumberFormatException();
            }
            return parsed;
        } catch (NumberFormatException exception) {
            throw new IllegalArgumentException("Capture option --" + option
                    + " must be a positive integer.", exception);
        }
    }

    private record Arguments(Map<String, String> values, java.util.Set<String> flags) {
        private static Arguments parse(String[] args) {
            Map<String, String> values = new LinkedHashMap<>();
            java.util.Set<String> flags = new java.util.LinkedHashSet<>();
            for (int index = 0; index < args.length; index++) {
                String argument = args[index];
                if (!argument.startsWith("--")) {
                    throw new IllegalArgumentException("Unexpected capture argument.");
                }
                String name = argument.substring(2);
                if (Set.of(
                        "headless",
                        "discard",
                        "overwrite",
                        "skip-compile",
                        "replay",
                        "ai-preview",
                        "approve-enrichment",
                        "allow-local-ai",
                        "allow-remote-ai",
                        "ignore-https-errors",
                        "block-service-workers").contains(name)) {
                    flags.add(name);
                } else {
                    if (index + 1 >= args.length || args[index + 1].startsWith("--")) {
                        throw new IllegalArgumentException("Capture option --" + name + " requires a value.");
                    }
                    values.put(name, args[++index]);
                }
            }
            return new Arguments(Map.copyOf(values), Set.copyOf(flags));
        }

        private String required(String name) {
            String value = values.get(name);
            if (value == null || value.isBlank()) {
                throw new IllegalArgumentException("Capture option --" + name + " is required.");
            }
            return value;
        }

        private String value(String name, String fallback) {
            String value = values.get(name);
            return value == null || value.isBlank() ? fallback : value;
        }

        private Path path(String name, Path fallback) {
            String value = values.get(name);
            return value == null || value.isBlank() ? fallback : Path.of(value);
        }

        private Path pathRequired(String name) {
            return Path.of(required(name));
        }

        private boolean flag(String name) {
            return flags.contains(name);
        }
    }
}
