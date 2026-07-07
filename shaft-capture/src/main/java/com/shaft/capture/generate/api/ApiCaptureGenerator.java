package com.shaft.capture.generate.api;

import com.shaft.capture.format.CaptureFormatException;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.generate.CaptureGenerationReport;
import com.shaft.capture.generate.GeneratedTestValidator;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureReadiness;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.network.BodyRef;
import com.shaft.capture.model.network.HttpRequestRecord;
import com.shaft.capture.model.network.HttpResponseRecord;
import com.shaft.capture.model.network.ResourceKind;
import com.shaft.capture.storage.NetworkBodyStore;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AccessDeniedException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Generates, optionally compiles, and optionally replays a {@code SHAFT.API} TestNG class from a
 * recorded session's API transactions. Mirrors
 * {@code com.shaft.capture.generate.CaptureGenerator}'s validate-&gt;analyze-&gt;render-&gt;
 * compile-&gt;replay-&gt;report staging, reusing {@link GeneratedTestValidator} and
 * {@link CaptureGenerationReport} rather than inventing parallel types.
 */
public final class ApiCaptureGenerator {
    private final CaptureJsonCodec codec;
    private final GeneratedTestValidator validator;
    private final NetworkBodyStore bodyStore;

    public ApiCaptureGenerator() {
        this(new CaptureJsonCodec(), new GeneratedTestValidator(), new NetworkBodyStore());
    }

    public ApiCaptureGenerator(CaptureJsonCodec codec, GeneratedTestValidator validator, NetworkBodyStore bodyStore) {
        this.codec = codec;
        this.validator = validator;
        this.bodyStore = bodyStore;
    }

    /**
     * Generates, validates, and reports one {@code SHAFT.API} test class.
     *
     * @param request generation options
     * @return generated artifacts and report
     */
    public ApiCaptureGenerationResult generate(ApiCaptureGenerationRequest request) {
        Path sessionPath = request.sessionPath().toAbsolutePath().normalize();
        Path outputRoot = request.outputDirectory().toAbsolutePath().normalize();
        Path reportPath = outputRoot.resolve("target/shaft-capture/api-generation-report.json");

        CaptureSession session;
        try {
            session = codec.read(sessionPath);
        } catch (RuntimeException exception) {
            return failure(reportPath, "", List.of("Session could not be read: " + safeMessage(exception)), request.overwrite());
        }

        String className = request.className().isBlank() ? defaultClassName(session) : request.className();
        List<String> unsupported = new ArrayList<>();
        if (!isValidJavaIdentifier(className)) {
            unsupported.add("Generated class name \"" + className + "\" is not a valid Java identifier.");
        }
        if (!isValidPackageName(request.packageName())) {
            unsupported.add("Package name \"" + request.packageName() + "\" is not a valid Java package name.");
        }

        Path bodiesDirectory = sessionPath.getParent().resolve(session.sessionId() + "-network-bodies");
        List<ApiTransaction> transactions = unsupported.isEmpty()
                ? extractTransactions(session, bodiesDirectory)
                : List.of();
        if (unsupported.isEmpty() && transactions.isEmpty()) {
            unsupported.add("No renderable API transactions (XHR/FETCH with a recorded response) "
                    + "were found in this session.");
        }
        if (!unsupported.isEmpty()) {
            return failure(reportPath, session.sessionId(), unsupported, request.overwrite());
        }

        Path sourcePath = outputRoot.resolve("src/test/java")
                .resolve(request.packageName().replace('.', '/'))
                .resolve(className + ".java")
                .normalize();
        Path testDataDirectory = outputRoot.resolve("src/test/resources/test-data/api-capture").normalize();
        Path classesDirectory = outputRoot.resolve("target/shaft-capture/classes").normalize();
        ensureWithin(outputRoot, sourcePath);
        ensureWithin(outputRoot, testDataDirectory);
        ensureWithin(outputRoot, classesDirectory);

        RenderedApiTest rendered = ApiTestRenderer.render(
                request.packageName(), className, transactions, request.style(), request.validationDepth());

        List<String> warnings = new ArrayList<>();
        if (!rendered.skippedTransactionIds().isEmpty()) {
            warnings.add("Skipped " + rendered.skippedTransactionIds().size()
                    + " recorded transaction(s) with an HTTP method SHAFT.API cannot build: "
                    + String.join(", ", rendered.skippedTransactionIds()));
        }

        if (!request.overwrite() && Files.exists(sourcePath)) {
            unsupported.add("Generated source already exists at " + sourcePath + " and overwrite was not requested.");
            return failure(reportPath, session.sessionId(), unsupported, request.overwrite());
        }

        try {
            atomicWrite(sourcePath, rendered.source());
            for (Map.Entry<String, String> artifact : rendered.testDataArtifacts().entrySet()) {
                Path artifactPath = outputRoot.resolve("src/test/resources/test-data").resolve(artifact.getKey()).normalize();
                ensureWithin(outputRoot, artifactPath);
                atomicWrite(artifactPath, artifact.getValue());
            }
        } catch (CaptureFormatException writeFailure) {
            unsupported.add("Generated artifacts could not be written: " + safeMessage(writeFailure));
            return failure(reportPath, session.sessionId(), unsupported, request.overwrite());
        }

        CaptureGenerationReport.Validation compilation = request.compile()
                ? validator.compile(sourcePath, classesDirectory)
                : CaptureGenerationReport.Validation.skipped("Compilation was not requested.");
        CaptureGenerationReport.Validation replay = (request.compile() && request.replay()
                && compilation.status() == CaptureGenerationReport.Validation.ValidationStatus.PASSED)
                ? validator.replay(request.packageName() + "." + className, classesDirectory,
                        testDataDirectory.getParent(), outputRoot, Duration.ofMinutes(2))
                : CaptureGenerationReport.Validation.skipped("Replay was not requested.");

        boolean failed = compilation.status() == CaptureGenerationReport.Validation.ValidationStatus.FAILED
                || replay.status() == CaptureGenerationReport.Validation.ValidationStatus.FAILED;
        CaptureGenerationReport.Status status = failed
                ? CaptureGenerationReport.Status.FAILED
                : CaptureGenerationReport.Status.SUCCESS;

        CaptureGenerationReport report = new CaptureGenerationReport(
                CaptureGenerationReport.CURRENT_SCHEMA_VERSION,
                session.sessionId(),
                status,
                relative(outputRoot, sourcePath),
                relative(outputRoot, testDataDirectory),
                CaptureReadiness.State.READY,
                List.of(),
                List.of(),
                List.of(),
                List.of(),
                List.of(),
                List.of(),
                List.of(),
                warnings,
                compilation,
                replay,
                CaptureGenerationReport.Enrichment.notRequested());
        writeReportIfPossible(reportPath, report, request.overwrite());
        return new ApiCaptureGenerationResult(sourcePath, testDataDirectory, reportPath, report);
    }

    // ---- transaction extraction ----

    private List<ApiTransaction> extractTransactions(CaptureSession session, Path bodiesDirectory) {
        List<ApiTransaction> transactions = new ArrayList<>();
        String lastDedupeKey = null;
        for (CaptureEvent event : session.events()) {
            if (!(event instanceof CaptureEvent.NetworkEvent networkEvent)) {
                continue;
            }
            if (!isRenderableResourceKind(networkEvent.resourceKind()) || networkEvent.response() == null) {
                continue;
            }
            String dedupeKey = networkEvent.request().method() + " " + networkEvent.request().url();
            if (dedupeKey.equals(lastDedupeKey)) {
                // Consecutive identical requests are treated as the same recorded polling call;
                // only the first is rendered, avoiding a wall of duplicate near-identical assertions.
                continue;
            }
            lastDedupeKey = dedupeKey;
            transactions.add(toApiTransaction(networkEvent, bodiesDirectory));
        }
        return List.copyOf(transactions);
    }

    private static boolean isRenderableResourceKind(ResourceKind kind) {
        return kind == ResourceKind.XHR || kind == ResourceKind.FETCH;
    }

    private ApiTransaction toApiTransaction(CaptureEvent.NetworkEvent event, Path bodiesDirectory) {
        HttpRequestRecord request = event.request();
        HttpResponseRecord response = event.response();
        String requestBody = resolveBody(request.body(), bodiesDirectory);
        String responseBody = resolveBody(response.body(), bodiesDirectory);
        return new ApiTransaction(
                event.transactionId(),
                request.method(),
                request.url(),
                originOf(request.url()),
                request.headers(),
                requestBody,
                response.statusCode(),
                response.headers(),
                responseBody,
                ResponseNormalizer.classify(responseBody));
    }

    private String resolveBody(BodyRef bodyRef, Path bodiesDirectory) {
        return bodyRef == null ? "" : bodyStore.read(bodyRef, bodiesDirectory);
    }

    private static String originOf(String url) {
        try {
            URI uri = new URI(url);
            String scheme = uri.getScheme();
            String host = uri.getHost();
            if (scheme == null || host == null) {
                return "";
            }
            int port = uri.getPort();
            return scheme.toLowerCase(Locale.ROOT) + "://" + host.toLowerCase(Locale.ROOT)
                    + (port == -1 ? "" : ":" + port);
        } catch (URISyntaxException | RuntimeException malformed) {
            return "";
        }
    }

    // ---- naming/validation ----

    private static String defaultClassName(CaptureSession session) {
        String sanitized = session.sessionId().replaceAll("[^A-Za-z0-9]", "_");
        if (sanitized.isBlank() || Character.isDigit(sanitized.charAt(0))) {
            sanitized = "Session" + sanitized;
        }
        return "ApiCapture" + Character.toUpperCase(sanitized.charAt(0)) + sanitized.substring(1) + "Test";
    }

    private static boolean isValidJavaIdentifier(String candidate) {
        if (candidate.isBlank() || !Character.isJavaIdentifierStart(candidate.charAt(0))) {
            return false;
        }
        return candidate.chars().allMatch(Character::isJavaIdentifierPart);
    }

    private static boolean isValidPackageName(String candidate) {
        if (candidate.isBlank()) {
            return false;
        }
        return java.util.Arrays.stream(candidate.split("\\.")).allMatch(ApiCaptureGenerator::isValidJavaIdentifier);
    }

    private static void ensureWithin(Path root, Path target) {
        if (!target.toAbsolutePath().normalize().startsWith(root.toAbsolutePath().normalize())) {
            throw new IllegalArgumentException("Generated artifact path escapes the output directory.");
        }
    }

    private static String relative(Path root, Path target) {
        try {
            return root.relativize(target).toString().replace('\\', '/');
        } catch (IllegalArgumentException notRelativizable) {
            return target.toString();
        }
    }

    private static String safeMessage(Exception exception) {
        String message = exception.getMessage();
        return message == null || message.isBlank() ? "Generation failed." : message;
    }

    // ---- report/io helpers ----

    private ApiCaptureGenerationResult failure(Path reportPath, String sessionId, List<String> unsupported, boolean overwrite) {
        CaptureGenerationReport report = new CaptureGenerationReport(
                CaptureGenerationReport.CURRENT_SCHEMA_VERSION,
                sessionId,
                CaptureGenerationReport.Status.FAILED,
                "",
                "",
                CaptureReadiness.State.READY,
                List.of(),
                List.of(),
                unsupported,
                List.of(),
                List.of(),
                List.of(),
                List.of(),
                List.of(),
                CaptureGenerationReport.Validation.skipped("Generation failed before compilation."),
                CaptureGenerationReport.Validation.skipped("Generation failed before replay."),
                CaptureGenerationReport.Enrichment.notRequested());
        writeReportIfPossible(reportPath, report, overwrite);
        return new ApiCaptureGenerationResult(null, null, reportPath, report);
    }

    private static void writeReportIfPossible(Path reportPath, CaptureGenerationReport report, boolean overwrite) {
        try {
            if (!overwrite && Files.exists(reportPath)) {
                return;
            }
            atomicWrite(reportPath, reportJson(report));
        } catch (RuntimeException ignored) {
            // Report writing is best-effort; a missing report must never fail generation that
            // otherwise succeeded.
        }
    }

    private static String reportJson(CaptureGenerationReport report) {
        return new tools.jackson.databind.json.JsonMapper()
                .writerWithDefaultPrettyPrinter()
                .writeValueAsString(report);
    }

    private static void atomicWrite(Path destination, String content) {
        Path absolute = destination.toAbsolutePath().normalize();
        Path temporary = null;
        try {
            Files.createDirectories(absolute.getParent());
            temporary = Files.createTempFile(absolute.getParent(), "." + absolute.getFileName(), ".tmp");
            Files.writeString(temporary, content, StandardCharsets.UTF_8);
            moveReplacing(temporary, absolute);
        } catch (IOException failure) {
            throw new CaptureFormatException("Generated artifact could not be written atomically.", failure);
        } finally {
            if (temporary != null) {
                try {
                    Files.deleteIfExists(temporary);
                } catch (IOException ignored) {
                    // Best-effort cleanup of an unpublished temporary artifact.
                }
            }
        }
    }

    private static void moveReplacing(Path temporary, Path destination) throws IOException {
        boolean atomic = true;
        for (int attempt = 1; attempt <= 50; attempt++) {
            try {
                if (atomic) {
                    Files.move(temporary, destination, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
                } else {
                    Files.move(temporary, destination, StandardCopyOption.REPLACE_EXISTING);
                }
                return;
            } catch (AtomicMoveNotSupportedException ignored) {
                atomic = false;
            } catch (AccessDeniedException exception) {
                if (attempt == 50) {
                    throw exception;
                }
                try {
                    Thread.sleep(10);
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    throw new IOException("Interrupted while publishing generated artifact.", interrupted);
                }
            }
        }
    }
}
