package com.shaft.capture.generate.api;

import com.shaft.capture.format.CaptureFormatException;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.generate.CaptureEnrichmentPreview;
import com.shaft.capture.generate.CaptureEnrichmentService;
import com.shaft.capture.generate.CaptureGenerationReport;
import com.shaft.capture.generate.CaptureGenerationRequest;
import com.shaft.capture.generate.GeneratedTestValidator;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureReadiness;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.network.BodyRef;
import com.shaft.capture.model.network.HttpRequestRecord;
import com.shaft.capture.model.network.HttpResponseRecord;
import com.shaft.capture.model.network.ResourceKind;
import com.shaft.capture.generate.api.internal.OpenApiCoverageReporter;
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
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

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
    private final CaptureEnrichmentService enrichmentService;

    public ApiCaptureGenerator() {
        this(new CaptureJsonCodec(), new GeneratedTestValidator(), new NetworkBodyStore(), new CaptureEnrichmentService());
    }

    public ApiCaptureGenerator(CaptureJsonCodec codec, GeneratedTestValidator validator, NetworkBodyStore bodyStore) {
        this(codec, validator, bodyStore, new CaptureEnrichmentService());
    }

    /**
     * Creates a generator with an injectable AI naming-enrichment service, for tests that need to
     * fake the provider boundary without any network/process call.
     *
     * @param codec session codec
     * @param validator compile/replay validator
     * @param bodyStore network body resolver
     * @param enrichmentService AI naming-enrichment service (see
     *                          {@link CaptureEnrichmentService#previewApiScenarioName})
     */
    public ApiCaptureGenerator(
            CaptureJsonCodec codec, GeneratedTestValidator validator, NetworkBodyStore bodyStore,
            CaptureEnrichmentService enrichmentService) {
        this.codec = codec;
        this.validator = validator;
        this.bodyStore = bodyStore;
        this.enrichmentService = enrichmentService;
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

        Analysis analysis = analyze(request, session, sessionPath);
        if (!analysis.unsupported().isEmpty()) {
            return failure(reportPath, session.sessionId(), analysis.unsupported(), request.overwrite());
        }

        return renderCompileAndReport(request, outputRoot, reportPath, session, analysis);
    }

    /**
     * Resolves the generated class name, validates it and the package name, and extracts
     * renderable transactions. Isolated from {@link #generate} purely to keep that method's own
     * cyclomatic/NPath complexity low -- PMD's NPath metric is multiplicative across sequential
     * branches in one method body, so folding every validation step into a single method
     * explodes combinatorially even though each step is simple.
     */
    private Analysis analyze(ApiCaptureGenerationRequest request, CaptureSession session, Path sessionPath) {
        String className = request.className().isBlank() ? defaultClassName(session) : request.className();
        List<String> unsupported = new ArrayList<>();
        if (!isValidJavaIdentifier(className)) {
            unsupported.add("Generated class name \"" + className + "\" is not a valid Java identifier.");
        }
        if (!isValidPackageName(request.packageName())) {
            unsupported.add("Package name \"" + request.packageName() + "\" is not a valid Java package name.");
        }
        if (!unsupported.isEmpty()) {
            return new Analysis(className, List.of(), unsupported);
        }

        Path bodiesDirectory = sessionPath.getParent().resolve(session.sessionId() + "-network-bodies");
        List<ApiTransaction> transactions = extractTransactions(session, bodiesDirectory, request.excludedTransactionIds());
        if (transactions.isEmpty()) {
            unsupported.add("No renderable API transactions (XHR/FETCH with a recorded response) "
                    + "were found in this session.");
        }
        return new Analysis(className, transactions, unsupported);
    }

    /**
     * Renders, writes, optionally compiles/replays, and reports one generated class. Isolated
     * from {@link #generate} for the same NPath reason as {@link #analyze}.
     */
    private ApiCaptureGenerationResult renderCompileAndReport(
            ApiCaptureGenerationRequest request, Path outputRoot, Path reportPath, CaptureSession session,
            Analysis analysis) {
        EnrichmentOutcome enrichmentOutcome = resolveEnrichment(request, session, outputRoot, analysis.className());
        if (!enrichmentOutcome.unsupported().isEmpty()) {
            return failure(reportPath, session.sessionId(), enrichmentOutcome.unsupported(), request.overwrite());
        }
        String className = enrichmentOutcome.className();

        Path sourcePath = outputRoot.resolve("src/test/java")
                .resolve(request.packageName().replace('.', '/'))
                .resolve(className + ".java")
                .normalize();
        Path testDataDirectory = outputRoot.resolve("src/test/resources/test-data/api-capture").normalize();
        Path classesDirectory = outputRoot.resolve("target/shaft-capture/classes").normalize();
        ensureWithin(outputRoot, sourcePath);
        ensureWithin(outputRoot, testDataDirectory);
        ensureWithin(outputRoot, classesDirectory);

        RenderedApiTest rendered = ApiTestRenderer.render(request.packageName(), className,
                analysis.transactions(), request.style(), request.validationDepth(), Map.of(),
                Set.copyOf(request.pinnedJsonPaths()));
        List<String> warnings = warningsFor(rendered);

        if (!request.overwrite() && Files.exists(sourcePath)) {
            return failure(reportPath, session.sessionId(),
                    List.of("Generated source already exists at " + sourcePath + " and overwrite was not requested."),
                    request.overwrite());
        }

        try {
            writeArtifacts(outputRoot, sourcePath, rendered);
        } catch (CaptureFormatException writeFailure) {
            return failure(reportPath, session.sessionId(),
                    List.of("Generated artifacts could not be written: " + safeMessage(writeFailure)), request.overwrite());
        }

        return compileAndReport(request, session, new Analysis(className, analysis.transactions(), List.of()),
                sourcePath, testDataDirectory, classesDirectory, warnings, reportPath, outputRoot,
                enrichmentOutcome.enrichment());
    }

    /**
     * Resolves the class name AI naming enrichment may apply, gated by {@code ApprovalPolicy} via
     * {@link CaptureEnrichmentService#previewApiScenarioName}. Only {@link ApiCodegenStyle#HYBRID_UI_API}
     * uses AI naming; every other style, and {@code enrichmentMode == NONE} (the default), returns the
     * deterministic class name unchanged and never calls AI. {@code PREVIEW} writes a reviewable
     * preview file and keeps the deterministic name; {@code APPLY} reads a previously reviewed
     * preview file and requires {@code enrichmentApproved} (enforced by
     * {@link ApiCaptureGenerationRequest}'s constructor) plus a matching deterministic fingerprint.
     */
    private EnrichmentOutcome resolveEnrichment(
            ApiCaptureGenerationRequest request, CaptureSession session, Path outputRoot, String deterministicClassName) {
        CaptureGenerationReport.Enrichment notRequested = CaptureGenerationReport.Enrichment.notRequested();
        if (request.style() != ApiCodegenStyle.HYBRID_UI_API
                || request.enrichmentMode() == CaptureGenerationRequest.EnrichmentMode.NONE) {
            return new EnrichmentOutcome(deterministicClassName, notRequested, List.of());
        }
        Path previewPath = request.enrichmentPreviewPath().toAbsolutePath().normalize();
        String deterministicMethodName = "recordedHybridScenario";
        String fingerprint = fingerprint(session.sessionId(), deterministicClassName, deterministicMethodName);

        if (request.enrichmentMode() == CaptureGenerationRequest.EnrichmentMode.PREVIEW) {
            CaptureEnrichmentPreview preview = enrichmentService.previewApiScenarioName(
                    session.sessionId(), fingerprint, transactionSummaries(session),
                    deterministicClassName, deterministicMethodName, request.aiApprovalPolicy());
            atomicWrite(previewPath, writeJson(preview));
            CaptureGenerationReport.Enrichment enrichment = new CaptureGenerationReport.Enrichment(
                    CaptureGenerationReport.Enrichment.EnrichmentStatus.PREVIEWED,
                    relative(outputRoot, previewPath), preview.diff(), preview.provider());
            return new EnrichmentOutcome(deterministicClassName, enrichment, List.of());
        }

        CaptureEnrichmentPreview preview;
        try {
            preview = readJson(previewPath, CaptureEnrichmentPreview.class);
        } catch (RuntimeException unreadable) {
            return new EnrichmentOutcome(deterministicClassName, notRequested,
                    List.of("AI enrichment preview could not be read: " + safeMessage(unreadable)));
        }
        if (!fingerprint.equals(preview.deterministicFingerprint())) {
            return new EnrichmentOutcome(deterministicClassName, notRequested,
                    List.of("AI enrichment preview does not match the deterministic source; regenerate the preview."));
        }
        String proposedClassName = preview.proposal().className();
        boolean validProposedName = !proposedClassName.isBlank() && isValidJavaIdentifier(proposedClassName);
        String resolvedClassName = validProposedName ? proposedClassName : deterministicClassName;
        CaptureGenerationReport.Enrichment enrichment = new CaptureGenerationReport.Enrichment(
                CaptureGenerationReport.Enrichment.EnrichmentStatus.APPLIED,
                relative(outputRoot, previewPath), preview.diff(), preview.provider());
        return new EnrichmentOutcome(resolvedClassName, enrichment, List.of());
    }

    private static List<String> transactionSummaries(CaptureSession session) {
        List<String> summaries = new ArrayList<>();
        for (CaptureEvent event : session.events()) {
            if (event instanceof CaptureEvent.NetworkEvent networkEvent) {
                summaries.add(networkEvent.request().method() + " " + networkEvent.request().url());
            }
        }
        return List.copyOf(summaries);
    }

    private static String fingerprint(String sessionId, String className, String methodName) {
        try {
            java.security.MessageDigest digest = java.security.MessageDigest.getInstance("SHA-256");
            byte[] bytes = digest.digest((sessionId + "\n" + className + "\n" + methodName)
                    .getBytes(StandardCharsets.UTF_8));
            return java.util.HexFormat.of().formatHex(bytes);
        } catch (java.security.NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 is unavailable.", exception);
        }
    }

    private static String writeJson(CaptureEnrichmentPreview preview) {
        return new tools.jackson.databind.json.JsonMapper().writerWithDefaultPrettyPrinter().writeValueAsString(preview);
    }

    private static <T> T readJson(Path path, Class<T> type) {
        try {
            String content = Files.readString(path, StandardCharsets.UTF_8);
            return new tools.jackson.databind.json.JsonMapper().readValue(content, type);
        } catch (IOException | RuntimeException failure) {
            throw new IllegalStateException("Could not read " + path + ": " + safeMessage(failure), failure);
        }
    }

    /**
     * Outcome of {@link #resolveEnrichment}: the class name to render with (deterministic, or an
     * AI-applied replacement), the enrichment status to report, and any reasons generation must
     * fail (empty when it can proceed).
     */
    private record EnrichmentOutcome(String className, CaptureGenerationReport.Enrichment enrichment, List<String> unsupported) {
    }

    private static List<String> warningsFor(RenderedApiTest rendered) {
        if (rendered.skippedTransactionIds().isEmpty()) {
            return List.of();
        }
        return List.of("Skipped " + rendered.skippedTransactionIds().size()
                + " recorded transaction(s) with an HTTP method SHAFT.API cannot build: "
                + String.join(", ", rendered.skippedTransactionIds()));
    }

    private void writeArtifacts(Path outputRoot, Path sourcePath, RenderedApiTest rendered) {
        atomicWrite(sourcePath, rendered.source());
        for (Map.Entry<String, String> artifact : rendered.testDataArtifacts().entrySet()) {
            Path artifactPath = outputRoot.resolve("src/test/resources/test-data").resolve(artifact.getKey()).normalize();
            ensureWithin(outputRoot, artifactPath);
            atomicWrite(artifactPath, artifact.getValue());
        }
    }

    private ApiCaptureGenerationResult compileAndReport(
            ApiCaptureGenerationRequest request, CaptureSession session, Analysis analysis, Path sourcePath,
            Path testDataDirectory, Path classesDirectory, List<String> warnings, Path reportPath, Path outputRoot,
            CaptureGenerationReport.Enrichment enrichment) {
        String className = analysis.className();
        CaptureGenerationReport.Validation compilation = request.compile()
                ? validator.compile(sourcePath, classesDirectory)
                : CaptureGenerationReport.Validation.skipped("Compilation was not requested.");
        boolean replayEligible = request.compile() && request.replay()
                && compilation.status() == CaptureGenerationReport.Validation.ValidationStatus.PASSED;
        CaptureGenerationReport.Validation replay = replayEligible
                ? validator.replay(request.packageName() + "." + className,
                        classesDirectory, testDataDirectory.getParent(), outputRoot, Duration.ofMinutes(2))
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
                enrichment,
                openApiCoverage(request, analysis.transactions()));
        writeReportIfPossible(reportPath, report, request.overwrite());
        return new ApiCaptureGenerationResult(sourcePath, testDataDirectory, reportPath, report);
    }

    private static CaptureGenerationReport.OpenApiCoverage openApiCoverage(
            ApiCaptureGenerationRequest request, List<ApiTransaction> transactions) {
        if (request.openApiSpecPath() == null) {
            return CaptureGenerationReport.OpenApiCoverage.notRequested();
        }
        OpenApiCoverageReporter.CoverageReport coverage =
                OpenApiCoverageReporter.report(request.openApiSpecPath(), transactions);
        return new CaptureGenerationReport.OpenApiCoverage(
                coverage.loadable(),
                coverage.loadFailureReason(),
                coverage.coveredOperations(),
                coverage.missingOperations(),
                coverage.undeclaredOperations(),
                coverage.totalDeclaredOperations(),
                coverage.coverageRatio());
    }

    /**
     * Outcome of {@link #analyze}: the resolved class name, extracted transactions (empty when
     * generation cannot proceed), and any reasons generation must fail (empty when it can).
     */
    private record Analysis(String className, List<ApiTransaction> transactions, List<String> unsupported) {
    }

    // ---- transaction extraction ----

    private List<ApiTransaction> extractTransactions(
            CaptureSession session, Path bodiesDirectory, List<String> excludedTransactionIds) {
        List<ApiTransaction> transactions = new ArrayList<>();
        String lastDedupeKey = null;
        for (CaptureEvent event : session.events()) {
            if (!(event instanceof CaptureEvent.NetworkEvent networkEvent)) {
                continue;
            }
            if (!isRenderableResourceKind(networkEvent.resourceKind()) || networkEvent.response() == null) {
                continue;
            }
            if (excludedTransactionIds.contains(networkEvent.transactionId())) {
                // Deselected in the recorder's transaction table (issue #3548 item 3): honor the
                // caller's include/exclude choice by omitting it before dedupe/rendering, not just
                // decorating the table client-side.
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
                ResponseNormalizer.classify(responseBody),
                event.correlatedUiSequence());
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
