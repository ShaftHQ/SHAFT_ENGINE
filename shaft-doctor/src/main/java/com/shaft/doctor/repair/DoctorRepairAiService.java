package com.shaft.doctor.repair;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.doctor.internal.DoctorRedactor;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.pilot.ai.AiExecutionService;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.EvidenceCategory;
import com.shaft.pilot.ai.EvidenceReference;
import com.shaft.pilot.json.JsonSchemaValidator;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;

/**
 * Generates optional schema-constrained file patches through the shared AI boundary.
 */
public final class DoctorRepairAiService {
    private static final String SCHEMA_RESOURCE =
            "/schema/shaft-doctor-repair-patch-1.0.schema.json";
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final JsonNode RESPONSE_SCHEMA = loadSchema();

    private final Function<AiRequest, AiResponse> executor;

    /**
     * Creates a service using the configured shared provider registry.
     */
    public DoctorRepairAiService() {
        this(new AiExecutionService()::execute);
    }

    DoctorRepairAiService(Function<AiRequest, AiResponse> executor) {
        this.executor = Objects.requireNonNull(executor, "executor");
    }

    /**
     * Requests structured file patches without applying them or executing provider commands.
     *
     * @param repositoryRoot approved repository root
     * @param diagnosis deterministic Doctor diagnosis
     * @param allowedPaths exact approved source file paths
     * @param request explicit provider policy
     * @return validated patches or an empty safe fallback
     */
    public DoctorRepairPatchResult generate(
            Path repositoryRoot,
            Diagnosis diagnosis,
            List<String> allowedPaths,
            DoctorRepairAiRequest request) {
        if (request == null || !request.enabled()) {
            return fallback(AiResponseStatus.DISABLED, "", "", "Provider patch generation is disabled.");
        }
        if (diagnosis == null) {
            throw new IllegalArgumentException("Doctor diagnosis is required.");
        }
        Set<EvidenceCategory> categories = EnumSet.of(EvidenceCategory.TEXT, EvidenceCategory.SOURCE);
        if (!request.approvalPolicy().allowedEvidenceCategories().containsAll(categories)) {
            return fallback(AiResponseStatus.CONSENT_REQUIRED, "", "",
                    "Explicit TEXT and SOURCE evidence consent is required.");
        }
        Path repository = realDirectory(repositoryRoot);
        List<EvidenceReference> source = sources(
                repository, allowedPaths, request.maxSourceFiles(), request.maxSourceBytes());
        if (source.isEmpty()) {
            return fallback(AiResponseStatus.INVALID_RESPONSE, "", "",
                    "No approved regular source files were available.");
        }
        Set<String> allowed = source.stream().map(EvidenceReference::id)
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        ObjectNode deterministicFallback = JSON.createObjectNode();
        deterministicFallback.put("schemaVersion", "1.0");
        deterministicFallback.putArray("patches");
        AiRequest.Builder builder = AiRequest.builder("shaft-doctor-repair-patch", RESPONSE_SCHEMA)
                .text(prompt(diagnosis, allowedPaths))
                .timeout(request.timeout())
                .budget(request.budget())
                .approvalPolicy(request.approvalPolicy())
                .deterministicFallback(deterministicFallback);
        source.forEach(builder::evidence);

        AiResponse response;
        try {
            response = executor.apply(builder.build());
        } catch (RuntimeException exception) {
            return fallback(AiResponseStatus.ERROR, "", "", "Provider patch generation failed.");
        }
        if (response == null || !response.successful()) {
            return fallback(response == null ? AiResponseStatus.ERROR : response.status(),
                    response == null ? "" : response.provider(),
                    response == null ? "" : response.model(),
                    response == null ? "Provider returned no patch result." : response.fallbackReason());
        }
        try {
            byte[] bytes = JSON.writeValueAsBytes(response.structuredPayload());
            if (bytes.length > request.maxResponseBytes()
                    || !JsonSchemaValidator.validate(RESPONSE_SCHEMA, response.structuredPayload()).isEmpty()) {
                return fallback(AiResponseStatus.INVALID_RESPONSE, response.provider(), response.model(),
                        "Provider patch output failed schema or size validation.");
            }
            JsonNode redacted = new DoctorRedactor().redact(response.structuredPayload());
            if (!redacted.equals(response.structuredPayload())) {
                return fallback(AiResponseStatus.INVALID_RESPONSE, response.provider(), response.model(),
                        "Provider patch output contained secret-like material.");
            }
            List<DoctorRepairRequest.FilePatch> patches = new ArrayList<>();
            for (JsonNode value : response.structuredPayload().path("patches")) {
                String path = value.path("path").asText().replace('\\', '/');
                if (!allowed.contains(path)) {
                    return fallback(AiResponseStatus.INVALID_RESPONSE, response.provider(), response.model(),
                            "Provider patch referenced a file outside the approved source allowlist.");
                }
                List<String> evidenceIds = textList(value.path("evidenceIds"));
                if (!allowed.containsAll(evidenceIds)) {
                    return fallback(AiResponseStatus.INVALID_RESPONSE, response.provider(), response.model(),
                            "Provider patch referenced evidence that was not submitted.");
                }
                DoctorRepairRequest.FilePatch.Operation operation =
                        DoctorRepairRequest.FilePatch.Operation.valueOf(value.path("operation").asText());
                if (operation == DoctorRepairRequest.FilePatch.Operation.CREATE) {
                    return fallback(AiResponseStatus.INVALID_RESPONSE, response.provider(), response.model(),
                            "Provider-created files require a separate reviewed deterministic patch.");
                }
                patches.add(new DoctorRepairRequest.FilePatch(
                        path,
                        operation,
                        value.path("content").asText(),
                        value.path("rationale").asText(),
                        evidenceIds));
            }
            if (patches.isEmpty()) {
                return fallback(AiResponseStatus.INVALID_RESPONSE, response.provider(), response.model(),
                        "Provider returned no structured file patches.");
            }
            return new DoctorRepairPatchResult(
                    AiResponseStatus.SUCCESS, response.provider(), response.model(), patches, "");
        } catch (RuntimeException | IOException exception) {
            return fallback(AiResponseStatus.INVALID_RESPONSE, response.provider(), response.model(),
                    "Provider patch output could not be validated.");
        }
    }

    private static List<EvidenceReference> sources(
            Path repository,
            List<String> allowedPaths,
            int maxFiles,
            long maxBytes) {
        List<EvidenceReference> result = new ArrayList<>();
        long retained = 0;
        for (String value : allowedPaths == null ? List.<String>of() : allowedPaths) {
            if (result.size() >= maxFiles || retained >= maxBytes) {
                break;
            }
            String normalized = value == null ? "" : value.trim().replace('\\', '/');
            Path relative = Path.of(normalized).normalize();
            Path source = repository.resolve(relative).normalize();
            if (normalized.isBlank() || relative.isAbsolute() || relative.startsWith("..")
                    || !source.startsWith(repository)
                    || !Files.isRegularFile(source, LinkOption.NOFOLLOW_LINKS)
                    || hasSymlink(repository, source)
                    || protectedPath(normalized)) {
                continue;
            }
            try {
                byte[] bytes = Files.readAllBytes(source);
                if (containsNul(bytes) || bytes.length > maxBytes - retained) {
                    continue;
                }
                String content = new String(bytes, StandardCharsets.UTF_8);
                String redacted = new DoctorRedactor().redact(content);
                if (!redacted.equals(content)) {
                    continue;
                }
                result.add(new EvidenceReference(
                        normalized,
                        EvidenceCategory.SOURCE,
                        mediaType(normalized),
                        content));
                retained += bytes.length;
            } catch (IOException ignored) {
                // Unreadable files are omitted from the provider request.
            }
        }
        return List.copyOf(result);
    }

    private static String prompt(Diagnosis diagnosis, List<String> allowedPaths) {
        try {
            return """
                    Propose the smallest repair supported by the deterministic SHAFT Doctor diagnosis.
                    Return only complete UTF-8 file replacements in the requested JSON schema.
                    Never return shell commands, workflow changes, credentials, generated files, binary data,
                    hidden reasoning, or paths outside the explicit allowlist. Cite only submitted source IDs.

                    Approved paths:
                    %s

                    Deterministic diagnosis:
                    %s
                    """.formatted(
                    String.join("\n", allowedPaths == null ? List.of() : allowedPaths),
                    JSON.writeValueAsString(diagnosis));
        } catch (IOException exception) {
            throw new IllegalArgumentException("Doctor diagnosis could not be serialized.", exception);
        }
    }

    private static List<String> textList(JsonNode values) {
        List<String> result = new ArrayList<>();
        for (JsonNode value : values) {
            result.add(value.asText());
        }
        return List.copyOf(new LinkedHashSet<>(result));
    }

    private static DoctorRepairPatchResult fallback(
            AiResponseStatus status,
            String provider,
            String model,
            String reason) {
        String safe = new DoctorRedactor().redact(reason);
        return new DoctorRepairPatchResult(
                status, provider, model, List.of(),
                safe.isBlank() ? "Provider patch generation was unavailable." : safe);
    }

    private static Path realDirectory(Path path) {
        try {
            Path real = path.toRealPath();
            if (!Files.isDirectory(real)) {
                throw new IllegalArgumentException("Doctor repair repository root must be a directory.");
            }
            return real;
        } catch (IOException exception) {
            throw new IllegalArgumentException("Doctor repair repository root cannot be resolved.", exception);
        }
    }

    private static boolean containsNul(byte[] value) {
        for (byte item : value) {
            if (item == 0) {
                return true;
            }
        }
        return false;
    }

    private static boolean hasSymlink(Path repository, Path source) {
        Path current = repository;
        for (Path segment : repository.relativize(source)) {
            current = current.resolve(segment);
            if (Files.isSymbolicLink(current)) {
                return true;
            }
        }
        return false;
    }

    private static boolean protectedPath(String value) {
        String normalized = value.toLowerCase(Locale.ROOT);
        return normalized.equals(".git") || normalized.startsWith(".git/")
                || normalized.equals(".github/workflows")
                || normalized.startsWith(".github/workflows/")
                || normalized.equals("target") || normalized.startsWith("target/")
                || normalized.equals("build") || normalized.startsWith("build/");
    }

    private static String mediaType(String path) {
        String normalized = path.toLowerCase(Locale.ROOT);
        if (normalized.endsWith(".java")) {
            return "text/x-java-source";
        }
        if (normalized.endsWith(".json")) {
            return "application/json";
        }
        if (normalized.endsWith(".xml")) {
            return "application/xml";
        }
        return "text/plain";
    }

    private static JsonNode loadSchema() {
        try (InputStream input = DoctorRepairAiService.class.getResourceAsStream(SCHEMA_RESOURCE)) {
            if (input == null) {
                throw new IllegalStateException("Bundled Doctor repair patch schema is missing.");
            }
            return JSON.readTree(input);
        } catch (IOException exception) {
            throw new IllegalStateException("Bundled Doctor repair patch schema could not be read.", exception);
        }
    }
}
