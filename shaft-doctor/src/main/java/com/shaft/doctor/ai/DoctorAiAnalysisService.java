package com.shaft.doctor.ai;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.doctor.DoctorAiAnalysisRequest;
import com.shaft.doctor.format.DoctorFormatException;
import com.shaft.doctor.format.DoctorJsonCodec;
import com.shaft.doctor.internal.DoctorHashing;
import com.shaft.doctor.internal.DoctorRedactor;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.DoctorAdvisory;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.pilot.ai.AiExecutionService;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.ai.EvidenceReference;
import com.shaft.pilot.config.PilotConfiguration;
import com.shaft.pilot.json.JsonSchemaValidator;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Builds minimized Doctor provider requests and validates separately rendered advisories.
 */
public final class DoctorAiAnalysisService {
    private static final String SCHEMA_RESOURCE = "/schema/shaft-doctor-advisory-1.0.schema.json";
    private static final int MAX_TEXT_LENGTH = 2_000;
    private static final ObjectMapper JSON = new ObjectMapper()
            .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);
    private static final JsonNode RESPONSE_SCHEMA = loadSchema();

    private final Function<AiRequest, AiResponse> executor;
    private final Supplier<PilotConfiguration> configurationSupplier;

    /**
     * Creates a service using current SHAFT properties and the shared provider registry.
     */
    public DoctorAiAnalysisService() {
        this(new AiExecutionService()::execute, PilotConfiguration::current);
    }

    /**
     * Creates a service with injectable execution and configuration boundaries.
     *
     * @param executor provider execution function
     * @param configurationSupplier effective configuration supplier
     */
    public DoctorAiAnalysisService(
            Function<AiRequest, AiResponse> executor,
            Supplier<PilotConfiguration> configurationSupplier) {
        this.executor = Objects.requireNonNull(executor, "executor");
        this.configurationSupplier = Objects.requireNonNull(configurationSupplier, "configurationSupplier");
    }

    /**
     * Produces an optional advisory without changing deterministic diagnosis fields.
     *
     * @param bundle already-redacted deterministic evidence bundle
     * @param diagnosis deterministic diagnosis
     * @param request explicit provider policy
     * @param outputDirectory Doctor report output directory
     * @return validated advisory or safe fallback notice
     */
    public DoctorAdvisory analyze(
            EvidenceBundle bundle,
            Diagnosis diagnosis,
            DoctorAiAnalysisRequest request,
            Path outputDirectory) {
        Objects.requireNonNull(bundle, "bundle");
        Objects.requireNonNull(diagnosis, "diagnosis");
        Objects.requireNonNull(request, "request");
        Objects.requireNonNull(outputDirectory, "outputDirectory");
        if (!request.enabled()) {
            return DoctorAdvisory.disabled();
        }

        ConfigurationIdentity identity = configurationIdentity(request);
        List<EvidenceReference> evidence = minimize(bundle, diagnosis, request);
        Set<com.shaft.pilot.ai.EvidenceCategory> requestedCategories =
                EnumSet.of(com.shaft.pilot.ai.EvidenceCategory.TEXT);
        evidence.forEach(item -> requestedCategories.add(item.category()));
        if (!request.approvalPolicy().allowedEvidenceCategories().containsAll(requestedCategories)) {
            return fallback(AiResponseStatus.CONSENT_REQUIRED, identity.provider(), identity.model(),
                    identity.identifier(), Duration.ZERO, AiUsage.empty(),
                    "Explicit consent is required for every minimized evidence category.", List.of());
        }

        String cacheKey = cacheKey(bundle, diagnosis, request, identity);
        Path cachePath = safeCachePath(outputDirectory, cacheKey);
        boolean ignoredCacheEntry = request.cacheEnabled() && cachePath == null;
        if (request.cacheEnabled() && cachePath != null && Files.isRegularFile(cachePath)) {
            try {
                DoctorAdvisory cached = JSON.readValue(cachePath.toFile(), DoctorAdvisory.class);
                validateCached(cached, evidence, identity);
                return new DoctorAdvisory(cached.schemaVersion(), cached.status(), cached.analysis(),
                        cached.metadata().asCacheHit());
            } catch (IOException | RuntimeException ignored) {
                ignoredCacheEntry = true;
            }
        }

        AiRequest providerRequest = providerRequest(bundle, diagnosis, request, evidence, identity);
        AiResponse response;
        try {
            response = executor.apply(providerRequest);
        } catch (RuntimeException exception) {
            return fallback(AiResponseStatus.ERROR, identity.provider(), identity.model(),
                    identity.identifier(), Duration.ZERO, AiUsage.empty(),
                    "Provider execution failed.", cacheWarning(ignoredCacheEntry));
        }
        if (response == null) {
            return fallback(AiResponseStatus.ERROR, identity.provider(), identity.model(),
                    identity.identifier(), Duration.ZERO, AiUsage.empty(),
                    "Provider did not return a result.", cacheWarning(ignoredCacheEntry));
        }
        if (!response.successful()) {
            return fallback(response.status(), safe(response.provider()), safe(response.model()),
                    identity.identifier(), response.duration(), response.usage(),
                    safeFallbackReason(response.fallbackReason()),
                    responseWarnings(response, ignoredCacheEntry));
        }

        try {
            byte[] responseBytes = JSON.writeValueAsBytes(response.structuredPayload());
            if (responseBytes.length > request.maxResponseBytes()) {
                return fallback(AiResponseStatus.INVALID_RESPONSE, safe(response.provider()), safe(response.model()),
                        identity.identifier(), response.duration(), response.usage(),
                        "Provider output exceeded the Doctor response size limit.",
                        cacheWarning(ignoredCacheEntry));
            }
            List<String> schemaErrors = JsonSchemaValidator.validate(RESPONSE_SCHEMA, response.structuredPayload());
            if (!schemaErrors.isEmpty()) {
                return fallback(AiResponseStatus.INVALID_RESPONSE, safe(response.provider()), safe(response.model()),
                        identity.identifier(), response.duration(), response.usage(),
                        "Provider output did not match the Doctor advisory schema.",
                        cacheWarning(ignoredCacheEntry));
            }

            DoctorRedactor redactor = new DoctorRedactor();
            JsonNode sanitized = redactor.redact(response.structuredPayload());
            List<String> warnings = new ArrayList<>(responseWarnings(response, ignoredCacheEntry));
            if (!sanitized.equals(response.structuredPayload())) {
                warnings.add("Sensitive-looking provider text was redacted before advisory rendering.");
            }
            DoctorAdvisory.ProviderAnalysis analysis =
                    parse(sanitized, evidence, diagnosis, warnings);
            DoctorAdvisory advisory = new DoctorAdvisory(
                    DoctorAdvisory.CURRENT_SCHEMA_VERSION,
                    DoctorAdvisory.Status.SUCCESS,
                    analysis,
                    new DoctorAdvisory.Metadata(
                            AiResponseStatus.SUCCESS,
                            safe(response.provider()),
                            safe(response.model()),
                            identity.identifier(),
                            millis(response.duration()),
                            response.usage(),
                            "",
                            false,
                            warnings));
            if (request.cacheEnabled() && cachePath != null) {
                writeCache(cachePath, advisory);
            }
            return advisory;
        } catch (JsonProcessingException | IllegalArgumentException exception) {
            return fallback(AiResponseStatus.INVALID_RESPONSE, safe(response.provider()), safe(response.model()),
                    identity.identifier(), response.duration(), response.usage(),
                    "Provider output failed Doctor advisory validation.", cacheWarning(ignoredCacheEntry));
        }
    }

    private static AiRequest providerRequest(
            EvidenceBundle bundle,
            Diagnosis diagnosis,
            DoctorAiAnalysisRequest request,
            List<EvidenceReference> evidence,
            ConfigurationIdentity identity) {
        ObjectNode fallback = JSON.createObjectNode();
        fallback.put("schemaVersion", DoctorAdvisory.ProviderAnalysis.CURRENT_SCHEMA_VERSION);
        fallback.putArray("observations");
        fallback.putArray("hypotheses");
        fallback.putArray("missingEvidence");
        fallback.putArray("recommendedActions");
        fallback.putArray("limitations");

        AiRequest.Builder builder = AiRequest.builder("shaft-doctor-advisory", RESPONSE_SCHEMA)
                .requestId("doctor-advisory-" + shortValue(bundle.bundleId())
                        + "-" + shortValue(identity.identifier()))
                .text(prompt(diagnosis))
                .timeout(request.timeout())
                .budget(request.budget())
                .approvalPolicy(request.approvalPolicy())
                .deterministicFallback(fallback);
        evidence.forEach(builder::evidence);
        return builder.build();
    }

    private static String prompt(Diagnosis diagnosis) {
        return """
                Analyze only the submitted redacted evidence and deterministic SHAFT Doctor diagnosis.
                Return conclusions in the requested JSON schema. Do not include chain-of-thought, hidden reasoning,
                credentials, patches, test-status changes, or evidence IDs that were not submitted.
                The deterministic diagnosis remains authoritative; identify uncertainty and contradictions explicitly.

                Deterministic diagnosis:
                """ + new DoctorJsonCodec().write(diagnosis);
    }

    private static List<EvidenceReference> minimize(
            EvidenceBundle bundle,
            Diagnosis diagnosis,
            DoctorAiAnalysisRequest request) {
        Set<String> citedIds = new LinkedHashSet<>();
        diagnosis.findings().forEach(finding -> citedIds.addAll(finding.evidenceIds()));
        List<EvidenceItem> candidates = bundle.evidence().stream()
                .filter(item -> item.content() != null && !item.content().isBlank())
                .filter(item -> citedIds.isEmpty() || citedIds.contains(item.id()))
                .sorted(Comparator.comparingInt(item -> evidencePriority(item.category())))
                .toList();

        List<EvidenceReference> selected = new ArrayList<>();
        long retainedBytes = 0;
        for (EvidenceItem item : candidates) {
            if (selected.size() >= request.maxEvidenceItems() || retainedBytes >= request.maxEvidenceBytes()) {
                break;
            }
            long remaining = request.maxEvidenceBytes() - retainedBytes;
            String content = boundedUtf8(item.content(), remaining);
            if (content.isBlank()) {
                continue;
            }
            selected.add(new EvidenceReference(
                    item.id(),
                    providerCategory(item.category()),
                    item.mediaType(),
                    content));
            retainedBytes += content.getBytes(StandardCharsets.UTF_8).length;
        }
        return List.copyOf(selected);
    }

    private static DoctorAdvisory.ProviderAnalysis parse(
            JsonNode payload,
            List<EvidenceReference> submittedEvidence,
            Diagnosis diagnosis,
            List<String> warnings) {
        Set<String> allowedIds = submittedEvidence.stream()
                .map(EvidenceReference::id)
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        String schemaVersion = safeText(payload.path("schemaVersion").asText(), "schema version");
        if (!DoctorAdvisory.ProviderAnalysis.CURRENT_SCHEMA_VERSION.equals(schemaVersion)) {
            throw new IllegalArgumentException("Unsupported advisory schema version.");
        }

        List<DoctorAdvisory.Observation> observations = new ArrayList<>();
        for (JsonNode item : payload.path("observations")) {
            List<String> evidenceIds = evidenceIds(item.path("evidenceIds"), allowedIds);
            boolean cited = !evidenceIds.isEmpty();
            if (!cited) {
                warnings.add("Provider observation is uncited.");
            }
            observations.add(new DoctorAdvisory.Observation(
                    safeText(item.path("statement").asText(), "observation"),
                    evidenceIds,
                    cited));
        }

        List<DoctorAdvisory.Hypothesis> hypotheses = new ArrayList<>();
        for (JsonNode item : payload.path("hypotheses")) {
            CauseCategory category = CauseCategory.valueOf(item.path("causeCategory").asText());
            Confidence confidence = Confidence.valueOf(item.path("confidence").asText());
            List<String> evidenceIds = evidenceIds(item.path("evidenceIds"), allowedIds);
            boolean cited = !evidenceIds.isEmpty();
            boolean contradicts = contradicts(diagnosis, category);
            if (!cited) {
                warnings.add("Provider hypothesis is uncited.");
            }
            if (contradicts) {
                warnings.add("Provider hypothesis contradicts the deterministic primary cause.");
            }
            hypotheses.add(new DoctorAdvisory.Hypothesis(
                    category,
                    safeText(item.path("statement").asText(), "hypothesis"),
                    confidence,
                    evidenceIds,
                    cited,
                    contradicts));
        }

        List<DoctorAdvisory.RecommendedAction> actions = new ArrayList<>();
        for (JsonNode item : payload.path("recommendedActions")) {
            List<String> evidenceIds = evidenceIds(item.path("evidenceIds"), allowedIds);
            boolean cited = !evidenceIds.isEmpty();
            if (!cited) {
                warnings.add("Provider recommended action is uncited.");
            }
            actions.add(new DoctorAdvisory.RecommendedAction(
                    safeText(item.path("title").asText(), "action title"),
                    safeText(item.path("action").asText(), "action"),
                    evidenceIds,
                    cited));
        }

        return new DoctorAdvisory.ProviderAnalysis(
                schemaVersion,
                observations,
                hypotheses,
                textList(payload.path("missingEvidence"), "missing evidence"),
                actions,
                textList(payload.path("limitations"), "limitation"));
    }

    private static List<String> evidenceIds(JsonNode values, Set<String> allowedIds) {
        LinkedHashSet<String> references = new LinkedHashSet<>();
        for (JsonNode value : values) {
            String id = safeText(value.asText(), "evidence ID");
            if (!allowedIds.contains(id)) {
                throw new IllegalArgumentException("Provider referenced evidence that was not submitted.");
            }
            references.add(id);
        }
        return List.copyOf(references);
    }

    private static List<String> textList(JsonNode values, String label) {
        List<String> result = new ArrayList<>();
        for (JsonNode value : values) {
            result.add(safeText(value.asText(), label));
        }
        return List.copyOf(result);
    }

    private static String safeText(String value, String label) {
        String normalized = value == null ? "" : value.trim();
        if (normalized.isEmpty() || normalized.length() > MAX_TEXT_LENGTH) {
            throw new IllegalArgumentException("Provider " + label + " is empty or oversized.");
        }
        for (int index = 0; index < normalized.length(); index++) {
            char character = normalized.charAt(index);
            if (Character.isISOControl(character) && character != '\n'
                    && character != '\r' && character != '\t') {
                throw new IllegalArgumentException("Provider " + label + " contains unsafe control text.");
            }
        }
        return normalized;
    }

    private ConfigurationIdentity configurationIdentity(DoctorAiAnalysisRequest request) {
        String provider = "none";
        String model = "";
        String effectiveConfiguration = "invalid";
        try {
            PilotConfiguration configuration = configurationSupplier.get();
            provider = safe(configuration.provider());
            String globalCategories = configuration.approvalPolicy().allowedEvidenceCategories().stream()
                    .map(Enum::name)
                    .sorted()
                    .collect(java.util.stream.Collectors.joining(","));
            if (!provider.isBlank() && !"none".equals(provider)) {
                var providerConfiguration = configuration.provider(provider);
                model = safe(providerConfiguration.model());
                effectiveConfiguration = configuration.enabled() + "\n"
                        + configuration.approvalPolicy().localInferenceAllowed() + "\n"
                        + configuration.approvalPolicy().remoteInferenceAllowed() + "\n"
                        + globalCategories + "\n"
                        + configuration.timeout() + "\n"
                        + configuration.maxRequestBytes() + "\n"
                        + configuration.maxInputTokens() + "\n"
                        + configuration.maxOutputTokens() + "\n"
                        + configuration.maxCostUsd() + "\n"
                        + configuration.retryMaxAttempts() + "\n"
                        + configuration.maxConcurrency() + "\n"
                        + providerConfiguration.endpoint() + "\n"
                        + new java.util.TreeMap<>(providerConfiguration.options());
            } else {
                effectiveConfiguration = configuration.enabled() + "\n"
                        + configuration.approvalPolicy().localInferenceAllowed() + "\n"
                        + configuration.approvalPolicy().remoteInferenceAllowed() + "\n"
                        + globalCategories;
            }
        } catch (RuntimeException ignored) {
            provider = "none";
            model = "";
        }
        String categories = request.approvalPolicy().allowedEvidenceCategories().stream()
                .map(Enum::name)
                .sorted()
                .collect(java.util.stream.Collectors.joining(","));
        String source = provider + "\n" + model + "\n" + effectiveConfiguration + "\n"
                + request.approvalPolicy().localInferenceAllowed() + "\n"
                + request.approvalPolicy().remoteInferenceAllowed() + "\n"
                + request.timeout() + "\n"
                + request.budget() + "\n" + categories + "\n"
                + request.maxEvidenceItems() + "\n" + request.maxEvidenceBytes() + "\n"
                + request.maxResponseBytes();
        return new ConfigurationIdentity(provider, model,
                DoctorHashing.sha256(source.getBytes(StandardCharsets.UTF_8)).substring(0, 16));
    }

    private static String cacheKey(
            EvidenceBundle bundle,
            Diagnosis diagnosis,
            DoctorAiAnalysisRequest request,
            ConfigurationIdentity identity) {
        String source = bundle.bundleId() + "\n"
                + DoctorHashing.sha256(new DoctorJsonCodec().write(diagnosis).getBytes(StandardCharsets.UTF_8)) + "\n"
                + identity.identifier() + "\n"
                + request.maxEvidenceItems() + "\n"
                + request.maxEvidenceBytes() + "\n"
                + request.maxResponseBytes();
        return DoctorHashing.sha256(source.getBytes(StandardCharsets.UTF_8));
    }

    private static void validateCached(
            DoctorAdvisory advisory,
            List<EvidenceReference> submittedEvidence,
            ConfigurationIdentity identity) {
        if (advisory.status() != DoctorAdvisory.Status.SUCCESS
                || advisory.metadata().providerStatus() != AiResponseStatus.SUCCESS
                || !identity.identifier().equals(advisory.metadata().configurationIdentifier())
                || !identity.provider().equals(advisory.metadata().provider())
                || !DoctorAdvisory.ProviderAnalysis.CURRENT_SCHEMA_VERSION
                .equals(advisory.analysis().schemaVersion())) {
            throw new IllegalArgumentException("Cached advisory metadata is not valid.");
        }
        Set<String> allowedIds = submittedEvidence.stream().map(EvidenceReference::id)
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        advisory.analysis().observations().forEach(value -> {
            safeText(value.statement(), "cached observation");
            validateCachedIds(value.evidenceIds(), allowedIds);
        });
        advisory.analysis().hypotheses().forEach(value -> {
            safeText(value.statement(), "cached hypothesis");
            validateCachedIds(value.evidenceIds(), allowedIds);
        });
        advisory.analysis().recommendedActions().forEach(value -> {
            safeText(value.title(), "cached action title");
            safeText(value.action(), "cached action");
            validateCachedIds(value.evidenceIds(), allowedIds);
        });
        advisory.analysis().missingEvidence().forEach(value -> safeText(value, "cached missing evidence"));
        advisory.analysis().limitations().forEach(value -> safeText(value, "cached limitation"));
        safeOptionalText(advisory.metadata().provider(), "cached provider");
        safeOptionalText(advisory.metadata().model(), "cached model");
        safeOptionalText(advisory.metadata().configurationIdentifier(), "cached configuration identifier");
        safeOptionalText(advisory.metadata().fallbackReason(), "cached fallback reason");
        advisory.metadata().warnings().forEach(value -> safeText(value, "cached warning"));
        JsonNode cachedTree = JSON.valueToTree(advisory);
        if (!new DoctorRedactor().redact(cachedTree).equals(cachedTree)) {
            throw new IllegalArgumentException("Cached advisory contains sensitive-looking text.");
        }
    }

    private static void validateCachedIds(List<String> values, Set<String> allowedIds) {
        if (!allowedIds.containsAll(values)) {
            throw new IllegalArgumentException("Cached advisory references unknown evidence.");
        }
    }

    private static void writeCache(Path path, DoctorAdvisory advisory) {
        Path temporary = null;
        try {
            Files.createDirectories(path.getParent());
            temporary = Files.createTempFile(path.getParent(), ".doctor-ai-", ".tmp");
            Files.writeString(temporary,
                    JSON.writerWithDefaultPrettyPrinter().writeValueAsString(advisory) + "\n",
                    StandardCharsets.UTF_8);
            Files.move(temporary, path, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException ignored) {
            // Cache persistence is optional and must never fail Doctor analysis.
        } finally {
            if (temporary != null) {
                try {
                    Files.deleteIfExists(temporary);
                } catch (IOException ignored) {
                    // Best-effort cleanup of an unpublished cache entry.
                }
            }
        }
    }

    private static DoctorAdvisory fallback(
            AiResponseStatus status,
            String provider,
            String model,
            String configurationIdentifier,
            Duration duration,
            AiUsage usage,
            String reason,
            List<String> warnings) {
        return new DoctorAdvisory(
                DoctorAdvisory.CURRENT_SCHEMA_VERSION,
                DoctorAdvisory.Status.FALLBACK,
                DoctorAdvisory.ProviderAnalysis.empty(),
                new DoctorAdvisory.Metadata(
                        status,
                        safe(provider),
                        safe(model),
                        safe(configurationIdentifier),
                        millis(duration),
                        usage,
                        safeFallbackReason(reason),
                        false,
                        warnings));
    }

    private static List<String> cacheWarning(boolean ignoredCacheEntry) {
        return ignoredCacheEntry
                ? List.of("An invalid safe advisory cache entry was ignored.")
                : List.of();
    }

    private static List<String> responseWarnings(AiResponse response, boolean ignoredCacheEntry) {
        List<String> warnings = new ArrayList<>(cacheWarning(ignoredCacheEntry));
        for (String warning : response.warnings()) {
            String sanitized = new DoctorRedactor().redact(warning);
            if (!sanitized.isBlank() && sanitized.length() <= MAX_TEXT_LENGTH) {
                warnings.add(sanitized);
            }
        }
        return List.copyOf(warnings);
    }

    private static String safeFallbackReason(String reason) {
        String sanitized = new DoctorRedactor().redact(reason);
        if (sanitized.isBlank()) {
            return "Provider analysis was unavailable; the deterministic diagnosis was retained.";
        }
        return sanitized.length() <= MAX_TEXT_LENGTH
                ? sanitized
                : "Provider analysis was unavailable; the deterministic diagnosis was retained.";
    }

    private static String safe(String value) {
        String sanitized = new DoctorRedactor().redact(value);
        return sanitized.length() <= MAX_TEXT_LENGTH ? sanitized : "";
    }

    private static String safeOptionalText(String value, String label) {
        String normalized = value == null ? "" : value.trim();
        if (normalized.length() > MAX_TEXT_LENGTH) {
            throw new IllegalArgumentException("Provider " + label + " is oversized.");
        }
        for (int index = 0; index < normalized.length(); index++) {
            char character = normalized.charAt(index);
            if (Character.isISOControl(character) && character != '\n'
                    && character != '\r' && character != '\t') {
                throw new IllegalArgumentException("Provider " + label + " contains unsafe control text.");
            }
        }
        return normalized;
    }

    private static boolean contradicts(Diagnosis diagnosis, CauseCategory category) {
        return diagnosis.primaryCause() != CauseCategory.UNKNOWN
                && category != CauseCategory.UNKNOWN
                && category != diagnosis.primaryCause()
                && !diagnosis.contributingCauses().contains(category);
    }

    private static int evidencePriority(com.shaft.doctor.model.EvidenceCategory category) {
        return switch (category) {
            case EXCEPTION_CHAIN -> 0;
            case SHAFT_LOG -> 1;
            case ALLURE_RESULT -> 2;
            case CONFIGURATION, ENVIRONMENT, DEPENDENCY_BUILD -> 3;
            case PAGE_SNAPSHOT -> 4;
            case SCREENSHOT, OTHER -> 5;
        };
    }

    private static com.shaft.pilot.ai.EvidenceCategory providerCategory(
            com.shaft.doctor.model.EvidenceCategory category) {
        return switch (category) {
            case EXCEPTION_CHAIN, SHAFT_LOG -> com.shaft.pilot.ai.EvidenceCategory.LOG;
            case PAGE_SNAPSHOT -> com.shaft.pilot.ai.EvidenceCategory.DOM;
            case CONFIGURATION, ENVIRONMENT, DEPENDENCY_BUILD ->
                    com.shaft.pilot.ai.EvidenceCategory.CONFIGURATION;
            case ALLURE_RESULT, OTHER, SCREENSHOT -> com.shaft.pilot.ai.EvidenceCategory.TEXT;
        };
    }

    private static String boundedUtf8(String value, long maxBytes) {
        if (maxBytes <= 0) {
            return "";
        }
        byte[] bytes = value.getBytes(StandardCharsets.UTF_8);
        if (bytes.length <= maxBytes) {
            return value;
        }
        int length = Math.toIntExact(Math.min(maxBytes, Integer.MAX_VALUE));
        String bounded = new String(bytes, 0, length, StandardCharsets.UTF_8);
        while (!bounded.isEmpty()
                && bounded.getBytes(StandardCharsets.UTF_8).length > maxBytes) {
            bounded = bounded.substring(0, bounded.length() - 1);
        }
        return bounded;
    }

    private static long millis(Duration duration) {
        return duration == null ? 0 : Math.max(0, duration.toMillis());
    }

    private static Path safeCachePath(Path outputDirectory, String cacheKey) {
        try {
            Path output = outputDirectory.toAbsolutePath().normalize();
            Files.createDirectories(output);
            Path resolvedOutput = output.toRealPath();
            Path cacheDirectory = output.resolve(".doctor-ai-cache");
            if (Files.exists(cacheDirectory)
                    && !cacheDirectory.toRealPath().startsWith(resolvedOutput)) {
                return null;
            }
            return cacheDirectory.resolve(cacheKey + ".json");
        } catch (IOException exception) {
            return null;
        }
    }

    private static String shortValue(String value) {
        String normalized = value == null ? "" : value;
        return normalized.substring(0, Math.min(16, normalized.length()));
    }

    private static JsonNode loadSchema() {
        try (InputStream input = DoctorAiAnalysisService.class.getResourceAsStream(SCHEMA_RESOURCE)) {
            if (input == null) {
                throw new DoctorFormatException("Bundled Doctor advisory schema is missing.");
            }
            return JSON.readTree(input);
        } catch (IOException exception) {
            throw new DoctorFormatException("Bundled Doctor advisory schema could not be read.", exception);
        }
    }

    private record ConfigurationIdentity(String provider, String model, String identifier) {
    }
}
