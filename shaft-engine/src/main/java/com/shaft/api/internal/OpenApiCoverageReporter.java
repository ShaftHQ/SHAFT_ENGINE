package com.shaft.api.internal;

import com.atlassian.oai.validator.interaction.ApiOperationResolver;
import com.atlassian.oai.validator.model.ApiOperationMatch;
import com.atlassian.oai.validator.model.Request;
import com.atlassian.oai.validator.restassured.OpenApiValidationFilter;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.api.RestActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.parser.OpenAPIV3Parser;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Collects OpenAPI operation coverage from SHAFT API requests and reuses parsed validators.
 */
public final class OpenApiCoverageReporter {
    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final ConcurrentMap<String, OpenApiValidationFilter> VALIDATION_FILTERS = new ConcurrentHashMap<>();
    private static final ConcurrentMap<String, SpecCoverage> COVERAGE_BY_SPEC = new ConcurrentHashMap<>();

    private OpenApiCoverageReporter() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns a cached OpenAPI validation filter for the supplied spec.
     *
     * @param specUrlOrDefinition OpenAPI URL, path, or inline definition
     * @return cached REST Assured validation filter
     */
    public static OpenApiValidationFilter validationFilter(String specUrlOrDefinition) {
        return VALIDATION_FILTERS.computeIfAbsent(normalizeSpec(specUrlOrDefinition), OpenApiValidationFilter::new);
    }

    /**
     * Enables coverage collection for a spec and remembers the current threshold.
     *
     * @param specUrlOrDefinition OpenAPI URL, path, or inline definition
     * @param threshold required coverage percentage from {@code 0} to {@code 100}
     */
    public static void start(String specUrlOrDefinition, int threshold) {
        if (isBlank(specUrlOrDefinition)) {
            throw new IllegalArgumentException(
                    "OpenAPI coverage reporting requires swagger.validation.url to be configured.");
        }
        validateThreshold(threshold);
        coverageFor(specUrlOrDefinition).setThreshold(threshold);
    }

    /**
     * Records one API interaction against the configured OpenAPI document.
     *
     * @param specUrlOrDefinition OpenAPI URL, path, or inline definition
     * @param requestType HTTP method
     * @param requestPath request path
     * @param failure validation failure, when one occurred
     */
    public static void recordInteraction(String specUrlOrDefinition, RestActions.RequestType requestType,
                                         String requestPath, Throwable failure) {
        if (isBlank(specUrlOrDefinition) || requestType == null) {
            return;
        }
        coverageFor(specUrlOrDefinition).record(requestType.name(), requestPath, failure);
    }

    /**
     * Logs and attaches the OpenAPI coverage report, returning a threshold failure when configured.
     *
     * @return threshold failure, or {@code null} when the run satisfies configured coverage
     */
    public static AssertionError reportAndGetThresholdFailure() {
        if (COVERAGE_BY_SPEC.isEmpty()) {
            return null;
        }
        String summary = buildSummaryText();
        ReportManager.log(summary);
        ReportManagerHelper.attach("json", "OpenAPI Coverage Report", buildSummaryJson());
        AssertionError thresholdFailure = thresholdFailure();
        reset();
        return thresholdFailure;
    }

    static String buildSummaryText() {
        StringBuilder summary = new StringBuilder("OpenAPI coverage summary");
        COVERAGE_BY_SPEC.values().stream()
                .sorted(Comparator.comparing(spec -> spec.specUrl))
                .forEach(spec -> spec.appendSummary(summary));
        return summary.toString();
    }

    static AssertionError thresholdFailure() {
        for (SpecCoverage spec : COVERAGE_BY_SPEC.values()) {
            if (spec.threshold > 0 && spec.coveragePercent() < spec.threshold) {
                return new AssertionError("OpenAPI coverage " + formatPercent(spec.coveragePercent())
                        + "% is below the configured " + spec.threshold + "% threshold for " + spec.specUrl + ".");
            }
        }
        return null;
    }

    static void reset() {
        COVERAGE_BY_SPEC.clear();
    }

    private static String buildSummaryJson() {
        ObjectNode root = MAPPER.createObjectNode();
        ArrayNode specs = root.putArray("specs");
        COVERAGE_BY_SPEC.values().stream()
                .sorted(Comparator.comparing(spec -> spec.specUrl))
                .forEach(spec -> spec.appendJson(specs.addObject()));
        return root.toPrettyString();
    }

    private static SpecCoverage coverageFor(String specUrlOrDefinition) {
        return COVERAGE_BY_SPEC.computeIfAbsent(normalizeSpec(specUrlOrDefinition), OpenApiCoverageReporter::loadSpec);
    }

    private static SpecCoverage loadSpec(String specUrl) {
        OpenAPI openApi = new OpenAPIV3Parser().read(specUrl);
        if (openApi == null || openApi.getPaths() == null) {
            throw new IllegalArgumentException("Unable to load OpenAPI specification: " + specUrl);
        }

        List<OperationCoverage> operations = new ArrayList<>();
        openApi.getPaths().forEach((path, pathItem) -> pathItem.readOperationsMap()
                .forEach((method, operation) -> operations.add(new OperationCoverage(path, method, operation))));
        return new SpecCoverage(specUrl, openApi, operations);
    }

    private static String normalizeSpec(String specUrlOrDefinition) {
        return specUrlOrDefinition == null ? "" : specUrlOrDefinition.replace("\\", "/");
    }

    private static String normalizePath(String path) {
        if (isBlank(path)) {
            return "/";
        }
        String normalized = path.split("\\?", 2)[0];
        if (!normalized.startsWith("/")) {
            normalized = "/" + normalized;
        }
        while (normalized.length() > 1 && normalized.endsWith("/")) {
            normalized = normalized.substring(0, normalized.length() - 1);
        }
        return normalized;
    }

    private static boolean isBlank(String value) {
        return value == null || value.isBlank();
    }

    private static void validateThreshold(int threshold) {
        if (threshold < 0 || threshold > 100) {
            throw new IllegalArgumentException("OpenAPI coverage threshold must be between 0 and 100.");
        }
    }

    private static String formatPercent(double value) {
        return String.format(Locale.ROOT, "%.2f", value);
    }

    private static boolean isValidationFailure(Throwable failure) {
        return failure instanceof OpenApiValidationFilter.OpenApiValidationException;
    }

    private static final class SpecCoverage {
        private final String specUrl;
        private final List<OperationCoverage> operations;
        private final ApiOperationResolver resolver;
        private final Map<String, OperationCoverage> operationByKey;
        private final ConcurrentMap<String, AtomicInteger> unmatched = new ConcurrentHashMap<>();
        private volatile int threshold;

        private SpecCoverage(String specUrl, OpenAPI openApi, List<OperationCoverage> operations) {
            this.specUrl = specUrl;
            this.operations = List.copyOf(operations);
            this.resolver = new ApiOperationResolver(openApi, null, false);
            this.operationByKey = new LinkedHashMap<>();
            this.operations.forEach(operation -> operationByKey.put(key(operation.method, operation.path), operation));
        }

        private void setThreshold(int threshold) {
            validateThreshold(threshold);
            this.threshold = threshold;
        }

        private void record(String method, String requestPath, Throwable failure) {
            String normalizedPath = normalizePath(requestPath);
            ApiOperationMatch match = resolver.findApiOperation(normalizedPath, Request.Method.valueOf(method));
            OperationCoverage operation = null;
            if (match.isPathFound() && match.isOperationAllowed()) {
                String operationKey = key(match.getApiOperation().getMethod().name(),
                        match.getApiOperation().getApiPath().original());
                operation = operationByKey.get(operationKey);
            }
            if (operation == null) {
                unmatched.computeIfAbsent(method + " " + normalizedPath, ignored -> new AtomicInteger()).incrementAndGet();
                return;
            }
            operation.hits.incrementAndGet();
            if (isValidationFailure(failure)) {
                operation.validationFailures.add(failure.getMessage());
            }
        }

        private double coveragePercent() {
            if (operations.isEmpty()) {
                return 0;
            }
            long exercised = operations.stream().filter(operation -> operation.hits.get() > 0).count();
            return exercised * 100.0 / operations.size();
        }

        private void appendSummary(StringBuilder summary) {
            long exercised = operations.stream().filter(operation -> operation.hits.get() > 0).count();
            summary.append(System.lineSeparator()).append(System.lineSeparator())
                    .append("OpenAPI coverage for ").append(specUrl).append(System.lineSeparator())
                    .append("Coverage: ").append(exercised).append("/").append(operations.size())
                    .append(" operations (").append(formatPercent(coveragePercent())).append("%)");
            appendOperations(summary, "Exercised operations", true);
            appendOperations(summary, "Untested operations", false);
            appendUnmatched(summary);
        }

        private void appendOperations(StringBuilder summary, String title, boolean exercised) {
            summary.append(System.lineSeparator()).append(title).append(":");
            Map<String, List<OperationCoverage>> byTag = new LinkedHashMap<>();
            operations.stream()
                    .filter(operation -> (operation.hits.get() > 0) == exercised)
                    .forEach(operation -> byTag.computeIfAbsent(operation.tag, ignored -> new ArrayList<>()).add(operation));
            if (byTag.isEmpty()) {
                summary.append(" none");
                return;
            }
            byTag.forEach((tag, taggedOperations) -> {
                summary.append(System.lineSeparator()).append("- ").append(tag);
                taggedOperations.forEach(operation -> summary.append(System.lineSeparator())
                        .append("  - ").append(operation.summary()));
            });
        }

        private void appendUnmatched(StringBuilder summary) {
            summary.append(System.lineSeparator()).append("Unmatched operations:");
            if (unmatched.isEmpty()) {
                summary.append(" none");
                return;
            }
            unmatched.entrySet().stream()
                    .sorted(Map.Entry.comparingByKey())
                    .forEach(entry -> summary.append(System.lineSeparator())
                            .append("- ").append(entry.getKey()).append(" hits=").append(entry.getValue().get()));
        }

        private void appendJson(ObjectNode specNode) {
            specNode.put("spec", specUrl);
            specNode.put("coveragePercent", coveragePercent());
            specNode.put("threshold", threshold);
            ArrayNode operationsNode = specNode.putArray("operations");
            operations.forEach(operation -> operation.appendJson(operationsNode.addObject()));
            ObjectNode unmatchedNode = specNode.putObject("unmatchedOperations");
            unmatched.entrySet().stream()
                    .sorted(Map.Entry.comparingByKey())
                    .forEach(entry -> unmatchedNode.put(entry.getKey(), entry.getValue().get()));
        }

        private static String key(String method, String path) {
            return method.toUpperCase(Locale.ROOT) + " " + normalizePath(path);
        }
    }

    private static final class OperationCoverage {
        private final String path;
        private final String method;
        private final String tag;
        private final String operationId;
        private final AtomicInteger hits = new AtomicInteger();
        private final ConcurrentLinkedQueue<String> validationFailures = new ConcurrentLinkedQueue<>();

        private OperationCoverage(String path, PathItem.HttpMethod method, Operation operation) {
            this.path = normalizePath(path);
            this.method = method.name();
            this.tag = operation.getTags() == null || operation.getTags().isEmpty()
                    ? "untagged"
                    : operation.getTags().getFirst();
            this.operationId = isBlank(operation.getOperationId())
                    ? this.method + " " + this.path
                    : operation.getOperationId();
        }

        private String summary() {
            return method + " " + path + " (" + operationId + ") hits=" + hits.get()
                    + " validationFailures=" + validationFailures.size();
        }

        private void appendJson(ObjectNode operationNode) {
            operationNode.put("tag", tag);
            operationNode.put("path", path);
            operationNode.put("method", method);
            operationNode.put("operationId", operationId);
            operationNode.put("hits", hits.get());
            ArrayNode failures = operationNode.putArray("validationFailures");
            validationFailures.forEach(failures::add);
        }
    }
}
