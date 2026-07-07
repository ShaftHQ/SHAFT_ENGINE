package com.shaft.capture.generate.api.internal;

import com.shaft.capture.generate.api.ApiTransaction;
import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.SafeConstructor;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

/**
 * Cross-reports the endpoints recorded in an API capture session against the operations declared
 * by a provided OpenAPI (2.0/3.x) spec, in either JSON or YAML. Read-only and deterministic --
 * this never mutates the spec or the recording, and never requires AI: it is a set comparison
 * between {@code METHOD normalized-path} pairs, where a normalized path replaces every recorded
 * path segment that looks like an identifier (numeric, or a UUID) with an OpenAPI-style
 * {@code {param}} placeholder so that e.g. {@code GET /orders/42} matches a declared
 * {@code GET /orders/{orderId}} operation.
 */
public final class OpenApiCoverageReporter {
    private static final ObjectMapper JSON = new JsonMapper();
    private static final Pattern NUMERIC_SEGMENT = Pattern.compile("\\d+");
    private static final Pattern UUID_SEGMENT = Pattern.compile(
            "(?i)[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}");
    private static final Set<String> HTTP_METHODS = Set.of(
            "get", "post", "put", "patch", "delete", "head", "options", "trace");

    private OpenApiCoverageReporter() {
    }

    /**
     * Loads an OpenAPI spec (JSON or YAML, detected by file extension, falling back to
     * content-sniffing) and reports coverage against the recorded transactions.
     *
     * @param specPath path to the OpenAPI spec file
     * @param transactions recorded, renderable API transactions
     * @return deterministic coverage report; {@link CoverageReport#loadable()} is {@code false} and
     *         every other field is empty when the spec could not be read or parsed
     */
    public static CoverageReport report(Path specPath, List<ApiTransaction> transactions) {
        String content;
        try {
            content = Files.readString(specPath, StandardCharsets.UTF_8);
        } catch (Exception unreadable) {
            return CoverageReport.notLoadable("OpenAPI spec could not be read: " + safeMessage(unreadable));
        }
        JsonNode root;
        try {
            root = parse(specPath, content);
        } catch (RuntimeException malformed) {
            return CoverageReport.notLoadable("OpenAPI spec could not be parsed: " + safeMessage(malformed));
        }
        Set<String> declaredOperations = declaredOperations(root);
        if (declaredOperations.isEmpty()) {
            return CoverageReport.notLoadable("OpenAPI spec has no \"paths\" with a recognized HTTP method.");
        }
        return diff(declaredOperations, transactions);
    }

    private static JsonNode parse(Path specPath, String content) {
        String fileName = specPath.getFileName() == null ? "" : specPath.getFileName().toString().toLowerCase(Locale.ROOT);
        boolean looksLikeYaml = fileName.endsWith(".yaml") || fileName.endsWith(".yml")
                || (!fileName.endsWith(".json") && !content.stripLeading().startsWith("{"));
        if (!looksLikeYaml) {
            return JSON.readTree(content);
        }
        LoaderOptions options = new LoaderOptions();
        Yaml yaml = new Yaml(new SafeConstructor(options));
        Object loaded = yaml.load(content);
        return JSON.valueToTree(loaded);
    }

    private static Set<String> declaredOperations(JsonNode root) {
        Set<String> operations = new LinkedHashSet<>();
        JsonNode paths = root.path("paths");
        paths.forEachEntry((rawPath, operationsNode) -> {
            String normalizedPath = normalizePath(rawPath);
            operationsNode.forEachEntry((method, operation) -> {
                if (HTTP_METHODS.contains(method.toLowerCase(Locale.ROOT))) {
                    operations.add(method.toUpperCase(Locale.ROOT) + " " + normalizedPath);
                }
            });
        });
        return operations;
    }

    private static CoverageReport diff(Set<String> declaredOperations, List<ApiTransaction> transactions) {
        Set<String> recordedOperations = new LinkedHashSet<>();
        for (ApiTransaction transaction : transactions) {
            recordedOperations.add(transaction.method() + " " + normalizePath(pathOf(transaction.url())));
        }

        List<String> covered = new ArrayList<>();
        List<String> undeclared = new ArrayList<>();
        for (String recorded : recordedOperations) {
            (declaredOperations.contains(recorded) ? covered : undeclared).add(recorded);
        }
        List<String> missing = new ArrayList<>();
        for (String declared : declaredOperations) {
            if (!recordedOperations.contains(declared)) {
                missing.add(declared);
            }
        }

        int totalDeclared = declaredOperations.size();
        double coverageRatio = totalDeclared == 0 ? 0.0 : (double) covered.size() / totalDeclared;

        return new CoverageReport(
                true,
                "",
                sorted(covered),
                sorted(missing),
                sorted(undeclared),
                totalDeclared,
                coverageRatio);
    }

    private static List<String> sorted(List<String> values) {
        return List.copyOf(new TreeSet<>(values));
    }

    private static String pathOf(String url) {
        try {
            String path = new URI(url).getPath();
            return path == null || path.isBlank() ? "/" : path;
        } catch (URISyntaxException | RuntimeException malformed) {
            return "/";
        }
    }

    /**
     * Replaces every path segment that looks like a recorded identifier (a run of digits, or a
     * UUID) with a stable {@code {param}} placeholder, so a recorded concrete path (e.g.
     * {@code /orders/42}) can match a declared templated operation (e.g. {@code /orders/{id}})
     * regardless of the declared parameter's name.
     */
    private static String normalizePath(String rawPath) {
        String path = rawPath == null || rawPath.isBlank() ? "/" : rawPath;
        String[] segments = path.split("/", -1);
        List<String> normalizedSegments = new ArrayList<>();
        for (String segment : segments) {
            if (!segment.isEmpty()) {
                normalizedSegments.add(normalizeSegment(segment));
            }
        }
        return "/" + String.join("/", normalizedSegments);
    }

    private static String normalizeSegment(String segment) {
        if (segment.startsWith("{") && segment.endsWith("}")) {
            return "{param}";
        }
        if (NUMERIC_SEGMENT.matcher(segment).matches() || UUID_SEGMENT.matcher(segment).matches()) {
            return "{param}";
        }
        return segment;
    }

    private static String safeMessage(Exception exception) {
        String message = exception.getMessage();
        return message == null || message.isBlank() ? exception.getClass().getSimpleName() : message;
    }

    /**
     * Deterministic coverage report diffing recorded transactions against a declared OpenAPI spec.
     *
     * @param loadable whether the spec could be read and parsed
     * @param loadFailureReason safe reason {@code loadable} is {@code false}; empty otherwise
     * @param coveredOperations {@code METHOD normalized-path} operations recorded AND declared,
     *                          sorted
     * @param missingOperations {@code METHOD normalized-path} operations declared but never
     *                          recorded, sorted
     * @param undeclaredOperations {@code METHOD normalized-path} operations recorded but not
     *                              declared by the spec, sorted
     * @param totalDeclaredOperations total number of operations declared by the spec
     * @param coverageRatio {@code coveredOperations.size() / totalDeclaredOperations}, {@code 0.0}
     *                      when the spec declares no operations
     */
    public record CoverageReport(
            boolean loadable,
            String loadFailureReason,
            List<String> coveredOperations,
            List<String> missingOperations,
            List<String> undeclaredOperations,
            int totalDeclaredOperations,
            double coverageRatio) {
        public CoverageReport {
            loadFailureReason = loadFailureReason == null ? "" : loadFailureReason;
            coveredOperations = coveredOperations == null ? List.of() : List.copyOf(coveredOperations);
            missingOperations = missingOperations == null ? List.of() : List.copyOf(missingOperations);
            undeclaredOperations = undeclaredOperations == null ? List.of() : List.copyOf(undeclaredOperations);
        }

        /**
         * Returns a not-loadable report carrying a safe failure reason.
         *
         * @param reason safe failure reason
         * @return not-loadable report
         */
        public static CoverageReport notLoadable(String reason) {
            return new CoverageReport(false, reason, List.of(), List.of(), List.of(), 0, 0.0);
        }
    }
}
