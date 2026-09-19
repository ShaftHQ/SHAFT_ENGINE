package com.shaft.doctor.history;

import com.shaft.doctor.analysis.DeterministicRuleEngine;
import com.shaft.doctor.internal.DoctorHashing;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.EvidenceProvenance;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Clusters failures by Doctor historical-signature keys (issue #5969 / S3-03).
 *
 * <p>FR-001: uses {@link DeterministicRuleEngine#clusteringKey} (fingerprint preferred, else
 * normalized signature). FR-002: each cluster lists impacted tests. FR-003: deterministic — no
 * cloud ML. Shared signatures merge; distinct signatures stay separate; a new unique signature is
 * its own cluster; empty results yield empty-state.
 */
public final class UniqueErrorClusterer {
    private static final ObjectMapper JSON = JsonMapper.builder().build();
    private static final Set<String> FAILURE_STATUSES = Set.of("failed", "broken");
    private static final String EMPTY_MESSAGE =
            "No unique-error clusters yet. Provide allure-results with failed/broken tests "
                    + "(Doctor-normalized signatures / clusterFingerprint) to group error → impacted tests.";

    private UniqueErrorClusterer() {
    }

    /**
     * Clusters from an already-collected Doctor evidence bundle.
     *
     * @param bundle current evidence bundle
     * @return unique-error cluster table
     */
    public static ErrorClusterModels.ClusterTable cluster(EvidenceBundle bundle) {
        if (bundle == null || bundle.evidence().isEmpty()) {
            return emptyTable("", "");
        }
        List<EvidenceItem> siblings = bundle.evidence();
        List<FailureRow> rows = new ArrayList<>();
        for (EvidenceItem item : siblings) {
            if (!DeterministicRuleEngine.clusterableFailure(item)) {
                continue;
            }
            // Prefer Allure-result shaped failures for impacted-test listing; diagnostics alone
            // contribute fingerprints via sibling lookup inside clusteringKey.
            if (item.category() != EvidenceCategory.ALLURE_RESULT
                    && item.attributes().getOrDefault("status", "").isBlank()) {
                continue;
            }
            String status = item.attributes().getOrDefault("status", "");
            if (!status.isBlank() && !FAILURE_STATUSES.contains(status.toLowerCase(Locale.ROOT))) {
                continue;
            }
            String key = DeterministicRuleEngine.clusteringKey(item, siblings);
            if (key == null || key.isBlank()) {
                continue;
            }
            rows.add(toFailureRow(item, key));
        }
        return buildTable(rows, "", "", List.of());
    }

    /**
     * Clusters failed/broken Allure results under {@code allureResultsRoot}, optionally enriching
     * fingerprints from Doctor JSON evidence ({@code clusterFingerprint} / {@code signature}).
     *
     * @param allureResultsRoot allure-results directory (may be missing → empty-state)
     * @param doctorJson optional Doctor report / evidence JSON
     * @return unique-error cluster table
     */
    public static ErrorClusterModels.ClusterTable cluster(Path allureResultsRoot, Path doctorJson) {
        List<String> warnings = new ArrayList<>();
        List<EvidenceItem> items = new ArrayList<>();
        String resultsPath = pathString(allureResultsRoot);
        String doctorPath = pathString(doctorJson);

        if (allureResultsRoot != null && Files.isDirectory(allureResultsRoot)) {
            items.addAll(readAllureFailures(allureResultsRoot, warnings));
        } else if (allureResultsRoot != null) {
            warnings.add("allure-results path is not a directory: " + resultsPath);
        }

        if (doctorJson != null && Files.isRegularFile(doctorJson)) {
            items.addAll(readDoctorEvidence(doctorJson, warnings));
        }

        if (items.isEmpty()) {
            ErrorClusterModels.ClusterTable empty = emptyTable(resultsPath, doctorPath);
            return new ErrorClusterModels.ClusterTable(
                    empty.schemaVersion(),
                    true,
                    empty.emptyMessage(),
                    resultsPath,
                    doctorPath,
                    0,
                    0,
                    List.of(),
                    List.copyOf(warnings));
        }

        EvidenceBundle bundle = new EvidenceBundle(
                EvidenceBundle.CURRENT_SCHEMA_VERSION,
                "cluster-bundle",
                items,
                null,
                Map.of("source", "unique-error-clusterer"));
        ErrorClusterModels.ClusterTable table = cluster(bundle);
        return new ErrorClusterModels.ClusterTable(
                table.schemaVersion(),
                table.empty(),
                table.emptyMessage(),
                resultsPath,
                doctorPath,
                table.clusterCount(),
                table.impactedTestCount(),
                table.clusters(),
                mergeWarnings(table.warnings(), warnings));
    }

    private static ErrorClusterModels.ClusterTable emptyTable(String resultsPath, String doctorPath) {
        return new ErrorClusterModels.ClusterTable(
                ErrorClusterModels.SCHEMA_VERSION,
                true,
                EMPTY_MESSAGE,
                resultsPath,
                doctorPath,
                0,
                0,
                List.of(),
                List.of());
    }

    private static ErrorClusterModels.ClusterTable buildTable(
            List<FailureRow> rows, String resultsPath, String doctorPath, List<String> warnings) {
        if (rows.isEmpty()) {
            return emptyTable(resultsPath, doctorPath);
        }
        Map<String, List<FailureRow>> byKey = new LinkedHashMap<>();
        for (FailureRow row : rows) {
            byKey.computeIfAbsent(row.signatureKey(), ignored -> new ArrayList<>()).add(row);
        }
        List<ErrorClusterModels.ErrorCluster> clusters = new ArrayList<>();
        for (Map.Entry<String, List<FailureRow>> entry : byKey.entrySet()) {
            List<FailureRow> members = new ArrayList<>(entry.getValue());
            members.sort(Comparator
                    .comparing(FailureRow::historyId)
                    .thenComparing(FailureRow::name)
                    .thenComparing(FailureRow::uuid));
            List<ErrorClusterModels.ImpactedTest> tests = members.stream()
                    .map(FailureRow::toImpacted)
                    .toList();
            String display = members.stream()
                    .map(FailureRow::failureMessage)
                    .filter(message -> message != null && !message.isBlank())
                    .findFirst()
                    .orElse(entry.getKey());
            clusters.add(new ErrorClusterModels.ErrorCluster(
                    entry.getKey(), display, tests.size(), tests));
        }
        clusters.sort(Comparator
                .comparingInt(ErrorClusterModels.ErrorCluster::impactedCount).reversed()
                .thenComparing(ErrorClusterModels.ErrorCluster::signatureKey));
        int impacted = clusters.stream().mapToInt(ErrorClusterModels.ErrorCluster::impactedCount).sum();
        return new ErrorClusterModels.ClusterTable(
                ErrorClusterModels.SCHEMA_VERSION,
                false,
                "",
                resultsPath,
                doctorPath,
                clusters.size(),
                impacted,
                clusters,
                List.copyOf(warnings));
    }

    private static FailureRow toFailureRow(EvidenceItem item, String key) {
        Map<String, String> attrs = item.attributes();
        String name = attrs.getOrDefault("name", "");
        String fullName = attrs.getOrDefault("fullName", name);
        return new FailureRow(
                key,
                attrs.getOrDefault("historyId", ""),
                name,
                fullName,
                attrs.getOrDefault("uuid", item.id()),
                attrs.getOrDefault("status", ""),
                attrs.getOrDefault("failureMessage", ""));
    }

    private static List<EvidenceItem> readAllureFailures(Path root, List<String> warnings) {
        List<EvidenceItem> items = new ArrayList<>();
        try (Stream<Path> stream = Files.list(root)) {
            List<Path> files = stream
                    .filter(Files::isRegularFile)
                    .filter(path -> {
                        String name = path.getFileName().toString();
                        return name.endsWith("-result.json") || name.endsWith("-result.jsonl");
                    })
                    .sorted()
                    .toList();
            for (Path file : files) {
                try {
                    JsonNode result = JSON.readTree(Files.readString(file, StandardCharsets.UTF_8));
                    EvidenceItem item = allureFailureItem(file, result);
                    if (item != null) {
                        items.add(item);
                    }
                } catch (IOException | RuntimeException exception) {
                    warnings.add("Skipped unreadable Allure result " + file.getFileName() + ": "
                            + exception.getMessage());
                }
            }
        } catch (IOException exception) {
            warnings.add("Could not list allure-results: " + exception.getMessage());
        }
        return items;
    }

    private static EvidenceItem allureFailureItem(Path file, JsonNode result) {
        String status = result.path("status").asText("").toLowerCase(Locale.ROOT);
        if (!FAILURE_STATUSES.contains(status)) {
            return null;
        }
        JsonNode details = result.path("statusDetails");
        String message = details.path("message").asText("");
        String trace = details.path("trace").asText("");
        String signature = normalizeSignature(message, trace);
        String fingerprint = firstNonBlank(
                result.path("clusterFingerprint").asText(""),
                result.path("cluster").path("fingerprint").asText(""),
                textFromLabels(result, "clusterFingerprint"));
        if (fingerprint.isBlank() && signature.isBlank()) {
            return null;
        }
        String historyId = firstNonBlank(
                result.path("historyId").asText(""),
                result.path("testCaseId").asText(""));
        String name = firstNonBlank(result.path("name").asText(""), result.path("fullName").asText(""));
        String fullName = firstNonBlank(result.path("fullName").asText(""), name);
        String uuid = result.path("uuid").asText(file.getFileName().toString());
        Map<String, String> attributes = new HashMap<>();
        attributes.put("status", status);
        attributes.put("historyId", historyId);
        attributes.put("name", name);
        attributes.put("fullName", fullName);
        attributes.put("uuid", uuid);
        attributes.put("failureMessage", shorten(message));
        attributes.put("traceTop", shorten(firstLine(trace)));
        attributes.put("signature", signature);
        if (!fingerprint.isBlank()) {
            attributes.put("clusterFingerprint", fingerprint);
            attributes.put("signature", fingerprint);
        }
        String digest = DoctorHashing.sha256(uuid.getBytes(StandardCharsets.UTF_8));
        return new EvidenceItem(
                "allure-" + digest.substring(0, 16),
                EvidenceCategory.ALLURE_RESULT,
                "application/json",
                "",
                digest,
                Math.max(1, message.length()),
                message,
                false,
                false,
                Map.copyOf(attributes),
                new EvidenceProvenance("allure-result-json", file.toString(), digest));
    }

    private static List<EvidenceItem> readDoctorEvidence(Path doctorJson, List<String> warnings) {
        List<EvidenceItem> items = new ArrayList<>();
        try {
            JsonNode root = JSON.readTree(Files.readString(doctorJson, StandardCharsets.UTF_8));
            collectDoctorEvidence(root, items, new int[]{0});
        } catch (IOException | RuntimeException exception) {
            warnings.add("Doctor JSON not used for clustering: " + exception.getMessage());
        }
        return items;
    }

    private static void collectDoctorEvidence(JsonNode node, List<EvidenceItem> sink, int[] counter) {
        if (node == null || node.isNull()) {
            return;
        }
        if (node.isObject()) {
            JsonNode attributesNode = node.path("attributes");
            String fingerprint = firstNonBlank(
                    text(node, "clusterFingerprint"),
                    text(attributesNode, "clusterFingerprint"),
                    node.path("cluster").path("fingerprint").asText(""),
                    text(attributesNode, "signature"),
                    text(node, "signature"));
            String status = firstNonBlank(
                    text(attributesNode, "status"),
                    text(node, "status")).toLowerCase(Locale.ROOT);
            boolean looksLikeFailure = FAILURE_STATUSES.contains(status)
                    || (!fingerprint.isBlank() && ("true".equalsIgnoreCase(text(attributesNode, "diagnostics"))
                    || node.path("category").asText("").contains("ALLURE")
                    || attributesNode.path("failureMessage").isTextual()));
            if (!fingerprint.isBlank() && (looksLikeFailure || FAILURE_STATUSES.contains(status))) {
                counter[0]++;
                Map<String, String> attributes = new HashMap<>();
                if (!status.isBlank()) {
                    attributes.put("status", status);
                } else {
                    attributes.put("status", "failed");
                }
                attributes.put("historyId", firstNonBlank(
                        text(attributesNode, "historyId"), text(node, "historyId")));
                attributes.put("name", firstNonBlank(
                        text(attributesNode, "name"), text(node, "name")));
                attributes.put("fullName", firstNonBlank(
                        text(attributesNode, "fullName"), text(node, "fullName"),
                        attributes.getOrDefault("name", "")));
                attributes.put("uuid", firstNonBlank(
                        text(attributesNode, "uuid"), text(node, "uuid"), text(node, "id")));
                attributes.put("failureMessage", firstNonBlank(
                        text(attributesNode, "failureMessage"),
                        text(node, "failureMessage"),
                        text(node, "summary"),
                        text(node.path("statusDetails"), "message")));
                attributes.put("clusterFingerprint", fingerprint);
                attributes.put("signature", fingerprint);
                String id = firstNonBlank(text(node, "id"), "doctor-" + counter[0]);
                String digest = DoctorHashing.sha256((id + fingerprint).getBytes(StandardCharsets.UTF_8));
                EvidenceCategory category = "true".equalsIgnoreCase(text(attributesNode, "diagnostics"))
                        ? EvidenceCategory.SHAFT_LOG
                        : EvidenceCategory.ALLURE_RESULT;
                if (category == EvidenceCategory.SHAFT_LOG) {
                    attributes.put("diagnostics", "true");
                }
                sink.add(new EvidenceItem(
                        id,
                        category,
                        "application/json",
                        "",
                        digest,
                        Math.max(1, fingerprint.length()),
                        fingerprint,
                        false,
                        false,
                        Map.copyOf(attributes),
                        new EvidenceProvenance("doctor-json", id, digest)));
            }
            for (JsonNode child : node) {
                collectDoctorEvidence(child, sink, counter);
            }
        } else if (node.isArray()) {
            for (JsonNode child : node) {
                collectDoctorEvidence(child, sink, counter);
            }
        }
    }

    /**
     * Same normalization as EvidenceCollector signature hashing (Doctor historical-signature keys).
     */
    static String normalizeSignature(String message, String trace) {
        String basis = message != null && !message.isBlank() ? message : firstLine(trace);
        if (basis == null || basis.isBlank()) {
            return "";
        }
        String normalized = basis.toLowerCase(Locale.ROOT)
                .replaceAll("0x[0-9a-f]+", "<hex>")
                .replaceAll("\\b\\d+\\b", "<n>")
                .replaceAll("\\s+", " ")
                .trim();
        return DoctorHashing.sha256(normalized.getBytes(StandardCharsets.UTF_8)).substring(0, 20);
    }

    private static String textFromLabels(JsonNode result, String labelName) {
        for (JsonNode label : result.path("labels")) {
            if (labelName.equals(label.path("name").asText())) {
                return label.path("value").asText("");
            }
        }
        return "";
    }

    private static String text(JsonNode node, String field) {
        if (node == null || node.isNull() || !node.isObject()) {
            return "";
        }
        JsonNode value = node.get(field);
        return value == null || value.isNull() ? "" : value.asText("").trim();
    }

    private static String firstNonBlank(String... values) {
        if (values == null) {
            return "";
        }
        for (String value : values) {
            if (value != null && !value.isBlank()) {
                return value.trim();
            }
        }
        return "";
    }

    private static String firstLine(String text) {
        if (text == null || text.isBlank()) {
            return "";
        }
        int newline = text.indexOf('\n');
        return newline < 0 ? text.trim() : text.substring(0, newline).trim();
    }

    private static String shorten(String value) {
        if (value == null) {
            return "";
        }
        String trimmed = value.trim();
        return trimmed.length() <= 240 ? trimmed : trimmed.substring(0, 240) + "…";
    }

    private static String pathString(Path path) {
        return path == null ? "" : path.toString();
    }

    private static List<String> mergeWarnings(List<String> left, List<String> right) {
        List<String> merged = new ArrayList<>();
        if (left != null) {
            merged.addAll(left);
        }
        if (right != null) {
            merged.addAll(right);
        }
        return List.copyOf(merged);
    }

    private record FailureRow(
            String signatureKey,
            String historyId,
            String name,
            String fullName,
            String uuid,
            String status,
            String failureMessage) {
        ErrorClusterModels.ImpactedTest toImpacted() {
            return new ErrorClusterModels.ImpactedTest(
                    historyId, name, fullName, uuid, status, failureMessage);
        }
    }
}
