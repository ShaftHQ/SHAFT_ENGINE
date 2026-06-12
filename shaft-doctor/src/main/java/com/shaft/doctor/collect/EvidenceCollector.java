package com.shaft.doctor.collect;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.shaft.doctor.DoctorAnalysisRequest;
import com.shaft.doctor.format.DoctorFormatException;
import com.shaft.doctor.format.DoctorJsonCodec;
import com.shaft.doctor.internal.DoctorHashing;
import com.shaft.doctor.internal.DoctorRedactor;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.EvidenceProvenance;
import com.shaft.doctor.model.RedactionSummary;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Stream;

/**
 * Collects bounded local evidence through explicit allowlisted adapter boundaries.
 */
public final class EvidenceCollector {
    private static final int MAX_FILES = 10_000;
    private static final int MAX_ATTRIBUTE_LENGTH = 2_048;
    private static final Set<String> TEXT_EXTENSIONS = Set.of(
            ".txt", ".log", ".md", ".properties", ".xml", ".yaml", ".yml",
            ".html", ".htm", ".mhtml", ".json", ".csv");
    private static final Set<String> SCREENSHOT_EXTENSIONS = Set.of(
            ".png", ".jpg", ".jpeg", ".gif", ".webp");

    private final ObjectMapper mapper = new ObjectMapper()
            .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);
    private final DoctorJsonCodec codec = new DoctorJsonCodec();
    private final DefaultPrettyPrinter printer = prettyPrinter();

    /**
     * Collects a deterministic portable evidence bundle.
     *
     * @param request explicit collection policy
     * @return evidence bundle
     */
    public EvidenceBundle collect(DoctorAnalysisRequest request) {
        List<Path> roots = realRoots(request.allowedRoots());
        List<Path> files = discoverFiles(request.inputPaths(), roots, request.outputDirectory());
        DoctorRedactor redactor = new DoctorRedactor();
        Map<String, EvidenceItem> items = new LinkedHashMap<>();
        CollectionState state = new CollectionState();

        for (Path file : files) {
            if (state.retainedBytes >= request.maxBundleBytes()) {
                state.omittedItems++;
                continue;
            }
            collectFile(file, roots, request, redactor, items, state);
        }

        List<EvidenceItem> sortedItems = items.values().stream()
                .sorted(Comparator.comparing(EvidenceItem::id))
                .toList();
        String fingerprint = sortedItems.stream()
                .map(item -> item.id() + ":" + item.sha256())
                .reduce("", (left, right) -> left + "\n" + right);
        String bundleId = "bundle-" + DoctorHashing.sha256(
                fingerprint.getBytes(StandardCharsets.UTF_8)).substring(0, 20);
        Map<String, String> metadata = new TreeMap<>();
        metadata.put("collector", "shaft-doctor");
        metadata.put("allureResultCount", Long.toString(sortedItems.stream()
                .filter(item -> item.category() == EvidenceCategory.ALLURE_RESULT)
                .filter(item -> !"true".equals(item.attributes().get("invalid")))
                .count()));
        metadata.put("invalidAllureResultCount", Long.toString(sortedItems.stream()
                .filter(item -> "true".equals(item.attributes().get("invalid")))
                .count()));
        metadata.put("minimumAllureResultCount", Integer.toString(request.minimumAllureResults()));
        metadata.put("retainedBytes", Long.toString(state.retainedBytes));
        return new EvidenceBundle(
                EvidenceBundle.CURRENT_SCHEMA_VERSION,
                bundleId,
                sortedItems,
                new RedactionSummary(
                        union(redactor.appliedRules(), state.importedRules),
                        union(redactor.removedFields(), state.importedFields),
                        state.omittedItems),
                metadata);
    }

    /**
     * Loads historical bundles after applying the same allowed-root boundary.
     *
     * @param request analysis request
     * @return historical bundles
     */
    public List<EvidenceBundle> loadHistoricalBundles(DoctorAnalysisRequest request) {
        List<Path> roots = realRoots(request.allowedRoots());
        List<EvidenceBundle> bundles = new ArrayList<>();
        for (Path path : request.historicalBundlePaths()) {
            Path real = allowedRealPath(path, roots);
            if (!Files.isRegularFile(real)) {
                throw new IllegalArgumentException("Historical Doctor bundle must be a regular file.");
            }
            bundles.add(codec.readBundle(real));
        }
        return List.copyOf(bundles);
    }

    private void collectFile(
            Path file,
            List<Path> roots,
            DoctorAnalysisRequest request,
            DoctorRedactor redactor,
            Map<String, EvidenceItem> items,
            CollectionState state) {
        String sourceReference = portableReference(file, roots);
        String lowerName = file.getFileName().toString().toLowerCase(Locale.ROOT);
        String extension = extension(lowerName);

        if (looksLikeBundle(file, lowerName)) {
            try {
                EvidenceBundle bundle = codec.readBundle(file);
                for (EvidenceItem item : bundle.evidence()) {
                    if (item.category() == EvidenceCategory.SCREENSHOT && !request.includeScreenshots()) {
                        state.omittedItems++;
                        continue;
                    }
                    if (item.category() == EvidenceCategory.PAGE_SNAPSHOT && !request.includePageSnapshots()) {
                        state.omittedItems++;
                        continue;
                    }
                    if (item.sizeBytes() > request.maxItemBytes()
                            || state.retainedBytes + item.sizeBytes() > request.maxBundleBytes()) {
                        state.omittedItems++;
                        continue;
                    }
                    copyImportedArtifact(file, item, roots, request);
                    items.putIfAbsent(item.id(), item);
                    state.retainedBytes += item.sizeBytes();
                }
                state.omittedItems += bundle.redaction().omittedItems();
                state.importedRules.addAll(bundle.redaction().appliedRules());
                state.importedFields.addAll(bundle.redaction().removedFieldNames());
                return;
            } catch (DoctorFormatException ignored) {
                // Ordinary JSON files continue through their category adapter.
            }
        }

        if (SCREENSHOT_EXTENSIONS.contains(extension)) {
            if (!request.includeScreenshots()) {
                state.omittedItems++;
                return;
            }
            collectBinary(file, sourceReference, EvidenceCategory.SCREENSHOT,
                    mediaType(extension), request, items, state);
            return;
        }
        if (isPageSnapshot(lowerName, extension) && !request.includePageSnapshots()) {
            state.omittedItems++;
            return;
        }
        if (!TEXT_EXTENSIONS.contains(extension) && !lowerName.equals("pom.xml")) {
            return;
        }

        if (lowerName.endsWith("-result.json")) {
            collectAllureResult(file, sourceReference, roots, request, redactor, items, state);
            return;
        }
        EvidenceCategory category = inferCategory(lowerName, extension);
        collectText(file, sourceReference, category, mediaType(extension),
                request, redactor, items, state);
    }

    private void collectAllureResult(
            Path file,
            String sourceReference,
            List<Path> roots,
            DoctorAnalysisRequest request,
            DoctorRedactor redactor,
            Map<String, EvidenceItem> items,
            CollectionState state) {
        byte[] source = readBounded(file, request.maxItemBytes());
        boolean sourceTruncated = FilesSize.size(file) > source.length;
        try {
            JsonNode parsed = mapper.readTree(source);
            if (parsed == null || !parsed.isObject()) {
                throw new IllegalArgumentException("Allure result root is not an object.");
            }
            JsonNode sanitized = redactor.redact(parsed);
            byte[] retained = mapper.writer(printer)
                    .writeValueAsBytes(sanitized);
            retained = limit(retained, request.maxItemBytes());
            Map<String, String> attributes = allureAttributes(sanitized);
            attributes.put("invalid", "false");
            addTextItem(sourceReference, EvidenceCategory.ALLURE_RESULT, "application/json",
                    retained, sourceTruncated || retained.length >= request.maxItemBytes(),
                    attributes, "allure-result-json", items, state, request.maxBundleBytes());
            String message = attributes.getOrDefault("failureMessage", "");
            String trace = detailsText(sanitized.path("statusDetails").path("trace").asText(""));
            if (!message.isBlank() || !trace.isBlank()) {
                String exceptionText = redactor.redact(message + (trace.isBlank() ? "" : "\n" + trace));
                addTextItem(sourceReference, EvidenceCategory.EXCEPTION_CHAIN, "text/plain",
                        limit(exceptionText.getBytes(StandardCharsets.UTF_8), request.maxItemBytes()),
                        sourceTruncated,
                        Map.of(
                                "status", attributes.getOrDefault("status", ""),
                                "historyId", attributes.getOrDefault("historyId", ""),
                                "signature", attributes.getOrDefault("signature", "")),
                        "allure-exception-chain", items, state, request.maxBundleBytes());
            }
            collectAllureAttachments(parsed, file, roots, request, redactor, items, state);
        } catch (IOException | IllegalArgumentException exception) {
            byte[] retained = "Malformed or truncated Allure result JSON."
                    .getBytes(StandardCharsets.UTF_8);
            addTextItem(sourceReference, EvidenceCategory.ALLURE_RESULT, "text/plain",
                    retained, sourceTruncated, Map.of("invalid", "true"),
                    "allure-result-json", items, state, request.maxBundleBytes());
        }
    }

    private void collectText(
            Path file,
            String sourceReference,
            EvidenceCategory category,
            String mediaType,
            DoctorAnalysisRequest request,
            DoctorRedactor redactor,
            Map<String, EvidenceItem> items,
            CollectionState state) {
        byte[] source = readBounded(file, request.maxItemBytes());
        boolean truncated = FilesSize.size(file) > source.length;
        String text = new String(source, StandardCharsets.UTF_8);
        String sanitized;
        if ("application/json".equals(mediaType)) {
            try {
                sanitized = mapper.writer(printer)
                        .writeValueAsString(redactor.redact(mapper.readTree(text)));
            } catch (JsonProcessingException exception) {
                sanitized = redactor.redact(text);
            }
        } else {
            sanitized = redactor.redact(text);
        }
        byte[] retained = limit(sanitized.getBytes(StandardCharsets.UTF_8), request.maxItemBytes());
        addTextItem(sourceReference, category, mediaType, retained,
                truncated || retained.length < sanitized.getBytes(StandardCharsets.UTF_8).length,
                Map.of(), adapter(category), items, state, request.maxBundleBytes());
    }

    private void collectAllureAttachments(
            JsonNode node,
            Path resultFile,
            List<Path> roots,
            DoctorAnalysisRequest request,
            DoctorRedactor redactor,
            Map<String, EvidenceItem> items,
            CollectionState state) {
        JsonNode attachments = node.path("attachments");
        if (attachments.isArray()) {
            for (JsonNode attachment : attachments) {
                String source = attachment.path("source").asText("");
                if (source.isBlank()) {
                    continue;
                }
                Path candidate = resultFile.getParent().resolve(source);
                if (!Files.exists(candidate)) {
                    continue;
                }
                Path artifact = allowedRealPath(candidate, roots);
                if (!Files.isRegularFile(artifact)) {
                    continue;
                }
                String name = attachment.path("name").asText("").toLowerCase(Locale.ROOT);
                String type = attachment.path("type").asText("").toLowerCase(Locale.ROOT);
                String extension = extension(artifact.getFileName().toString().toLowerCase(Locale.ROOT));
                String reference = portableReference(artifact, roots);
                if (type.startsWith("image/") || SCREENSHOT_EXTENSIONS.contains(extension)) {
                    if (request.includeScreenshots()) {
                        collectBinary(artifact, reference, EvidenceCategory.SCREENSHOT,
                                type.isBlank() ? mediaType(extension) : type, request, items, state);
                    } else {
                        state.omittedItems++;
                    }
                } else if (type.contains("html") || type.contains("multipart/related")
                        || name.contains("page source") || name.contains("snapshot")) {
                    if (request.includePageSnapshots()) {
                        collectText(artifact, reference, EvidenceCategory.PAGE_SNAPSHOT,
                                type.isBlank() ? mediaType(extension) : type,
                                request, redactor, items, state);
                    } else {
                        state.omittedItems++;
                    }
                } else if (name.contains("log") || name.contains("action history")
                        || name.contains("shaft")) {
                    collectText(artifact, reference, EvidenceCategory.SHAFT_LOG,
                            type.isBlank() ? mediaType(extension) : type,
                            request, redactor, items, state);
                }
            }
        }
        for (JsonNode child : node) {
            if (child.isContainerNode()) {
                collectAllureAttachments(child, resultFile, roots, request, redactor, items, state);
            }
        }
    }

    private void collectBinary(
            Path file,
            String sourceReference,
            EvidenceCategory category,
            String mediaType,
            DoctorAnalysisRequest request,
            Map<String, EvidenceItem> items,
            CollectionState state) {
        byte[] retained = readBounded(file, request.maxItemBytes());
        boolean truncated = FilesSize.size(file) > retained.length;
        if (state.retainedBytes + retained.length > request.maxBundleBytes()) {
            state.omittedItems++;
            return;
        }
        String digest = DoctorHashing.sha256(retained);
        String id = evidenceId(category, sourceReference, digest);
        String extension = extension(file.getFileName().toString().toLowerCase(Locale.ROOT));
        String relativePath = "artifacts/" + id + extension;
        Path destination = request.outputDirectory().resolve(relativePath).normalize();
        if (!destination.startsWith(request.outputDirectory())) {
            throw new IllegalArgumentException("Doctor artifact destination escaped the output directory.");
        }
        try {
            Files.createDirectories(destination.getParent());
            Files.write(destination, retained);
        } catch (IOException exception) {
            throw new IllegalStateException("Doctor binary evidence could not be retained.", exception);
        }
        EvidenceItem item = new EvidenceItem(
                id, category, mediaType, relativePath, digest, retained.length, null,
                false, truncated, Map.of(),
                new EvidenceProvenance(adapter(category), sourceReference, digest));
        items.putIfAbsent(id, item);
        state.retainedBytes += retained.length;
    }

    private static void addTextItem(
            String sourceReference,
            EvidenceCategory category,
            String mediaType,
            byte[] retained,
            boolean truncated,
            Map<String, String> attributes,
            String adapter,
            Map<String, EvidenceItem> items,
            CollectionState state,
            long maxBundleBytes) {
        if (state.retainedBytes + retained.length > maxBundleBytes) {
            state.omittedItems++;
            return;
        }
        String digest = DoctorHashing.sha256(retained);
        String id = evidenceId(category, sourceReference, digest);
        EvidenceItem item = new EvidenceItem(
                id, category, mediaType, "", digest, retained.length,
                new String(retained, StandardCharsets.UTF_8), true, truncated,
                attributes, new EvidenceProvenance(adapter, sourceReference, digest));
        items.putIfAbsent(id, item);
        state.retainedBytes += retained.length;
    }

    private static Map<String, String> allureAttributes(JsonNode result) {
        Map<String, String> attributes = new TreeMap<>();
        put(attributes, "uuid", result.path("uuid").asText(""));
        put(attributes, "historyId", result.path("historyId").asText(
                result.path("testCaseId").asText("")));
        put(attributes, "name", result.path("fullName").asText(
                result.path("name").asText("")));
        put(attributes, "status", result.path("status").asText("").toLowerCase(Locale.ROOT));
        put(attributes, "start", result.path("start").asText(""));
        put(attributes, "stop", result.path("stop").asText(""));
        JsonNode details = result.path("statusDetails");
        String message = details.path("message").asText("");
        String trace = details.path("trace").asText("");
        put(attributes, "failureMessage", shorten(message));
        put(attributes, "traceTop", shorten(firstLine(trace)));
        put(attributes, "signature", signature(message, trace));
        String method = label(result, "testMethod");
        put(attributes, "testMethod", method);
        put(attributes, "fixturePhase", fixturePhase(result.path("name").asText(""), method));
        return attributes;
    }

    private static String label(JsonNode result, String name) {
        for (JsonNode label : result.path("labels")) {
            if (name.equals(label.path("name").asText())) {
                return label.path("value").asText("");
            }
        }
        return "";
    }

    private static String fixturePhase(String name, String method) {
        String value = (method.isBlank() ? name : method).toLowerCase(Locale.ROOT);
        if (value.startsWith("before") || value.startsWith("setup") || value.startsWith("init")) {
            return "setup";
        }
        if (value.startsWith("after") || value.startsWith("tear") || value.startsWith("cleanup")) {
            return "cleanup";
        }
        return "";
    }

    private static String signature(String message, String trace) {
        String basis = !message.isBlank() ? message : firstLine(trace);
        if (basis.isBlank()) {
            return "";
        }
        String normalized = basis.toLowerCase(Locale.ROOT)
                .replaceAll("0x[0-9a-f]+", "<hex>")
                .replaceAll("\\b\\d+\\b", "<n>")
                .replaceAll("\\s+", " ")
                .trim();
        return DoctorHashing.sha256(normalized.getBytes(StandardCharsets.UTF_8)).substring(0, 20);
    }

    private static List<Path> discoverFiles(
            List<Path> inputs,
            List<Path> roots,
            Path outputDirectory) {
        Set<Path> files = new LinkedHashSet<>();
        Path normalizedOutput = outputDirectory.toAbsolutePath().normalize();
        for (Path input : inputs) {
            Path real = allowedRealPath(input, roots);
            if (Files.isRegularFile(real)) {
                files.add(real);
            } else if (Files.isDirectory(real)) {
                try (Stream<Path> stream = Files.walk(real)) {
                    stream.filter(Files::isRegularFile)
                            .map(path -> allowedRealPath(path, roots))
                            .filter(path -> !path.startsWith(normalizedOutput))
                            .sorted()
                            .limit(MAX_FILES + 1L)
                            .forEach(files::add);
                } catch (IOException exception) {
                    throw new IllegalArgumentException("Doctor input directory could not be read.", exception);
                }
            } else {
                throw new IllegalArgumentException("Doctor input must be a regular file or directory.");
            }
            if (files.size() > MAX_FILES) {
                throw new IllegalArgumentException("Doctor input exceeds the maximum file count.");
            }
        }
        return List.copyOf(files);
    }

    private static void copyImportedArtifact(
            Path bundleFile,
            EvidenceItem item,
            List<Path> roots,
            DoctorAnalysisRequest request) {
        if (item.relativePath().isBlank() || item.content() != null) {
            return;
        }
        Path source = allowedRealPath(bundleFile.getParent().resolve(item.relativePath()), roots);
        byte[] bytes = readBounded(source, request.maxItemBytes());
        if (!DoctorHashing.sha256(bytes).equals(item.sha256())) {
            throw new IllegalArgumentException("Imported Doctor artifact checksum does not match its bundle.");
        }
        Path destination = request.outputDirectory().resolve(item.relativePath()).normalize();
        if (!destination.startsWith(request.outputDirectory())) {
            throw new IllegalArgumentException("Imported Doctor artifact escaped the output directory.");
        }
        try {
            Files.createDirectories(destination.getParent());
            Files.write(destination, bytes);
        } catch (IOException exception) {
            throw new IllegalStateException("Imported Doctor artifact could not be retained.", exception);
        }
    }

    private static List<Path> realRoots(List<Path> roots) {
        return roots.stream().map(root -> {
            try {
                return root.toRealPath();
            } catch (IOException exception) {
                throw new IllegalArgumentException("Doctor allowed root does not exist.", exception);
            }
        }).distinct().toList();
    }

    private static Path allowedRealPath(Path path, List<Path> roots) {
        try {
            Path real = path.toRealPath();
            if (roots.stream().noneMatch(real::startsWith)) {
                throw new IllegalArgumentException("Doctor input is outside the explicit allowed roots.");
            }
            return real;
        } catch (IOException exception) {
            throw new IllegalArgumentException("Doctor input path does not exist or cannot be resolved.", exception);
        }
    }

    private static String portableReference(Path file, List<Path> roots) {
        for (int index = 0; index < roots.size(); index++) {
            Path root = roots.get(index);
            if (file.startsWith(root)) {
                String relative = root.relativize(file).toString().replace('\\', '/');
                if (relative.isBlank()) {
                    relative = file.getFileName().toString();
                }
                return "root-" + (index + 1) + "/" + relative;
            }
        }
        throw new IllegalArgumentException("Doctor input is outside the explicit allowed roots.");
    }

    private static byte[] readBounded(Path file, long maximum) {
        try (var input = Files.newInputStream(file)) {
            return input.readNBytes(Math.toIntExact(Math.min(maximum, Integer.MAX_VALUE)));
        } catch (IOException exception) {
            throw new IllegalArgumentException("Doctor evidence file could not be read.", exception);
        }
    }

    private static byte[] limit(byte[] bytes, long maximum) {
        if (bytes.length <= maximum) {
            return bytes;
        }
        byte[] limited = new byte[Math.toIntExact(maximum)];
        System.arraycopy(bytes, 0, limited, 0, limited.length);
        return limited;
    }

    private static boolean looksLikeBundle(Path file, String lowerName) {
        if (!lowerName.endsWith(".json") || lowerName.endsWith("-result.json")) {
            return false;
        }
        String prefix = new String(readBounded(file, 8_192), StandardCharsets.UTF_8);
        return prefix.contains("\"bundleId\"") && prefix.contains("\"evidence\"");
    }

    private static EvidenceCategory inferCategory(String lowerName, String extension) {
        if (isPageSnapshot(lowerName, extension)) {
            return EvidenceCategory.PAGE_SNAPSHOT;
        }
        if (lowerName.endsWith(".log") || lowerName.contains("shaft")
                || lowerName.contains("action")) {
            return EvidenceCategory.SHAFT_LOG;
        }
        if (lowerName.equals("pom.xml") || lowerName.contains("dependency")
                || lowerName.contains("build") || lowerName.contains("surefire")) {
            return EvidenceCategory.DEPENDENCY_BUILD;
        }
        if (lowerName.contains("environment") || lowerName.equals("executor.json")
                || lowerName.contains("system-info")) {
            return EvidenceCategory.ENVIRONMENT;
        }
        if (lowerName.endsWith(".properties") || lowerName.contains("config")
                || lowerName.contains("capabilities")) {
            return EvidenceCategory.CONFIGURATION;
        }
        return EvidenceCategory.OTHER;
    }

    private static boolean isPageSnapshot(String lowerName, String extension) {
        return Set.of(".html", ".htm", ".mhtml").contains(extension)
                || lowerName.contains("page-source")
                || lowerName.contains("page_source")
                || lowerName.contains("snapshot");
    }

    private static String adapter(EvidenceCategory category) {
        return switch (category) {
            case ALLURE_RESULT -> "allure-result-json";
            case SHAFT_LOG -> "shaft-log";
            case EXCEPTION_CHAIN -> "exception-chain";
            case SCREENSHOT -> "screenshot";
            case PAGE_SNAPSHOT -> "page-snapshot";
            case ENVIRONMENT -> "environment-metadata";
            case DEPENDENCY_BUILD -> "dependency-build-metadata";
            case CONFIGURATION -> "configuration-summary";
            case OTHER -> "explicit-artifact";
        };
    }

    private static String evidenceId(EvidenceCategory category, String sourceReference, String digest) {
        String basis = category.name() + "\n" + sourceReference + "\n" + digest;
        return "e-" + DoctorHashing.sha256(basis.getBytes(StandardCharsets.UTF_8)).substring(0, 20);
    }

    private static DefaultPrettyPrinter prettyPrinter() {
        DefaultPrettyPrinter printer = new DefaultPrettyPrinter();
        DefaultIndenter indenter = new DefaultIndenter("  ", "\n");
        printer.indentArraysWith(indenter);
        printer.indentObjectsWith(indenter);
        return printer;
    }

    private static String mediaType(String extension) {
        return switch (extension) {
            case ".json" -> "application/json";
            case ".xml" -> "application/xml";
            case ".html", ".htm" -> "text/html";
            case ".mhtml" -> "multipart/related";
            case ".yaml", ".yml" -> "application/yaml";
            case ".csv" -> "text/csv";
            case ".properties", ".txt", ".log", ".md" -> "text/plain";
            case ".png" -> "image/png";
            case ".jpg", ".jpeg" -> "image/jpeg";
            case ".gif" -> "image/gif";
            case ".webp" -> "image/webp";
            default -> "application/octet-stream";
        };
    }

    private static String extension(String lowerName) {
        int dot = lowerName.lastIndexOf('.');
        return dot < 0 ? "" : lowerName.substring(dot);
    }

    private static String firstLine(String value) {
        return value == null ? "" : value.lines()
                .map(String::trim)
                .filter(line -> !line.isBlank())
                .findFirst()
                .orElse("");
    }

    private static String detailsText(String value) {
        return value == null ? "" : value.trim();
    }

    private static String shorten(String value) {
        String normalized = value == null ? "" : value.replaceAll("\\s+", " ").trim();
        return normalized.length() <= MAX_ATTRIBUTE_LENGTH
                ? normalized
                : normalized.substring(0, MAX_ATTRIBUTE_LENGTH);
    }

    private static void put(Map<String, String> attributes, String name, String value) {
        if (value != null && !value.isBlank()) {
            attributes.put(name, shorten(value));
        }
    }

    private static List<String> union(List<String> values, Set<String> imported) {
        Set<String> combined = new LinkedHashSet<>(values);
        combined.addAll(imported);
        return combined.stream().sorted().toList();
    }

    private static final class CollectionState {
        private long retainedBytes;
        private int omittedItems;
        private final Set<String> importedRules = new LinkedHashSet<>();
        private final Set<String> importedFields = new LinkedHashSet<>();
    }

    private static final class FilesSize {
        private FilesSize() {
            throw new IllegalStateException("Utility class");
        }

        private static long size(Path path) {
            try {
                return Files.size(path);
            } catch (IOException exception) {
                throw new IllegalArgumentException("Doctor evidence size could not be read.", exception);
            }
        }
    }
}
