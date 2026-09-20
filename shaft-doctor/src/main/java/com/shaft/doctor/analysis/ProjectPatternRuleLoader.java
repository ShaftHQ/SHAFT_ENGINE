package com.shaft.doctor.analysis;

import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.EvidenceItem;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Loads optional project Allure {@code categories.json} / SHAFT {@code shaft-patterns.json}
 * rules on top of {@link DeterministicRuleEngine} built-ins (issue #5970 / S3-04).
 *
 * <p>FR-001: additive load. FR-002: invalid regex fails closed (skipped + recorded error).
 * FR-003: project files cannot remove built-in rules — delete/remove directives are ignored.</p>
 */
public final class ProjectPatternRuleLoader {
    static final String CATEGORIES_FILE = "categories.json";
    static final String SHAFT_PATTERNS_FILE = "shaft-patterns.json";

    private static final ObjectMapper JSON = JsonMapper.builder().build();
    private static final Set<String> DELETE_KEYS = Set.of(
            "remove", "delete", "removeBuiltin", "deleteBuiltin", "exclude", "drop");

    private ProjectPatternRuleLoader() {
    }

    /**
     * Discovers optional rule files beside Doctor input paths (Allure results dirs preferred).
     *
     * @param inputPaths analysis input paths
     * @return existing categories.json / shaft-patterns.json paths in discovery order
     */
    public static List<Path> discover(List<Path> inputPaths) {
        LinkedHashSet<Path> found = new LinkedHashSet<>();
        if (inputPaths == null) {
            return List.of();
        }
        for (Path input : inputPaths) {
            if (input == null) {
                continue;
            }
            Path normalized = input.toAbsolutePath().normalize();
            Path dir = Files.isDirectory(normalized) ? normalized : normalized.getParent();
            if (dir == null) {
                continue;
            }
            addIfRegularFile(found, dir.resolve(CATEGORIES_FILE));
            addIfRegularFile(found, dir.resolve(SHAFT_PATTERNS_FILE));
        }
        return List.copyOf(found);
    }

    /**
     * Loads and compiles project rules from the given files.
     *
     * @param files categories.json and/or shaft-patterns.json paths
     * @return compiled additive rules plus fail-closed errors for invalid patterns
     */
    public static LoadResult load(List<Path> files) {
        List<CompiledProjectRule> rules = new ArrayList<>();
        List<String> errors = new ArrayList<>();
        if (files == null || files.isEmpty()) {
            return new LoadResult(List.of(), List.of());
        }
        int index = 0;
        for (Path file : files) {
            if (file == null || !Files.isRegularFile(file)) {
                continue;
            }
            index = loadFile(file, rules, errors, index);
        }
        return new LoadResult(List.copyOf(rules), List.copyOf(errors));
    }

    /**
     * Loads a single optional file when present.
     *
     * @param file path that may or may not exist
     * @return load result (empty when missing)
     */
    public static LoadResult loadOptional(Path file) {
        if (file == null || !Files.isRegularFile(file)) {
            return new LoadResult(List.of(), List.of());
        }
        return load(List.of(file));
    }

    private static int loadFile(
            Path file, List<CompiledProjectRule> rules, List<String> errors, int startIndex) {
        try {
            String raw = Files.readString(file, StandardCharsets.UTF_8);
            JsonNode root = JSON.readTree(raw);
            JsonNode array = root;
            if (root != null && root.isObject()) {
                if (root.has("categories")) {
                    array = root.get("categories");
                } else if (root.has("patterns")) {
                    array = root.get("patterns");
                }
            }
            if (array == null || !array.isArray()) {
                errors.add("Project rule file is not a JSON array (Allure categories shape): " + file);
                return startIndex;
            }
            int index = startIndex;
            for (JsonNode entry : array) {
                if (entry == null || !entry.isObject()) {
                    errors.add("Skipped non-object project rule entry in " + file.getFileName());
                    continue;
                }
                if (isDeleteDirective(entry)) {
                    // FR-003: project files cannot remove built-in rules.
                    errors.add("Ignored delete/remove directive in " + file.getFileName()
                            + " (built-in rules are immutable).");
                    continue;
                }
                CompiledProjectRule compiled = compileEntry(entry, file, index, errors);
                if (compiled != null) {
                    rules.add(compiled);
                    index++;
                }
            }
            return index;
        } catch (IOException | RuntimeException exception) {
            errors.add("Failed to read project rule file " + file + ": " + exception.getMessage());
            return startIndex;
        }
    }

    private static CompiledProjectRule compileEntry(
            JsonNode entry, Path file, int index, List<String> errors) {
        String name = text(entry, "name");
        if (name.isBlank()) {
            errors.add("Skipped project rule without name in " + file.getFileName());
            return null;
        }
        CauseCategory category = resolveCategory(entry, name, errors, file);
        if (category == null) {
            return null;
        }
        String messageRegex = text(entry, "messageRegex");
        String traceRegex = text(entry, "traceRegex");
        if (messageRegex.isBlank() && traceRegex.isBlank()) {
            errors.add("Skipped project rule '" + name + "' without messageRegex/traceRegex in "
                    + file.getFileName());
            return null;
        }
        Pattern messagePattern;
        Pattern tracePattern;
        try {
            messagePattern = compileRegex(messageRegex);
            tracePattern = compileRegex(traceRegex);
        } catch (PatternSyntaxException invalid) {
            // FR-002: fail closed — do not apply the rule.
            errors.add("Invalid regex in project rule '" + name + "' (" + file.getFileName() + "): "
                    + invalid.getMessage());
            return null;
        }
        Set<String> statuses = readStatuses(entry);
        String id = "project-" + slug(name) + "-" + index;
        String title = name;
        String action = "Investigate as project-classified '" + name
                + "' using the cited failure evidence before changing product or test code.";
        return new CompiledProjectRule(id, category, Confidence.HIGH, title, action,
                messagePattern, tracePattern, statuses);
    }

    private static CauseCategory resolveCategory(
            JsonNode entry, String name, List<String> errors, Path file) {
        String explicit = firstNonBlank(
                text(entry, "causeCategory"),
                text(entry, "shaftCategory"),
                text(entry, "category"));
        if (!explicit.isBlank()) {
            CauseCategory mapped = mapCategoryToken(explicit);
            if (mapped == null) {
                errors.add("Unknown causeCategory '" + explicit + "' for project rule '" + name
                        + "' in " + file.getFileName());
            }
            return mapped;
        }
        CauseCategory fromName = mapCategoryToken(name);
        if (fromName == null) {
            errors.add("Could not map project rule name '" + name
                    + "' to a CauseCategory in " + file.getFileName()
                    + "; set causeCategory explicitly.");
        }
        return fromName;
    }

    private static CauseCategory mapCategoryToken(String raw) {
        if (raw == null || raw.isBlank()) {
            return null;
        }
        String token = raw.trim().toUpperCase(Locale.ROOT)
                .replace('-', '_')
                .replace(' ', '_');
        while (token.contains("__")) {
            token = token.replace("__", "_");
        }
        // Strict aliases only — Allure display names must set causeCategory explicitly
        // so generic categories.json files cannot steal built-in Doctor precedence.
        return switch (token) {
            case "ENVIRONMENT", "ENVIRONMENT_CONFIGURATION", "CONFIGURATION" ->
                    CauseCategory.ENVIRONMENT_CONFIGURATION;
            case "INFRASTRUCTURE" -> CauseCategory.INFRASTRUCTURE;
            case "LOCATOR" -> CauseCategory.LOCATOR;
            case "DATA" -> CauseCategory.DATA;
            case "TEST" -> CauseCategory.TEST;
            case "PRODUCT" -> CauseCategory.PRODUCT;
            case "TIMING", "TIMING_SYNCHRONIZATION", "SYNCHRONIZATION" ->
                    CauseCategory.TIMING_SYNCHRONIZATION;
            case "UNKNOWN" -> CauseCategory.UNKNOWN;
            default -> {
                try {
                    yield CauseCategory.valueOf(token);
                } catch (IllegalArgumentException ignored) {
                    yield null;
                }
            }
        };
    }

    private static Pattern compileRegex(String regex) {
        if (regex == null || regex.isBlank()) {
            return null;
        }
        // Prefer Allure categories.json semantics: full-match with DOTALL.
        return Pattern.compile(regex, Pattern.DOTALL);
    }

    private static Set<String> readStatuses(JsonNode entry) {
        JsonNode node = entry.get("matchedStatuses");
        if (node == null || !node.isArray() || node.isEmpty()) {
            return Set.of();
        }
        LinkedHashSet<String> statuses = new LinkedHashSet<>();
        for (JsonNode status : node) {
            if (status != null && status.isValueNode()) {
                String value = status.asText("").trim().toLowerCase(Locale.ROOT);
                if (!value.isBlank()) {
                    statuses.add(value);
                }
            }
        }
        return Set.copyOf(statuses);
    }

    private static boolean isDeleteDirective(JsonNode entry) {
        for (String key : DELETE_KEYS) {
            JsonNode value = entry.get(key);
            if (value != null && (value.isBoolean() && value.booleanValue()
                    || value.isValueNode() && "true".equalsIgnoreCase(value.asText()))) {
                return true;
            }
        }
        return false;
    }

    private static void addIfRegularFile(LinkedHashSet<Path> sink, Path candidate) {
        if (Files.isRegularFile(candidate)) {
            sink.add(candidate.toAbsolutePath().normalize());
        }
    }

    private static String text(JsonNode node, String field) {
        if (node == null || field == null) {
            return "";
        }
        JsonNode value = node.get(field);
        if (value == null || value.isNull() || !value.isValueNode()) {
            return "";
        }
        String text = value.asText("");
        return text == null ? "" : text.trim();
    }

    private static String firstNonBlank(String... values) {
        for (String value : values) {
            if (value != null && !value.isBlank()) {
                return value;
            }
        }
        return "";
    }

    private static String slug(String name) {
        String slug = name.toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9]+", "-");
        slug = slug.replaceAll("^-+", "").replaceAll("-+$", "");
        return slug.isBlank() ? "rule" : slug;
    }

    /**
     * Result of loading optional project pattern files.
     *
     * @param rules compiled additive rules in precedence order
     * @param errors fail-closed messages for invalid or ignored entries
     */
    public record LoadResult(List<CompiledProjectRule> rules, List<String> errors) {
        /**
         * Creates an immutable load result.
         */
        public LoadResult {
            rules = rules == null ? List.of() : List.copyOf(rules);
            errors = errors == null ? List.of() : List.copyOf(errors);
        }

        /**
         * @return true when at least one rule compiled
         */
        public boolean hasRules() {
            return !rules.isEmpty();
        }
    }

    /**
     * One compiled project rule ready for {@link DeterministicRuleEngine}.
     *
     * @param id stable rule id (always {@code project-} prefixed)
     * @param category Doctor cause category
     * @param confidence match confidence
     * @param title human title (usually Allure category name)
     * @param action remediation action text
     * @param messagePattern optional Allure messageRegex
     * @param tracePattern optional Allure traceRegex
     * @param matchedStatuses empty means any status (Allure semantics)
     */
    public record CompiledProjectRule(
            String id,
            CauseCategory category,
            Confidence confidence,
            String title,
            String action,
            Pattern messagePattern,
            Pattern tracePattern,
            Set<String> matchedStatuses) {
        /**
         * Creates an immutable compiled rule.
         */
        public CompiledProjectRule {
            matchedStatuses = matchedStatuses == null ? Set.of() : Set.copyOf(matchedStatuses);
        }

        /**
         * Allure-compatible match against one evidence item.
         *
         * @param item evidence item
         * @return true when status/message/trace constraints all pass
         */
        public boolean matches(EvidenceItem item) {
            if (item == null) {
                return false;
            }
            String status = item.attributes().getOrDefault("status", "").toLowerCase(Locale.ROOT);
            if (!matchedStatuses.isEmpty() && !matchedStatuses.contains(status)) {
                return false;
            }
            String message = item.attributes().getOrDefault("failureMessage", "");
            if (message.isBlank() && item.content() != null) {
                message = item.content();
            }
            String trace = item.attributes().getOrDefault("traceTop", "");
            if (trace.isBlank() && item.content() != null) {
                trace = item.content();
            }
            if (messagePattern != null && !messagePattern.matcher(nullToEmpty(message)).matches()) {
                return false;
            }
            if (tracePattern != null && !tracePattern.matcher(nullToEmpty(trace)).matches()) {
                return false;
            }
            return messagePattern != null || tracePattern != null;
        }

        private static String nullToEmpty(String value) {
            return value == null ? "" : value;
        }
    }
}
