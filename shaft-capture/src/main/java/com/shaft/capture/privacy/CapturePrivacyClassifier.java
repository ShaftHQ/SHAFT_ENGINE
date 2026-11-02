package com.shaft.capture.privacy;

import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.RedactionSummary;
import com.shaft.pilot.security.Redactor;

import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Applies deterministic privacy classification before capture data reaches disk.
 */
public final class CapturePrivacyClassifier {
    private final CapturePrivacyPolicy policy;
    private final List<Pattern> sensitivePatterns;

    /**
     * Creates a classifier using the supplied policy.
     *
     * @param policy privacy policy
     */
    public CapturePrivacyClassifier(CapturePrivacyPolicy policy) {
        this.policy = policy == null ? CapturePrivacyPolicy.defaults() : policy;
        sensitivePatterns = compile(this.policy.sensitiveValuePatterns());
    }

    /**
     * Creates a classifier with conservative defaults.
     */
    public CapturePrivacyClassifier() {
        this(CapturePrivacyPolicy.defaults());
    }

    /**
     * Classifies a typed or expected value and creates an external reference.
     *
     * @param logicalName logical field name
     * @param rawValue original value
     * @param selector optional source selector
     * @param attributes optional source attributes
     * @return safe classification result
     */
    public ClassifiedValue classifyValue(
            String logicalName,
            String rawValue,
            String selector,
            Map<String, String> attributes) {
        String safeName = safeIdentifier(logicalName);
        Set<String> rules = new TreeSet<>();
        boolean secret = isSensitiveName(logicalName, policy.sensitiveFieldNames(), "sensitive-field", rules)
                || isSensitiveSelector(selector, rules)
                || containsSensitiveAttribute(attributes, rules)
                || matchesSensitivePattern(rawValue, rules);
        ExternalTestDataReference reference;
        String externalizedValue;
        if (secret) {
            reference = new ExternalTestDataReference(
                    "data." + safeName,
                    safeName,
                    ExternalTestDataReference.DataSource.ENVIRONMENT,
                    "",
                    "",
                    ExternalTestDataReference.DataClassification.SECRET);
            externalizedValue = null;
        } else {
            reference = new ExternalTestDataReference(
                    "data." + safeName,
                    safeName,
                    ExternalTestDataReference.DataSource.JSON,
                    policy.externalDataPath(),
                    "/values/" + escapePointer("data." + safeName),
                    ExternalTestDataReference.DataClassification.ORDINARY);
            externalizedValue = rawValue == null ? "" : rawValue;
            rules.add("externalized-test-data");
        }
        return new ClassifiedValue(reference, externalizedValue,
                new RedactionSummary(1, 0, 0, rules));
    }

    /**
     * Creates a safe upload reference without retaining an absolute path or file contents.
     *
     * @param logicalName logical upload name
     * @param sourcePath source path used only to derive a safe basename
     * @param mediaType safe media type
     * @param sizeBytes safe file-size metadata
     * @return safe upload classification
     */
    public ClassifiedUpload classifyUpload(
            String logicalName,
            String sourcePath,
            String mediaType,
            long sizeBytes) {
        if (sizeBytes < 0) {
            throw new IllegalArgumentException("Upload size cannot be negative.");
        }
        String safeName = safeIdentifier(logicalName);
        String fileName = sanitizeArtifactFilename(sourcePath);
        ExternalTestDataReference reference = new ExternalTestDataReference(
                "upload." + safeName,
                safeName,
                ExternalTestDataReference.DataSource.FILE_FIXTURE,
                "test-data/uploads/" + fileName,
                "",
                ExternalTestDataReference.DataClassification.UPLOAD);
        return new ClassifiedUpload(reference, fileName, normalizeMediaType(mediaType), sizeBytes,
                new RedactionSummary(1, 0, 0, Set.of("logical-upload-reference")));
    }

    /**
     * Redacts configured URL parameters and secret-looking values.
     *
     * @param rawUrl original URL
     * @return sanitized URL and safe summary
     */
    public SanitizedText sanitizeUrl(String rawUrl) {
        String value = rawUrl == null ? "" : rawUrl;
        int queryIndex = value.indexOf('?');
        if (queryIndex < 0) {
            return redactPatternMatches(value, false);
        }
        int fragmentIndex = value.indexOf('#', queryIndex);
        String prefix = value.substring(0, queryIndex + 1);
        String query = fragmentIndex < 0
                ? value.substring(queryIndex + 1)
                : value.substring(queryIndex + 1, fragmentIndex);
        String fragment = fragmentIndex < 0 ? "" : value.substring(fragmentIndex);
        List<String> sanitized = new ArrayList<>();
        int redacted = 0;
        Set<String> rules = new TreeSet<>();
        for (String pair : query.split("&", -1)) {
            String[] parts = pair.split("=", 2);
            String encodedName = parts[0];
            String name = decode(encodedName).toLowerCase(Locale.ROOT);
            String rawParameterValue = parts.length == 2 ? decode(parts[1]) : "";
            if (isSensitiveName(name, policy.sensitiveUrlParameters(), "sensitive-url-parameter", rules)
                    || matchesSensitivePattern(rawParameterValue, rules)) {
                String reference = "[data:" + safeIdentifier(name) + "]";
                sanitized.add(encodedName + "=" + encode(reference));
                redacted++;
            } else {
                sanitized.add(pair);
            }
        }
        SanitizedText patternSafe = redactPatternMatches(prefix + String.join("&", sanitized) + fragment, true);
        rules.addAll(patternSafe.summary().appliedRules());
        return new SanitizedText(patternSafe.value(),
                new RedactionSummary(0, 0,
                        redacted + patternSafe.summary().redactedUrlParameterCount(), rules));
    }

    /**
     * Removes configured sensitive attributes and redacts secret-looking values.
     *
     * @param attributes raw element attributes
     * @return sanitized attributes and safe summary
     */
    public SanitizedAttributes sanitizeAttributes(Map<String, String> attributes) {
        if (attributes == null || attributes.isEmpty()) {
            return new SanitizedAttributes(Map.of(), RedactionSummary.empty());
        }
        Map<String, String> sanitized = new TreeMap<>();
        Set<String> rules = new TreeSet<>();
        int removed = 0;
        int redactedValues = 0;
        for (Map.Entry<String, String> entry : attributes.entrySet()) {
            String name = entry.getKey() == null ? "" : entry.getKey().trim().toLowerCase(Locale.ROOT);
            if (isSensitiveName(name, policy.sensitiveAttributeNames(), "sensitive-attribute", rules)) {
                removed++;
                continue;
            }
            SanitizedText value = redactPatternMatches(entry.getValue(), false);
            if (!value.summary().appliedRules().isEmpty()) {
                redactedValues++;
                rules.addAll(value.summary().appliedRules());
            }
            sanitized.put(name, value.value());
        }
        return new SanitizedAttributes(Map.copyOf(sanitized),
                new RedactionSummary(redactedValues, removed, 0, rules));
    }

    /**
     * Redacts configured secret-looking patterns from browser metadata text.
     *
     * @param rawValue original text
     * @return sanitized text and safe summary
     */
    public SanitizedText sanitizeText(String rawValue) {
        return redactPatternMatches(rawValue, false);
    }

    /**
     * Produces a safe basename for upload or evidence artifacts.
     *
     * @param sourcePath original path or filename
     * @return sanitized basename without secret-looking content
     */
    public String sanitizeArtifactFilename(String sourcePath) {
        String fileName;
        try {
            fileName = Path.of(sourcePath == null ? "" : sourcePath).getFileName().toString();
        } catch (RuntimeException exception) {
            fileName = "artifact.bin";
        }
        if (fileName.isBlank()) {
            fileName = "artifact.bin";
        }
        String extension = "";
        int extensionIndex = fileName.lastIndexOf('.');
        if (extensionIndex > 0 && extensionIndex < fileName.length() - 1) {
            extension = fileName.substring(extensionIndex).replaceAll("[^A-Za-z0-9.]", "");
        }
        String normalizedName = fileName.toLowerCase(Locale.ROOT);
        if (matchesSensitivePattern(fileName, new TreeSet<>())
                || policy.sensitiveFieldNames().stream().anyMatch(normalizedName::contains)) {
            return "redacted-artifact" + extension;
        }
        String safe = fileName.replaceAll("[^A-Za-z0-9._-]", "-").replaceAll("-+", "-");
        return safe.isBlank() ? "artifact.bin" : safe;
    }

    private SanitizedText redactPatternMatches(String rawValue, boolean url) {
        String sanitized = rawValue == null ? "" : rawValue;
        Set<String> rules = new TreeSet<>();
        for (Pattern pattern : sensitivePatterns) {
            String updated = pattern.matcher(sanitized).replaceAll(Redactor.REPLACEMENT);
            if (!updated.equals(sanitized)) {
                sanitized = updated;
                rules.add("sensitive-value-pattern");
            }
        }
        return new SanitizedText(sanitized,
                new RedactionSummary(rules.isEmpty() ? 0 : 1, 0, url && !rules.isEmpty() ? 1 : 0, rules));
    }

    private boolean isSensitiveSelector(String selector, Set<String> rules) {
        String normalized = selector == null ? "" : selector.trim().toLowerCase(Locale.ROOT);
        boolean matched = policy.sensitiveSelectors().stream()
                .map(value -> value.toLowerCase(Locale.ROOT))
                .anyMatch(value -> !value.isBlank() && normalized.contains(value))
                || policy.sensitiveFieldNames().stream().anyMatch(normalized::contains);
        if (matched) {
            rules.add("sensitive-selector");
        }
        return matched;
    }

    private boolean containsSensitiveAttribute(Map<String, String> attributes, Set<String> rules) {
        if (attributes == null) {
            return false;
        }
        for (Map.Entry<String, String> entry : attributes.entrySet()) {
            String name = entry.getKey() == null ? "" : entry.getKey().trim().toLowerCase(Locale.ROOT);
            boolean sensitiveValue = Set.of("type", "name", "id", "autocomplete")
                    .contains(name)
                    && isSensitiveName(entry.getValue(), policy.sensitiveFieldNames(),
                    "sensitive-attribute-value", rules);
            if (isSensitiveName(name, policy.sensitiveAttributeNames(), "sensitive-attribute", rules)
                    || sensitiveValue
                    || matchesSensitivePattern(entry.getValue(), rules)) {
                return true;
            }
        }
        return false;
    }

    private boolean matchesSensitivePattern(String value, Set<String> rules) {
        String candidate = value == null ? "" : value;
        boolean matched = sensitivePatterns.stream().anyMatch(pattern -> pattern.matcher(candidate).find());
        if (matched) {
            rules.add("sensitive-value-pattern");
        }
        return matched;
    }

    private static boolean isSensitiveName(
            String value,
            Set<String> configured,
            String rule,
            Set<String> rules) {
        String normalized = value == null
                ? ""
                : value.trim().toLowerCase(Locale.ROOT).replace('_', '-');
        boolean matched = configured.stream().anyMatch(name ->
                normalized.equals(name) || normalized.startsWith(name + "-") || normalized.endsWith("-" + name));
        if (matched) {
            rules.add(rule);
        }
        return matched;
    }

    private static String safeIdentifier(String value) {
        String normalized = value == null
                ? "value"
                : value.trim().toLowerCase(Locale.ROOT)
                .replaceAll("[^a-z0-9]+", "-")
                .replaceAll("^-|-$", "");
        return normalized.isBlank() ? "value" : normalized;
    }

    private static String escapePointer(String value) {
        return value.replace("~", "~0").replace("/", "~1");
    }

    private static String decode(String value) {
        try {
            return URLDecoder.decode(value, StandardCharsets.UTF_8);
        } catch (IllegalArgumentException ignored) {
            return value;
        }
    }

    private static String encode(String value) {
        return URLEncoder.encode(value, StandardCharsets.UTF_8).replace("+", "%20");
    }

    private static String normalizeMediaType(String value) {
        String normalized = value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
        return normalized.matches("[a-z0-9!#$&^_.+-]+/[a-z0-9!#$&^_.+-]+")
                ? normalized
                : "application/octet-stream";
    }

    private static List<Pattern> compile(List<String> expressions) {
        List<Pattern> patterns = new ArrayList<>();
        for (String expression : expressions) {
            if (expression == null || expression.isBlank()) {
                continue;
            }
            try {
                patterns.add(Pattern.compile(expression));
            } catch (PatternSyntaxException exception) {
                throw new IllegalArgumentException("Capture privacy policy contains an invalid value pattern.",
                        exception);
            }
        }
        return List.copyOf(patterns);
    }

    /**
     * Sanitized text and safe redaction summary.
     *
     * @param value sanitized text
     * @param summary safe summary
     */
    public record SanitizedText(String value, RedactionSummary summary) {
        /**
         * Creates a sanitized text result.
         */
        public SanitizedText {
            value = value == null ? "" : value;
            summary = summary == null ? RedactionSummary.empty() : summary;
        }
    }

    /**
     * Sanitized attributes and safe redaction summary.
     *
     * @param attributes sanitized attributes
     * @param summary safe summary
     */
    public record SanitizedAttributes(Map<String, String> attributes, RedactionSummary summary) {
        /**
         * Creates a sanitized attribute result.
         */
        public SanitizedAttributes {
            attributes = attributes == null
                    ? Map.of()
                    : Map.copyOf(new LinkedHashMap<>(attributes));
            summary = summary == null ? RedactionSummary.empty() : summary;
        }
    }

    /**
     * Safe upload reference and metadata.
     *
     * @param reference logical upload reference
     * @param safeFileName sanitized basename
     * @param mediaType normalized media type
     * @param sizeBytes file size
     * @param summary safe summary
     */
    public record ClassifiedUpload(
            ExternalTestDataReference reference,
            String safeFileName,
            String mediaType,
            long sizeBytes,
            RedactionSummary summary) {
        /**
         * Creates safe upload metadata.
         */
        public ClassifiedUpload {
            if (reference == null) {
                throw new IllegalArgumentException("Classified upload requires a data reference.");
            }
            safeFileName = safeFileName == null ? "upload.bin" : safeFileName;
            mediaType = mediaType == null ? "application/octet-stream" : mediaType;
            if (sizeBytes < 0) {
                throw new IllegalArgumentException("Upload size cannot be negative.");
            }
            summary = summary == null ? RedactionSummary.empty() : summary;
        }
    }
}
