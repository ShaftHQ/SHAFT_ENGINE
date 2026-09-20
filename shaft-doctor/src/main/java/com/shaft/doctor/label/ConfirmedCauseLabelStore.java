package com.shaft.doctor.label;

import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.Finding;
import com.shaft.doctor.model.RankedCause;
import tools.jackson.databind.DeserializationFeature;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.SerializationFeature;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;

/**
 * Gitignored local workspace store for user-confirmed Doctor cause labels (issue #5971 / S3-05).
 *
 * <p>Keys are Doctor historical-signature strings. Values are CauseCategory values overlapping
 * ReportPortal / BrowserStack defect buckets. Evidence paths are never persisted.
 */
public final class ConfirmedCauseLabelStore {
    /**
     * Workspace-relative store path (must remain gitignored).
     */
    public static final String RELATIVE = ".shaft/doctor-confirmed-labels.json";

    private static final ObjectMapper JSON = JsonMapper.builder()
            .enable(SerializationFeature.INDENT_OUTPUT, SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS)
            .disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
            .build();

    private static final Pattern EVIDENCE_PATH = Pattern.compile(
            "(?i)(^|[\\s\"'=])((?:[A-Za-z]:)?[\\\\/]?(?:Users|home|var|tmp|temp|allure-results|target)"
                    + "[^\\s\"']*|[^\\s\"']+\\.(?:json|png|html|txt|log|zip))");

    private ConfirmedCauseLabelStore() {
    }

    /**
     * Resolves the store file under a workspace root.
     *
     * @param workspaceRoot project / MCP workspace root
     * @return absolute store path
     */
    public static Path storePath(Path workspaceRoot) {
        if (workspaceRoot == null) {
            throw new IllegalArgumentException("Workspace root is required for confirmed cause labels.");
        }
        return workspaceRoot.toAbsolutePath().normalize().resolve(RELATIVE);
    }

    /**
     * Looks up the last confirmed cause for a signature.
     *
     * @param workspaceRoot workspace root containing {@link #RELATIVE}
     * @param signature Doctor historical-signature key
     * @return suggestion (matched or not)
     */
    public static ConfirmedCauseLabelModels.Suggestion suggest(Path workspaceRoot, String signature) {
        Path path = storePath(workspaceRoot);
        String key = normalizeSignature(signature);
        if (key.isBlank()) {
            return new ConfirmedCauseLabelModels.Suggestion(
                    ConfirmedCauseLabelModels.Suggestion.CURRENT_SCHEMA_VERSION,
                    false,
                    "",
                    CauseCategory.UNKNOWN,
                    "",
                    "Signature is blank; cannot suggest a confirmed defect label.",
                    path.toString());
        }
        Optional<ConfirmedCauseLabelModels.LabelEntry> entry = read(path).labels().entrySet().stream()
                .filter(e -> key.equals(e.getKey()))
                .map(Map.Entry::getValue)
                .findFirst();
        if (entry.isEmpty()) {
            return ConfirmedCauseLabelModels.Suggestion.none(path.toString());
        }
        ConfirmedCauseLabelModels.LabelEntry label = entry.get();
        return new ConfirmedCauseLabelModels.Suggestion(
                ConfirmedCauseLabelModels.Suggestion.CURRENT_SCHEMA_VERSION,
                true,
                key,
                label.causeCategory(),
                label.displayAlias(),
                "Suggested confirmed cause " + label.displayAlias()
                        + " from prior user confirmation for this signature.",
                path.toString());
    }

    /**
     * Persists a confirmed cause category for a signature. Override replaces any prior label.
     *
     * @param workspaceRoot workspace root
     * @param signature Doctor historical-signature key
     * @param causeToken user-facing or enum token (PRODUCT/TEST/ENVIRONMENT/LOCATOR/TIMING/…)
     * @param evidencePathIgnored optional evidence path — redacted / never stored (FR-003)
     * @return confirm result
     */
    public static ConfirmedCauseLabelModels.ConfirmResult confirm(
            Path workspaceRoot,
            String signature,
            String causeToken,
            String evidencePathIgnored) {
        Path path = storePath(workspaceRoot);
        List<String> warnings = new ArrayList<>();
        if (evidencePathIgnored != null && !evidencePathIgnored.isBlank()) {
            warnings.add("Evidence path was ignored and not written to the confirmed-label store.");
        }
        String key = normalizeSignature(signature);
        if (key.isBlank()) {
            return new ConfirmedCauseLabelModels.ConfirmResult(
                    ConfirmedCauseLabelModels.ConfirmResult.CURRENT_SCHEMA_VERSION,
                    "error",
                    "Signature is blank; confirmed defect label was not persisted.",
                    "",
                    CauseCategory.UNKNOWN,
                    "",
                    false,
                    path.toString(),
                    warnings);
        }
        if (looksLikeEvidencePath(key)) {
            return new ConfirmedCauseLabelModels.ConfirmResult(
                    ConfirmedCauseLabelModels.ConfirmResult.CURRENT_SCHEMA_VERSION,
                    "error",
                    "Signature looks like an evidence path; refused to persist (FR-003).",
                    "",
                    CauseCategory.UNKNOWN,
                    "",
                    false,
                    path.toString(),
                    warnings);
        }
        CauseCategory category;
        try {
            category = parseConfirmableCategory(causeToken);
        } catch (IllegalArgumentException exception) {
            return new ConfirmedCauseLabelModels.ConfirmResult(
                    ConfirmedCauseLabelModels.ConfirmResult.CURRENT_SCHEMA_VERSION,
                    "error",
                    exception.getMessage(),
                    key,
                    CauseCategory.UNKNOWN,
                    "",
                    false,
                    path.toString(),
                    warnings);
        }
        String alias = displayAlias(category);
        ConfirmedCauseLabelModels.StoreDocument current = read(path);
        boolean replaced = current.labels().containsKey(key);
        Map<String, ConfirmedCauseLabelModels.LabelEntry> next = new LinkedHashMap<>(current.labels());
        next.put(key, new ConfirmedCauseLabelModels.LabelEntry(category, alias, Instant.now().toString()));
        write(path, new ConfirmedCauseLabelModels.StoreDocument(
                ConfirmedCauseLabelModels.StoreDocument.CURRENT_SCHEMA_VERSION, next));
        return new ConfirmedCauseLabelModels.ConfirmResult(
                ConfirmedCauseLabelModels.ConfirmResult.CURRENT_SCHEMA_VERSION,
                "ok",
                replaced
                        ? "Overrode confirmed cause " + alias + " for signature."
                        : "Persisted confirmed cause " + alias + " for signature.",
                key,
                category,
                alias,
                replaced,
                path.toString(),
                warnings);
    }

    /**
     * Applies a matched suggestion onto a diagnosis without replacing the deterministic primary cause.
     *
     * @param diagnosis deterministic diagnosis
     * @param suggestion matched or unmatched suggestion
     * @return diagnosis with suggestion finding / ranked cause when matched; otherwise unchanged
     */
    public static Diagnosis applySuggestion(Diagnosis diagnosis, ConfirmedCauseLabelModels.Suggestion suggestion) {
        if (diagnosis == null || suggestion == null || !suggestion.matched()) {
            return diagnosis;
        }
        CauseCategory suggested = suggestion.causeCategory();
        String alias = suggestion.displayAlias().isBlank() ? displayAlias(suggested) : suggestion.displayAlias();
        Finding finding = new Finding(
                "confirmed-label-suggestion",
                Finding.Kind.INFERENCE,
                suggested,
                Finding.Severity.INFO,
                "Suggested confirmed cause " + alias,
                suggestion.message() + " Confirm or override via doctor_confirm_cause_label.",
                "confirmed-label-memory",
                List.of());
        RankedCause ranked = new RankedCause(
                suggested,
                88,
                Confidence.HIGH,
                "Previously confirmed by the user for signature " + suggestion.signature() + ".",
                List.of(),
                "Treat this failure as " + alias
                        + " based on the last confirmed label for this signature, or override.");
        List<Finding> findings = new ArrayList<>();
        findings.add(finding);
        findings.addAll(diagnosis.findings());
        List<RankedCause> rankedCauses = new ArrayList<>();
        rankedCauses.add(ranked);
        for (RankedCause existing : diagnosis.rankedCauses()) {
            if (existing.category() != suggested) {
                rankedCauses.add(existing);
            }
        }
        String summary = diagnosis.summary();
        if (!summary.contains("Suggested confirmed cause")) {
            summary = summary + " Suggested confirmed cause: " + alias + ".";
        }
        String rationale = diagnosis.rationale();
        if (!rationale.contains("confirmed-label-memory")) {
            rationale = rationale + " confirmed-label-memory suggests " + alias
                    + " from prior user confirmation.";
        }
        return new Diagnosis(
                diagnosis.schemaVersion(),
                diagnosis.primaryCause(),
                diagnosis.contributingCauses(),
                diagnosis.confidence(),
                summary,
                rationale,
                findings,
                diagnosis.remediations(),
                diagnosis.missingEvidence(),
                rankedCauses);
    }

    /**
     * Maps a confirmable user/token string onto a CauseCategory.
     *
     * @param raw PRODUCT/TEST/ENVIRONMENT/LOCATOR/TIMING or enum name
     * @return mapped category
     */
    public static CauseCategory parseConfirmableCategory(String raw) {
        if (raw == null || raw.isBlank()) {
            throw new IllegalArgumentException(
                    "Cause category is required (PRODUCT, TEST, ENVIRONMENT, LOCATOR, or TIMING).");
        }
        String token = raw.trim().toUpperCase(Locale.ROOT).replace('-', '_').replace(' ', '_');
        return switch (token) {
            case "PRODUCT" -> CauseCategory.PRODUCT;
            case "TEST" -> CauseCategory.TEST;
            case "ENVIRONMENT", "ENVIRONMENT_CONFIGURATION", "CONFIGURATION" ->
                    CauseCategory.ENVIRONMENT_CONFIGURATION;
            case "LOCATOR" -> CauseCategory.LOCATOR;
            case "TIMING", "TIMING_SYNCHRONIZATION", "SYNCHRONIZATION" ->
                    CauseCategory.TIMING_SYNCHRONIZATION;
            default -> throw new IllegalArgumentException(
                    "Unsupported confirmable cause '" + raw
                            + "'. Use PRODUCT, TEST, ENVIRONMENT, LOCATOR, or TIMING.");
        };
    }

    /**
     * Short display alias for a confirmable category.
     *
     * @param category cause category
     * @return PRODUCT/TEST/ENVIRONMENT/LOCATOR/TIMING or enum name
     */
    public static String displayAlias(CauseCategory category) {
        if (category == null) {
            return "UNKNOWN";
        }
        return switch (category) {
            case PRODUCT -> "PRODUCT";
            case TEST -> "TEST";
            case ENVIRONMENT_CONFIGURATION -> "ENVIRONMENT";
            case LOCATOR -> "LOCATOR";
            case TIMING_SYNCHRONIZATION -> "TIMING";
            default -> category.name();
        };
    }

    static String normalizeSignature(String signature) {
        if (signature == null) {
            return "";
        }
        return signature.trim();
    }

    static boolean looksLikeEvidencePath(String value) {
        if (value == null || value.isBlank()) {
            return false;
        }
        String trimmed = value.trim();
        if (trimmed.contains("://")) {
            return true;
        }
        if (trimmed.contains("/") || trimmed.contains("\\")) {
            return EVIDENCE_PATH.matcher(trimmed).find()
                    || trimmed.endsWith(".json")
                    || trimmed.endsWith(".png")
                    || trimmed.endsWith(".html")
                    || trimmed.contains("allure-results")
                    || trimmed.contains("doctor-evidence");
        }
        return false;
    }

    private static ConfirmedCauseLabelModels.StoreDocument read(Path path) {
        if (!Files.isRegularFile(path)) {
            return ConfirmedCauseLabelModels.StoreDocument.empty();
        }
        try {
            String raw = Files.readString(path, StandardCharsets.UTF_8).strip();
            if (raw.isEmpty()) {
                return ConfirmedCauseLabelModels.StoreDocument.empty();
            }
            ConfirmedCauseLabelModels.StoreDocument document =
                    JSON.readValue(raw, ConfirmedCauseLabelModels.StoreDocument.class);
            return document == null ? ConfirmedCauseLabelModels.StoreDocument.empty() : document;
        } catch (IOException | RuntimeException ignored) {
            return ConfirmedCauseLabelModels.StoreDocument.empty();
        }
    }

    private static void write(Path path, ConfirmedCauseLabelModels.StoreDocument document) {
        Path absolute = path.toAbsolutePath().normalize();
        Path temporary = null;
        try {
            Files.createDirectories(absolute.getParent());
            temporary = Files.createTempFile(absolute.getParent(), ".doctor-labels-", ".tmp");
            String payload = JSON.writerWithDefaultPrettyPrinter().writeValueAsString(document) + "\n";
            Files.writeString(temporary, payload, StandardCharsets.UTF_8);
            Files.move(temporary, absolute, StandardCopyOption.REPLACE_EXISTING);
            temporary = null;
        } catch (IOException exception) {
            throw new IllegalStateException("Confirmed defect label store could not be persisted.", exception);
        } finally {
            if (temporary != null) {
                try {
                    Files.deleteIfExists(temporary);
                } catch (IOException ignored) {
                    // Best-effort cleanup.
                }
            }
        }
    }
}
