package com.shaft.doctor.label;

import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ConfirmedCauseLabelStoreTest {
    @Test
    void secondOccurrenceSuggestsStoredEnvironment(@TempDir Path workspace) {
        ConfirmedCauseLabelModels.ConfirmResult confirmed = ConfirmedCauseLabelStore.confirm(
                workspace, "sig-grid-timeout", "ENVIRONMENT", null);
        assertEquals("ok", confirmed.status());
        assertEquals(CauseCategory.ENVIRONMENT_CONFIGURATION, confirmed.causeCategory());
        assertEquals("ENVIRONMENT", confirmed.displayAlias());
        assertFalse(confirmed.replaced());

        ConfirmedCauseLabelModels.Suggestion suggestion =
                ConfirmedCauseLabelStore.suggest(workspace, "sig-grid-timeout");
        assertTrue(suggestion.matched());
        assertEquals(CauseCategory.ENVIRONMENT_CONFIGURATION, suggestion.causeCategory());
        assertEquals("ENVIRONMENT", suggestion.displayAlias());
    }

    @Test
    void overrideReplacesStoredLabel(@TempDir Path workspace) {
        ConfirmedCauseLabelStore.confirm(workspace, "sig-login", "LOCATOR", null);
        ConfirmedCauseLabelModels.ConfirmResult overridden = ConfirmedCauseLabelStore.confirm(
                workspace, "sig-login", "TIMING", "/tmp/allure-results/foo-result.json");
        assertEquals("ok", overridden.status());
        assertTrue(overridden.replaced());
        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, overridden.causeCategory());
        assertTrue(overridden.warnings().stream()
                .anyMatch(warning -> warning.toLowerCase().contains("evidence path")));

        String raw = read(workspace.resolve(ConfirmedCauseLabelStore.RELATIVE));
        assertFalse(raw.contains("allure-results"));
        assertFalse(raw.contains("/tmp/"));
        assertTrue(raw.contains("TIMING_SYNCHRONIZATION"));

        ConfirmedCauseLabelModels.Suggestion suggestion =
                ConfirmedCauseLabelStore.suggest(workspace, "sig-login");
        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, suggestion.causeCategory());
        assertEquals("TIMING", suggestion.displayAlias());
    }

    @Test
    void applySuggestionAnnotatesDiagnosisWithoutReplacingPrimary(@TempDir Path workspace) {
        ConfirmedCauseLabelStore.confirm(workspace, "sig-product", "PRODUCT", null);
        Diagnosis original = new Diagnosis(
                Diagnosis.CURRENT_SCHEMA_VERSION,
                CauseCategory.LOCATOR,
                List.of(),
                Confidence.MEDIUM,
                "Locator mismatch.",
                "Deterministic locator rule matched.",
                List.of(),
                List.of(),
                List.of(),
                List.of());
        ConfirmedCauseLabelModels.Suggestion suggestion =
                ConfirmedCauseLabelStore.suggest(workspace, "sig-product");
        Diagnosis annotated = ConfirmedCauseLabelStore.applySuggestion(original, suggestion);
        assertEquals(CauseCategory.LOCATOR, annotated.primaryCause());
        assertTrue(annotated.summary().contains("Suggested confirmed cause: PRODUCT"));
        assertEquals(CauseCategory.PRODUCT, annotated.rankedCauses().getFirst().category());
        assertEquals("confirmed-label-suggestion", annotated.findings().getFirst().id());
    }

    @Test
    void storePathIsGitignoredRelative() throws Exception {
        Path rootIgnore = Files.isRegularFile(Path.of(".gitignore"))
                ? Path.of(".gitignore")
                : Path.of("..", ".gitignore");
        assertTrue(Files.isRegularFile(rootIgnore), "Expected repo .gitignore at " + rootIgnore.toAbsolutePath());
        String ignore = Files.readString(rootIgnore.toAbsolutePath().normalize(), StandardCharsets.UTF_8);
        assertTrue(ignore.contains(ConfirmedCauseLabelStore.RELATIVE),
                "Store path must be gitignored: " + ConfirmedCauseLabelStore.RELATIVE);
    }

    @Test
    void evidenceLookingSignatureIsRejected(@TempDir Path workspace) {
        ConfirmedCauseLabelModels.ConfirmResult refused = ConfirmedCauseLabelStore.confirm(
                workspace,
                "/home/user/project/allure-results/abc-result.json",
                "PRODUCT",
                null);
        assertEquals("error", refused.status());
        assertFalse(Files.exists(workspace.resolve(ConfirmedCauseLabelStore.RELATIVE)));
    }

    private static String read(Path path) {
        try {
            return Files.readString(path, StandardCharsets.UTF_8);
        } catch (Exception exception) {
            throw new AssertionError(exception);
        }
    }
}
