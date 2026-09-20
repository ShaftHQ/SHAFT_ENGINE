package com.shaft.doctor.analysis;

import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.EvidenceProvenance;
import com.shaft.doctor.model.Finding;
import com.shaft.doctor.model.RedactionSummary;
import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * S3-04 / #5970: optional Allure categories.json and shaft-patterns.json on top of
 * {@link DeterministicRuleEngine}.
 */
class ProjectPatternRuleLoaderTest {
    private static final Path FIXTURES = Path.of("src/test/resources/fixtures/project-rules");

    @Test
    void sc001TimeoutFixtureIsEnvironmentViaProjectRule() {
        Path categories = FIXTURES.resolve("timeout-environment/categories.json");
        DeterministicRuleEngine engine = DeterministicRuleEngine.withProjectRuleFiles(List.of(categories));

        Diagnosis diagnosis = engine.diagnose(
                bundle(List.of(failure("e-timeout", "test-grid", "A grid timeout waiting for node allocation"))),
                List.of());

        assertEquals(CauseCategory.ENVIRONMENT_CONFIGURATION, diagnosis.primaryCause(), diagnosis.toString());
        assertTrue(diagnosis.findings().stream().anyMatch(finding ->
                        finding.ruleId().startsWith("project-environment")),
                diagnosis.findings().toString());
        // Built-in timing-timeout would also match "timeout"; project rule must win as primary.
        assertFalse(diagnosis.primaryCause() == CauseCategory.TIMING_SYNCHRONIZATION);
    }

    @Test
    void sc002LocatorFixtureStillUsesBuiltInRules() {
        Path categories = FIXTURES.resolve("locator-builtin/categories.json");
        DeterministicRuleEngine engine = DeterministicRuleEngine.withProjectRuleFiles(List.of(categories));

        Diagnosis diagnosis = engine.diagnose(
                bundle(List.of(failure("e-loc", "test-locator",
                        "NoSuchElementException: unable to locate element"))),
                List.of());

        assertEquals(CauseCategory.LOCATOR, diagnosis.primaryCause(), diagnosis.toString());
        assertTrue(diagnosis.findings().stream().anyMatch(finding ->
                        "locator-not-found".equals(finding.ruleId())),
                diagnosis.findings().toString());
    }

    @Test
    void fr002InvalidRegexFailsClosedWithoutCrash() {
        Path categories = FIXTURES.resolve("invalid-regex/categories.json");
        DeterministicRuleEngine engine = DeterministicRuleEngine.withProjectRuleFiles(List.of(categories));

        Diagnosis diagnosis = engine.diagnose(
                bundle(List.of(failure("e-timeout", "test-grid", "A grid timeout waiting for node allocation"))),
                List.of());

        // Invalid project rule skipped → built-in timing-timeout classifies the failure.
        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, diagnosis.primaryCause(), diagnosis.toString());
        assertTrue(diagnosis.findings().stream().anyMatch(finding ->
                        "project-pattern-fail-closed".equals(finding.ruleId())),
                diagnosis.findings().toString());
        assertTrue(diagnosis.findings().stream()
                .filter(finding -> "project-pattern-fail-closed".equals(finding.ruleId()))
                .anyMatch(finding -> finding.severity() == Finding.Severity.WARNING));
    }

    @Test
    void fr003ProjectFilesCannotRemoveBuiltInRules() {
        Path categories = FIXTURES.resolve("cannot-remove-builtin/categories.json");
        ProjectPatternRuleLoader.LoadResult loaded = ProjectPatternRuleLoader.load(List.of(categories));
        assertTrue(loaded.errors().stream().anyMatch(error -> error.contains("delete/remove")),
                loaded.errors().toString());
        // Delete directives ignored; additive ENVIRONMENT rule still compiles.
        assertTrue(loaded.hasRules(), loaded.toString());

        DeterministicRuleEngine engine = new DeterministicRuleEngine(loaded.rules(), loaded.errors());
        Diagnosis locator = engine.diagnose(
                bundle(List.of(failure("e-loc", "test-locator",
                        "NoSuchElementException: unable to locate element"))),
                List.of());
        assertEquals(CauseCategory.LOCATOR, locator.primaryCause(), locator.toString());

        Diagnosis timeout = engine.diagnose(
                bundle(List.of(failure("e-timeout", "test-grid", "A grid timeout waiting for node allocation"))),
                List.of());
        assertEquals(CauseCategory.ENVIRONMENT_CONFIGURATION, timeout.primaryCause(), timeout.toString());
    }

    @Test
    void discoverFindsCategoriesAndShaftPatternsBesideInput() {
        Path dir = FIXTURES.resolve("timeout-environment");
        List<Path> found = ProjectPatternRuleLoader.discover(List.of(dir));
        assertEquals(2, found.size(), found.toString());
        assertTrue(found.stream().anyMatch(path -> path.getFileName().toString().equals("categories.json")));
        assertTrue(found.stream().anyMatch(path -> path.getFileName().toString().equals("shaft-patterns.json")));
    }

    @Test
    void builtInEngineUnchangedWithoutProjectFiles() {
        DeterministicRuleEngine engine = new DeterministicRuleEngine();
        Diagnosis diagnosis = engine.diagnose(
                bundle(List.of(failure("e-timeout", "test-grid", "A grid timeout waiting for node allocation"))),
                List.of());
        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, diagnosis.primaryCause());
        assertEquals(Confidence.MEDIUM, diagnosis.confidence());
    }

    private static EvidenceItem failure(String id, String name, String message) {
        return new EvidenceItem(
                id,
                EvidenceCategory.ALLURE_RESULT,
                "application/json",
                "",
                "sha-" + id,
                Math.max(1, message.length()),
                message,
                false,
                false,
                Map.of(
                        "status", "failed",
                        "name", name,
                        "historyId", name,
                        "failureMessage", message,
                        "signature", "sig-" + message,
                        "start", "1",
                        "stop", "2"),
                new EvidenceProvenance("allure-result-json", "root/" + id + ".json", "sha-" + id));
    }

    private static EvidenceBundle bundle(List<EvidenceItem> evidence) {
        return new EvidenceBundle(
                EvidenceBundle.CURRENT_SCHEMA_VERSION,
                "bundle-project-rules",
                evidence,
                new RedactionSummary(List.of(), List.of(), 0),
                Map.of("minimumAllureResultCount", "1"));
    }
}
