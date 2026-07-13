package com.shaft.doctor.analysis;

import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.EvidenceProvenance;
import com.shaft.doctor.model.Finding;
import com.shaft.doctor.model.RankedCause;
import com.shaft.doctor.model.RedactionSummary;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verifies deterministic ranked-cause computation added on top of the existing
 * primary/contributing-cause rule engine: multi-match ordering, per-category dedupe,
 * the disjoint-high-confidence bailout, fix-prompt content, and the schema-version bump.
 */
class DeterministicRuleEngineTest {
    private static final DeterministicRuleEngine ENGINE = new DeterministicRuleEngine();

    @Test
    void rankedCausesAreOrderedByTrustAndDedupedByCategory() {
        EvidenceItem interactionAndAssertion = failure(
                "e-1", "test-blockedClick",
                "ElementClickInterceptedException: element is not clickable at point. "
                        + "AssertionError: expected: true but was: false");
        EvidenceItem timeout = failure("e-2", "test-timeout",
                "TimeoutException: condition failed to be met");
        EvidenceItem staleReference = failure("e-3", "test-staleReference",
                "StaleElementReferenceException: stale element reference");

        Diagnosis diagnosis = ENGINE.diagnose(
                bundle(List.of(interactionAndAssertion, timeout, staleReference)), List.of());

        List<RankedCause> ranked = diagnosis.rankedCauses();
        assertTrue(ranked.size() >= 3, ranked.toString());
        // Trust is strictly non-increasing down the list.
        for (int index = 1; index < ranked.size(); index++) {
            assertTrue(ranked.get(index - 1).trustPercentage() >= ranked.get(index).trustPercentage(),
                    ranked.toString());
        }
        // Every trust value stays within the deterministic-humility bounds.
        ranked.forEach(cause -> assertTrue(cause.trustPercentage() >= 5 && cause.trustPercentage() <= 95,
                cause.toString()));
        // Per-category dedupe: LOCATOR is represented once, by the higher-scoring
        // locator-interaction match rather than the lower-scoring locator-stale match.
        long locatorEntries = ranked.stream().filter(cause -> cause.category() == CauseCategory.LOCATOR).count();
        assertEquals(1, locatorEntries, ranked.toString());
        RankedCause locator = ranked.stream()
                .filter(cause -> cause.category() == CauseCategory.LOCATOR)
                .findFirst().orElseThrow();
        assertTrue(locator.evidenceIds().contains("e-1"), locator.toString());
        assertTrue(ranked.stream().anyMatch(cause -> cause.category() == CauseCategory.TEST));
        assertTrue(ranked.stream().anyMatch(cause -> cause.category() == CauseCategory.TIMING_SYNCHRONIZATION));
    }

    @Test
    void disjointHighConfidenceBailoutStillPopulatesRankedCauses() {
        EvidenceItem locatorFailure = failure("e-1", "test-locator",
                "NoSuchElementException: unable to locate element");
        EvidenceItem dataFailure = failure("e-2", "test-data",
                "Test data mismatch: invalid test data");

        Diagnosis diagnosis = ENGINE.diagnose(bundle(List.of(locatorFailure, dataFailure)), List.of());

        assertEquals(CauseCategory.UNKNOWN, diagnosis.primaryCause());
        assertEquals(Confidence.LOW, diagnosis.confidence());
        assertTrue(diagnosis.contributingCauses().containsAll(List.of(CauseCategory.DATA, CauseCategory.LOCATOR)),
                diagnosis.contributingCauses().toString());

        List<RankedCause> ranked = diagnosis.rankedCauses();
        assertEquals(2, ranked.size(), ranked.toString());
        assertTrue(ranked.stream().anyMatch(cause -> cause.category() == CauseCategory.LOCATOR));
        assertTrue(ranked.stream().anyMatch(cause -> cause.category() == CauseCategory.DATA));
        // Disjoint competing HIGH causes both take the contradiction penalty, so neither
        // one dominates the way an uncontested single cause would.
        ranked.forEach(cause -> assertTrue(cause.trustPercentage() < 90, cause.toString()));
    }

    @Test
    void fixPromptCitesTestMethodAndRecommendedAction() {
        EvidenceItem interaction = failure("e-1", "example.Test.blockedClick",
                "ElementClickInterceptedException: element is obscured");

        Diagnosis diagnosis = ENGINE.diagnose(bundle(List.of(interaction)), List.of());

        RankedCause locator = diagnosis.rankedCauses().stream()
                .filter(cause -> cause.category() == CauseCategory.LOCATOR)
                .findFirst().orElseThrow();
        assertTrue(locator.fixPrompt().contains("example.Test.blockedClick"), locator.fixPrompt());
        assertTrue(locator.fixPrompt().contains(
                "Capture the blocking page state, wait for interactability, "
                        + "and avoid bypassing the user-visible condition."),
                locator.fixPrompt());
    }

    @Test
    void schemaVersionWasBumpedForTheAdditiveRankedCausesField() {
        EvidenceItem locatorFailure = failure("e-1", "test-locator",
                "NoSuchElementException: unable to locate element");

        Diagnosis diagnosis = ENGINE.diagnose(bundle(List.of(locatorFailure)), List.of());

        assertEquals("1.1", Diagnosis.CURRENT_SCHEMA_VERSION);
        assertEquals(Diagnosis.CURRENT_SCHEMA_VERSION, diagnosis.schemaVersion());
    }

    @Test
    void noMatchedRuleLeavesRankedCausesEmpty() {
        EvidenceItem unclassified = failure("e-1", "test-unclassified", "Unclassified synthetic failure");

        Diagnosis diagnosis = ENGINE.diagnose(bundle(List.of(unclassified)), List.of());

        assertEquals(CauseCategory.UNKNOWN, diagnosis.primaryCause());
        assertTrue(diagnosis.rankedCauses().isEmpty(), diagnosis.rankedCauses().toString());
    }

    @Test
    void retryHiddenFailureWithNoDirectFailuresYieldsTimingSynchronizationDiagnosis() {
        // A "skipped" attempt followed by a "passed" final attempt: neither status is in
        // FAILURE_STATUSES, so `failures` is empty, but the retry group still hid a non-final
        // issue -- this must surface as a TIMING_SYNCHRONIZATION diagnosis rather than UNKNOWN.
        EvidenceItem skippedAttempt = attempt("e-1", "hist-hidden", "skipped", "test-hidden", "", 100);
        EvidenceItem passedFinal = attempt("e-2", "hist-hidden", "passed", "test-hidden", "", 200);

        Diagnosis diagnosis = ENGINE.diagnose(bundle(List.of(skippedAttempt, passedFinal)), List.of());

        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, diagnosis.primaryCause());
        assertEquals(Confidence.MEDIUM, diagnosis.confidence());
    }

    @Test
    void retryFindingsCoverGroupSizeAndOutcomeCombinations() {
        EvidenceItem singleAttempt = failure("e-single", "test-single", "Unclassified single attempt failure");
        EvidenceItem bothPassedFirst = attempt("e-passed-1", "hist-both-passed", "passed",
                "test-both-passed", "", 100);
        EvidenceItem bothPassedSecond = attempt("e-passed-2", "hist-both-passed", "passed",
                "test-both-passed", "", 200);
        EvidenceItem stillFailingFirst = attempt("e-fail-1", "hist-still-failing", "failed",
                "test-still-failing", "Unclassified still failing", 100);
        EvidenceItem stillFailingSecond = attempt("e-fail-2", "hist-still-failing", "failed",
                "test-still-failing", "Unclassified still failing", 200);
        EvidenceItem hiddenFirst = attempt("e-hidden-1", "hist-hidden-2", "skipped", "test-hidden-2", "", 100);
        EvidenceItem hiddenSecond = attempt("e-hidden-2", "hist-hidden-2", "passed", "test-hidden-2", "", 200);

        Diagnosis diagnosis = ENGINE.diagnose(bundle(List.of(
                singleAttempt, bothPassedFirst, bothPassedSecond,
                stillFailingFirst, stillFailingSecond, hiddenFirst, hiddenSecond)), List.of());

        List<String> retryFindingRuleIds = diagnosis.findings().stream()
                .filter(finding -> "retry-correlation".equals(finding.ruleId()))
                .map(Finding::id)
                .toList();
        assertEquals(1, retryFindingRuleIds.size(),
                "Only the skipped-then-passed group is a hidden-failure retry: " + diagnosis.findings());
    }

    @Test
    void historicalSignatureFindingIsAddedWhenAFailureRepeatsAndNullHistoryIsHandled() {
        EvidenceItem currentFailure = failure("e-1", "test-recurrent",
                "NoSuchElementException: unable to locate element");

        // A null history list must be tolerated (the "history == null ? List.of() : history" branch).
        Diagnosis withNullHistory = assertDoesNotThrow(() -> ENGINE.diagnose(bundle(List.of(currentFailure)), null));
        assertTrue(withNullHistory.findings().stream()
                .noneMatch(finding -> "historical-signature-correlation".equals(finding.ruleId())));

        EvidenceItem historicalFailure = failure("e-old-1", "test-recurrent-old",
                "NoSuchElementException: unable to locate element");
        EvidenceBundle historicalBundle = bundle(List.of(historicalFailure));

        Diagnosis withHistory = ENGINE.diagnose(bundle(List.of(currentFailure)), List.of(historicalBundle));

        assertTrue(withHistory.findings().stream()
                .anyMatch(finding -> "historical-signature-correlation".equals(finding.ruleId())));
    }

    @Test
    void accessibilityFindingsMapSeverityFromViolationCountsAndSkipZeroViolationAudits() {
        EvidenceItem passedAllure = passedAllure("e-passed", "test-ok");
        EvidenceItem healthyAudit = accessibilityAudit("e-a0", "HealthyPage", 0, 0, 0, 0, 0, "");
        EvidenceItem criticalAudit = accessibilityAudit("e-a1", "CriticalPage", 1, 1, 0, 0, 0, "");
        EvidenceItem moderateAudit = accessibilityAudit("e-a2", "ModeratePage", 1, 0, 0, 1, 0, "moderate-rule");
        EvidenceItem minorAudit = accessibilityAudit("e-a3", "MinorPage", 1, 0, 0, 0, 1, "");

        Diagnosis diagnosis = ENGINE.diagnose(bundle(
                List.of(passedAllure, healthyAudit, criticalAudit, moderateAudit, minorAudit)), List.of());

        List<Finding> accessibilityFindings = diagnosis.findings().stream()
                .filter(finding -> "accessibility-audit".equals(finding.ruleId()))
                .toList();
        assertEquals(3, accessibilityFindings.size(),
                "The zero-violation audit must be skipped: " + diagnosis.findings());
        assertTrue(accessibilityFindings.stream().anyMatch(finding ->
                finding.severity() == Finding.Severity.ERROR && finding.detail().contains("CriticalPage")));
        assertTrue(accessibilityFindings.stream().anyMatch(finding ->
                finding.severity() == Finding.Severity.WARNING && finding.detail().contains("moderate-rule")));
        assertTrue(accessibilityFindings.stream().anyMatch(finding ->
                finding.severity() == Finding.Severity.INFO && finding.detail().contains("MinorPage")));
    }

    @Test
    void locatorStaleAloneAppendsTimingSynchronizationAsAContributor() {
        EvidenceItem staleFailure = failure("e-1", "test-stale-alone",
                "StaleElementReferenceException: stale element reference");

        Diagnosis diagnosis = ENGINE.diagnose(bundle(List.of(staleFailure)), List.of());

        assertEquals(CauseCategory.LOCATOR, diagnosis.primaryCause());
        assertEquals(List.of(CauseCategory.TIMING_SYNCHRONIZATION), diagnosis.contributingCauses());
    }

    @Test
    void locatorStaleContributorIsNotDuplicatedWhenTimingAlreadyContributes() {
        EvidenceItem staleFailure = failure("e-1", "test-stale",
                "StaleElementReferenceException: stale element reference");
        EvidenceItem timeoutFailure = failure("e-2", "test-timeout", "TimeoutException: timed out");

        Diagnosis diagnosis = ENGINE.diagnose(bundle(List.of(staleFailure, timeoutFailure)), List.of());

        assertEquals(CauseCategory.LOCATOR, diagnosis.primaryCause());
        assertEquals(List.of(CauseCategory.TIMING_SYNCHRONIZATION), diagnosis.contributingCauses());
    }

    @Test
    void retryHiddenFailureAppendsTimingSynchronizationWhenPrimaryIsNotTimingRelated() {
        EvidenceItem locatorFailure = failure("e-1", "test-locator",
                "NoSuchElementException: unable to locate element");
        EvidenceItem hiddenFirst = attempt("e-hidden-1", "hist-hidden", "skipped", "test-hidden", "", 100);
        EvidenceItem hiddenSecond = attempt("e-hidden-2", "hist-hidden", "passed", "test-hidden", "", 200);

        Diagnosis diagnosis = ENGINE.diagnose(
                bundle(List.of(locatorFailure, hiddenFirst, hiddenSecond)), List.of());

        assertEquals(CauseCategory.LOCATOR, diagnosis.primaryCause());
        assertEquals(List.of(CauseCategory.TIMING_SYNCHRONIZATION), diagnosis.contributingCauses());
    }

    @Test
    void retryHiddenFailureDoesNotDuplicateWhenPrimaryIsAlreadyTimingSynchronization() {
        EvidenceItem timeoutFailure = failure("e-1", "test-timeout", "TimeoutException: timed out");
        EvidenceItem hiddenFirst = attempt("e-hidden-1", "hist-hidden", "skipped", "test-hidden", "", 100);
        EvidenceItem hiddenSecond = attempt("e-hidden-2", "hist-hidden", "passed", "test-hidden", "", 200);

        Diagnosis diagnosis = ENGINE.diagnose(
                bundle(List.of(timeoutFailure, hiddenFirst, hiddenSecond)), List.of());

        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION, diagnosis.primaryCause());
        assertTrue(diagnosis.contributingCauses().isEmpty(), diagnosis.contributingCauses().toString());
    }

    @Test
    void fixPromptListsEveryCitedEvidenceItemAndTruncatesLongMessages() {
        String longMessage = "ElementClickInterceptedException: " + "E".repeat(250);
        EvidenceItem first = failure("e-1", "test-first", "ElementClickInterceptedException: element is obscured");
        EvidenceItem second = failure("e-2", "test-second", longMessage);

        Diagnosis diagnosis = ENGINE.diagnose(bundle(List.of(first, second)), List.of());

        RankedCause locator = diagnosis.rankedCauses().stream()
                .filter(cause -> cause.category() == CauseCategory.LOCATOR)
                .findFirst().orElseThrow();
        assertTrue(locator.fixPrompt().contains("test-first"), locator.fixPrompt());
        assertTrue(locator.fixPrompt().contains("test-second"), locator.fixPrompt());
        assertTrue(locator.fixPrompt().contains("…"), "A message over 200 characters must be ellipsis-truncated");
    }

    @Test
    void nonNumericMinimumAllureResultCountMetadataFallsBackToTheDefault() {
        EvidenceItem passedAllure = passedAllure("e-1", "test-ok");
        EvidenceBundle bundleWithBadMetadata = new EvidenceBundle(
                EvidenceBundle.CURRENT_SCHEMA_VERSION,
                "bundle-test",
                List.of(passedAllure),
                new RedactionSummary(List.of(), List.of(), 0),
                Map.of("minimumAllureResultCount", "not-a-number"));

        Diagnosis diagnosis = assertDoesNotThrow(() -> ENGINE.diagnose(bundleWithBadMetadata, List.of()));

        assertFalse(diagnosis.summary().contains("incomplete"),
                "A non-numeric minimum count must fall back to the default of 1, not treat evidence as incomplete: "
                        + diagnosis.summary());
    }

    private static EvidenceItem failure(String id, String name, String message) {
        return attempt(id, name, "failed", name, message, 1);
    }

    private static EvidenceItem attempt(
            String id, String historyId, String status, String name, String message, long start) {
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
                        "status", status,
                        "name", name,
                        "historyId", historyId,
                        "failureMessage", message,
                        "signature", message.isBlank() ? "" : "sig-" + message,
                        "start", Long.toString(start),
                        "stop", Long.toString(start + 1)),
                new EvidenceProvenance("allure-result-json", "root/" + id + ".json", "sha-" + id));
    }

    private static EvidenceItem passedAllure(String id, String name) {
        return attempt(id, name, "passed", name, "", 1);
    }

    private static EvidenceItem accessibilityAudit(
            String id,
            String pageName,
            int violations,
            int critical,
            int serious,
            int moderate,
            int minor,
            String topRuleIds) {
        Map<String, String> attributes = new java.util.TreeMap<>();
        attributes.put("pageName", pageName);
        attributes.put("violationsCount", Integer.toString(violations));
        attributes.put("criticalCount", Integer.toString(critical));
        attributes.put("seriousCount", Integer.toString(serious));
        attributes.put("moderateCount", Integer.toString(moderate));
        attributes.put("minorCount", Integer.toString(minor));
        if (!topRuleIds.isBlank()) {
            attributes.put("topRuleIds", topRuleIds);
        }
        return new EvidenceItem(
                id,
                EvidenceCategory.ACCESSIBILITY_AUDIT,
                "application/json",
                "",
                "sha-" + id,
                10,
                "{}",
                false,
                false,
                attributes,
                new EvidenceProvenance("accessibility-audit-json", "root/" + id + ".json", "sha-" + id));
    }

    private static EvidenceBundle bundle(List<EvidenceItem> evidence) {
        return new EvidenceBundle(
                EvidenceBundle.CURRENT_SCHEMA_VERSION,
                "bundle-test",
                evidence,
                new RedactionSummary(List.of(), List.of(), 0),
                Map.of("minimumAllureResultCount", "1"));
    }
}
