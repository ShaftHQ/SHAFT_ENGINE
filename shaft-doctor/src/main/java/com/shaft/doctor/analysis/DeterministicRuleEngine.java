package com.shaft.doctor.analysis;

import com.shaft.doctor.internal.DoctorHashing;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.Finding;
import com.shaft.doctor.model.Remediation;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;

/**
 * Ordered explainable rules for common SHAFT failure categories.
 */
public final class DeterministicRuleEngine {
    private static final Set<String> FAILURE_STATUSES = Set.of("failed", "broken");
    private static final Set<String> NON_FINAL_ISSUE_STATUSES = Set.of("failed", "broken", "skipped");
    private static final List<Rule> RULES = List.of(
            rule("infrastructure-driver-startup", CauseCategory.ENVIRONMENT_CONFIGURATION, Confidence.HIGH,
                    text -> containsAny(text, "sessionnotcreatedexception", "session not created",
                            "unable to obtain driver", "this version of chromedriver",
                            "driver executable", "browser failed to start", "could not start a new session"),
                    "Driver or browser startup failed",
                    "Align browser, driver, and runtime configuration, then retry a minimal session."),
            rule("infrastructure-remote-mobile", CauseCategory.INFRASTRUCTURE, Confidence.HIGH,
                    text -> containsAny(text, "appium", "remote grid", "selenium grid",
                            "connection refused", "unknown host", "dns", "socketexception",
                            "connectexception", "gateway timeout", "service unavailable"),
                    "Remote automation infrastructure failed",
                    "Verify the Grid or Appium endpoint, network reachability, capacity, and service health."),
            rule("infrastructure-host-resource", CauseCategory.INFRASTRUCTURE, Confidence.HIGH,
                    text -> containsAny(text, "no space left on device", "too many open files",
                            "outofmemoryerror", "cannot allocate memory", "accessdeniedexception",
                            "permission denied", "read-only file system", "filesystemexception"),
                    "Host resource or filesystem failure",
                    "Restore disk, memory, file-handle, and permission capacity before rerunning."),
            rule("locator-not-found", CauseCategory.LOCATOR, Confidence.HIGH,
                    text -> containsAny(text, "nosuchelementexception", "no such element",
                            "unable to locate element", "element could not be found"),
                    "Locator did not resolve an element",
                    "Inspect the cited locator against the failing page state and update it only after confirming the intended element."),
            rule("locator-duplicate", CauseCategory.LOCATOR, Confidence.HIGH,
                    text -> containsAny(text, "strict mode violation", "more than one element",
                            "multiple elements", "duplicate match", "expected 1 matching element"),
                    "Locator resolved multiple elements",
                    "Make the locator uniquely identify the intended element and add a uniqueness assertion."),
            rule("locator-stale", CauseCategory.LOCATOR, Confidence.MEDIUM,
                    text -> containsAny(text, "staleelementreferenceexception", "stale element reference",
                            "element is no longer attached"),
                    "Element reference became stale",
                    "Re-resolve the element after the DOM transition and wait for the stable post-transition state."),
            rule("locator-interaction", CauseCategory.LOCATOR, Confidence.HIGH,
                    text -> containsAny(text, "elementclickinterceptedexception", "elementnotinteractableexception",
                            "not interactable", "other element would receive", "element is obscured",
                            "element is hidden", "not clickable at point", "covered by"),
                    "Element was hidden, covered, or not interactable",
                    "Capture the blocking page state, wait for interactability, and avoid bypassing the user-visible condition."),
            rule("locator-context", CauseCategory.LOCATOR, Confidence.HIGH,
                    text -> containsAny(text, "nosuchframeexception", "no such frame",
                            "nosuchwindowexception", "no such window", "target frame detached",
                            "wrong frame", "wrong window"),
                    "Browser frame or window context was incorrect",
                    "Re-establish the intended window and frame context immediately before locating the element."),
            rule("data-mismatch", CauseCategory.DATA, Confidence.HIGH,
                    text -> containsAny(text, "test data mismatch", "dataset mismatch", "csv mismatch",
                            "json data mismatch", "data provider", "missing test data",
                            "invalid test data", "fixture data"),
                    "Test data did not match the scenario",
                    "Validate the cited dataset, parameter mapping, and environment-specific data prerequisites."),
            rule("assertion-mismatch", CauseCategory.TEST, Confidence.HIGH,
                    text -> containsAny(text, "assertionerror", "expected:", "expected [",
                            "expected <", "but was", "but found", "comparisonfailure",
                            "assertion failed"),
                    "Assertion expected and actual values differed",
                    "Confirm the requirement and assertion scope, then correct the test expectation or report a product defect with cited evidence."),
            rule("timing-timeout", CauseCategory.TIMING_SYNCHRONIZATION, Confidence.MEDIUM,
                    text -> containsAny(text, "timeoutexception", "timed out", "timeout waiting",
                            "condition failed to be met", "wait timed out", "element never became"),
                    "A wait or operation timed out",
                    "Wait on the specific observable readiness condition and record timing evidence before increasing timeouts."),
            rule("test-fixture", CauseCategory.TEST, Confidence.HIGH,
                    text -> containsAny(text, "beforemethod", "beforeclass", "beforesuite",
                            "aftermethod", "afterclass", "aftersuite", "setup failed",
                            "teardown failed", "cleanup failed"),
                    "Test setup or cleanup failed",
                    "Isolate the failing fixture, make cleanup idempotent, and preserve the original test failure when teardown also fails."),
            rule("test-parallel-race", CauseCategory.TEST, Confidence.MEDIUM,
                    text -> containsAny(text, "concurrentmodificationexception", "race condition",
                            "shared state", "threadlocal", "already quit", "session id is null",
                            "parallel execution", "intermittent null"),
                    "Parallel shared state is a likely contributor",
                    "Move mutable state to method-local or thread-owned storage and clear lifecycle state after each test."),
            rule("product-http-server", CauseCategory.PRODUCT, Confidence.MEDIUM,
                    text -> containsAny(text, "http 500", "status code 500", "internal server error",
                            "bad gateway", "application error", "product defect", "server-side error"),
                    "The application under test reported a server-side failure",
                    "Correlate the failing request with application logs and reproduce outside the test harness before assigning ownership.")
    );

    /**
     * Diagnoses one current bundle with optional historical bundles.
     *
     * @param bundle current evidence
     * @param history older evidence bundles
     * @return deterministic diagnosis
     */
    public Diagnosis diagnose(EvidenceBundle bundle, List<EvidenceBundle> history) {
        List<EvidenceItem> allure = bundle.evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.ALLURE_RESULT)
                .toList();
        List<EvidenceItem> validAllure = allure.stream()
                .filter(item -> !"true".equals(item.attributes().get("invalid")))
                .toList();
        List<EvidenceItem> failures = validAllure.stream()
                .filter(item -> FAILURE_STATUSES.contains(item.attributes().get("status")))
                .toList();
        List<EvidenceItem> supplemental = bundle.evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.SHAFT_LOG
                        || item.category() == EvidenceCategory.EXCEPTION_CHAIN)
                .toList();
        List<Finding> findings = new ArrayList<>();
        List<Remediation> remediations = new ArrayList<>();
        List<String> missingEvidence = new ArrayList<>();
        int minimumResults = integer(bundle.metadata().get("minimumAllureResultCount"), 1);

        if (validAllure.size() < minimumResults) {
            List<String> ids = allure.stream().map(EvidenceItem::id).toList();
            findings.add(finding("empty-allure", Finding.Kind.OBSERVATION,
                    CauseCategory.ENVIRONMENT_CONFIGURATION, Finding.Severity.ERROR,
                    "The Allure result set is empty or smaller than expected",
                    validAllure.isEmpty() && allure.isEmpty()
                            ? "The supplied evidence contained no populated *-result.json files."
                            : "Doctor found " + validAllure.size() + " valid result(s), below the explicit minimum of "
                                    + minimumResults + ".",
                    "allure-completeness", ids));
            remediations.add(remediation("collect-allure", "Collect populated Allure results",
                    "Run the intended tests, verify that *-result.json files are populated, and analyze the result directory again.",
                    findings, ids));
            missingEvidence.add("At least one valid populated Allure test result.");
            return diagnosis(CauseCategory.ENVIRONMENT_CONFIGURATION, List.of(), Confidence.HIGH,
                    "Test execution evidence is incomplete; success cannot be inferred.",
                    "The completeness rule runs before failure classification and found no valid Allure result.",
                    findings, remediations, missingEvidence);
        }

        addAttemptObservations(failures, findings);
        RetrySummary retry = addRetryFindings(validAllure, findings, remediations);
        addHistoricalSignatureFinding(failures, history, findings);

        List<RuleMatch> matches = new ArrayList<>();
        for (Rule rule : RULES) {
            List<EvidenceItem> matched = failures.stream()
                    .filter(item -> rule.predicate().test(searchText(item)))
                    .toList();
            if (matched.isEmpty()) {
                matched = supplemental.stream()
                        .filter(item -> rule.predicate().test(searchText(item)))
                        .toList();
            }
            if (!matched.isEmpty()) {
                matches.add(new RuleMatch(rule, matched));
                findings.add(finding(rule.id(), Finding.Kind.INFERENCE, rule.category(),
                        Finding.Severity.ERROR, rule.title(),
                        "Rule " + rule.id() + " matched sanitized failure text in "
                                + matched.size() + " cited evidence item(s).",
                        rule.id(), matched.stream().map(EvidenceItem::id).toList()));
                remediations.add(new Remediation(
                        "r-" + rule.id(), rule.title(), rule.action(),
                        List.of("f-" + stableSuffix(rule.id())),
                        matched.stream().map(EvidenceItem::id).toList()));
            }
        }

        if (failures.isEmpty()) {
            if (retry.hiddenFailure()) {
                return diagnosis(CauseCategory.TIMING_SYNCHRONIZATION,
                        List.of(), Confidence.MEDIUM,
                        "The final result is green, but earlier failed or broken attempts show flake evidence.",
                        "Retry correlation found a non-final failure followed by a passed final attempt.",
                        findings, remediations,
                        List.of("A non-retried reproduction or additional timing and state evidence."));
            }
            return diagnosis(CauseCategory.UNKNOWN, List.of(), Confidence.UNKNOWN,
                    "No failed or broken Allure attempt was present to diagnose.",
                    "Doctor does not infer a root cause from passed-only evidence.",
                    findings, remediations,
                    List.of("A failed or broken attempt with exception and action history evidence."));
        }

        if (matches.isEmpty()) {
            List<String> ids = failures.stream().map(EvidenceItem::id).toList();
            findings.add(finding("unknown-signature", Finding.Kind.INFERENCE,
                    CauseCategory.UNKNOWN, Finding.Severity.WARNING,
                    "Failure signature did not match a deterministic rule",
                    "The failure is retained as unknown rather than assigning an unsupported root cause.",
                    "unknown-fallback", ids));
            remediations.add(remediation("collect-context", "Collect missing diagnostic context",
                    "Add the SHAFT action history, full exception chain, environment metadata, and an approved page snapshot if relevant.",
                    findings, ids));
            missingEvidence.add("A recognized failure signature or additional contextual evidence.");
            return diagnosis(CauseCategory.UNKNOWN, List.of(), Confidence.UNKNOWN,
                    "The available evidence is insufficient for a supported root-cause classification.",
                    "No ordered rule matched the sanitized failed or broken attempts.",
                    findings, remediations, missingEvidence);
        }

        Set<CauseCategory> highConfidenceCategories = EnumSet.noneOf(CauseCategory.class);
        matches.stream()
                .map(RuleMatch::rule)
                .filter(rule -> rule.confidence() == Confidence.HIGH)
                .map(Rule::category)
                .forEach(highConfidenceCategories::add);
        if (highConfidenceCategories.size() > 1 && matchesAreDisjoint(matches)) {
            List<String> ids = matches.stream()
                    .flatMap(match -> match.evidence().stream())
                    .map(EvidenceItem::id)
                    .distinct()
                    .sorted()
                    .toList();
            findings.add(finding("contradictory-causes", Finding.Kind.INFERENCE,
                    CauseCategory.UNKNOWN, Finding.Severity.WARNING,
                    "Independent evidence supports multiple primary causes",
                    "Doctor will not select one high-confidence cause across disjoint failing attempts.",
                    "contradictory-evidence", ids));
            missingEvidence.add("Run-level ownership or isolation that identifies which failing attempt is primary.");
            return diagnosis(CauseCategory.UNKNOWN, highConfidenceCategories.stream().sorted().toList(),
                    Confidence.LOW,
                    "The run contains contradictory high-confidence cause evidence.",
                    "Multiple high-confidence rules matched disjoint evidence, so deterministic precedence was not used to claim one root cause.",
                    findings, remediations, missingEvidence);
        }

        Rule primary = matches.getFirst().rule();
        List<CauseCategory> contributors = matches.stream()
                .map(match -> match.rule().category())
                .filter(category -> category != primary.category())
                .distinct()
                .toList();
        if ("locator-stale".equals(primary.id()) && !contributors.contains(CauseCategory.TIMING_SYNCHRONIZATION)) {
            contributors = append(contributors, CauseCategory.TIMING_SYNCHRONIZATION);
        }
        if (retry.hiddenFailure() && primary.category() != CauseCategory.TIMING_SYNCHRONIZATION
                && !contributors.contains(CauseCategory.TIMING_SYNCHRONIZATION)) {
            contributors = append(contributors, CauseCategory.TIMING_SYNCHRONIZATION);
        }
        return diagnosis(primary.category(), contributors, primary.confidence(),
                primary.title() + ".",
                "The first matching rule in stable precedence order was " + primary.id()
                        + "; later matches are reported as contributing causes.",
                findings, remediations, missingEvidence);
    }

    private static void addAttemptObservations(List<EvidenceItem> failures, List<Finding> findings) {
        for (EvidenceItem failure : failures) {
            String name = failure.attributes().getOrDefault("name", "unknown test");
            String message = failure.attributes().getOrDefault(
                    "failureMessage", "No failure message was recorded.");
            findings.add(finding("attempt-" + failure.id(), Finding.Kind.OBSERVATION,
                    CauseCategory.UNKNOWN, Finding.Severity.ERROR,
                    "Failed or broken test attempt",
                    name + ": " + message,
                    "allure-attempt-observation", List.of(failure.id())));
        }
    }

    private static RetrySummary addRetryFindings(
            List<EvidenceItem> attempts,
            List<Finding> findings,
            List<Remediation> remediations) {
        Map<String, List<EvidenceItem>> groups = new LinkedHashMap<>();
        for (EvidenceItem attempt : attempts) {
            String key = attempt.attributes().getOrDefault(
                    "historyId", attempt.attributes().getOrDefault("name", attempt.id()));
            groups.computeIfAbsent(key, ignored -> new ArrayList<>()).add(attempt);
        }
        boolean hiddenFailure = false;
        for (List<EvidenceItem> group : groups.values()) {
            group.sort(Comparator
                    .comparingLong(DeterministicRuleEngine::start)
                    .thenComparing(EvidenceItem::id));
            EvidenceItem last = group.getLast();
            boolean priorFailure = group.subList(0, Math.max(0, group.size() - 1)).stream()
                    .anyMatch(item -> NON_FINAL_ISSUE_STATUSES.contains(item.attributes().get("status")));
            if (group.size() > 1 && priorFailure && "passed".equals(last.attributes().get("status"))) {
                hiddenFailure = true;
                List<String> ids = group.stream().map(EvidenceItem::id).toList();
                Finding finding = finding("retry-" + last.id(), Finding.Kind.OBSERVATION,
                        CauseCategory.TIMING_SYNCHRONIZATION, Finding.Severity.WARNING,
                        "A retry hid an earlier failed or broken attempt",
                        "The final attempt passed, but Doctor retained and inspected all non-final attempts.",
                        "retry-correlation", ids);
                findings.add(finding);
                remediations.add(new Remediation(
                        "r-retry-" + stableSuffix(last.id()),
                        "Investigate retry-only failure",
                        "Reproduce without retries and compare timing, state isolation, and environment evidence across attempts.",
                        List.of(finding.id()), ids));
            }
        }
        return new RetrySummary(hiddenFailure);
    }

    private static void addHistoricalSignatureFinding(
            List<EvidenceItem> failures,
            List<EvidenceBundle> history,
            List<Finding> findings) {
        Set<String> historical = new LinkedHashSet<>();
        for (EvidenceBundle older : history == null ? List.<EvidenceBundle>of() : history) {
            older.evidence().stream()
                    .filter(item -> FAILURE_STATUSES.contains(item.attributes().get("status")))
                    .map(item -> item.attributes().getOrDefault("signature", ""))
                    .filter(signature -> !signature.isBlank())
                    .forEach(historical::add);
        }
        List<EvidenceItem> repeated = failures.stream()
                .filter(item -> historical.contains(item.attributes().getOrDefault("signature", "")))
                .toList();
        if (!repeated.isEmpty()) {
            findings.add(finding("historical-signature", Finding.Kind.OBSERVATION,
                    CauseCategory.UNKNOWN, Finding.Severity.WARNING,
                    "Failure signature recurred across evidence bundles",
                    "A normalized current failure signature was also present in supplied historical bundles.",
                    "historical-signature-correlation",
                    repeated.stream().map(EvidenceItem::id).toList()));
        }
    }

    private static Diagnosis diagnosis(
            CauseCategory primary,
            List<CauseCategory> contributors,
            Confidence confidence,
            String summary,
            String rationale,
            List<Finding> findings,
            List<Remediation> remediations,
            List<String> missingEvidence) {
        return new Diagnosis(
                Diagnosis.CURRENT_SCHEMA_VERSION,
                primary,
                contributors,
                confidence,
                summary,
                rationale,
                findings.stream().sorted(Comparator.comparing(Finding::id)).toList(),
                remediations.stream().distinct().sorted(Comparator.comparing(Remediation::id)).toList(),
                missingEvidence.stream().distinct().sorted().toList());
    }

    private static Finding finding(
            String seed,
            Finding.Kind kind,
            CauseCategory category,
            Finding.Severity severity,
            String title,
            String detail,
            String ruleId,
            List<String> evidenceIds) {
        return new Finding("f-" + stableSuffix(seed), kind, category, severity,
                title, detail, ruleId, evidenceIds.stream().distinct().sorted().toList());
    }

    private static Remediation remediation(
            String seed,
            String title,
            String action,
            List<Finding> findings,
            List<String> evidenceIds) {
        return new Remediation("r-" + stableSuffix(seed), title, action,
                findings.stream().map(Finding::id).distinct().sorted().toList(),
                evidenceIds.stream().distinct().sorted().toList());
    }

    private static String stableSuffix(String value) {
        return DoctorHashing.sha256(value.getBytes(StandardCharsets.UTF_8)).substring(0, 16);
    }

    private static long start(EvidenceItem item) {
        try {
            return Long.parseLong(item.attributes().getOrDefault("start", "0"));
        } catch (NumberFormatException exception) {
            return 0;
        }
    }

    private static String searchText(EvidenceItem item) {
        return (item.attributes().getOrDefault("failureMessage", "") + "\n"
                + item.attributes().getOrDefault("traceTop", "") + "\n"
                + item.attributes().getOrDefault("fixturePhase", "") + "\n"
                + (item.content() == null ? "" : item.content()))
                .toLowerCase(Locale.ROOT);
    }

    private static boolean containsAny(String text, String... needles) {
        for (String needle : needles) {
            if (text.contains(needle)) {
                return true;
            }
        }
        return false;
    }

    private static boolean matchesAreDisjoint(List<RuleMatch> matches) {
        List<Set<String>> evidenceSets = matches.stream()
                .filter(match -> match.rule().confidence() == Confidence.HIGH)
                .map(match -> match.evidence().stream().map(EvidenceItem::id)
                        .collect(java.util.stream.Collectors.toSet()))
                .toList();
        for (int left = 0; left < evidenceSets.size(); left++) {
            for (int right = left + 1; right < evidenceSets.size(); right++) {
                if (!java.util.Collections.disjoint(evidenceSets.get(left), evidenceSets.get(right))) {
                    return false;
                }
            }
        }
        return true;
    }

    private static int integer(String value, int fallback) {
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException exception) {
            return fallback;
        }
    }

    private static Rule rule(
            String id,
            CauseCategory category,
            Confidence confidence,
            Predicate<String> predicate,
            String title,
            String action) {
        return new Rule(id, category, confidence, predicate, title, action);
    }

    private static <T> List<T> append(List<T> values, T value) {
        List<T> copy = new ArrayList<>(values);
        copy.add(value);
        return List.copyOf(copy);
    }

    private record Rule(
            String id,
            CauseCategory category,
            Confidence confidence,
            Predicate<String> predicate,
            String title,
            String action) {
    }

    private record RuleMatch(Rule rule, List<EvidenceItem> evidence) {
    }

    private record RetrySummary(boolean hiddenFailure) {
    }
}
