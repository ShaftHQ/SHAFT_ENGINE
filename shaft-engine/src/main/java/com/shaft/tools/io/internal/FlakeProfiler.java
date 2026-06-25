package com.shaft.tools.io.internal;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.tools.io.ReportManager;
import org.apache.logging.log4j.Level;

import java.util.Comparator;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

/**
 * Collects opt-in action timing, wait, retry, locator, healing, and evidence-cost signals.
 */
public final class FlakeProfiler {
    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final ThreadLocal<TestProfile> CURRENT_TEST = new ThreadLocal<>();
    private static final Queue<TestProfile> TESTS = new ConcurrentLinkedQueue<>();
    private static final int TOP_LIMIT = 10;
    private static final Pattern SECRET_ASSIGNMENT = Pattern.compile(
            "(?i)(password|passwd|secret|token|api[-_]?key|authorization|cookie)\\s*[:=]\\s*[^\\s,;]+");
    private static final Pattern LONG_TOKEN = Pattern.compile("\\b[a-zA-Z0-9_-]{32,}\\b");

    private FlakeProfiler() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns whether flake profiling is enabled for the current thread.
     *
     * @return {@code true} when profiler collection is enabled
     */
    public static boolean isEnabled() {
        return SHAFT.Properties.reporting.flakeProfilerEnabled();
    }

    /**
     * Clears all profiler state.
     */
    public static void reset() {
        CURRENT_TEST.remove();
        TESTS.clear();
    }

    /**
     * Starts a test-scoped profile.
     *
     * @param info test metadata
     */
    public static void startTest(TestExecutionInfo info) {
        if (!isEnabled()) {
            CURRENT_TEST.remove();
            return;
        }
        runSafely(() -> CURRENT_TEST.set(new TestProfile(info)));
    }

    /**
     * Finishes and optionally attaches the current test profile.
     *
     * @param info test metadata
     * @param status final test status text
     */
    public static void finishTest(TestExecutionInfo info, String status) {
        if (!isEnabled()) {
            CURRENT_TEST.remove();
            return;
        }
        runSafely(() -> {
            TestProfile profile = currentProfile(info);
            profile.status.set(valueOrBlank(status));
            if (profile.hasSignals()) {
                TESTS.add(profile);
                if (SHAFT.Properties.reporting.flakeProfilerAttachPerTest()) {
                    attachProfile(profile);
                }
            }
            CURRENT_TEST.remove();
        });
    }

    /**
     * Records one completed action boundary.
     *
     * @param sample action sample to add to the current test profile
     */
    public static void recordAction(ActionSample sample) {
        if (!isEnabled()) {
            return;
        }
        runSafely(() -> currentProfile(null).recordAction(new ActionEvent(
                sample.category(), sample.name(), sample.target(), sample.durationMillis(),
                sample.locatorLookupCount(), sample.matchCount(), sample.staleElementRetries(),
                sample.healingAttempts(), sample.waitLoopCount(), sample.successful())));
    }

    /**
     * Records the time spent capturing a report artifact.
     *
     * @param category evidence category
     * @param name evidence name
     * @param durationMillis elapsed capture duration
     */
    public static void recordEvidenceCapture(String category, String name, long durationMillis) {
        if (!isEnabled()) {
            return;
        }
        runSafely(() -> currentProfile(null).recordEvidence(new EvidenceEvent(category, name, durationMillis)));
    }

    /**
     * Records a retry attempt and supporting-evidence state.
     *
     * @param testName test method name
     * @param attempt retry attempt number
     * @param maxAttempts configured maximum retry attempts
     * @param supportingEvidenceEnabled whether enhanced evidence capture is enabled
     */
    public static void recordRetryAttempt(String testName, int attempt, int maxAttempts, boolean supportingEvidenceEnabled) {
        if (!isEnabled()) {
            return;
        }
        runSafely(() -> currentProfile(null).recordRetry(
                new RetryEvent(testName, attempt, maxAttempts, supportingEvidenceEnabled)));
    }

    /**
     * Writes the suite-level report and returns a configured severe-risk failure.
     *
     * @return assertion error when severe flake risk should fail the run; otherwise {@code null}
     */
    public static AssertionError reportAndGetFailure() {
        if (!isEnabled() || TESTS.isEmpty()) {
            reset();
            return null;
        }
        try {
            String json = buildSummaryJson();
            String html = buildSummaryHtml();
            ReportManager.log(buildSummaryText());
            ReportManagerHelper.attach("json", "flake-profile.json", json);
            ReportManagerHelper.attach("html", "Flake Profile", html);
            int severeRiskCount = severeRiskCount();
            reset();
            if (severeRiskCount > 0 && SHAFT.Properties.reporting.flakeProfilerFailOnSevereFlakeRisk()) {
                return new AssertionError("Severe flake risk actions were found: " + severeRiskCount + ".");
            }
        } catch (RuntimeException exception) {
            ReportManagerHelper.logDiscrete("Could not attach SHAFT flake profile: " + exception.getMessage(), Level.WARN);
            reset();
        }
        return null;
    }

    static String buildSummaryJson() {
        ObjectNode root = MAPPER.createObjectNode();
        List<TestProfile> profiles = TESTS.stream().toList();
        root.put("totalTests", profiles.size());
        root.put("totalActions", profiles.stream().mapToInt(TestProfile::actionCount).sum());
        root.put("retryAttempts", profiles.stream().mapToInt(TestProfile::retryCount).sum());
        root.put("evidenceCaptureMillis", profiles.stream().mapToLong(TestProfile::evidenceCaptureMillis).sum());
        root.put("severeRiskCount", severeRiskCount());
        ArrayNode tests = root.putArray("tests");
        profiles.forEach(profile -> profile.appendJson(tests.addObject()));
        appendActions(root.putArray("topSlowActions"), topSlowActions(profiles));
        appendActions(root.putArray("waitHeavyActions"), waitHeavyActions(profiles));
        return root.toPrettyString();
    }

    private static TestProfile currentProfile(TestExecutionInfo info) {
        TestProfile profile = CURRENT_TEST.get();
        if (profile == null) {
            profile = new TestProfile(info);
            CURRENT_TEST.set(profile);
        }
        return profile;
    }

    private static void attachProfile(TestProfile profile) {
        try {
            ReportManagerHelper.attach("json", "flake-profile.json", profile.toJson().toPrettyString());
            ReportManagerHelper.attach("html", "Flake Profile", profile.toHtml());
        } catch (RuntimeException exception) {
            ReportManagerHelper.logDiscrete("Could not attach per-test SHAFT flake profile: "
                    + exception.getMessage(), Level.WARN);
        }
    }

    private static String buildSummaryText() {
        return "Flake profile summary: tests=" + TESTS.size()
                + ", actions=" + TESTS.stream().mapToInt(TestProfile::actionCount).sum()
                + ", retries=" + TESTS.stream().mapToInt(TestProfile::retryCount).sum()
                + ", severeRiskActions=" + severeRiskCount() + ".";
    }

    private static String buildSummaryHtml() {
        StringBuilder rows = new StringBuilder();
        TESTS.forEach(profile -> rows.append("<tr><td>")
                .append(htmlEscape(profile.test.get()))
                .append("</td><td>")
                .append(profile.actionCount())
                .append("</td><td>")
                .append(profile.retryCount())
                .append("</td><td>")
                .append(profile.evidenceCaptureMillis())
                .append("</td><td>")
                .append(profile.severeRiskCount())
                .append("</td></tr>"));
        return """
                <!doctype html>
                <html lang="en">
                <head><meta charset="utf-8"><title>SHAFT Flake Profile</title></head>
                <body>
                <h1>SHAFT Flake Profile</h1>
                <table>
                <thead><tr><th>Test</th><th>Actions</th><th>Retries</th><th>Evidence ms</th><th>Severe risk</th></tr></thead>
                <tbody>%s</tbody>
                </table>
                </body>
                </html>
                """.formatted(rows);
    }

    private static int severeRiskCount() {
        return TESTS.stream().mapToInt(TestProfile::severeRiskCount).sum();
    }

    private static List<ActionEvent> topSlowActions(List<TestProfile> profiles) {
        return profiles.stream()
                .flatMap(profile -> profile.actions.stream())
                .sorted(Comparator.comparingLong(ActionEvent::durationMillis).reversed())
                .limit(TOP_LIMIT)
                .toList();
    }

    private static List<ActionEvent> waitHeavyActions(List<TestProfile> profiles) {
        return profiles.stream()
                .flatMap(profile -> profile.actions.stream())
                .filter(ActionEvent::isWaitHeavy)
                .sorted(Comparator.comparingInt(ActionEvent::waitLoopCount).reversed()
                        .thenComparing(Comparator.comparingLong(ActionEvent::durationMillis).reversed()))
                .limit(TOP_LIMIT)
                .toList();
    }

    private static void appendActions(ArrayNode array, List<ActionEvent> actions) {
        actions.forEach(action -> action.appendJson(array.addObject()));
    }

    private static String testId(TestExecutionInfo info) {
        if (info == null) {
            return "unknown";
        }
        if (info.stableId() != null && !info.stableId().isBlank()) {
            return safe(info.stableId());
        }
        if (info.className() != null && !info.className().isBlank() && info.methodName() != null && !info.methodName().isBlank()) {
            return safe(info.className() + "." + info.methodName());
        }
        return safe(valueOrBlank(info.displayName()));
    }

    private static String safe(String value) {
        String sanitized = value == null ? "" : value;
        sanitized = SECRET_ASSIGNMENT.matcher(sanitized).replaceAll("$1=[REDACTED]");
        sanitized = LONG_TOKEN.matcher(sanitized).replaceAll("[REDACTED]");
        return sanitized.replaceAll("[\\p{Cntrl}&&[^\\r\\n\\t]]", "").trim();
    }

    private static String valueOrBlank(String value) {
        return value == null ? "" : value;
    }

    private static String htmlEscape(String value) {
        return value.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&#39;");
    }

    private static void runSafely(Runnable runnable) {
        try {
            runnable.run();
        } catch (RuntimeException exception) {
            ReportManagerHelper.logDiscrete("SHAFT flake profiler ignored an internal error: "
                    + exception.getMessage(), Level.DEBUG);
        }
    }

    private static final class TestProfile {
        private final AtomicReference<String> test;
        private final AtomicReference<String> className;
        private final AtomicReference<String> methodName;
        private final AtomicReference<String> status = new AtomicReference<>("");
        private final Queue<ActionEvent> actions = new ConcurrentLinkedQueue<>();
        private final Queue<EvidenceEvent> evidence = new ConcurrentLinkedQueue<>();
        private final Queue<RetryEvent> retries = new ConcurrentLinkedQueue<>();
        private final AtomicLong evidenceCaptureMillis = new AtomicLong();

        private TestProfile(TestExecutionInfo info) {
            this.test = new AtomicReference<>(testId(info));
            this.className = new AtomicReference<>(safe(info == null ? "" : info.className()));
            this.methodName = new AtomicReference<>(safe(info == null ? "" : info.methodName()));
        }

        private void recordAction(ActionEvent action) {
            actions.add(action);
        }

        private void recordEvidence(EvidenceEvent event) {
            evidence.add(event);
            evidenceCaptureMillis.addAndGet(event.durationMillis());
        }

        private void recordRetry(RetryEvent retry) {
            retries.add(retry);
        }

        private boolean hasSignals() {
            return !actions.isEmpty() || !evidence.isEmpty() || !retries.isEmpty();
        }

        private int actionCount() {
            return actions.size();
        }

        private int retryCount() {
            return retries.size();
        }

        private long evidenceCaptureMillis() {
            return evidenceCaptureMillis.get();
        }

        private int severeRiskCount() {
            return (int) actions.stream().filter(ActionEvent::isSevereRisk).count();
        }

        private ObjectNode toJson() {
            ObjectNode node = MAPPER.createObjectNode();
            appendJson(node);
            return node;
        }

        private void appendJson(ObjectNode node) {
            node.put("test", test.get());
            node.put("className", className.get());
            node.put("methodName", methodName.get());
            node.put("status", status.get());
            node.put("totalActions", actionCount());
            node.put("retryAttempts", retryCount());
            node.put("evidenceCaptureMillis", evidenceCaptureMillis());
            node.put("severeRiskCount", severeRiskCount());
            ArrayNode actionNodes = node.putArray("actions");
            actions.forEach(action -> action.appendJson(actionNodes.addObject()));
            appendActions(node.putArray("topSlowActions"), actions.stream()
                    .sorted(Comparator.comparingLong(ActionEvent::durationMillis).reversed())
                    .limit(TOP_LIMIT)
                    .toList());
            appendActions(node.putArray("waitHeavyActions"), actions.stream()
                    .filter(ActionEvent::isWaitHeavy)
                    .sorted(Comparator.comparingInt(ActionEvent::waitLoopCount).reversed()
                            .thenComparing(Comparator.comparingLong(ActionEvent::durationMillis).reversed()))
                    .limit(TOP_LIMIT)
                    .toList());
            ArrayNode evidenceNodes = node.putArray("evidence");
            evidence.forEach(event -> event.appendJson(evidenceNodes.addObject()));
            ArrayNode retryNodes = node.putArray("retryHistory");
            retries.forEach(retry -> retry.appendJson(retryNodes.addObject()));
        }

        private String toHtml() {
            return """
                    <!doctype html>
                    <html lang="en">
                    <head><meta charset="utf-8"><title>SHAFT Flake Profile</title></head>
                    <body><h1>SHAFT Flake Profile</h1><pre>%s</pre></body>
                    </html>
                    """.formatted(htmlEscape(toJson().toPrettyString()));
        }
    }

    public static final class ActionSample {
        private final String category;
        private final String name;
        private String target = "";
        private long durationMillis;
        private int locatorLookupCount;
        private int matchCount;
        private int staleElementRetries;
        private int healingAttempts;
        private int waitLoopCount;
        private boolean successful = true;

        private ActionSample(String category, String name) {
            this.category = category;
            this.name = name;
        }

        public static ActionSample of(String category, String name) {
            return new ActionSample(category, name);
        }

        public ActionSample target(String target) {
            this.target = target;
            return this;
        }

        public ActionSample durationMillis(long durationMillis) {
            this.durationMillis = durationMillis;
            return this;
        }

        public ActionSample locatorLookupCount(int locatorLookupCount) {
            this.locatorLookupCount = locatorLookupCount;
            return this;
        }

        public ActionSample matchCount(int matchCount) {
            this.matchCount = matchCount;
            return this;
        }

        public ActionSample staleElementRetries(int staleElementRetries) {
            this.staleElementRetries = staleElementRetries;
            return this;
        }

        public ActionSample healingAttempts(int healingAttempts) {
            this.healingAttempts = healingAttempts;
            return this;
        }

        public ActionSample waitLoopCount(int waitLoopCount) {
            this.waitLoopCount = waitLoopCount;
            return this;
        }

        public ActionSample successful(boolean successful) {
            this.successful = successful;
            return this;
        }

        private String category() {
            return category;
        }

        private String name() {
            return name;
        }

        private String target() {
            return target;
        }

        private long durationMillis() {
            return durationMillis;
        }

        private int locatorLookupCount() {
            return locatorLookupCount;
        }

        private int matchCount() {
            return matchCount;
        }

        private int staleElementRetries() {
            return staleElementRetries;
        }

        private int healingAttempts() {
            return healingAttempts;
        }

        private int waitLoopCount() {
            return waitLoopCount;
        }

        private boolean successful() {
            return successful;
        }
    }

    private record ActionEvent(String category, String name, String target, long durationMillis,
                               int locatorLookupCount, int matchCount, int staleElementRetries,
                               int healingAttempts, int waitLoopCount, boolean successful) {
        private ActionEvent {
            category = safe(category);
            name = safe(name);
            target = safe(target);
            durationMillis = Math.max(0, durationMillis);
            locatorLookupCount = Math.max(0, locatorLookupCount);
            matchCount = Math.max(0, matchCount);
            staleElementRetries = Math.max(0, staleElementRetries);
            healingAttempts = Math.max(0, healingAttempts);
            waitLoopCount = Math.max(0, waitLoopCount);
        }

        private boolean isWaitHeavy() {
            return waitLoopCount > 1;
        }

        private boolean isSevereRisk() {
            return durationMillis >= SHAFT.Properties.reporting.flakeProfilerSlowActionThresholdMs()
                    && (waitLoopCount > 1 || staleElementRetries > 0 || healingAttempts > 0 || !successful);
        }

        private void appendJson(ObjectNode node) {
            node.put("category", category);
            node.put("name", name);
            node.put("target", target);
            node.put("durationMillis", durationMillis);
            node.put("locatorLookupCount", locatorLookupCount);
            node.put("matchCount", matchCount);
            node.put("staleElementRetries", staleElementRetries);
            node.put("healingAttempts", healingAttempts);
            node.put("waitLoopCount", waitLoopCount);
            node.put("successful", successful);
        }
    }

    private record EvidenceEvent(String category, String name, long durationMillis) {
        private EvidenceEvent {
            category = safe(category);
            name = safe(name);
            durationMillis = Math.max(0, durationMillis);
        }

        private void appendJson(ObjectNode node) {
            node.put("category", category);
            node.put("name", name);
            node.put("durationMillis", durationMillis);
        }
    }

    private record RetryEvent(String testName, int attempt, int maxAttempts, boolean supportingEvidenceEnabled) {
        private RetryEvent {
            testName = safe(testName);
            attempt = Math.max(0, attempt);
            maxAttempts = Math.max(0, maxAttempts);
        }

        private void appendJson(ObjectNode node) {
            node.put("testName", testName);
            node.put("attempt", attempt);
            node.put("maxAttempts", maxAttempts);
            node.put("supportingEvidenceEnabled", supportingEvidenceEnabled);
        }
    }
}
