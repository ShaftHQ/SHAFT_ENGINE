package com.shaft.listeners;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.ExecutionCountsTracker;
import com.shaft.listeners.internal.ExecutionLifecycleHelper;
import com.shaft.listeners.internal.JunitListenerHelper;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.tools.io.internal.ExecutionSummaryReport;
import com.shaft.tools.io.internal.IssueReporter;
import com.shaft.tools.io.internal.ProjectStructureManager;
import com.shaft.tools.io.internal.ReportContext;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.ValidationsHelper;
import io.qameta.allure.model.Status;
import lombok.Getter;
import org.junit.platform.engine.TestExecutionResult;
import org.junit.platform.engine.UniqueId;
import org.junit.platform.engine.reporting.FileEntry;
import org.junit.platform.engine.reporting.ReportEntry;
import org.junit.platform.engine.support.descriptor.MethodSource;
import org.junit.platform.launcher.LauncherSession;
import org.junit.platform.launcher.LauncherSessionListener;
import org.junit.platform.launcher.TestExecutionListener;
import org.junit.platform.launcher.TestIdentifier;
import org.junit.platform.launcher.TestPlan;

import javax.annotation.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.function.Consumer;
import java.util.regex.Pattern;

/**
 * JUnit Platform launcher listener that connects Jupiter execution to SHAFT reporting.
 */
public class JunitListener implements LauncherSessionListener {
    private static final List<TestIdentifier> passedTests = Collections.synchronizedList(new ArrayList<>());
    private static final List<TestIdentifier> failedTests = Collections.synchronizedList(new ArrayList<>());
    private static final List<TestIdentifier> skippedTests = Collections.synchronizedList(new ArrayList<>());
    private static final ExecutionCountsTracker countsTracker = new ExecutionCountsTracker();
    private static long executionStartTime;
    private static boolean isEngineReady = false;
    private static final String ALLURE_LISTENER_DEACTIVATE = "junit.platform.execution.listeners.deactivate";
    private static final String ALLURE_JUNIT_PLATFORM_LISTENER = "io.qameta.allure.junitplatform.AllureJunitPlatform";
    @Getter
    private static volatile Boolean isLastFinishedTestOK = true;

    static {
        // JUnit keeps only the first junit-platform.properties on the classpath.
        // shaft-engine tests also ship one under test resources, which would hide
        // the deactivation in the main resource. System properties are consulted
        // first, and this class is loaded before auto-registered listeners.
        publishAllureDeactivationProperty();
    }

    @Override
    public void launcherSessionOpened(LauncherSession session) {
        // Surefire 3.6.1 runs TestNG on the JUnit Platform testng-engine. Register
        // one listener: SHAFT lifecycle plus an optional filtered Allure copy that
        // replaces the deactivated SPI listener. A second registration would break
        // the single-listener contract once the engine is ready.
        if (!isEngineReady) {
            session.getLauncher().registerTestExecutionListeners(
                    new ShaftJunitExecutionListener(createOptionalFilteredAllureListener()));
        }
    }

    static boolean isTestNgEngine(TestIdentifier testIdentifier) {
        List<UniqueId.Segment> segments = testIdentifier.getUniqueIdObject().getSegments();
        for (int index = segments.size() - 1; index >= 0; index--) {
            UniqueId.Segment segment = segments.get(index);
            if ("engine".equals(segment.getType())) {
                return "testng".equals(segment.getValue());
            }
        }
        return false;
    }

    static boolean hasNonTestNgTests(TestPlan testPlan) {
        return testPlan.getRoots().stream().anyMatch(root -> hasNonTestNgWork(testPlan, root));
    }

    private static boolean hasNonTestNgWork(TestPlan testPlan, TestIdentifier node) {
        if (isTestNgEngine(node)) {
            return false;
        }
        if (node.isTest()) {
            return true;
        }
        for (TestIdentifier child : testPlan.getChildren(node)) {
            if (hasNonTestNgChildWork(testPlan, child)) {
                return true;
            }
        }
        return false;
    }

    private static boolean hasNonTestNgChildWork(TestPlan testPlan, TestIdentifier child) {
        if (isTestNgEngine(child)) {
            return false;
        }
        if (child.isTest()) {
            return true;
        }
        Set<TestIdentifier> grandchildren = testPlan.getChildren(child);
        if (grandchildren.isEmpty()) {
            // Leaf non-TestNG container (for example @ParameterizedTest, @RepeatedTest, @TestFactory).
            return true;
        }
        for (TestIdentifier grandchild : grandchildren) {
            if (hasNonTestNgChildWork(testPlan, grandchild)) {
                return true;
            }
        }
        return false;
    }

    static TestExecutionListener createFilteredAllureListener(TestExecutionListener delegate) {
        return new FilteredAllureListener(delegate);
    }

    /**
     * Surefire deactivates the SPI Allure listener. When that deactivation is active
     * and the runtime class exists, return the filtered copy SHAFT forwards to.
     */
    private static Optional<TestExecutionListener> createOptionalFilteredAllureListener() {
        if (!isAllureJunitPlatformDeactivated()) {
            return Optional.empty();
        }
        return newAllureJunitPlatformListener().map(JunitListener::createFilteredAllureListener);
    }

    /**
     * allure-jupiter depends on allure-junit-platform at runtime, so the listener
     * class is not visible to shaft-engine compilation.
     */
    private static Optional<TestExecutionListener> newAllureJunitPlatformListener() {
        try {
            Class<?> type = Class.forName("io.qameta.allure.junitplatform.AllureJunitPlatform");
            Object listener = type.getDeclaredConstructor().newInstance();
            if (listener instanceof TestExecutionListener executionListener) {
                return Optional.of(executionListener);
            }
            return Optional.empty();
        } catch (ClassNotFoundException exception) {
            return Optional.empty();
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Unable to construct AllureJunitPlatform.", exception);
        }
    }

    private static void publishAllureDeactivationProperty() {
        String current = System.getProperty(ALLURE_LISTENER_DEACTIVATE);
        if (deactivatesAllureJunitPlatform(current) || !classpathDeactivatesAllureJunitPlatform()) {
            return;
        }
        String updated = current == null || current.isBlank()
                ? ALLURE_JUNIT_PLATFORM_LISTENER
                : current + "," + ALLURE_JUNIT_PLATFORM_LISTENER;
        System.setProperty(ALLURE_LISTENER_DEACTIVATE, updated);
    }

    private static boolean isAllureJunitPlatformDeactivated() {
        return deactivatesAllureJunitPlatform(System.getProperty(ALLURE_LISTENER_DEACTIVATE))
                || classpathDeactivatesAllureJunitPlatform();
    }

    static boolean deactivatesAllureJunitPlatform(@Nullable String raw) {
        if (raw == null || raw.isBlank()) {
            return false;
        }
        int start = 0;
        for (int index = 0; index <= raw.length(); index++) {
            if (index == raw.length() || raw.charAt(index) == ',') {
                String token = raw.substring(start, index).trim();
                if (matchesDeactivatePattern(token, ALLURE_JUNIT_PLATFORM_LISTENER)) {
                    return true;
                }
                start = index + 1;
            }
        }
        return false;
    }

    /**
     * JUnit 6.1.3 {@code ClassNamePatternFilterUtils}: {@code .} matches {@code .} or
     * {@code $}, {@code *} is one or more characters, and every other character is literal.
     */
    private static boolean matchesDeactivatePattern(String token, String className) {
        if (token.isEmpty()) {
            return false;
        }
        StringBuilder pattern = new StringBuilder("^");
        for (int index = 0; index < token.length(); index++) {
            char character = token.charAt(index);
            if (character == '*') {
                pattern.append(".+");
            } else if (character == '.') {
                pattern.append("[.$]");
            } else {
                pattern.append(Pattern.quote(String.valueOf(character)));
            }
        }
        pattern.append('$');
        return Pattern.compile(pattern.toString()).matcher(className).matches();
    }

    private static boolean classpathDeactivatesAllureJunitPlatform() {
        return resourcesDeactivateAllure(Thread.currentThread().getContextClassLoader())
                || resourcesDeactivateAllure(JunitListener.class.getClassLoader());
    }

    private static boolean resourcesDeactivateAllure(ClassLoader loader) {
        if (loader == null) {
            return false;
        }
        try {
            Enumeration<URL> resources = loader.getResources("junit-platform.properties");
            while (resources.hasMoreElements()) {
                try (InputStream input = resources.nextElement().openStream()) {
                    Properties properties = new Properties();
                    properties.load(input);
                    if (deactivatesAllureJunitPlatform(properties.getProperty(ALLURE_LISTENER_DEACTIVATE))) {
                        return true;
                    }
                }
            }
        } catch (IOException ignored) {
            return false;
        }
        return false;
    }

    private void engineTearDown() {
        ExecutionCountsTracker.Counts counts = countsTracker.snapshot();
        if (counts.finalPassed() + counts.failed() + counts.skipped() == 0
                && (!passedTests.isEmpty() || !failedTests.isEmpty() || !skippedTests.isEmpty())) {
            counts = legacyCountsSnapshot();
        }
        ExecutionLifecycleHelper.engineTearDown(executionStartTime, counts);
    }

    static void recordRetriedFailure(TestExecutionInfo info) {
        countsTracker.recordFailed(info);
    }

    static void clearRecordedFailures() {
        countsTracker.clearFailures();
    }

    private void afterInvocation(TestExecutionInfo info) {
        ReportManagerHelper.setDiscreteLogging(SHAFT.Properties.reporting.alwaysLogDiscreetly());
        ExecutionLifecycleHelper.attachTestArtifacts(info);
    }

    private void onTestSuccess(TestIdentifier testIdentifier, TestExecutionInfo info) {
        passedTests.add(testIdentifier);
        countsTracker.recordPassed(info);
        isLastFinishedTestOK = true;
        ExecutionLifecycleHelper.logFinishedTestInformation(info, "Passed");
        appendToExecutionSummaryReport(info, "", ExecutionSummaryReport.StatusIcon.PASSED, ExecutionSummaryReport.Status.PASSED);
        IssueReporter.updateIssuesLog(info, true);
    }

    private void onTestFailure(TestIdentifier testIdentifier, Throwable throwable, TestExecutionInfo info) {
        failedTests.add(testIdentifier);
        countsTracker.recordFailed(info);
        isLastFinishedTestOK = false;
        ExecutionLifecycleHelper.logFinishedTestInformation(info, "Failed", throwable);
        appendToExecutionSummaryReport(info, throwable == null ? "" : throwable.getMessage(), ExecutionSummaryReport.StatusIcon.FAILED, ExecutionSummaryReport.Status.FAILED);
        IssueReporter.updateIssuesLog(info, false);
    }

    private void onTestSkipped(TestIdentifier testIdentifier, String reason, TestExecutionInfo info) {
        skippedTests.add(testIdentifier);
        countsTracker.recordSkipped(info);
        isLastFinishedTestOK = false;
        ExecutionLifecycleHelper.logFinishedTestInformation(info, "Skipped");
        appendToExecutionSummaryReport(info, reason, ExecutionSummaryReport.StatusIcon.SKIPPED, ExecutionSummaryReport.Status.SKIPPED);
    }

    private void appendToExecutionSummaryReport(TestExecutionInfo info, String errorMessage,
                                                ExecutionSummaryReport.StatusIcon statusIcon,
                                                ExecutionSummaryReport.Status status) {
        ExecutionLifecycleHelper.appendExecutionSummaryReport(info, errorMessage, statusIcon, status);
    }

    private void appendToExecutionSummaryReport(TestIdentifier testIdentifier, String errorMessage,
                                                ExecutionSummaryReport.StatusIcon statusIcon,
                                                ExecutionSummaryReport.Status status) {
        if (testIdentifier.isTest()) {
            appendToExecutionSummaryReport(toTestExecutionInfo(testIdentifier, null), errorMessage, statusIcon, status);
        }
    }

    private static ExecutionCountsTracker.Counts legacyCountsSnapshot() {
        ExecutionCountsTracker tracker = new ExecutionCountsTracker();
        passedTests.forEach(testIdentifier -> tracker.recordPassed(toTestExecutionInfo(testIdentifier, null)));
        failedTests.forEach(testIdentifier -> tracker.recordFailed(toTestExecutionInfo(testIdentifier, new AssertionError())));
        skippedTests.forEach(testIdentifier -> tracker.recordSkipped(toTestExecutionInfo(testIdentifier, null)));
        return tracker.snapshot();
    }

    private static TestExecutionInfo toTestExecutionInfo(TestIdentifier testIdentifier, Throwable throwable) {
        String className = "";
        String methodName = testIdentifier.getDisplayName();
        Method method = null;
        Optional<MethodSource> methodSource = testIdentifier.getSource()
                .filter(MethodSource.class::isInstance)
                .map(MethodSource.class::cast);
        if (methodSource.isPresent()) {
            className = methodSource.get().getClassName();
            methodName = methodSource.get().getMethodName();
            method = resolveMethod(className, methodName);
        }
        String description = testIdentifier.getLegacyReportingName();
        return new TestExecutionInfo(testIdentifier.getUniqueId(), className, methodName,
                testIdentifier.getDisplayName(), description, method, throwable, false);
    }

    private static Method resolveMethod(String className, String methodName) {
        try {
            for (Method method : Class.forName(className).getDeclaredMethods()) {
                if (method.getName().equals(methodName)) {
                    return method;
                }
            }
        } catch (ReflectiveOperationException ignored) {
            // Summary metadata is best-effort when JUnit does not expose a resolvable method.
        }
        return null;
    }

    private static Throwable resolveThrowable(TestExecutionResult result) {
        return result.getThrowable().orElseGet(() -> result.getStatus() == TestExecutionResult.Status.FAILED
                ? new AssertionError("Test Failed")
                : null);
    }

    private static Status toAllureStatus(TestExecutionResult result) {
        return switch (result.getStatus()) {
            case FAILED -> Status.FAILED;
            case ABORTED -> Status.SKIPPED;
            default -> Status.PASSED;
        };
    }

    private final class ShaftJunitExecutionListener implements TestExecutionListener {
        private final Optional<TestExecutionListener> filteredAllureListener;
        private boolean activeForCurrentPlan = false;

        private ShaftJunitExecutionListener(Optional<TestExecutionListener> filteredAllureListener) {
            this.filteredAllureListener = filteredAllureListener;
        }

        private void forwardAllure(Consumer<TestExecutionListener> call) {
            filteredAllureListener.ifPresent(call);
        }

        @Override
        public void testPlanExecutionStarted(TestPlan testPlan) {
            forwardAllure(listener -> listener.testPlanExecutionStarted(testPlan));
            if (!hasNonTestNgTests(testPlan)) {
                activeForCurrentPlan = false;
                return;
            }
            activeForCurrentPlan = true;
            executionStartTime = System.currentTimeMillis();
            passedTests.clear();
            failedTests.clear();
            skippedTests.clear();
            countsTracker.clear();
            ExecutionLifecycleHelper.engineSetup(ProjectStructureManager.RunType.JUNIT);
            isEngineReady = true;
        }

        @Override
        public void testPlanExecutionFinished(TestPlan testPlan) {
            forwardAllure(listener -> listener.testPlanExecutionFinished(testPlan));
            if (!activeForCurrentPlan) {
                return;
            }
            activeForCurrentPlan = false;
            engineTearDown();
        }

        @Override
        public void dynamicTestRegistered(TestIdentifier testIdentifier) {
            forwardAllure(listener -> listener.dynamicTestRegistered(testIdentifier));
        }

        @Override
        public void executionSkipped(TestIdentifier testIdentifier, String reason) {
            forwardAllure(listener -> listener.executionSkipped(testIdentifier, reason));
            if (!activeForCurrentPlan || isTestNgEngine(testIdentifier)) {
                return;
            }
            if (testIdentifier.isTest()) {
                try {
                    TestExecutionInfo info = toTestExecutionInfo(testIdentifier,
                            new org.opentest4j.TestAbortedException(reason));
                    ReportContext.start(info);
                    ReportContext.setStatus(Status.SKIPPED);
                    afterInvocation(info);
                    onTestSkipped(testIdentifier, reason, info);
                } finally {
                    ReportContext.clearPreservingSensitiveEvidence();
                }
            }
        }

        @Override
        public void executionStarted(TestIdentifier testIdentifier) {
            forwardAllure(listener -> listener.executionStarted(testIdentifier));
            if (!activeForCurrentPlan || isTestNgEngine(testIdentifier)) {
                return;
            }
            if (testIdentifier.isTest()) {
                TestExecutionInfo info = toTestExecutionInfo(testIdentifier, null);
                ReportContext.start(info);
                ReportContext.setStatus(Status.PASSED);
                JunitListenerHelper.setTestName(testIdentifier);
                ExecutionLifecycleHelper.logTestInformation(info);
            }
        }

        @Override
        public void executionFinished(TestIdentifier testIdentifier, TestExecutionResult testExecutionResult) {
            forwardAllure(listener -> listener.executionFinished(testIdentifier, testExecutionResult));
            if (!activeForCurrentPlan || isTestNgEngine(testIdentifier)) {
                return;
            }
            if (!testIdentifier.isTest()) {
                return;
            }
            try {
                Throwable throwable = resolveThrowable(testExecutionResult);
                TestExecutionInfo info = toTestExecutionInfo(testIdentifier, throwable);
                ReportContext.update(info);
                ReportContext.setStatus(toAllureStatus(testExecutionResult));
                switch (testExecutionResult.getStatus()) {
                    case SUCCESSFUL -> {
                        AssertionError verificationError = ValidationsHelper.getVerificationErrorToForceFail();
                        if (verificationError != null) {
                            ValidationsHelper.attachVerificationSummary();
                            ValidationsHelper.resetVerificationStateAfterFailing();
                            TestExecutionInfo failedInfo = toTestExecutionInfo(testIdentifier, verificationError);
                            ReportContext.update(failedInfo);
                            ReportContext.setStatus(Status.FAILED);
                            afterInvocation(failedInfo);
                            onTestFailure(testIdentifier, verificationError, failedInfo);
                        } else {
                            afterInvocation(info);
                            onTestSuccess(testIdentifier, info);
                        }
                    }
                    case FAILED, ABORTED -> {
                        afterInvocation(info);
                        onTestFailure(testIdentifier, throwable, info);
                    }
                }
            } finally {
                ReportContext.clearPreservingSensitiveEvidence();
            }
        }

        @Override
        public void reportingEntryPublished(TestIdentifier testIdentifier, ReportEntry entry) {
            forwardAllure(listener -> listener.reportingEntryPublished(testIdentifier, entry));
        }

        @Override
        public void fileEntryPublished(TestIdentifier testIdentifier, FileEntry file) {
            forwardAllure(listener -> listener.fileEntryPublished(testIdentifier, file));
        }
    }

    private static final class FilteredAllureListener implements TestExecutionListener {
        private final TestExecutionListener delegate;
        private boolean activeForCurrentPlan = false;

        private FilteredAllureListener(TestExecutionListener delegate) {
            this.delegate = delegate;
        }

        @Override
        public void testPlanExecutionStarted(TestPlan testPlan) {
            activeForCurrentPlan = hasNonTestNgTests(testPlan);
            if (activeForCurrentPlan) {
                delegate.testPlanExecutionStarted(testPlan);
            }
        }

        @Override
        public void testPlanExecutionFinished(TestPlan testPlan) {
            if (activeForCurrentPlan) {
                activeForCurrentPlan = false;
                delegate.testPlanExecutionFinished(testPlan);
            }
        }

        @Override
        public void dynamicTestRegistered(TestIdentifier testIdentifier) {
            if (!activeForCurrentPlan || isTestNgEngine(testIdentifier)) {
                return;
            }
            delegate.dynamicTestRegistered(testIdentifier);
        }

        @Override
        public void executionSkipped(TestIdentifier testIdentifier, String reason) {
            if (!activeForCurrentPlan || isTestNgEngine(testIdentifier)) {
                return;
            }
            delegate.executionSkipped(testIdentifier, reason);
        }

        @Override
        public void executionStarted(TestIdentifier testIdentifier) {
            if (!activeForCurrentPlan || isTestNgEngine(testIdentifier)) {
                return;
            }
            delegate.executionStarted(testIdentifier);
        }

        @Override
        public void executionFinished(TestIdentifier testIdentifier, TestExecutionResult testExecutionResult) {
            if (!activeForCurrentPlan || isTestNgEngine(testIdentifier)) {
                return;
            }
            delegate.executionFinished(testIdentifier, testExecutionResult);
        }

        @Override
        public void reportingEntryPublished(TestIdentifier testIdentifier, ReportEntry entry) {
            if (!activeForCurrentPlan || isTestNgEngine(testIdentifier)) {
                return;
            }
            delegate.reportingEntryPublished(testIdentifier, entry);
        }

        @Override
        public void fileEntryPublished(TestIdentifier testIdentifier, FileEntry file) {
            if (!activeForCurrentPlan || isTestNgEngine(testIdentifier)) {
                return;
            }
            delegate.fileEntryPublished(testIdentifier, file);
        }
    }
}
