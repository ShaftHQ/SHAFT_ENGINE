package com.shaft.listeners;

import com.epam.reportportal.listeners.ItemStatus;
import com.epam.reportportal.service.ReportPortal;
import com.epam.reportportal.testng.ITestNGService;
import com.epam.reportportal.testng.TestNGService;
import com.epam.reportportal.utils.MemoizingSupplier;
import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.*;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.*;
import lombok.Getter;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.testng.*;
import org.testng.annotations.ITestAnnotation;
import org.testng.internal.IResultListener2;
import org.testng.xml.XmlSuite;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * Central TestNG listener that orchestrates SHAFT's engine lifecycle, test
 * execution tracking, reporting integration, and suite-level configuration.
 *
 * <p>This listener is automatically registered via the Maven Surefire plugin
 * configuration and implements multiple TestNG listener interfaces to hook
 * into every phase of the test lifecycle:
 * <ul>
 *   <li>{@link IAlterSuiteListener} &ndash; Modifies suite XML before execution.</li>
 *   <li>{@link IAnnotationTransformer} &ndash; Applies retry analyzers and adjusts annotations.</li>
 *   <li>{@link IExecutionListener} &ndash; Engine setup on start, telemetry/reporting on finish.</li>
 *   <li>{@link ISuiteListener} &ndash; Suite-level setup and teardown.</li>
 *   <li>{@link IInvokedMethodListener} &ndash; Before/after each test method invocation.</li>
 *   <li>{@link ITestListener} &amp; {@link IResultListener2} &ndash; Test result tracking.</li>
 *   <li>{@link IMethodInterceptor} &ndash; Test method ordering/filtering.</li>
 * </ul>
 *
 * <p>Thread safety: Shared state (e.g., passed/failed/skipped lists) is
 * protected with synchronized collections or {@link java.util.concurrent.atomic.AtomicInteger}.
 * Per-thread state uses {@link ThreadLocal} variables.
 *
 * @see com.shaft.listeners.internal.TestNGListenerHelper
 */
public class TestNGListener implements IAlterSuiteListener, IAnnotationTransformer,
        IExecutionListener, ISuiteListener, IInvokedMethodListener, ITestListener, IResultListener2, IMethodInterceptor {

    private static final Logger logger = LogManager.getLogger(TestNGListener.class);

    public static final Supplier<ITestNGService> REPORT_PORTAL_SERVICE = new MemoizingSupplier<>(() -> new TestNGService(ReportPortal.builder().build()));
    private static final List<ITestNGMethod> passedTests = Collections.synchronizedList(new ArrayList<>());
    private static final List<ITestNGMethod> failedTests = Collections.synchronizedList(new ArrayList<>());
    private static final List<ITestNGMethod> skippedTests = Collections.synchronizedList(new ArrayList<>());
    private static final ExecutionCountsTracker countsTracker = new ExecutionCountsTracker();
    // ReportPortal
    private static final AtomicInteger REPORT_PORTAL_INSTANCES = new AtomicInteger(0);
    /** Tracks the test class currently active on each thread to detect class boundaries. */
    private static final ThreadLocal<Class<?>> activeTestClass = new ThreadLocal<>();
    @Getter
    private static ITestResult iTestResult;
    private static long executionStartTime;
    @Getter
    private static boolean isReportPortalEnabled;
    private ITestNGService reportPortalTestNGService;
    private boolean isReportPortalEnabledForListener;

    private record TestExecutionCounts(int passed, int failed, int skipped, int flaky) {
        int finalPassed() {
            return passed + flaky;
        }
    }

    public static ProjectStructureManager.RunType identifyRunType() {
        return ProjectStructureManager.identifyRunType();
    }

    public static void engineSetup(ProjectStructureManager.RunType runType) {
        ExecutionLifecycleHelper.engineSetup(runType);
    }

    /**
     * gets invoked before TestNG proceeds with invoking any other listener.
     */
    @Override
    public void onExecutionStart() {
        Reporter.setEscapeHtml(false);
        resetTrackedResultState();
        resetThreadLocalLifecycleState(false);
        engineSetup(ProjectStructureManager.RunType.TESTNG);
        initializeReportPortalIfEnabled();
    }

    private void initializeReportPortalIfEnabled() {
        String rpEnableValue = ThreadLocalPropertiesManager.getProperty("rp.enable");
        TestNGListener.isReportPortalEnabled = rpEnableValue != null && Boolean.parseBoolean(rpEnableValue.trim());
        this.isReportPortalEnabledForListener = TestNGListener.isReportPortalEnabled;
        if (this.isReportPortalEnabledForListener) {
            this.reportPortalTestNGService = REPORT_PORTAL_SERVICE.get();
            if (REPORT_PORTAL_INSTANCES.incrementAndGet() > 1) {
                String warning = "More than one ReportPortal listener is configured.";
                ReportManagerHelper.logDiscrete(warning, Level.WARN);
            }
            ReportManagerHelper.logDiscrete("Starting ReportPortal reporting.", Level.DEBUG);
            this.reportPortalTestNGService.startLaunch();
        }
    }

    /**
     * Implementations of this interface will gain access to the {@link XmlSuite} object and thus let
     * users be able to alter a suite or a test based on their own needs.
     *
     * @param suites - The list of {@link XmlSuite}s that are part of the current execution.
     */
    @Override
    public void alter(List<XmlSuite> suites) {
        switch (TestNGListener.identifyRunType()) {
            case TESTNG -> TestNGListenerHelper.configureTestNGProperties(suites);
            case CUCUMBER -> CucumberHelper.configureCucumberProperties(suites);
        }
        TestNGListenerHelper.attachConfigurationHelperClass(suites);
        //All alterations should be finalized before duplicating the
        //test suites for cross browser execution
        TestNGListenerHelper.configureCrossBrowserExecution(suites);
    }

    /**
     * This method is invoked before the SuiteRunner starts.
     *
     * @param suite The suite
     */
    @Override
    public void onStart(ISuite suite) {
        TestNGListenerHelper.setTotalNumberOfTests(suite);
        executionStartTime = System.currentTimeMillis();
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.startTestSuite(suite);
    }

    @Override
    public void onFinish(ISuite suite) {
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.finishTestSuite(suite);
    }

    @Override
    public void onStart(ITestContext testContext) {
        // Defensive assignment: in some Surefire/TestNG execution paths (for example,
        // method-filtered runs) annotation transformation may not attach the retry analyzer.
        // Binding here ensures all discovered test methods in this context carry retries.
        Arrays.stream(testContext.getAllTestMethods())
                .filter(ITestNGMethod::isTest)
                .forEach(method -> method.setRetryAnalyzerClass(RetryAnalyzer.class));
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.startTest(testContext);
    }

    @Override
    public void onFinish(ITestContext testContext) {
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.finishTest(testContext);
    }

    @Override
    public void beforeConfiguration(ITestResult testResult) {
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.startConfiguration(testResult);
    }

    @Override
    public void onConfigurationFailure(ITestResult testResult) {
        if (testResult.getThrowable() != null) {
            TestNGListenerHelper.setPendingConfigFailure(testResult.getThrowable());
        }
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.sendReportPortalMsg(testResult);
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.finishTestMethod(ItemStatus.FAILED, testResult);
    }

    @Override
    public void onConfigurationSuccess(ITestResult testResult) {
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.finishTestMethod(ItemStatus.PASSED, testResult);
    }

    @Override
    public void onConfigurationSkip(ITestResult testResult) {
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.startConfiguration(testResult);
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.finishTestMethod(ItemStatus.SKIPPED, testResult);
    }

    @Override
    public void onTestFailedButWithinSuccessPercentage(ITestResult result) {
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.finishTestMethod(ItemStatus.FAILED, result);
    }

    /**
     * This method will be invoked by TestNG to give you a chance to modify a TestNG annotation read
     * from your test classes. You can change the values you need by calling any of the setters on the
     * ITest interface.
     *
     * <p>Note that only one of the three parameters testClass, testConstructor and testMethod will be
     * non-null.
     *
     * @param annotation      The annotation that was read from your test class.
     * @param testClass       If the annotation was found on a class, this parameter represents this class
     *                        (null otherwise).
     * @param testConstructor If the annotation was found on a constructor, this parameter represents
     *                        this constructor (null otherwise).
     * @param testMethod      If the annotation was found on a method, this parameter represents this
     *                        method (null otherwise).
     */
    @Override
    public void transform(ITestAnnotation annotation, Class testClass, Constructor testConstructor, Method testMethod) {
        annotation.setRetryAnalyzer(RetryAnalyzer.class);
    }

    @Override
    public List<IMethodInstance> intercept(List<IMethodInstance> methods, ITestContext context) {
        // Additional safety net: method interception runs right before invocation ordering.
        // Keeping this hook alongside onStart() guarantees retries even when methods are
        // dynamically filtered/reordered by TestNG or other interceptors.
        methods.stream()
                .map(IMethodInstance::getMethod)
                .filter(ITestNGMethod::isTest)
                .forEach(method -> method.setRetryAnalyzerClass(RetryAnalyzer.class));
        return methods;
    }

    /**
     * A listener that gets invoked before a method is invoked by TestNG. This listener will
     * be invoked for configuration and test methods irrespective of whether they pass/fail or get
     * skipped. This listener invocation can be disabled for SKIPPED tests through one of the below
     * mechanisms:
     *
     * <ul>
     *   <li>Command line parameter <code>alwaysRunListeners</code>
     *   <li>Build tool
     *   <li>Via {@code TestNG.alwaysRunListeners(false)}
     * </ul>
     */
    @Override
    public void beforeInvocation(IInvokedMethod method, ITestResult iTestResult, ITestContext iTestContext) {
        ReportContext.start(toTestExecutionInfo(iTestResult));
        ReportContext.setLogSink(log -> Reporter.log(log, false));
        ReportContext.setStatus(io.qameta.allure.model.Status.PASSED);
        var elapsedTime = System.currentTimeMillis() - executionStartTime;
        if (SHAFT.Properties.reporting.debugMode()) {
            ReportManager.logDiscrete("elapsedTime: " + elapsedTime + "ms");
        }
        if (elapsedTime >= SHAFT.Properties.testNG.testSuiteTimeout() * 60000) {
            throw new SkipException("Skipping method as the test suite has exceeded the defined timeout of " + SHAFT.Properties.testNG.testSuiteTimeout() + " minutes.");
        }
        if (method.isTestMethod()) {
            RetryAnalyzer.activateSupportingEvidenceCaptureForRetryAttempt();
        }
        // Clear per-thread property overrides only when a new test class begins its lifecycle
        // on a pooled thread.  Checking for class identity change prevents incorrectly clearing
        // overrides set by an earlier @BeforeClass method on the same class.
        if (method.isConfigurationMethod() && method.getTestMethod().isBeforeClassConfiguration()) {
            Class<?> incomingClass = method.getTestMethod().getRealClass();
            if (!incomingClass.equals(activeTestClass.get())) {
                Properties.clearForCurrentThread();
                activeTestClass.set(incomingClass);
            }
        }
        TestNGListenerHelper.setXmlTest(method.getTestMethod().getXmlTest());
        JiraHelper.prepareTestResultAttributes(method, iTestResult);
        TestNGListenerHelper.setTestName(iTestContext);
        TestNGListenerHelper.logTestInformation(iTestResult);
        TestNGListenerHelper.failFast(iTestResult);
        TestNGListenerHelper.skipTestsWithLinkedIssues(iTestResult);
        TestNGListener.iTestResult = iTestResult;
    }

    /**
     * A listener that gets invoked after a method is invoked by TestNG. This listener will
     * be invoked for configuration and test methods irrespective of whether they pass/fail or get
     * skipped. This listener invocation can be disabled for SKIPPED tests through one of the below
     * mechanisms:
     *
     * <ul>
     *   <li>Command line parameter <code>alwaysRunListeners</code>
     *   <li>Build tool
     *   <li>Via {@code TestNG.alwaysRunListeners(false)}
     * </ul>
     */
    @Override
    public void afterInvocation(IInvokedMethod iInvokedMethod, ITestResult iTestResult, ITestContext iTestContext) {
        ReportContext.setStatus(toAllureStatus(iTestResult));
        IssueReporter.updateTestStatusInCaseOfVerificationFailure(iTestResult);
        ReportContext.setStatus(toAllureStatus(iTestResult));
        IssueReporter.updateIssuesLog(iTestResult);
        TestNGListenerHelper.updateTestMethods(iTestResult);
//            TestNGListenerHelper.updateConfigurationMethodLogs(iTestResult);
        TestNGListenerHelper.logFinishedTestInformation(iTestResult);
        ReportManagerHelper.setDiscreteLogging(SHAFT.Properties.reporting.alwaysLogDiscreetly());
        if (iInvokedMethod.isTestMethod()) {
            RetryAnalyzer.restoreSupportingEvidenceCaptureForRetryAttempt();
        }
        // Clean up thread-local state when the last after-class config method finishes
        if (iInvokedMethod.isConfigurationMethod() && iInvokedMethod.getTestMethod().isAfterClassConfiguration()) {
            activeTestClass.remove();
            TestNGListenerHelper.cleanup();
        }
        ReportContext.clear();
    }

    /**
     * gets invoked at the very last (after attachTestArtifacts generation phase), before TestNG exits the JVM.
     */
    @Override
    public void onExecutionFinish() {
        try {
            TestExecutionCounts counts = getDeduplicatedTestExecutionCounts();
            ExecutionLifecycleHelper.engineTearDown(executionStartTime,
                    new ExecutionCountsTracker.Counts(counts.passed(), counts.failed(), counts.skipped(), counts.flaky()));
            if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.finishLaunch();
        } finally {
            resetTrackedResultState();
            resetThreadLocalLifecycleState(true);
        }
    }

    private static TestExecutionInfo toTestExecutionInfo(ITestResult testResult) {
        if (testResult == null || testResult.getMethod() == null) {
            return new TestExecutionInfo("unknown", "", "", "", "", null, null, false);
        }
        ITestNGMethod testMethod = testResult.getMethod();
        Method javaMethod = testMethod.getConstructorOrMethod() == null
                ? null
                : testMethod.getConstructorOrMethod().getMethod();
        String className = testMethod.getTestClass() == null ? "" : testMethod.getTestClass().getName();
        String methodName = testMethod.getMethodName();
        String stableId = className + "." + methodName;
        return new TestExecutionInfo(stableId, className, methodName, methodName,
                testMethod.getDescription(), javaMethod, testResult.getThrowable(), testResult.wasRetried());
    }

    private static io.qameta.allure.model.Status toAllureStatus(ITestResult testResult) {
        if (testResult == null) {
            return io.qameta.allure.model.Status.PASSED;
        }
        return switch (testResult.getStatus()) {
            case ITestResult.FAILURE -> io.qameta.allure.model.Status.FAILED;
            case ITestResult.SKIP -> io.qameta.allure.model.Status.SKIPPED;
            default -> io.qameta.allure.model.Status.PASSED;
        };
    }

    private static TestExecutionCounts getDeduplicatedTestExecutionCounts() {
        ExecutionCountsTracker.Counts counts = countsTracker.snapshot();
        if (counts.finalPassed() + counts.failed() + counts.skipped() > 0) {
            return new TestExecutionCounts(counts.passed(), counts.failed(), counts.skipped(), counts.flaky());
        }
        return getDeduplicatedTestExecutionCounts(passedTests, failedTests, skippedTests);
    }

    private static void resetTrackedResultState() {
        synchronized (passedTests) {
            passedTests.clear();
        }
        synchronized (failedTests) {
            failedTests.clear();
        }
        synchronized (skippedTests) {
            skippedTests.clear();
        }
        countsTracker.clear();
    }

    private static void resetThreadLocalLifecycleState(boolean clearProperties) {
        activeTestClass.remove();
        TestNGListenerHelper.cleanup();
        ReportContext.clear();
        if (clearProperties) {
            Properties.clearForCurrentThread();
        }
    }

    private static TestExecutionCounts getDeduplicatedTestExecutionCounts(
            List<ITestNGMethod> passedMethods,
            List<ITestNGMethod> failedMethods,
            List<ITestNGMethod> skippedMethods) {
        // Deduplicate test method counts to remove retry-inflated numbers.
        // A method that failed on early attempts but eventually passed is counted as FLAKY.
        // Categories are mutually exclusive: passed | failed | skipped | flaky.
        Set<ITestNGMethod> passedSet = new HashSet<>(passedMethods);
        Set<ITestNGMethod> flakySet = failedMethods.stream()
                .filter(passedSet::contains)
                .collect(Collectors.toCollection(HashSet::new));
        Set<ITestNGMethod> failedSet = failedMethods.stream()
                .filter(m -> !passedSet.contains(m))
                .collect(Collectors.toCollection(HashSet::new));
        int uniquePassed = passedSet.size() - flakySet.size();
        int uniqueFailed = failedSet.size();
        int uniqueFlaky = flakySet.size();
        Set<ITestNGMethod> resolvedSet = new HashSet<>();
        resolvedSet.addAll(passedSet);
        resolvedSet.addAll(failedSet);
        int uniqueSkipped = (int) skippedMethods.stream().filter(m -> !resolvedSet.contains(m)).distinct().count();
        return new TestExecutionCounts(uniquePassed, uniqueFailed, uniqueSkipped, uniqueFlaky);
    }

    @Override
    public void onTestStart(ITestResult testResult) {
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.startTestMethod(testResult);
    }

    @Override
    public void onTestSuccess(ITestResult testResult) {
        passedTests.add(testResult.getMethod());
        countsTracker.recordPassed(toTestExecutionInfo(testResult));
        ExecutionSummaryReport.casesDetailsIncrement(TestNGListenerHelper.getTmsLinkAnnotationValue(testResult), testResult.getMethod().getQualifiedName().replace("." + testResult.getMethod().getMethodName(), ""),
                testResult.getMethod().getMethodName(), testResult.getMethod().getDescription(), "",
                ExecutionSummaryReport.StatusIcon.PASSED.getValue() + ExecutionSummaryReport.Status.PASSED.name(), TestNGListenerHelper.getIssueAnnotationValue(testResult));
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.finishTestMethod(ItemStatus.PASSED, testResult);
    }

    @Override
    public void onTestFailure(ITestResult testResult) {
        failedTests.add(testResult.getMethod());
        countsTracker.recordFailed(toTestExecutionInfo(testResult));
        ExecutionSummaryReport.casesDetailsIncrement(TestNGListenerHelper.getTmsLinkAnnotationValue(testResult), testResult.getMethod().getQualifiedName().replace("." + testResult.getMethod().getMethodName(), ""),
                testResult.getMethod().getMethodName(), testResult.getMethod().getDescription(), testResult.getThrowable().getMessage(),
                ExecutionSummaryReport.StatusIcon.FAILED.getValue() + ExecutionSummaryReport.Status.FAILED.name(), TestNGListenerHelper.getIssueAnnotationValue(testResult));
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.sendReportPortalMsg(testResult);
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.finishTestMethod(ItemStatus.FAILED, testResult);
    }

    @Override
    public void onTestSkipped(ITestResult testResult) {
        skippedTests.add(testResult.getMethod());
        countsTracker.recordSkipped(toTestExecutionInfo(testResult));
        String throwableMessage = testResult.getThrowable() != null ? testResult.getThrowable().getMessage() : "";
        ExecutionSummaryReport.casesDetailsIncrement(TestNGListenerHelper.getTmsLinkAnnotationValue(testResult), testResult.getMethod().getQualifiedName().replace("." + testResult.getMethod().getMethodName(), ""),
                testResult.getMethod().getMethodName(), testResult.getMethod().getDescription(), throwableMessage,
                ExecutionSummaryReport.StatusIcon.SKIPPED.getValue() + ExecutionSummaryReport.Status.SKIPPED.name(), TestNGListenerHelper.getIssueAnnotationValue(testResult));
        if (this.isReportPortalEnabledForListener) this.reportPortalTestNGService.finishTestMethod(ItemStatus.SKIPPED, testResult);
    }
}
