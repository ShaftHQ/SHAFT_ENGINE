package testPackage.unitTests;

import com.epam.reportportal.listeners.ItemStatus;
import com.epam.reportportal.testng.ITestNGService;
import com.shaft.listeners.TestNGListener;
import com.shaft.listeners.internal.ExecutionCountsTracker;
import com.shaft.listeners.internal.ExecutionLifecycleHelper;
import com.shaft.listeners.internal.JiraHelper;
import com.shaft.listeners.internal.RetryAnalyzer;
import com.shaft.listeners.internal.TestNGListenerHelper;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.internal.ExecutionSummaryReport;
import com.shaft.tools.io.internal.IssueReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.IInvokedMethod;
import org.testng.IMethodInstance;
import org.testng.ITestContext;
import org.testng.ITestClass;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNull;

@Test(singleThreaded = true)
public class TestNGListenerCoverageUnitTest {

    @AfterMethod(alwaysRun = true)
    public void afterMethod() throws Exception {
        setReportPortalEnabled(false);
        TestNGListenerHelper.setPendingConfigFailure(null);
        com.shaft.properties.internal.Properties.clearForCurrentThread();
        System.clearProperty("shaft.shard");
    }

    @Test
    public void interceptWithoutShardPropertyReturnsMethodsUnchanged() {
        TestNGListener listener = new TestNGListener();
        List<IMethodInstance> methods = List.of(
                methodInstance("testPackage.unitTests.TestNGListenerCoverageUnitTest", "alpha", true),
                methodInstance("testPackage.unitTests.TestNGListenerCoverageUnitTest", "beta", true));

        List<IMethodInstance> result = listener.intercept(methods, Mockito.mock(ITestContext.class));

        assertEquals(result.size(), 2);
    }

    @Test
    public void interceptFiltersOutMethodsNotInTheRequestedShard() {
        System.setProperty("shaft.shard", "1/2");
        TestNGListener listener = new TestNGListener();
        List<IMethodInstance> methods = new ArrayList<>();
        for (int i = 0; i < 20; i++) {
            methods.add(methodInstance("testPackage.unitTests.TestNGListenerCoverageUnitTest", "test" + i, true));
        }

        List<IMethodInstance> shardOne = listener.intercept(new ArrayList<>(methods), Mockito.mock(ITestContext.class));
        System.setProperty("shaft.shard", "2/2");
        List<IMethodInstance> shardTwo = listener.intercept(new ArrayList<>(methods), Mockito.mock(ITestContext.class));

        Assert.assertTrue(shardOne.size() < methods.size(), "Shard 1/2 should drop some methods");
        Assert.assertTrue(shardTwo.size() < methods.size(), "Shard 2/2 should drop some methods");
        assertEquals(shardOne.size() + shardTwo.size(), methods.size(),
                "The union of both shards must equal the full method list with no overlap");
    }

    @Test
    public void interceptNeverFiltersOutConfigurationMethods() {
        System.setProperty("shaft.shard", "1/4");
        TestNGListener listener = new TestNGListener();
        List<IMethodInstance> methods = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            methods.add(methodInstance("testPackage.unitTests.TestNGListenerCoverageUnitTest", "config" + i, false));
        }

        List<IMethodInstance> result = listener.intercept(methods, Mockito.mock(ITestContext.class));

        assertEquals(result.size(), 10, "Non-test (configuration) methods must never be shard-filtered.");
    }

    private static IMethodInstance methodInstance(String className, String methodName, boolean isTest) {
        IMethodInstance instance = Mockito.mock(IMethodInstance.class);
        ITestNGMethod method = createTestMethod(methodName);
        ITestClass testClass = Mockito.mock(ITestClass.class);
        Mockito.when(testClass.getName()).thenReturn(className);
        Mockito.when(method.getTestClass()).thenReturn(testClass);
        Mockito.when(method.isTest()).thenReturn(isTest);
        Mockito.when(instance.getMethod()).thenReturn(method);
        return instance;
    }

    @Test
    public void onConfigurationFailureShouldStorePendingThrowableWhenThrowableExists() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult testResult = Mockito.mock(ITestResult.class);
        RuntimeException throwable = new RuntimeException("config failure");
        Mockito.when(testResult.getThrowable()).thenReturn(throwable);

        setReportPortalEnabled(false);
        listener.onConfigurationFailure(testResult);

        Throwable pendingFailure = TestNGListenerHelper.getAndClearPendingConfigFailure();
        assertEquals(pendingFailure, throwable);
    }

    @Test
    public void onExecutionStartShouldNotInitializeReportPortalWhenDisabled() throws Exception {
        ThreadLocalPropertiesManager.setProperty("rp.enable", "false");
        TestNGListener listener = new TestNGListener();

        Method initializeReportPortal = TestNGListener.class.getDeclaredMethod("initializeReportPortalIfEnabled");
        initializeReportPortal.setAccessible(true);
        initializeReportPortal.invoke(listener);

        assertNull(getReportPortalService(listener));
    }

    @Test
    public void onExecutionStartShouldResetTrackedResultState() throws Exception {
        TestNGListener listener = new TestNGListener();
        TrackedResultState originalState = captureTrackedResultState();
        try {
            getTrackedMethods("passedTests").add(createTestMethod("stalePassed"));
            getTrackedMethods("failedTests").add(createTestMethod("staleFailed"));
            getTrackedMethods("skippedTests").add(createTestMethod("staleSkipped"));
            ThreadLocalPropertiesManager.setProperty("rp.enable", "false");

            try (MockedStatic<ExecutionLifecycleHelper> ignoredExecutionLifecycle =
                         Mockito.mockStatic(ExecutionLifecycleHelper.class)) {
                listener.onExecutionStart();
            }

            assertEquals(getTrackedMethods("passedTests").size(), 0);
            assertEquals(getTrackedMethods("failedTests").size(), 0);
            assertEquals(getTrackedMethods("skippedTests").size(), 0);
        } finally {
            restoreTrackedResultState(originalState);
        }
    }

    @Test
    public void onExecutionFinishShouldClearThreadLocalLifecycleState() throws Exception {
        TestNGListener listener = new TestNGListener();
        TrackedResultState originalState = captureTrackedResultState();
        String originalDisableLogging = ThreadLocalPropertiesManager.getProperty("disableLogging");
        String threadLocalOverride = "true".equalsIgnoreCase(originalDisableLogging) ? "false" : "true";
        try {
            ThreadLocalPropertiesManager.setProperty("disableLogging", threadLocalOverride);

            try (MockedStatic<ExecutionLifecycleHelper> ignoredExecutionLifecycle =
                         Mockito.mockStatic(ExecutionLifecycleHelper.class)) {
                listener.onExecutionFinish();
            }

            assertEquals(ThreadLocalPropertiesManager.getProperty("disableLogging"), originalDisableLogging);
        } finally {
            restoreTrackedResultState(originalState);
            com.shaft.properties.internal.Properties.clearForCurrentThread();
        }
    }

    @Test
    public void onExecutionFinishShouldFinishReportPortalLaunchWhenEngineTearDownFails() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestNGService reportPortalService = Mockito.mock(ITestNGService.class);
        AssertionError coverageFailure = new AssertionError("OpenAPI coverage below threshold");
        TrackedResultState originalState = captureTrackedResultState();
        try {
            setExecutionStartTime(System.currentTimeMillis());
            setReportPortalService(listener, reportPortalService);
            setReportPortalEnabled(listener, true);

            try (MockedStatic<ExecutionLifecycleHelper> executionLifecycleHelperMock =
                         Mockito.mockStatic(ExecutionLifecycleHelper.class)) {
                executionLifecycleHelperMock.when(() -> ExecutionLifecycleHelper.engineTearDown(
                                Mockito.anyLong(), Mockito.any(ExecutionCountsTracker.Counts.class)))
                        .thenThrow(coverageFailure);

                AssertionError thrown = Assert.expectThrows(AssertionError.class, listener::onExecutionFinish);

                assertEquals(thrown, coverageFailure);
                Mockito.verify(reportPortalService).finishLaunch();
            }
        } finally {
            setReportPortalEnabled(listener, false);
            setReportPortalEnabled(false);
            restoreTrackedResultState(originalState);
        }
    }

    @Test
    public void onConfigurationFailureShouldReportToReportPortalWhenEnabled() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult testResult = Mockito.mock(ITestResult.class);
        Mockito.when(testResult.getThrowable()).thenReturn(null);

        ITestNGService reportPortalService = Mockito.mock(ITestNGService.class);
        setReportPortalService(listener, reportPortalService);
        setReportPortalEnabled(listener, true);
        try {
            listener.onConfigurationFailure(testResult);
            Mockito.verify(reportPortalService).sendReportPortalMsg(testResult);
            Mockito.verify(reportPortalService).finishTestMethod(ItemStatus.FAILED, testResult);
        } finally {
            setReportPortalEnabled(false);
        }
    }

    @Test
    public void onConfigurationSuccessAndSkipShouldReportToReportPortalWhenEnabled() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult testResult = Mockito.mock(ITestResult.class);

        ITestNGService reportPortalService = Mockito.mock(ITestNGService.class);
        setReportPortalService(listener, reportPortalService);
        setReportPortalEnabled(listener, true);
        try {
            listener.onConfigurationSuccess(testResult);
            listener.onConfigurationSkip(testResult);

            Mockito.verify(reportPortalService).finishTestMethod(ItemStatus.PASSED, testResult);
            Mockito.verify(reportPortalService).startConfiguration(testResult);
            Mockito.verify(reportPortalService).finishTestMethod(ItemStatus.SKIPPED, testResult);
        } finally {
            setReportPortalEnabled(false);
        }
    }

    @Test
    public void onTestFailedButWithinSuccessPercentageShouldReportFailureWhenEnabled() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult testResult = Mockito.mock(ITestResult.class);

        ITestNGService reportPortalService = Mockito.mock(ITestNGService.class);
        setReportPortalService(listener, reportPortalService);
        setReportPortalEnabled(listener, true);
        try {
            listener.onTestFailedButWithinSuccessPercentage(testResult);
            Mockito.verify(reportPortalService).finishTestMethod(ItemStatus.FAILED, testResult);
            assertNull(TestNGListenerHelper.getAndClearPendingConfigFailure());
        } finally {
            setReportPortalEnabled(false);
        }
    }

    @Test
    public void onTestFailureShouldReportToReportPortalWhenEnabled() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult testResult = createTestResult("failingTest", new RuntimeException("failed"));
        TrackedResultState originalState = captureTrackedResultState();

        ITestNGService reportPortalService = Mockito.mock(ITestNGService.class);
        setReportPortalService(listener, reportPortalService);
        setReportPortalEnabled(listener, true);
        try {
            listener.onTestFailure(testResult);
            Mockito.verify(reportPortalService).sendReportPortalMsg(testResult);
            Mockito.verify(reportPortalService).finishTestMethod(ItemStatus.FAILED, testResult);
        } finally {
            restoreTrackedResultState(originalState);
            setReportPortalEnabled(false);
        }
    }

    @Test
    public void onTestSkippedShouldHandleNullThrowableAndReportToReportPortalWhenEnabled() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult testResult = createTestResult("skippedTest", null);
        TrackedResultState originalState = captureTrackedResultState();

        ITestNGService reportPortalService = Mockito.mock(ITestNGService.class);
        setReportPortalService(listener, reportPortalService);
        setReportPortalEnabled(listener, true);
        try {
            listener.onTestSkipped(testResult);
            Mockito.verify(reportPortalService).finishTestMethod(ItemStatus.SKIPPED, testResult);
        } finally {
            restoreTrackedResultState(originalState);
            setReportPortalEnabled(false);
        }
    }

    @Test
    public void onTestSkippedShouldNotRecordExecutionSummaryRowForRetrySuppressedAttempt() throws Exception {
        // TestNG's TestInvoker forces the ITestResult status to SKIP (and sets wasRetried=true)
        // for every failed attempt that will be retried (see TestInvoker#handleInvocationResult).
        // Only the terminal attempt keeps its real FAILURE/SUCCESS status. Reporting a row here
        // for a retry-suppressed attempt is exactly the "retry-inflated" duplicate-entry bug from #3810.
        TestNGListener listener = new TestNGListener();
        ITestResult retrySuppressedAttempt = createTestResult("deterministicFailure", new RuntimeException("boom"));
        Mockito.when(retrySuppressedAttempt.wasRetried()).thenReturn(true);
        TrackedResultState originalState = captureTrackedResultState();
        try (MockedStatic<ExecutionSummaryReport> executionSummaryReportMock = Mockito.mockStatic(ExecutionSummaryReport.class)) {
            listener.onTestSkipped(retrySuppressedAttempt);

            executionSummaryReportMock.verify(() -> ExecutionSummaryReport.casesDetailsIncrement(
                    Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                    Mockito.anyString(), Mockito.anyString(), Mockito.anyString()), Mockito.never());
        } finally {
            restoreTrackedResultState(originalState);
        }
    }

    @Test
    public void onTestSkippedShouldRecordExecutionSummaryRowForGenuineSkip() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult genuineSkip = createTestResult("skippedDueToDependency", null);
        Mockito.when(genuineSkip.wasRetried()).thenReturn(false);
        TrackedResultState originalState = captureTrackedResultState();
        try (MockedStatic<ExecutionSummaryReport> executionSummaryReportMock = Mockito.mockStatic(ExecutionSummaryReport.class)) {
            listener.onTestSkipped(genuineSkip);

            executionSummaryReportMock.verify(() -> ExecutionSummaryReport.casesDetailsIncrement(
                    Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                    Mockito.anyString(), Mockito.eq(ExecutionSummaryReport.StatusIcon.SKIPPED.getValue() + ExecutionSummaryReport.Status.SKIPPED.name()),
                    Mockito.anyString()), Mockito.times(1));
        } finally {
            restoreTrackedResultState(originalState);
        }
    }

    @Test
    public void deterministicFailureWithRetriesShouldRecordExactlyOneExecutionSummaryRowForFinalFailure() throws Exception {
        // Reproduces #3810: retryMaximumNumberOfAttempts=2 against a test that fails identically
        // every attempt. TestNG invokes the method 3 times total; the first 2 failures are coerced
        // to SKIP+wasRetried=true by TestInvoker, only the 3rd (final) attempt keeps FAILURE status.
        TestNGListener listener = new TestNGListener();
        RuntimeException deterministicFailure = new RuntimeException("assertion failed: 2 != 3");
        ITestResult retryAttempt1 = createTestResult("deterministicFailure", deterministicFailure);
        Mockito.when(retryAttempt1.wasRetried()).thenReturn(true);
        ITestResult retryAttempt2 = createTestResult("deterministicFailure", deterministicFailure);
        Mockito.when(retryAttempt2.wasRetried()).thenReturn(true);
        ITestResult finalAttempt = createTestResult("deterministicFailure", deterministicFailure);
        Mockito.when(finalAttempt.wasRetried()).thenReturn(false);
        TrackedResultState originalState = captureTrackedResultState();

        try (MockedStatic<ExecutionSummaryReport> executionSummaryReportMock = Mockito.mockStatic(ExecutionSummaryReport.class)) {
            listener.onTestSkipped(retryAttempt1);
            listener.onTestSkipped(retryAttempt2);
            listener.onTestFailure(finalAttempt);

            executionSummaryReportMock.verify(() -> ExecutionSummaryReport.casesDetailsIncrement(
                    Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                    Mockito.anyString(), Mockito.anyString(), Mockito.anyString()), Mockito.times(1));
            executionSummaryReportMock.verify(() -> ExecutionSummaryReport.casesDetailsIncrement(
                    Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                    Mockito.anyString(), Mockito.eq(ExecutionSummaryReport.StatusIcon.FAILED.getValue() + ExecutionSummaryReport.Status.FAILED.name()),
                    Mockito.anyString()), Mockito.times(1));
        } finally {
            restoreTrackedResultState(originalState);
        }
    }

    @Test
    public void retriedThenPassedShouldRecordExactlyOneExecutionSummaryRowForFinalPass() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult retryAttempt = createTestResult("flakyThenPassed", new RuntimeException("transient"));
        Mockito.when(retryAttempt.wasRetried()).thenReturn(true);
        ITestResult finalPass = createTestResult("flakyThenPassed", null);
        TrackedResultState originalState = captureTrackedResultState();

        try (MockedStatic<ExecutionSummaryReport> executionSummaryReportMock = Mockito.mockStatic(ExecutionSummaryReport.class)) {
            listener.onTestSkipped(retryAttempt);
            listener.onTestSuccess(finalPass);

            executionSummaryReportMock.verify(() -> ExecutionSummaryReport.casesDetailsIncrement(
                    Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                    Mockito.anyString(), Mockito.eq(ExecutionSummaryReport.StatusIcon.PASSED.getValue() + ExecutionSummaryReport.Status.PASSED.name()),
                    Mockito.anyString()), Mockito.times(1));
            executionSummaryReportMock.verify(() -> ExecutionSummaryReport.casesDetailsIncrement(
                    Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                    Mockito.anyString(), Mockito.anyString(), Mockito.anyString()), Mockito.times(1));
        } finally {
            restoreTrackedResultState(originalState);
        }
    }

    @Test
    public void distinctDataProviderInvocationsShouldEachRecordTheirOwnExecutionSummaryRow() throws Exception {
        // Guards against the rejected fix's collapse bug: a Set<ITestNGMethod> guard would treat every
        // data-provider invocation of the same method (same ITestNGMethod reference) as a duplicate.
        TestNGListener listener = new TestNGListener();
        ITestResult invocationOne = createTestResult("dataDrivenTest", null);
        ITestResult invocationTwo = createTestResult("dataDrivenTest", null);
        TrackedResultState originalState = captureTrackedResultState();

        try (MockedStatic<ExecutionSummaryReport> executionSummaryReportMock = Mockito.mockStatic(ExecutionSummaryReport.class)) {
            listener.onTestSuccess(invocationOne);
            listener.onTestSuccess(invocationTwo);

            executionSummaryReportMock.verify(() -> ExecutionSummaryReport.casesDetailsIncrement(
                    Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                    Mockito.anyString(), Mockito.anyString(), Mockito.anyString()), Mockito.times(2));
        } finally {
            restoreTrackedResultState(originalState);
        }
    }

    @Test
    public void deduplicatedExecutionCountsShouldKeepFinalFailuresAlignedWithIssueSummary() throws Exception {
        ITestNGMethod passedOnly = createTestMethod("passedOnly");
        ITestNGMethod flakyThenPassed = createTestMethod("flakyThenPassed");
        ITestNGMethod failedOnly = createTestMethod("failedOnly");
        ITestNGMethod skippedOnly = createTestMethod("skippedOnly");

        Method getDeduplicatedTestExecutionCounts = TestNGListener.class.getDeclaredMethod(
                "getDeduplicatedTestExecutionCounts", List.class, List.class, List.class);
        getDeduplicatedTestExecutionCounts.setAccessible(true);
        Object counts = getDeduplicatedTestExecutionCounts.invoke(null,
                List.of(passedOnly, flakyThenPassed),
                List.of(flakyThenPassed, failedOnly, failedOnly),
                List.of(skippedOnly, failedOnly));

        assertEquals(invokeCount(counts, "passed"), 1);
        assertEquals(invokeCount(counts, "failed"), 1);
        assertEquals(invokeCount(counts, "skipped"), 1);
        assertEquals(invokeCount(counts, "flaky"), 1);
        assertEquals(invokeCount(counts, "finalPassed"), 2);
    }

    @Test
    public void invocationHooksShouldActivateAndRestoreRetryEvidenceForTestMethods() throws Exception {
        TestNGListener listener = new TestNGListener();
        IInvokedMethod invokedMethod = Mockito.mock(IInvokedMethod.class);
        ITestNGMethod testMethod = createTestMethod("retryEvidenceTest");
        ITestResult testResult = createTestResult("retryEvidenceTest", null);
        ITestContext testContext = Mockito.mock(ITestContext.class);
        Mockito.when(invokedMethod.isTestMethod()).thenReturn(true);
        Mockito.when(invokedMethod.isConfigurationMethod()).thenReturn(false);
        Mockito.when(invokedMethod.getTestMethod()).thenReturn(testMethod);
        setExecutionStartTime(System.currentTimeMillis());

        try (MockedStatic<RetryAnalyzer> retryAnalyzer = Mockito.mockStatic(RetryAnalyzer.class);
             MockedStatic<JiraHelper> ignoredJiraHelper = Mockito.mockStatic(JiraHelper.class);
             MockedStatic<TestNGListenerHelper> ignoredListenerHelper = Mockito.mockStatic(TestNGListenerHelper.class);
             MockedStatic<IssueReporter> ignoredIssueReporter = Mockito.mockStatic(IssueReporter.class);
             MockedStatic<ReportManagerHelper> ignoredReportManagerHelper = Mockito.mockStatic(ReportManagerHelper.class)) {
            listener.beforeInvocation(invokedMethod, testResult, testContext);
            listener.afterInvocation(invokedMethod, testResult, testContext);

            retryAnalyzer.verify(RetryAnalyzer::activateSupportingEvidenceCaptureForRetryAttempt);
            retryAnalyzer.verify(RetryAnalyzer::restoreSupportingEvidenceCaptureForRetryAttempt);
        }
    }

    @SuppressWarnings("unchecked")
    private static List<ITestNGMethod> getTrackedMethods(String fieldName) throws Exception {
        Field trackedMethodsField = TestNGListener.class.getDeclaredField(fieldName);
        trackedMethodsField.setAccessible(true);
        return (List<ITestNGMethod>) trackedMethodsField.get(null);
    }

    private static TrackedResultState captureTrackedResultState() throws Exception {
        return new TrackedResultState(
                new ArrayList<>(getTrackedMethods("passedTests")),
                new ArrayList<>(getTrackedMethods("failedTests")),
                new ArrayList<>(getTrackedMethods("skippedTests")));
    }

    private static void restoreTrackedResultState(TrackedResultState state) throws Exception {
        restoreTrackedMethods("passedTests", state.passedTests());
        restoreTrackedMethods("failedTests", state.failedTests());
        restoreTrackedMethods("skippedTests", state.skippedTests());
    }

    private static void restoreTrackedMethods(String fieldName, List<ITestNGMethod> methods) throws Exception {
        List<ITestNGMethod> trackedMethods = getTrackedMethods(fieldName);
        trackedMethods.clear();
        trackedMethods.addAll(methods);
    }

    private record TrackedResultState(List<ITestNGMethod> passedTests, List<ITestNGMethod> failedTests,
                                      List<ITestNGMethod> skippedTests) {
    }

    private static ITestResult createTestResult(String methodName, Throwable throwable) {
        ITestResult testResult = Mockito.mock(ITestResult.class);
        ITestNGMethod testMethod = createTestMethod(methodName);
        ITestClass testClass = Mockito.mock(ITestClass.class);

        Mockito.when(testResult.getMethod()).thenReturn(testMethod);
        Mockito.when(testResult.getTestClass()).thenReturn(testClass);
        Mockito.when(testClass.getName()).thenReturn("testPackage.unitTests.TestNGListenerCoverageUnitTest");
        Mockito.when(testResult.getThrowable()).thenReturn(throwable);

        return testResult;
    }

    private static ITestNGMethod createTestMethod(String methodName) {
        ITestNGMethod testMethod = Mockito.mock(ITestNGMethod.class);
        Mockito.when(testMethod.getMethodName()).thenReturn(methodName);
        Mockito.when(testMethod.getQualifiedName()).thenReturn("testPackage.unitTests.TestNGListenerCoverageUnitTest." + methodName);
        Mockito.when(testMethod.getDescription()).thenReturn("description");
        return testMethod;
    }

    private static int invokeCount(Object counts, String methodName) throws Exception {
        Method method = counts.getClass().getDeclaredMethod(methodName);
        method.setAccessible(true);
        return (int) method.invoke(counts);
    }

    private static void setReportPortalEnabled(boolean value) throws Exception {
        Field reportPortalEnabledField = TestNGListener.class.getDeclaredField("isReportPortalEnabled");
        reportPortalEnabledField.setAccessible(true);
        reportPortalEnabledField.set(null, value);
    }

    private static void setExecutionStartTime(long value) throws Exception {
        Field executionStartTimeField = TestNGListener.class.getDeclaredField("executionStartTime");
        executionStartTimeField.setAccessible(true);
        executionStartTimeField.set(null, value);
    }

    private static void setReportPortalEnabled(TestNGListener listener, boolean value) throws Exception {
        Field reportPortalEnabledField = TestNGListener.class.getDeclaredField("isReportPortalEnabledForListener");
        reportPortalEnabledField.setAccessible(true);
        reportPortalEnabledField.set(listener, value);
    }

    private static void setReportPortalService(TestNGListener listener, ITestNGService reportPortalService) throws Exception {
        Field reportPortalServiceField = TestNGListener.class.getDeclaredField("reportPortalTestNGService");
        reportPortalServiceField.setAccessible(true);
        reportPortalServiceField.set(listener, reportPortalService);
    }

    private static ITestNGService getReportPortalService(TestNGListener listener) throws Exception {
        Field reportPortalServiceField = TestNGListener.class.getDeclaredField("reportPortalTestNGService");
        reportPortalServiceField.setAccessible(true);
        return (ITestNGService) reportPortalServiceField.get(listener);
    }
}
