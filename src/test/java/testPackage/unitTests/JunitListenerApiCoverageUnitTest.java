package testPackage.unitTests;

import com.shaft.api.RequestBuilder;
import com.shaft.gui.internal.image.AnimatedGifManager;
import com.shaft.gui.internal.video.RecordManager;
import com.shaft.listeners.JunitListener;
import com.shaft.listeners.TestNGListener;
import com.shaft.listeners.internal.JiraHelper;
import com.shaft.listeners.internal.JunitListenerHelper;
import com.shaft.tools.internal.FirestoreRestClient;
import com.shaft.tools.internal.security.GoogleTink;
import com.shaft.tools.io.internal.AllureManager;
import com.shaft.tools.io.internal.ApiPerformanceExecutionReport;
import com.shaft.tools.io.internal.ExecutionSummaryReport;
import com.shaft.tools.io.internal.ProjectStructureManager;
import com.shaft.tools.io.internal.RealtimeReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.junit.platform.engine.TestExecutionResult;
import org.junit.platform.engine.TestDescriptor;
import org.junit.platform.engine.UniqueId;
import org.junit.platform.engine.support.descriptor.MethodSource;
import org.junit.platform.launcher.Launcher;
import org.junit.platform.launcher.LauncherSession;
import org.junit.platform.launcher.TestExecutionListener;
import org.junit.platform.launcher.TestIdentifier;
import org.junit.platform.launcher.TestPlan;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.when;

public class JunitListenerApiCoverageUnitTest {

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() throws Exception {
        resetListenerState();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() throws Exception {
        resetListenerState();
    }

    @Test
    public void launcherSessionExecutionListenerShouldHandleLifecycleEvents() throws Exception {
        LauncherSession session = mock(LauncherSession.class);
        Launcher launcher = mock(Launcher.class);
        when(session.getLauncher()).thenReturn(launcher);

        AtomicReference<TestExecutionListener> capturedListener = new AtomicReference<>();
        Mockito.doAnswer(invocation -> {
            Object argument = invocation.getArgument(0);
            if (argument instanceof TestExecutionListener[] listeners) {
                capturedListener.set(listeners[0]);
            } else if (argument instanceof TestExecutionListener listener) {
                capturedListener.set(listener);
            }
            return null;
        }).when(launcher).registerTestExecutionListeners(any(TestExecutionListener[].class));

        TestPlan testPlan = mock(TestPlan.class);
        TestIdentifier rootNode = mockContainerIdentifier("[engine:junit-jupiter]", "JUnit Jupiter");
        TestIdentifier methodTest = mockMethodIdentifier(
                "[engine:junit-jupiter]/[class:com.example.SampleTest]/[method:sampleMethod()]",
                "sampleMethod()",
                "com.example.SampleTest",
                "sampleMethod");

        when(testPlan.getRoots()).thenReturn(java.util.Set.of(rootNode));
        when(testPlan.getChildren(rootNode)).thenReturn(java.util.Set.of(methodTest));
        when(testPlan.getChildren(methodTest)).thenReturn(java.util.Set.of());

        TestIdentifier failedTest = mockMethodIdentifier(
                "[engine:junit-jupiter]/[class:com.example.SampleTest]/[method:failedMethod()]",
                "failedMethod()",
                "com.example.SampleTest",
                "failedMethod");
        TestIdentifier abortedTest = mockMethodIdentifier(
                "[engine:junit-jupiter]/[class:com.example.SampleTest]/[method:abortedMethod()]",
                "abortedMethod()",
                "com.example.SampleTest",
                "abortedMethod");
        TestIdentifier skippedTest = mockMethodIdentifier(
                "[engine:junit-jupiter]/[class:com.example.SampleTest]/[method:skippedMethod()]",
                "skippedMethod()",
                "com.example.SampleTest",
                "skippedMethod");

        try (MockedStatic<TestNGListener> testNgListenerMock = Mockito.mockStatic(TestNGListener.class);
             MockedStatic<JunitListenerHelper> junitListenerHelperMock = Mockito.mockStatic(JunitListenerHelper.class);
             MockedStatic<RealtimeReporter> realtimeReporterMock = Mockito.mockStatic(RealtimeReporter.class);
             MockedStatic<ReportManagerHelper> reportManagerHelperMock = Mockito.mockStatic(ReportManagerHelper.class);
             MockedStatic<JiraHelper> jiraHelperMock = Mockito.mockStatic(JiraHelper.class);
             MockedStatic<GoogleTink> googleTinkMock = Mockito.mockStatic(GoogleTink.class);
             MockedStatic<AllureManager> allureManagerMock = Mockito.mockStatic(AllureManager.class);
             MockedStatic<ExecutionSummaryReport> executionSummaryReportMock = Mockito.mockStatic(ExecutionSummaryReport.class);
             MockedStatic<RequestBuilder> requestBuilderMock = Mockito.mockStatic(RequestBuilder.class);
             MockedStatic<ApiPerformanceExecutionReport> apiPerformanceExecutionReportMock = Mockito.mockStatic(ApiPerformanceExecutionReport.class);
             MockedStatic<FirestoreRestClient> firestoreRestClientMock = Mockito.mockStatic(FirestoreRestClient.class);
             MockedStatic<RecordManager> recordManagerMock = Mockito.mockStatic(RecordManager.class);
             MockedStatic<AnimatedGifManager> animatedGifManagerMock = Mockito.mockStatic(AnimatedGifManager.class)) {

            requestBuilderMock.when(RequestBuilder::getPerformanceData).thenReturn(Map.of());

            new JunitListener().launcherSessionOpened(session);
            TestExecutionListener executionListener = capturedListener.get();
            Assert.assertNotNull(executionListener, "A TestExecutionListener should be registered on first session open.");
            Assert.assertEquals(executionListener.getClass().getName(), "com.shaft.listeners.JunitListener$1");

            executionListener.testPlanExecutionStarted(testPlan);
            executionListener.executionStarted(methodTest);
            executionListener.executionFinished(methodTest, TestExecutionResult.successful());
            executionListener.executionFinished(failedTest, TestExecutionResult.failed(new RuntimeException("boom")));
            executionListener.executionFinished(abortedTest, TestExecutionResult.aborted(new IllegalStateException("aborted")));
            executionListener.executionSkipped(skippedTest, "skip reason");
            executionListener.testPlanExecutionFinished(testPlan);

            Assert.assertEquals(((List<?>) getStaticField("passedTests")).size(), 1);
            Assert.assertEquals(((List<?>) getStaticField("failedTests")).size(), 2);
            Assert.assertEquals(((List<?>) getStaticField("skippedTests")).size(), 1);

            testNgListenerMock.verify(() -> TestNGListener.engineSetup(ProjectStructureManager.RunType.JUNIT));
            realtimeReporterMock.verify(() -> RealtimeReporter.initialize("JUnit Test Run"));
            realtimeReporterMock.verify(() -> RealtimeReporter.onExecutionFinished());
            junitListenerHelperMock.verify(() -> JunitListenerHelper.setTestName(methodTest));
            junitListenerHelperMock.verify(() -> JunitListenerHelper.logTestInformation(methodTest));
            reportManagerHelperMock.verify(() -> ReportManagerHelper.logEngineClosure());
            Assert.assertFalse(JunitListener.getIsLastFinishedTestOK(), "A failure/abort/skipped sequence should leave last finished test status as false.");
        }
    }

    @Test
    public void launcherSessionShouldNotRegisterExecutionListenerWhenEngineAlreadyReady() throws Exception {
        setStaticField("isEngineReady", true);

        LauncherSession session = mock(LauncherSession.class);
        Launcher launcher = mock(Launcher.class);
        when(session.getLauncher()).thenReturn(launcher);

        new JunitListener().launcherSessionOpened(session);

        Mockito.verify(launcher, never()).registerTestExecutionListeners(any(TestExecutionListener[].class));
    }

    @Test
    public void privateHelpersShouldHandleMethodSourceAndNonTestBranches() throws Exception {
        JunitListener listener = new JunitListener();

        Method junitTestId = JunitListener.class.getDeclaredMethod("junitTestId", TestIdentifier.class);
        junitTestId.setAccessible(true);

        TestIdentifier methodSourceIdentifier = mockMethodIdentifier(
                "[engine:junit-jupiter]/[class:com.example.SampleTest]/[method:sampleMethod()]",
                "sampleMethod()",
                "com.example.SampleTest",
                "sampleMethod");
        String idFromMethodSource = (String) junitTestId.invoke(null, methodSourceIdentifier);
        Assert.assertEquals(idFromMethodSource, "com.example.SampleTest#sampleMethod");

        TestIdentifier displayNameOnlyIdentifier = mockContainerIdentifier(
                "[engine:junit-jupiter]/[class:com.example.SampleTest]",
                "displayNameOnly");
        String idFromDisplayName = (String) junitTestId.invoke(null, displayNameOnlyIdentifier);
        Assert.assertEquals(idFromDisplayName, "#displayNameOnly");

        Method appendToExecutionSummaryReport = JunitListener.class.getDeclaredMethod(
                "appendToExecutionSummaryReport",
                TestIdentifier.class,
                String.class,
                ExecutionSummaryReport.StatusIcon.class,
                ExecutionSummaryReport.Status.class);
        appendToExecutionSummaryReport.setAccessible(true);

        TestIdentifier nonTestIdentifier = mockContainerIdentifier(
                "[engine:junit-jupiter]/[class:com.example.SampleTest]",
                "container");

        try (MockedStatic<ExecutionSummaryReport> executionSummaryReportMock = Mockito.mockStatic(ExecutionSummaryReport.class)) {
            appendToExecutionSummaryReport.invoke(listener, nonTestIdentifier, "ignored", ExecutionSummaryReport.StatusIcon.SKIPPED, ExecutionSummaryReport.Status.SKIPPED);
            executionSummaryReportMock.verifyNoInteractions();

            appendToExecutionSummaryReport.invoke(listener, methodSourceIdentifier, "failure", ExecutionSummaryReport.StatusIcon.FAILED, ExecutionSummaryReport.Status.FAILED);
            executionSummaryReportMock.verify(() -> ExecutionSummaryReport.casesDetailsIncrement(
                    anyString(), anyString(), anyString(), anyString(), anyString(), anyString(), anyString()));
        }
    }

    private static TestIdentifier mockMethodIdentifier(String uniqueId, String displayName, String className, String methodName) {
        TestIdentifier testIdentifier = mock(TestIdentifier.class);
        when(testIdentifier.isTest()).thenReturn(true);
        when(testIdentifier.getUniqueId()).thenReturn(uniqueId);
        when(testIdentifier.getDisplayName()).thenReturn(displayName);
        when(testIdentifier.getType()).thenReturn(TestDescriptor.Type.TEST);
        when(testIdentifier.getLegacyReportingName()).thenReturn(displayName);
        when(testIdentifier.getSource()).thenReturn(Optional.of(MethodSource.from(className, methodName)));
        when(testIdentifier.getUniqueIdObject()).thenReturn(UniqueId.parse(uniqueId));
        return testIdentifier;
    }

    private static TestIdentifier mockContainerIdentifier(String uniqueId, String displayName) {
        TestIdentifier testIdentifier = mock(TestIdentifier.class);
        when(testIdentifier.isTest()).thenReturn(false);
        when(testIdentifier.getUniqueId()).thenReturn(uniqueId);
        when(testIdentifier.getDisplayName()).thenReturn(displayName);
        when(testIdentifier.getType()).thenReturn(TestDescriptor.Type.CONTAINER);
        when(testIdentifier.getLegacyReportingName()).thenReturn(displayName);
        when(testIdentifier.getSource()).thenReturn(Optional.empty());
        when(testIdentifier.getUniqueIdObject()).thenReturn(UniqueId.parse(uniqueId));
        return testIdentifier;
    }

    @SuppressWarnings("unchecked")
    private static void resetListenerState() throws Exception {
        ((List<TestIdentifier>) getStaticField("passedTests")).clear();
        ((List<TestIdentifier>) getStaticField("failedTests")).clear();
        ((List<TestIdentifier>) getStaticField("skippedTests")).clear();
        setStaticField("executionStartTime", 0L);
        setStaticField("isEngineReady", false);
        setStaticField("isLastFinishedTestOK", true);
    }

    private static Object getStaticField(String fieldName) throws Exception {
        Field field = JunitListener.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        return field.get(null);
    }

    private static void setStaticField(String fieldName, Object value) throws Exception {
        Field field = JunitListener.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(null, value);
    }
}
