package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.JunitListener;
import com.shaft.listeners.TestNGListener;
import com.shaft.listeners.internal.JiraHelper;
import com.shaft.listeners.internal.JunitListenerHelper;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.internal.FirestoreRestClient;
import com.shaft.tools.internal.security.GoogleTink;
import com.shaft.tools.io.internal.AllureManager;
import com.shaft.tools.io.internal.ApiPerformanceExecutionReport;
import com.shaft.tools.io.internal.ExecutionSummaryReport;
import com.shaft.tools.io.internal.ProjectStructureManager;
import com.shaft.tools.io.internal.RealtimeReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.api.RequestBuilder;
import com.shaft.gui.internal.image.AnimatedGifManager;
import com.shaft.gui.internal.video.RecordManager;
import org.junit.platform.engine.TestDescriptor;
import org.junit.platform.engine.TestExecutionResult;
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
import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

@Test(singleThreaded = true)
public class JunitListenerApiCoverageUnitTest {

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() throws Exception {
        resetListenerState();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() throws Exception {
        resetListenerState();
        Properties.clearForCurrentThread();
    }

    @Test
    public void launcherLifecycleShouldCoverExecutionStartFinishAndStatusTransitions() throws Exception {
        String originalVideoScope = SHAFT.Properties.visuals.videoParamsScope();

        AtomicReference<TestExecutionListener> capturedExecutionListener = new AtomicReference<>();

        try {
            SHAFT.Properties.visuals.set().videoParamsScope("TestMethod");
            try (MockedStatic<TestNGListener> testngListenerMock = Mockito.mockStatic(TestNGListener.class);
                 MockedStatic<JunitListenerHelper> junitListenerHelperMock = Mockito.mockStatic(JunitListenerHelper.class);
                 MockedStatic<RealtimeReporter> realtimeReporterMock = Mockito.mockStatic(RealtimeReporter.class);
                 MockedStatic<ReportManagerHelper> reportManagerHelperMock = Mockito.mockStatic(ReportManagerHelper.class);
                 MockedStatic<RecordManager> recordManagerMock = Mockito.mockStatic(RecordManager.class);
                 MockedStatic<AnimatedGifManager> animatedGifManagerMock = Mockito.mockStatic(AnimatedGifManager.class);
                 MockedStatic<ExecutionSummaryReport> executionSummaryReportMock = Mockito.mockStatic(ExecutionSummaryReport.class);
                 MockedStatic<JiraHelper> jiraHelperMock = Mockito.mockStatic(JiraHelper.class);
                 MockedStatic<GoogleTink> googleTinkMock = Mockito.mockStatic(GoogleTink.class);
                 MockedStatic<AllureManager> allureManagerMock = Mockito.mockStatic(AllureManager.class);
                 MockedStatic<RequestBuilder> requestBuilderMock = Mockito.mockStatic(RequestBuilder.class)) {

                requestBuilderMock.when(RequestBuilder::getPerformanceData).thenReturn(Collections.emptyMap());
                realtimeReporterMock.when(() -> RealtimeReporter.buildTestId(Mockito.anyString(), Mockito.anyString()))
                        .thenAnswer(invocation -> invocation.getArgument(0, String.class) + "#" + invocation.getArgument(1, String.class));
                realtimeReporterMock.when(() -> RealtimeReporter.classNameToFilePath(Mockito.anyString())).thenReturn("");

            LauncherSession session = Mockito.mock(LauncherSession.class);
            Launcher launcher = Mockito.mock(Launcher.class);
            Mockito.when(session.getLauncher()).thenReturn(launcher);
            Mockito.doAnswer(invocation -> {
                capturedExecutionListener.set(invocation.getArgument(0));
                return null;
            }).when(launcher).registerTestExecutionListeners(Mockito.any(TestExecutionListener.class));

            JunitListener listener = new JunitListener();
            listener.launcherSessionOpened(session);
            Assert.assertNotNull(capturedExecutionListener.get(), "JUnit listener should register a TestExecutionListener.");

            TestIdentifier rootNode = createIdentifier(
                    "[engine:junit-jupiter]/[class:testPackage.Root]",
                    "Root",
                    TestDescriptor.Type.CONTAINER,
                    Optional.empty());
            TestIdentifier methodBackedTest = createIdentifier(
                    "[engine:junit-jupiter]/[class:testPackage.SampleTests]/[method:sampleMethod()]",
                    "sampleMethod()",
                    TestDescriptor.Type.TEST,
                    Optional.of(MethodSource.from("testPackage.SampleTests", "sampleMethod")));
            TestIdentifier displayNameOnlyTest = createIdentifier(
                    "[engine:junit-jupiter]/[class:testPackage.SampleTests]/[method:displayNameOnly()]",
                    "displayNameOnly",
                    TestDescriptor.Type.TEST,
                    Optional.empty());

            TestPlan testPlan = Mockito.mock(TestPlan.class);
            Mockito.when(testPlan.getRoots()).thenReturn(Set.of(rootNode));
            Mockito.when(testPlan.getChildren(rootNode)).thenReturn(Set.of(methodBackedTest, displayNameOnlyTest));
            Mockito.when(testPlan.getChildren(methodBackedTest)).thenReturn(Set.of());
            Mockito.when(testPlan.getChildren(displayNameOnlyTest)).thenReturn(Set.of());

            TestExecutionListener testExecutionListener = capturedExecutionListener.get();
            testExecutionListener.testPlanExecutionStarted(testPlan);

            realtimeReporterMock.verify(() -> RealtimeReporter.initialize("JUnit Test Run"));
            realtimeReporterMock.verify(() -> RealtimeReporter.onTestsPlanned(Mockito.argThat(planned -> {
                if (planned.size() != 2) {
                    return false;
                }
                boolean foundMethodSourceCard = planned.stream().anyMatch(card -> "testPackage.SampleTests".equals(card.className)
                        && "sampleMethod".equals(card.methodName));
                boolean foundDisplayNameCard = planned.stream().anyMatch(card -> "".equals(card.className)
                        && "displayNameOnly".equals(card.methodName));
                return foundMethodSourceCard && foundDisplayNameCard;
            })));
            testngListenerMock.verify(() -> TestNGListener.engineSetup(ProjectStructureManager.RunType.JUNIT));

            testExecutionListener.executionStarted(rootNode);
            realtimeReporterMock.verify(() -> RealtimeReporter.onTestStarted(Mockito.anyString()), Mockito.never());

            testExecutionListener.executionStarted(methodBackedTest);
            realtimeReporterMock.verify(() -> RealtimeReporter.setCurrentTestId("testPackage.SampleTests#sampleMethod"));
            realtimeReporterMock.verify(() -> RealtimeReporter.onTestStarted("testPackage.SampleTests#sampleMethod"));

            testExecutionListener.executionFinished(methodBackedTest, TestExecutionResult.successful());
            testExecutionListener.executionFinished(methodBackedTest, TestExecutionResult.failed(new RuntimeException("boom")));
            testExecutionListener.executionFinished(displayNameOnlyTest, TestExecutionResult.failed(null));
            testExecutionListener.executionSkipped(displayNameOnlyTest, "skip reason");

            testExecutionListener.testPlanExecutionFinished(testPlan);
            waitBrieflyForAsyncTeardown(Duration.ofSeconds(1));

            reportManagerHelperMock.verify(() -> ReportManagerHelper.setDiscreteLogging(Mockito.anyBoolean()), Mockito.atLeastOnce());
            recordManagerMock.verify(RecordManager::attachVideoRecording, Mockito.atLeastOnce());
            animatedGifManagerMock.verify(AnimatedGifManager::attachAnimatedGif, Mockito.atLeastOnce());
            realtimeReporterMock.verify(() -> RealtimeReporter.onTestFinished(
                    Mockito.eq("testPackage.SampleTests#sampleMethod"), Mockito.eq(RealtimeReporter.TestStatus.PASSED), Mockito.isNull()));
            realtimeReporterMock.verify(() -> RealtimeReporter.onTestFinished(
                    Mockito.eq("testPackage.SampleTests#sampleMethod"), Mockito.eq(RealtimeReporter.TestStatus.FAILED), Mockito.any(Throwable.class)));
            realtimeReporterMock.verify(() -> RealtimeReporter.onTestFinished(
                    Mockito.eq("#displayNameOnly"), Mockito.eq(RealtimeReporter.TestStatus.FAILED), Mockito.any(Throwable.class)));
            realtimeReporterMock.verify(() -> RealtimeReporter.onTestFinished(
                    Mockito.eq("#displayNameOnly"), Mockito.eq(RealtimeReporter.TestStatus.SKIPPED), Mockito.isNull()));
            realtimeReporterMock.verify(RealtimeReporter::clearCurrentTestId, Mockito.atLeastOnce());

            jiraHelperMock.verify(JiraHelper::reportExecutionStatusToJira);
            googleTinkMock.verify(GoogleTink::encrypt);
            allureManagerMock.verify(AllureManager::generateAllureReportArchive);
            allureManagerMock.verify(AllureManager::openAllureReportAfterExecution);
            executionSummaryReportMock.verify(() -> ExecutionSummaryReport.generateExecutionSummaryReport(Mockito.anyInt(), Mockito.anyInt(), Mockito.anyInt(), Mockito.anyLong(), Mockito.anyLong()));
            reportManagerHelperMock.verify(ReportManagerHelper::logEngineClosure);

            Assert.assertFalse(JunitListener.getIsLastFinishedTestOK(), "Last finished test should be marked as not OK after failed/skipped outcomes.");

            setStaticField("isEngineReady", true);
            listener.launcherSessionOpened(session);
            Mockito.verify(launcher, Mockito.times(1)).registerTestExecutionListeners(Mockito.any(TestExecutionListener.class));
        }
        } finally {
            SHAFT.Properties.visuals.set().videoParamsScope(originalVideoScope);
        }
    }

    @Test
    public void engineTearDownShouldComputeFlakyFailedAndSkippedTelemetryBuckets() throws Exception {
        TestIdentifier passedOnly = createIdentifier(
                "[engine:junit-jupiter]/[class:testPackage.TelemetryTests]/[method:passedOnly()]",
                "passedOnly",
                TestDescriptor.Type.TEST,
                Optional.of(MethodSource.from("testPackage.TelemetryTests", "passedOnly")));
        TestIdentifier flakyThenPassed = createIdentifier(
                "[engine:junit-jupiter]/[class:testPackage.TelemetryTests]/[method:flakyThenPassed()]",
                "flakyThenPassed",
                TestDescriptor.Type.TEST,
                Optional.of(MethodSource.from("testPackage.TelemetryTests", "flakyThenPassed")));
        TestIdentifier failedOnly = createIdentifier(
                "[engine:junit-jupiter]/[class:testPackage.TelemetryTests]/[method:failedOnly()]",
                "failedOnly",
                TestDescriptor.Type.TEST,
                Optional.of(MethodSource.from("testPackage.TelemetryTests", "failedOnly")));
        TestIdentifier skippedOnly = createIdentifier(
                "[engine:junit-jupiter]/[class:testPackage.TelemetryTests]/[method:skippedOnly()]",
                "skippedOnly",
                TestDescriptor.Type.TEST,
                Optional.of(MethodSource.from("testPackage.TelemetryTests", "skippedOnly")));

        getStaticList("passedTests").addAll(List.of(passedOnly, flakyThenPassed));
        getStaticList("failedTests").addAll(List.of(flakyThenPassed, failedOnly));
        getStaticList("skippedTests").addAll(List.of(skippedOnly, failedOnly));
        setStaticField("executionStartTime", System.currentTimeMillis() - 5_000L);

        try (MockedStatic<ReportManagerHelper> reportManagerHelperMock = Mockito.mockStatic(ReportManagerHelper.class);
             MockedStatic<JiraHelper> jiraHelperMock = Mockito.mockStatic(JiraHelper.class);
             MockedStatic<GoogleTink> googleTinkMock = Mockito.mockStatic(GoogleTink.class);
             MockedStatic<AllureManager> allureManagerMock = Mockito.mockStatic(AllureManager.class);
             MockedStatic<RealtimeReporter> realtimeReporterMock = Mockito.mockStatic(RealtimeReporter.class);
             MockedStatic<ExecutionSummaryReport> executionSummaryReportMock = Mockito.mockStatic(ExecutionSummaryReport.class);
             MockedStatic<RequestBuilder> requestBuilderMock = Mockito.mockStatic(RequestBuilder.class)) {

            requestBuilderMock.when(RequestBuilder::getPerformanceData).thenReturn(Collections.emptyMap());

            JunitListener listener = new JunitListener();
            Method engineTearDown = JunitListener.class.getDeclaredMethod("engineTearDown");
            engineTearDown.setAccessible(true);
            engineTearDown.invoke(listener);
            waitBrieflyForAsyncTeardown(Duration.ofSeconds(1));

            executionSummaryReportMock.verify(() -> ExecutionSummaryReport.generateExecutionSummaryReport(Mockito.eq(2), Mockito.eq(2), Mockito.eq(2), Mockito.anyLong(), Mockito.anyLong()));
            reportManagerHelperMock.verify(ReportManagerHelper::logEngineClosure);
            jiraHelperMock.verify(JiraHelper::reportExecutionStatusToJira);
            googleTinkMock.verify(GoogleTink::encrypt);
            allureManagerMock.verify(AllureManager::generateAllureReportArchive);
            allureManagerMock.verify(AllureManager::openAllureReportAfterExecution);
            realtimeReporterMock.verify(RealtimeReporter::onExecutionFinished);
        }
    }

    private static TestIdentifier createIdentifier(String uniqueId, String displayName, TestDescriptor.Type type,
                                                   Optional<org.junit.platform.engine.TestSource> source) {
        TestDescriptor descriptor = Mockito.mock(TestDescriptor.class);
        Mockito.when(descriptor.getUniqueId()).thenReturn(UniqueId.parse(uniqueId));
        Mockito.when(descriptor.getDisplayName()).thenReturn(displayName);
        Mockito.when(descriptor.getLegacyReportingName()).thenReturn(displayName);
        Mockito.when(descriptor.getType()).thenReturn(type);
        Mockito.when(descriptor.getSource()).thenReturn(source);
        Mockito.when(descriptor.getTags()).thenReturn(Set.of());
        return TestIdentifier.from(descriptor);
    }

    @SuppressWarnings("unchecked")
    private static List<TestIdentifier> getStaticList(String fieldName) throws Exception {
        Field field = JunitListener.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        return (List<TestIdentifier>) field.get(null);
    }

    private static void setStaticField(String fieldName, Object value) throws Exception {
        Field field = JunitListener.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(null, value);
    }

    private static void resetListenerState() throws Exception {
        getStaticList("passedTests").clear();
        getStaticList("failedTests").clear();
        getStaticList("skippedTests").clear();
        setStaticField("isEngineReady", false);
        setStaticField("isLastFinishedTestOK", true);
        setStaticField("executionStartTime", 0L);
    }

    private static void waitBrieflyForAsyncTeardown(Duration duration) {
        try {
            Thread.sleep(duration.toMillis());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            Assert.fail("Interrupted while waiting for asynchronous listener callbacks.", e);
        }
    }
}
