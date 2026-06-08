package testPackage.unitTests;

import com.shaft.listeners.CucumberTestRunnerListener;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.cucumber.plugin.event.*;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.Reporter;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

public class CucumberTestRunnerListenerUnitTest {
    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        Reporter.setCurrentTestResult(null);
        Properties.clearForCurrentThread();
    }

    @Test
    public void setEventPublisherShouldRegisterHandlers() {
        CucumberTestRunnerListener listener = new CucumberTestRunnerListener();
        EventPublisher publisher = Mockito.mock(EventPublisher.class);

        listener.setEventPublisher(publisher);

        Mockito.verify(publisher, Mockito.atLeastOnce()).registerHandlerFor(Mockito.eq(TestSourceParsed.class), Mockito.any());
        Mockito.verify(publisher, Mockito.atLeastOnce()).registerHandlerFor(Mockito.eq(TestCaseStarted.class), Mockito.any());
        Mockito.verify(publisher, Mockito.atLeastOnce()).registerHandlerFor(Mockito.eq(TestCaseFinished.class), Mockito.any());
        Mockito.verify(publisher, Mockito.atLeastOnce()).registerHandlerFor(Mockito.eq(TestStepStarted.class), Mockito.any());
    }

    @Test
    public void handleTestSourceParsedShouldSetAndIncrementTotalTests() throws Exception {
        CucumberTestRunnerListener listener = new CucumberTestRunnerListener();
        Path featureFile = Files.createTempFile("cucumber-test-runner-listener", ".feature");
        Files.writeString(featureFile, """
                Feature: Source parser feature
                  Scenario: first scenario
                    Given first step
                  Scenario: second scenario
                    Given second step
                """);
        int initialTotalTests = ReportManagerHelper.getTotalNumberOfTests();
        ReportManagerHelper.setTotalNumberOfTests(0);

        try {
            TestSourceParsed event = Mockito.mock(TestSourceParsed.class);
            Mockito.when(event.getUri()).thenReturn(featureFile.toUri());
            Mockito.when(event.getNodes()).thenReturn(List.of(Mockito.mock(Node.class)));

            invokePrivate(listener, "handleTestSourceParsed", TestSourceParsed.class, event);
            Assert.assertEquals(ReportManagerHelper.getTotalNumberOfTests(), 2);
            invokePrivate(listener, "handleTestSourceParsed", TestSourceParsed.class, event);
            Assert.assertEquals(ReportManagerHelper.getTotalNumberOfTests(), 4);
        } finally {
            ReportManagerHelper.setTotalNumberOfTests(initialTotalTests);
            Files.deleteIfExists(featureFile);
        }
    }

    @Test
    public void caseStartedHandlerShouldPopulateScenarioDataAndFeatureName() throws Exception {
        CucumberTestRunnerListener listener = new CucumberTestRunnerListener();
        Path featureFile = Files.createTempFile("cucumber-test-runner-listener-started", ".feature");
        Files.writeString(featureFile, """
                Feature: Feature from file
                  Scenario: Parsed scenario
                    Given a started step
                """);

        Field featureNameField = ReportManagerHelper.class.getDeclaredField("featureName");
        featureNameField.setAccessible(true);
        String initialFeatureName = (String) featureNameField.get(null);

        try {
            PickleStepTestStep pickleStepTestStep = Mockito.mock(PickleStepTestStep.class);
            Step step = Mockito.mock(Step.class);
            Mockito.when(step.getKeyword()).thenReturn("Given ");
            Mockito.when(step.getText()).thenReturn("a started step");
            Mockito.when(pickleStepTestStep.getStep()).thenReturn(step);

            TestCase testCase = Mockito.mock(TestCase.class);
            Mockito.when(testCase.getTestSteps()).thenReturn(List.of(pickleStepTestStep));
            Mockito.when(testCase.getUri()).thenReturn(featureFile.toUri());
            Mockito.when(testCase.getName()).thenReturn("Parsed scenario");
            Mockito.when(testCase.getKeyword()).thenReturn("Scenario");

            TestCaseStarted event = Mockito.mock(TestCaseStarted.class);
            Mockito.when(event.getTestCase()).thenReturn(testCase);

            invokePrivate(listener, "caseStartedHandler", TestCaseStarted.class, event);

            Assert.assertEquals(getLastStartedScenarioName(), "Parsed scenario");
            Assert.assertEquals(featureNameField.get(null), "Feature from file");
        } finally {
            featureNameField.set(null, initialFeatureName);
            Files.deleteIfExists(featureFile);
        }
    }

    @Test
    public void caseFinishedHandlerShouldAttachArtifactsInNativeMode() throws Exception {
        CucumberTestRunnerListener listener = new CucumberTestRunnerListener();
        setLastStartedScenarioName("Native scenario");
        Reporter.setCurrentTestResult(null);

        try (MockedStatic<com.shaft.gui.internal.video.RecordManager> recordManager = Mockito.mockStatic(com.shaft.gui.internal.video.RecordManager.class);
             MockedStatic<com.shaft.gui.internal.image.AnimatedGifManager> gifManager = Mockito.mockStatic(com.shaft.gui.internal.image.AnimatedGifManager.class);
             MockedStatic<com.shaft.listeners.internal.TestNGListenerHelper> testNGListenerHelper = Mockito.mockStatic(com.shaft.listeners.internal.TestNGListenerHelper.class);
             MockedStatic<ReportManagerHelper> reportManagerHelper = Mockito.mockStatic(ReportManagerHelper.class)) {

            testNGListenerHelper.when(() -> com.shaft.listeners.internal.TestNGListenerHelper.createTestLog(Mockito.anyList()))
                    .thenReturn("native cucumber log");

            TestCaseFinished event = Mockito.mock(TestCaseFinished.class);
            invokePrivate(listener, "caseFinishedHandler", TestCaseFinished.class, event);

            recordManager.verifyNoInteractions();
            gifManager.verify(com.shaft.gui.internal.image.AnimatedGifManager::attachAnimatedGif);
            reportManagerHelper.verify(() -> ReportManagerHelper.attachTestLog("Native scenario", "native cucumber log"));
        }
    }

    @Test
    public void getFeatureShouldThrowForInvalidResource() {
        CucumberTestRunnerListener listener = new CucumberTestRunnerListener();
        Throwable throwable = Assert.expectThrows(Throwable.class,
                () -> invokePrivate(listener, "getFeature", URI.class, URI.create("file:/non-existent.feature")));
        Assert.assertTrue(throwable.getCause() instanceof RuntimeException);
    }

    @Test
    public void stepStartedHandlerShouldLogHookAndPickleSteps() throws Exception {
        CucumberTestRunnerListener listener = new CucumberTestRunnerListener();

        HookTestStep hookStep = Mockito.mock(HookTestStep.class);
        Mockito.when(hookStep.getHookType()).thenReturn(HookType.BEFORE);
        TestStepStarted hookEvent = Mockito.mock(TestStepStarted.class);
        Mockito.when(hookEvent.getTestStep()).thenReturn(hookStep);

        PickleStepTestStep pickleStep = Mockito.mock(PickleStepTestStep.class);
        Step step = Mockito.mock(Step.class);
        Mockito.when(step.getKeyword()).thenReturn("When ");
        Mockito.when(step.getText()).thenReturn("a step starts");
        Mockito.when(pickleStep.getStep()).thenReturn(step);
        TestStepStarted pickleEvent = Mockito.mock(TestStepStarted.class);
        Mockito.when(pickleEvent.getTestStep()).thenReturn(pickleStep);

        try (MockedStatic<ReportManager> reportManager = Mockito.mockStatic(ReportManager.class)) {
            invokePrivate(listener, "stepStartedHandler", TestStepStarted.class, hookEvent);
            invokePrivate(listener, "stepStartedHandler", TestStepStarted.class, pickleEvent);

            reportManager.verify(() -> ReportManager.logDiscrete("Scenario Hook: BEFORE"));
            reportManager.verify(() -> ReportManager.logDiscrete("Scenario Step: When a step starts"));
        }
    }

    private static Object invokePrivate(CucumberTestRunnerListener listener, String methodName, Class<?> parameterType, Object value) throws Exception {
        Method method = CucumberTestRunnerListener.class.getDeclaredMethod(methodName, parameterType);
        method.setAccessible(true);
        return method.invoke(listener, value);
    }

    private static String getLastStartedScenarioName() throws Exception {
        Field field = CucumberTestRunnerListener.class.getDeclaredField("lastStartedScenarioName");
        field.setAccessible(true);
        return (String) field.get(null);
    }

    private static void setLastStartedScenarioName(String value) throws Exception {
        Field field = CucumberTestRunnerListener.class.getDeclaredField("lastStartedScenarioName");
        field.setAccessible(true);
        field.set(null, value);
    }
}
