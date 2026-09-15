package testPackage.unitTests;

import com.shaft.listeners.CucumberFeatureListener;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.cucumber.plugin.event.*;
import io.qameta.allure.AllureLifecycle;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class CucumberFeatureListenerUnitTest {
    @Test
    public void constructorsAndEventPublisherRegistrationShouldWork() {
        new CucumberFeatureListener();
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));
        EventPublisher publisher = Mockito.mock(EventPublisher.class);
        listener.setEventPublisher(publisher);
        // Parent AllureCucumber7Jvm handlers plus SHAFT overlays
        Mockito.verify(publisher, Mockito.atLeast(6)).registerHandlerFor(Mockito.any(), Mockito.any());
    }

    @Test
    public void shaftOverlaysShouldTrackScenarioNameAndStepHealth() throws Exception {
        AllureLifecycle lifecycle = Mockito.mock(AllureLifecycle.class);
        CucumberFeatureListener listener = new CucumberFeatureListener(lifecycle);

        URI featureUri = URI.create("file:///tmp/cucumberFeatureListenerOverlay.feature");
        TestSourceRead sourceRead = Mockito.mock(TestSourceRead.class);
        Mockito.when(sourceRead.getUri()).thenReturn(featureUri);
        Mockito.when(sourceRead.getSource()).thenReturn("""
                Feature: Listener feature
                  Scenario: Listener scenario
                    Given listener step
                """);
        invokePrivate(listener, "handleFeatureStartedHandler", TestSourceRead.class, sourceRead);

        TestCase testCase = Mockito.mock(TestCase.class);
        Mockito.when(testCase.getUri()).thenReturn(featureUri);
        Mockito.when(testCase.getName()).thenReturn("Listener scenario");
        Mockito.when(testCase.getTags()).thenReturn(List.of());
        io.cucumber.plugin.event.Location caseLocation = Mockito.mock(io.cucumber.plugin.event.Location.class);
        Mockito.when(caseLocation.getLine()).thenReturn(3);
        Mockito.when(testCase.getLocation()).thenReturn(caseLocation);
        Mockito.when(testCase.getLine()).thenReturn(3);
        Step pickleStepData = Mockito.mock(Step.class);
        Mockito.when(pickleStepData.getKeyword()).thenReturn("Given ");
        Mockito.when(pickleStepData.getText()).thenReturn("listener step");
        PickleStepTestStep pickleStepTestStep = Mockito.mock(PickleStepTestStep.class);
        Mockito.when(pickleStepTestStep.getStep()).thenReturn(pickleStepData);
        Mockito.when(testCase.getTestSteps()).thenReturn(List.of(pickleStepTestStep));

        TestCaseStarted testCaseStarted = Mockito.mock(TestCaseStarted.class);
        Mockito.when(testCaseStarted.getTestCase()).thenReturn(testCase);
        invokePrivate(listener, "handleShaftTestCaseStarted", TestCaseStarted.class, testCaseStarted);
        Assert.assertEquals(CucumberFeatureListener.getLastStartedScenarioName(), "Listener scenario");

        Result passed = Mockito.mock(Result.class);
        Mockito.when(passed.getStatus()).thenReturn(io.cucumber.plugin.event.Status.PASSED);
        TestStepFinished stepFinished = Mockito.mock(TestStepFinished.class);
        Mockito.when(stepFinished.getResult()).thenReturn(passed);
        invokePrivate(listener, "handleShaftTestStepFinished", TestStepFinished.class, stepFinished);
        Assert.assertEquals(CucumberFeatureListener.getIsLastFinishedStepOK(), Boolean.TRUE);
    }

    @Test
    public void sourceParsingShouldCountPickles() throws Exception {
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));
        Path featureFile = Files.createTempFile("cucumberFeatureListener", ".feature");
        Files.writeString(featureFile, """
                Feature: Parsed feature
                  Scenario: Parsed scenario
                    Given parsed step
                """);
        URI featureUri = featureFile.toUri();
        int initialTotalTests = ReportManagerHelper.getTotalNumberOfTests();
        ReportManagerHelper.setTotalNumberOfTests(0);
        try {
            TestSourceParsed testSourceParsed = Mockito.mock(TestSourceParsed.class);
            Mockito.when(testSourceParsed.getNodes()).thenReturn(List.of(Mockito.mock(Node.class)));
            Mockito.when(testSourceParsed.getUri()).thenReturn(featureUri);

            invokePrivate(listener, "handleTestSourceParsed", TestSourceParsed.class, testSourceParsed);
            Assert.assertEquals(ReportManagerHelper.getTotalNumberOfTests(), 1);
            invokePrivate(listener, "handleTestSourceParsed", TestSourceParsed.class, testSourceParsed);
            Assert.assertEquals(ReportManagerHelper.getTotalNumberOfTests(), 2);
        } finally {
            ReportManagerHelper.setTotalNumberOfTests(initialTotalTests);
            Files.deleteIfExists(featureFile);
        }

        invokePrivate(listener, "handleFeatureFinishedHandler", TestRunFinished.class, Mockito.mock(TestRunFinished.class));
    }

    private static void invokePrivate(CucumberFeatureListener listener, String methodName, Class<?> parameterType, Object value) throws Exception {
        Method method = CucumberFeatureListener.class.getDeclaredMethod(methodName, parameterType);
        method.setAccessible(true);
        method.invoke(listener, value);
    }
}
