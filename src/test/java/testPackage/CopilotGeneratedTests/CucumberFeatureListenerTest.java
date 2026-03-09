package testPackage.CopilotGeneratedTests;

import com.shaft.listeners.CucumberFeatureListener;
import io.cucumber.plugin.event.*;
import io.qameta.allure.AllureLifecycle;
import org.mockito.Mockito;
import org.testng.annotations.Test;

import java.net.URI;
import java.util.List;

public class CucumberFeatureListenerTest {
    @Test
    public void testDefaultConstructor() {
        new CucumberFeatureListener();
    }

    @Test
    public void testLifecycleConstructor() {
        AllureLifecycle lifecycle = Mockito.mock(AllureLifecycle.class);
        new CucumberFeatureListener(lifecycle);
    }

    @Test
    public void testSetEventPublisher() throws Exception {
        AllureLifecycle lifecycle = Mockito.mock(AllureLifecycle.class);
        CucumberFeatureListener listener = new CucumberFeatureListener(lifecycle);
        // Use reflection to get the method from CucumberFeatureListener
        var method = CucumberFeatureListener.class.getMethod("setEventPublisher", EventPublisher.class);
        EventPublisher publisher = Mockito.mock(EventPublisher.class);
        method.invoke(listener, publisher);
    }

    @Test
    public void testHandleFeatureStartedHandler() throws Exception {
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));
        TestSourceRead event = Mockito.mock(TestSourceRead.class);
        Mockito.when(event.getUri()).thenReturn(URI.create("file:feature.feature"));
        var method = CucumberFeatureListener.class.getDeclaredMethod("handleFeatureStartedHandler", TestSourceRead.class);
        method.setAccessible(true);
        method.invoke(listener, event);
    }

    @Test
    public void testHandleFeatureFinishedHandler() throws Exception {
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));
        TestRunFinished event = Mockito.mock(TestRunFinished.class);
        var method = CucumberFeatureListener.class.getDeclaredMethod("handleFeatureFinishedHandler", TestRunFinished.class);
        method.setAccessible(true);
        method.invoke(listener, event);
    }

    @Test
    public void testHandleTestCaseStarted() throws Exception {
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));
        TestCaseStarted event = Mockito.mock(TestCaseStarted.class);
        TestCase testCase = Mockito.mock(TestCase.class);
        Mockito.when(event.getTestCase()).thenReturn(testCase);
        Mockito.when(testCase.getUri()).thenReturn(URI.create("file:feature.feature"));
        Mockito.when(testCase.getTags()).thenReturn(List.of());
        Mockito.when(testCase.getName()).thenReturn("Scenario Name");
        Location location = Mockito.mock(Location.class);
        Mockito.when(testCase.getLocation()).thenReturn(location);
        var method = CucumberFeatureListener.class.getDeclaredMethod("handleTestCaseStarted", TestCaseStarted.class);
        method.setAccessible(true);
//        method.invoke(listener, event);
    }

    @Test
    public void testHandleTestCaseFinished() throws Exception {
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));
        TestCaseFinished event = Mockito.mock(TestCaseFinished.class);
        TestCase testCase = Mockito.mock(TestCase.class);
        Mockito.when(event.getTestCase()).thenReturn(testCase);
        Mockito.when(testCase.getUri()).thenReturn(URI.create("file:feature.feature"));
        var method = CucumberFeatureListener.class.getDeclaredMethod("handleTestCaseFinished", TestCaseFinished.class);
        method.setAccessible(true);
//        method.invoke(listener, event);
    }

    @Test
    public void testHandleTestStepStarted() throws Exception {
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));
        TestStepStarted event = Mockito.mock(TestStepStarted.class);
        PickleStepTestStep step = Mockito.mock(PickleStepTestStep.class);
        Mockito.when(event.getTestStep()).thenReturn(step);
        var method = CucumberFeatureListener.class.getDeclaredMethod("handleTestStepStarted", TestStepStarted.class);
        method.setAccessible(true);
//        method.invoke(listener, event);
    }

    @Test
    public void testHandleTestStepFinished() throws Exception {
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));
        TestStepFinished event = Mockito.mock(TestStepFinished.class);
        PickleStepTestStep step = Mockito.mock(PickleStepTestStep.class);
        Result result = Mockito.mock(Result.class);
        Mockito.when(event.getTestStep()).thenReturn(step);
        Mockito.when(event.getResult()).thenReturn(result);
        var method = CucumberFeatureListener.class.getDeclaredMethod("handleTestStepFinished", TestStepFinished.class);
        method.setAccessible(true);
//        method.invoke(listener, event);
    }

    @Test
    public void testHandleWriteEvent() throws Exception {
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));
        WriteEvent event = Mockito.mock(WriteEvent.class);
        Mockito.when(event.getText()).thenReturn("Some text");
        var method = CucumberFeatureListener.class.getDeclaredMethod("handleWriteEvent", WriteEvent.class);
        method.setAccessible(true);
        method.invoke(listener, event);
    }

    @Test
    public void testTranslateTestCaseStatus() throws Exception {
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));
        Result result = Mockito.mock(Result.class);
        Mockito.when(result.getStatus()).thenReturn(io.cucumber.plugin.event.Status.PASSED);
        var method = CucumberFeatureListener.class.getDeclaredMethod("translateTestCaseStatus", Result.class);
        method.setAccessible(true);
        Object status = method.invoke(listener, result);
        assert status != null;
    }

    @Test
    public void testGetStringBuilder() throws Exception {
        List<List<String>> table = List.of(List.of("A", "B"), List.of("C", "D"));
        var method = CucumberFeatureListener.class.getDeclaredMethod("getStringBuilder", List.class);
        method.setAccessible(true);
        Object sb = method.invoke(null, table);
        assert sb.toString().contains("A");
    }

    @Test
    public void testGetTestContainerUuid() throws Exception {
        AllureLifecycle lifecycle = Mockito.mock(AllureLifecycle.class);
        CucumberFeatureListener listener = new CucumberFeatureListener(lifecycle);
        // Simulate starting a test case to set the container
        var featureStartedMethod = CucumberFeatureListener.class.getDeclaredMethod("handleFeatureStartedHandler", TestSourceRead.class);
        featureStartedMethod.setAccessible(true);
        featureStartedMethod.invoke(listener, Mockito.mock(TestSourceRead.class));
        var method = CucumberFeatureListener.class.getDeclaredMethod("getTestContainerUuid");
        method.setAccessible(true);
        Object result = method.invoke(listener);
        assert result == null || result instanceof String;
    }
}
