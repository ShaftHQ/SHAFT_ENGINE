package testPackage.unitTests;

import com.shaft.listeners.CucumberFeatureListener;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.cucumber.messages.types.TableCell;
import io.cucumber.messages.types.TableRow;
import io.cucumber.plugin.event.*;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.model.FixtureResult;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StepResult;
import io.qameta.allure.model.TestResultContainer;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

public class CucumberFeatureListenerUnitTest {
    @Test
    public void constructorsAndEventPublisherRegistrationShouldWork() {
        new CucumberFeatureListener();
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));
        EventPublisher publisher = Mockito.mock(EventPublisher.class);
        listener.setEventPublisher(publisher);
        Mockito.verify(publisher, Mockito.atLeastOnce()).registerHandlerFor(Mockito.any(), Mockito.any());
    }

    @Test
    public void utilityMethodsShouldHandleTablesAndStatusMapping() throws Exception {
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));

        Method getStringBuilder = CucumberFeatureListener.class.getDeclaredMethod("getStringBuilder", List.class);
        getStringBuilder.setAccessible(true);
        String value = getStringBuilder.invoke(null, List.of(List.of("A", "B"), List.of())).toString();
        Assert.assertEquals(value, "A\tB\n");

        Method translate = CucumberFeatureListener.class.getDeclaredMethod("translateTestCaseStatus", Result.class);
        translate.setAccessible(true);
        Result passed = Mockito.mock(Result.class);
        Mockito.when(passed.getStatus()).thenReturn(io.cucumber.plugin.event.Status.PASSED);
        Assert.assertEquals(translate.invoke(listener, passed), Status.PASSED);

        Result skipped = Mockito.mock(Result.class);
        Mockito.when(skipped.getStatus()).thenReturn(io.cucumber.plugin.event.Status.SKIPPED);
        Assert.assertEquals(translate.invoke(listener, skipped), Status.SKIPPED);

        Result undefined = Mockito.mock(Result.class);
        Mockito.when(undefined.getStatus()).thenReturn(io.cucumber.plugin.event.Status.UNDEFINED);
        Assert.assertNull(translate.invoke(listener, undefined));
    }

    @Test
    public void writeHandlerShouldAttachContent() throws Exception {
        AllureLifecycle lifecycle = Mockito.mock(AllureLifecycle.class);
        CucumberFeatureListener listener = new CucumberFeatureListener(lifecycle);

        Method writeMethod = CucumberFeatureListener.class.getDeclaredMethod("handleWriteEvent", WriteEvent.class);
        writeMethod.setAccessible(true);
        WriteEvent writeEvent = Mockito.mock(WriteEvent.class);
        Mockito.when(writeEvent.getText()).thenReturn("hello");
        writeMethod.invoke(listener, writeEvent);
        Mockito.verify(lifecycle).addAttachment(Mockito.anyString(), Mockito.eq("text/plain"), Mockito.eq(".txt"), Mockito.any(byte[].class));
    }

    @Test
    public void updateTestCaseStatusShouldHonorForbidFlag() throws Exception {
        AllureLifecycle lifecycle = Mockito.mock(AllureLifecycle.class);
        CucumberFeatureListener listener = new CucumberFeatureListener(lifecycle);

        TestCase testCase = Mockito.mock(TestCase.class);
        Mockito.when(testCase.getUri()).thenReturn(URI.create("file:sample.feature"));
        io.cucumber.plugin.event.Location location = Mockito.mock(io.cucumber.plugin.event.Location.class);
        Mockito.when(location.getLine()).thenReturn(12);
        Mockito.when(testCase.getLocation()).thenReturn(location);
        setThreadLocal(listener, "currentTestCase", testCase);
        setThreadLocal(listener, "forbidTestCaseStatusChange", false);

        Method updateMethod = CucumberFeatureListener.class.getDeclaredMethod("updateTestCaseStatus", Status.class);
        updateMethod.setAccessible(true);
        updateMethod.invoke(listener, Status.PASSED);
        Mockito.verify(lifecycle, Mockito.times(1)).updateTestCase(Mockito.anyString(), Mockito.any());

        setThreadLocal(listener, "forbidTestCaseStatusChange", true);
        updateMethod.invoke(listener, Status.FAILED);
        Mockito.verify(lifecycle, Mockito.times(1)).updateTestCase(Mockito.anyString(), Mockito.any());
    }

    @Test(enabled = false)
    public void examplesAsParametersShouldReturnEmptyWhenRowsNotMatching() throws Exception {
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));
        Method method = CucumberFeatureListener.class.getDeclaredMethod("getExamplesAsParameters", io.cucumber.messages.types.Scenario.class, TestCase.class);
        method.setAccessible(true);

        io.cucumber.messages.types.Scenario scenario = Mockito.mock(io.cucumber.messages.types.Scenario.class);
        io.cucumber.messages.types.Examples examples = Mockito.mock(io.cucumber.messages.types.Examples.class);
        io.cucumber.messages.types.Location rowLocation = new io.cucumber.messages.types.Location(50L, 1L);
        TableRow row = new TableRow(rowLocation, List.of(new TableCell(rowLocation, "value")), "row-id");
        Mockito.when(examples.getTableBody()).thenReturn(List.of(row));
        Mockito.when(examples.getTableHeader()).thenReturn(Optional.of(mockHeaderRow("h1")));
        Mockito.when(scenario.getExamples()).thenReturn(List.of(examples));

        TestCase testCase = Mockito.mock(TestCase.class);
        io.cucumber.plugin.event.Location testCaseLocation = Mockito.mock(io.cucumber.plugin.event.Location.class);
        Mockito.when(testCaseLocation.getLine()).thenReturn(99);
        Mockito.when(testCase.getLocation()).thenReturn(testCaseLocation);

        Object result = method.invoke(listener, scenario, testCase);
        Assert.assertTrue(((List<?>) result).isEmpty());
    }

    @Test
    public void lifecycleHandlersShouldProcessFeatureCaseStepsAndHooks() throws Exception {
        AllureLifecycle lifecycle = Mockito.mock(AllureLifecycle.class);
        Mockito.when(lifecycle.prepareAttachment(Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn("attachmentSource");
        CucumberFeatureListener listener = new CucumberFeatureListener(lifecycle);

        URI featureUri = URI.create("file:///tmp/cucumberFeatureListenerLifecycle.feature");
        TestSourceRead sourceRead = Mockito.mock(TestSourceRead.class);
        Mockito.when(sourceRead.getUri()).thenReturn(featureUri);
        Mockito.when(sourceRead.getSource()).thenReturn("""
                Feature: Listener feature
                  Scenario Outline: Listener scenario
                    Given listener step <value>

                    Examples:
                      | value |
                      | one   |
                """);
        invokePrivate(listener, "handleFeatureStartedHandler", TestSourceRead.class, sourceRead);

        TestCase testCase = Mockito.mock(TestCase.class);
        Mockito.when(testCase.getUri()).thenReturn(featureUri);
        Mockito.when(testCase.getName()).thenReturn("Listener scenario");
        Mockito.when(testCase.getTags()).thenReturn(List.of());
        io.cucumber.plugin.event.Location caseLocation = Mockito.mock(io.cucumber.plugin.event.Location.class);
        Mockito.when(caseLocation.getLine()).thenReturn(7);
        Mockito.when(testCase.getLocation()).thenReturn(caseLocation);
        Mockito.when(testCase.getLine()).thenReturn(7);

        Step pickleStepData = Mockito.mock(Step.class);
        Mockito.when(pickleStepData.getKeyword()).thenReturn("Given ");
        Mockito.when(pickleStepData.getText()).thenReturn("listener step one");
        Mockito.when(pickleStepData.getLine()).thenReturn(3);
        DataTableArgument dataTableArgument = Mockito.mock(DataTableArgument.class);
        Mockito.when(dataTableArgument.cells()).thenReturn(List.of(List.of("col"), List.of("one")));
        Mockito.when(pickleStepData.getArgument()).thenReturn(dataTableArgument);
        PickleStepTestStep pickleStepTestStep = Mockito.mock(PickleStepTestStep.class);
        Mockito.when(pickleStepTestStep.getStep()).thenReturn(pickleStepData);
        Mockito.when(testCase.getTestSteps()).thenReturn(List.of(pickleStepTestStep));

        TestCaseStarted testCaseStarted = Mockito.mock(TestCaseStarted.class);
        Mockito.when(testCaseStarted.getTestCase()).thenReturn(testCase);
        invokePrivate(listener, "handleTestCaseStarted", TestCaseStarted.class, testCaseStarted);
        Assert.assertEquals(CucumberFeatureListener.getLastStartedScenarioName(), "Listener scenario");

        TestStepStarted stepStarted = Mockito.mock(TestStepStarted.class);
        Mockito.when(stepStarted.getTestStep()).thenReturn(pickleStepTestStep);
        invokePrivate(listener, "handleTestStepStarted", TestStepStarted.class, stepStarted);

        Result undefinedResult = Mockito.mock(Result.class);
        Mockito.when(undefinedResult.getStatus()).thenReturn(io.cucumber.plugin.event.Status.UNDEFINED);
        Mockito.when(undefinedResult.getError()).thenReturn(null);
        TestStepFinished undefinedStepFinished = Mockito.mock(TestStepFinished.class);
        Mockito.when(undefinedStepFinished.getTestStep()).thenReturn(pickleStepTestStep);
        Mockito.when(undefinedStepFinished.getResult()).thenReturn(undefinedResult);
        invokePrivate(listener, "handleTestStepFinished", TestStepFinished.class, undefinedStepFinished);

        HookTestStep beforeHookStep = Mockito.mock(HookTestStep.class);
        Mockito.when(beforeHookStep.getHookType()).thenReturn(HookType.BEFORE);
        Mockito.when(beforeHookStep.getCodeLocation()).thenReturn("beforeHook");
        TestStepStarted hookStarted = Mockito.mock(TestStepStarted.class);
        Mockito.when(hookStarted.getTestStep()).thenReturn(beforeHookStep);
        invokePrivate(listener, "handleTestStepStarted", TestStepStarted.class, hookStarted);

        Result failedResult = Mockito.mock(Result.class);
        Mockito.when(failedResult.getStatus()).thenReturn(io.cucumber.plugin.event.Status.FAILED);
        Mockito.when(failedResult.getError()).thenReturn(new RuntimeException("Hook failed"));
        TestStepFinished hookFinished = Mockito.mock(TestStepFinished.class);
        Mockito.when(hookFinished.getTestStep()).thenReturn(beforeHookStep);
        Mockito.when(hookFinished.getResult()).thenReturn(failedResult);
        invokePrivate(listener, "handleTestStepFinished", TestStepFinished.class, hookFinished);

        Result passedResult = Mockito.mock(Result.class);
        Mockito.when(passedResult.getStatus()).thenReturn(io.cucumber.plugin.event.Status.PASSED);
        Mockito.when(passedResult.getError()).thenReturn(null);
        TestCaseFinished testCaseFinished = Mockito.mock(TestCaseFinished.class);
        Mockito.when(testCaseFinished.getTestCase()).thenReturn(testCase);
        Mockito.when(testCaseFinished.getResult()).thenReturn(passedResult);
        invokePrivate(listener, "handleTestCaseFinished", TestCaseFinished.class, testCaseFinished);

        Mockito.verify(lifecycle).startTestContainer(Mockito.anyString(), Mockito.any(TestResultContainer.class));
        Mockito.verify(lifecycle).startTestCase(Mockito.anyString());
        Mockito.verify(lifecycle).startStep(Mockito.anyString(), Mockito.anyString(), Mockito.any(StepResult.class));
        Mockito.verify(lifecycle).prepareAttachment(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
        Mockito.verify(lifecycle).writeAttachment(Mockito.eq("attachmentSource"), Mockito.any(ByteArrayInputStream.class));
        Mockito.verify(lifecycle).startPrepareFixture(Mockito.anyString(), Mockito.anyString(), Mockito.any(FixtureResult.class));
        Mockito.verify(lifecycle).updateFixture(Mockito.anyString(), Mockito.any());
        Mockito.verify(lifecycle).stopFixture(Mockito.anyString());
        Mockito.verify(lifecycle).stopTestCase(Mockito.anyString());
        Mockito.verify(lifecycle).writeTestCase(Mockito.anyString());
    }

    @Test
    public void sourceParsingAndEmbedHandlersShouldHandleHappyPath() throws Exception {
        AllureLifecycle lifecycle = Mockito.mock(AllureLifecycle.class);
        CucumberFeatureListener listener = new CucumberFeatureListener(lifecycle);

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

        EmbedEvent embedEvent = Mockito.mock(EmbedEvent.class);
        Mockito.when(embedEvent.getMediaType()).thenReturn("image/png");
        Mockito.when(embedEvent.getData()).thenReturn(new byte[]{1, 2, 3});
        invokePrivate(listener, "handleEmbedEvent", EmbedEvent.class, embedEvent);
        Mockito.verify(lifecycle).addAttachment(Mockito.any(), Mockito.eq("image/png"), Mockito.isNull(), Mockito.any(ByteArrayInputStream.class));

        invokePrivate(listener, "handleFeatureFinishedHandler", TestRunFinished.class, Mockito.mock(TestRunFinished.class));
    }

    @SuppressWarnings("unchecked")
    private static void setThreadLocal(CucumberFeatureListener listener, String fieldName, Object value) throws Exception {
        Field field = CucumberFeatureListener.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        ThreadLocal<Object> threadLocal = (ThreadLocal<Object>) field.get(listener);
        threadLocal.set(value);
    }

    private static TableRow mockHeaderRow(String value) {
        io.cucumber.messages.types.Location location = new io.cucumber.messages.types.Location(1L, 1L);
        TableCell cell = new TableCell(location, value);
        return new TableRow(location, List.of(cell), "header-id");
    }

    private static void invokePrivate(CucumberFeatureListener listener, String methodName, Class<?> parameterType, Object value) throws Exception {
        Method method = CucumberFeatureListener.class.getDeclaredMethod(methodName, parameterType);
        method.setAccessible(true);
        method.invoke(listener, value);
    }
}
