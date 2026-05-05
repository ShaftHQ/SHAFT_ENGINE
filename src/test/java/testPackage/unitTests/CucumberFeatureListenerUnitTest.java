package testPackage.unitTests;

import com.shaft.listeners.CucumberFeatureListener;
import io.cucumber.messages.types.TableCell;
import io.cucumber.messages.types.TableRow;
import io.cucumber.plugin.event.*;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.model.Status;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
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

    @Test
    public void examplesAsParametersShouldReturnEmptyWhenRowsNotMatching() throws Exception {
        CucumberFeatureListener listener = new CucumberFeatureListener(Mockito.mock(AllureLifecycle.class));
        Method method = CucumberFeatureListener.class.getDeclaredMethod("getExamplesAsParameters", io.cucumber.messages.types.Scenario.class, TestCase.class);
        method.setAccessible(true);

        io.cucumber.messages.types.Scenario scenario = Mockito.mock(io.cucumber.messages.types.Scenario.class);
        io.cucumber.messages.types.Examples examples = Mockito.mock(io.cucumber.messages.types.Examples.class);
        TableRow row = Mockito.mock(TableRow.class);
        io.cucumber.messages.types.Location rowLocation = Mockito.mock(io.cucumber.messages.types.Location.class);
        Mockito.when(rowLocation.getLine()).thenReturn(50L);
        Mockito.when(row.getLocation()).thenReturn(rowLocation);
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

    @SuppressWarnings("unchecked")
    private static void setThreadLocal(CucumberFeatureListener listener, String fieldName, Object value) throws Exception {
        Field field = CucumberFeatureListener.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        ThreadLocal<Object> threadLocal = (ThreadLocal<Object>) field.get(listener);
        threadLocal.set(value);
    }

    private static TableRow mockHeaderRow(String value) {
        TableCell cell = Mockito.mock(TableCell.class);
        Mockito.when(cell.getValue()).thenReturn(value);
        TableRow header = Mockito.mock(TableRow.class);
        Mockito.when(header.getCells()).thenReturn(List.of(cell));
        return header;
    }
}
