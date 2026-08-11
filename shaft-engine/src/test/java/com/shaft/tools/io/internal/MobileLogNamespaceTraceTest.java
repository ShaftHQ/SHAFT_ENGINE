package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.driver.MobileLogActionsContract;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ws.StringWebSocketClient;
import org.mockito.Mockito;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.function.Consumer;

public class MobileLogNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
        ReportContext.clear();
    }

    @Test
    public void everyLogOperationShouldEmitOnePayloadFreeAppiumEvent() {
        Fixture fixture = fixture("log-trace-flow");
        MobileLogActionsContract logs = new SHAFT.GUI.WebDriver(fixture.driver()).mobile().logs();
        String privateMessage = "private-device-log-7821";
        String privateError = "private-device-error-491";

        logs.start();
        fixture.messageHandlers().getFirst().accept(privateMessage);
        fixture.errorHandlers().getFirst().accept(new IllegalStateException(privateError));
        Assert.assertEquals(logs.messages().getFirst().text(), privateMessage);
        Assert.assertEquals(logs.errors().getFirst().message(), privateError);
        logs.clear();
        logs.stop();

        List<TraceEventRecorder.ActionEvent> events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 5);
        Assert.assertEquals(events.stream().map(TraceEventRecorder.ActionEvent::name).toList(),
                List.of("start", "messages", "errors", "clear", "stop"));
        for (TraceEventRecorder.ActionEvent event : events) {
            Assert.assertEquals(event.category(), "mobile/logs");
            Assert.assertEquals(event.status(), "passed");
            Assert.assertEquals(event.backend(), AutomationBackend.APPIUM);
            Assert.assertEquals(event.locator(), "<device-logs>");
            Assert.assertFalse(event.toString().contains(privateMessage));
            Assert.assertFalse(event.toString().contains(privateError));
        }
    }

    @Test
    public void providerAndStaleSessionFailuresShouldEmitOneTruthfulEvent() {
        Fixture fixture = fixture("log-trace-failure");
        IllegalStateException providerFailure = new IllegalStateException("provider start failed");
        Mockito.doThrow(providerFailure).when(fixture.driver()).startLogcatBroadcast();
        MobileLogActionsContract logs = new SHAFT.GUI.WebDriver(fixture.driver()).mobile().logs();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, logs::start);

        Assert.assertSame(thrown, providerFailure);
        assertSingleFailed("start", IllegalStateException.class.getName());
        TraceEventRecorder.clear();

        Mockito.when(fixture.driver().getSessionId()).thenReturn(null);
        Assert.expectThrows(UnsupportedOperationException.class, logs::messages);
        assertSingleFailed("messages", UnsupportedOperationException.class.getName());
    }

    private static void assertSingleFailed(String operation, String exceptionType) {
        List<TraceEventRecorder.ActionEvent> events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        TraceEventRecorder.ActionEvent event = events.getFirst();
        Assert.assertEquals(event.category(), "mobile/logs");
        Assert.assertEquals(event.name(), operation);
        Assert.assertEquals(event.status(), "failed");
        Assert.assertEquals(event.backend(), AutomationBackend.APPIUM);
        Assert.assertEquals(event.locator(), "<device-logs>");
        Assert.assertEquals(event.exceptionType(), exceptionType);
    }

    private static Fixture fixture(String id) {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        StringWebSocketClient client = Mockito.mock(StringWebSocketClient.class);
        CopyOnWriteArrayList<Consumer<String>> messages = new CopyOnWriteArrayList<>();
        CopyOnWriteArrayList<Consumer<Throwable>> errors = new CopyOnWriteArrayList<>();
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId(id));
        Mockito.when(driver.getLogcatClient()).thenReturn(client);
        Mockito.when(client.isListening()).thenReturn(false);
        Mockito.when(client.getMessageHandlers()).thenReturn(messages);
        Mockito.when(client.getErrorHandlers()).thenReturn(errors);
        Mockito.when(client.getConnectionHandlers()).thenReturn(new CopyOnWriteArrayList<>());
        Mockito.when(client.getDisconnectionHandlers()).thenReturn(new CopyOnWriteArrayList<>());
        Mockito.doAnswer(invocation -> messages.add(invocation.getArgument(0)))
                .when(driver).addLogcatMessagesListener(Mockito.any());
        Mockito.doAnswer(invocation -> errors.add(invocation.getArgument(0)))
                .when(driver).addLogcatErrorsListener(Mockito.any());
        return new Fixture(driver, messages, errors);
    }

    private record Fixture(AndroidDriver driver,
                           CopyOnWriteArrayList<Consumer<String>> messageHandlers,
                           CopyOnWriteArrayList<Consumer<Throwable>> errorHandlers) { }
}
