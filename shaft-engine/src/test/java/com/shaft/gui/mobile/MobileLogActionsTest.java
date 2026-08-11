package com.shaft.gui.mobile;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.driver.MobileLogActionsContract;
import com.shaft.gui.driver.MobileLogMessage;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.ws.StringWebSocketClient;
import org.mockito.Mockito;
import org.openqa.selenium.remote.HttpCommandExecutor;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.net.URI;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

public class MobileLogActionsTest {
    @Test
    public void androidCaptureShouldBeCrossThreadBoundedIdempotentClearableAndStoppable() throws Exception {
        AndroidFixture fixture = android("android-log-flow", false);
        var mobile = new SHAFT.GUI.WebDriver(fixture.driver()).mobile();
        MobileLogActionsContract logs = mobile.logs();

        try {
            logs.start();
        } catch (UnsupportedOperationException missingImplementation) {
            Assert.fail("Supported Android log capture should start.", missingImplementation);
        }
        mobile.logs().start();
        Mockito.verify(fixture.driver(), Mockito.times(1))
                .startLogcatBroadcast("grid.example", 8443);
        Assert.assertEquals(fixture.messageHandlers().size(), 1);
        Assert.assertEquals(fixture.errorHandlers().size(), 1);

        try (var executor = Executors.newSingleThreadExecutor()) {
            executor.submit(() -> fixture.messageHandlers().getFirst().accept("first line")).get();
            executor.submit(() -> fixture.errorHandlers().getFirst()
                    .accept(new IllegalStateException("listener failed"))).get();
        }
        Assert.assertEquals(logs.messages().getFirst().source(), "logcat");
        Assert.assertEquals(logs.messages().getFirst().text(), "first line");
        Assert.assertEquals(logs.errors().getFirst().type(), IllegalStateException.class.getName());
        Assert.assertEquals(logs.errors().getFirst().message(), "listener failed");
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> logs.messages().add(new MobileLogMessage(
                        java.time.Instant.now(), "logcat", "mutation")));

        Assert.assertSame(logs.clear(), logs);
        Assert.assertTrue(logs.messages().isEmpty());
        Assert.assertTrue(logs.errors().isEmpty());
        Consumer<String> handler = fixture.messageHandlers().getFirst();
        for (int index = 0; index <= 1_000; index++) {
            handler.accept("line-" + index);
        }
        Assert.assertEquals(logs.messages().size(), 1_000);
        Assert.assertEquals(logs.messages().getFirst().text(), "line-1");
        Assert.assertEquals(logs.messages().getLast().text(), "line-1000");

        Assert.assertSame(logs.stop(), logs);
        Assert.assertTrue(fixture.messageHandlers().isEmpty());
        Assert.assertTrue(fixture.errorHandlers().isEmpty());
        Mockito.verify(fixture.driver()).executeScript("mobile: stopLogsBroadcast");
        logs.stop();
        Mockito.verify(fixture.driver(), Mockito.times(1)).executeScript("mobile: stopLogsBroadcast");
    }

    @Test
    public void iosCaptureAndSiblingSessionsShouldRemainIsolated() throws Exception {
        IOSFixture first = ios("ios-log-first", false);
        IOSFixture second = ios("ios-log-second", false);
        MobileLogActionsContract firstLogs = new SHAFT.GUI.WebDriver(first.driver()).mobile().logs().start();
        MobileLogActionsContract secondLogs = new SHAFT.GUI.WebDriver(second.driver()).mobile().logs().start();

        first.messageHandlers().getFirst().accept("first-session");
        second.messageHandlers().getFirst().accept(null);
        second.errorHandlers().getFirst().accept(null);

        Assert.assertEquals(firstLogs.messages().getFirst().source(), "syslog");
        Assert.assertEquals(firstLogs.messages().getFirst().text(), "first-session");
        Assert.assertEquals(secondLogs.messages().getFirst().text(), "");
        Assert.assertEquals(secondLogs.errors().getFirst().type(), Throwable.class.getName());
        firstLogs.clear();
        Assert.assertTrue(firstLogs.messages().isEmpty());
        Assert.assertEquals(secondLogs.messages().size(), 1);
        Mockito.verify(first.driver()).startSyslogBroadcast("ios-grid.example", 443);
        Mockito.verify(second.driver()).startSyslogBroadcast("ios-grid.example", 443);
    }

    @Test
    public void startAndStopShouldRollbackOrComposeWithoutHidingProviderFailures() throws Exception {
        AndroidFixture failedStart = android("android-log-start-failure", false);
        IllegalStateException startFailure = new IllegalStateException("start failed");
        Mockito.doThrow(startFailure).when(failedStart.driver())
                .startLogcatBroadcast("grid.example", 8443);
        MobileLogActionsContract failedStartLogs = new SHAFT.GUI.WebDriver(failedStart.driver()).mobile().logs();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, failedStartLogs::start);

        Assert.assertSame(thrown, startFailure);
        Assert.assertTrue(failedStart.messageHandlers().isEmpty());
        Assert.assertTrue(failedStart.errorHandlers().isEmpty());

        AndroidFixture foreign = android("android-log-foreign", true);
        Consumer<String> foreignHandler = ignored -> { };
        foreign.messageHandlers().add(foreignHandler);
        MobileLogActionsContract foreignLogs = new SHAFT.GUI.WebDriver(foreign.driver()).mobile().logs().start();
        Assert.assertEquals(foreign.messageHandlers().size(), 2);
        foreignLogs.stop();
        Assert.assertEquals(foreign.messageHandlers(), List.of(foreignHandler));
        Mockito.verify(foreign.driver(), Mockito.never()).startLogcatBroadcast(Mockito.anyString(), Mockito.anyInt());
        Mockito.verify(foreign.driver(), Mockito.never()).executeScript("mobile: stopLogsBroadcast");

        AndroidFixture retryStop = android("android-log-stop-retry", false);
        IllegalStateException stopFailure = new IllegalStateException("stop failed");
        Mockito.doThrow(stopFailure).doReturn(null).when(retryStop.driver())
                .executeScript("mobile: stopLogsBroadcast");
        MobileLogActionsContract retryLogs = new SHAFT.GUI.WebDriver(retryStop.driver()).mobile().logs().start();
        RuntimeException failedStop = Assert.expectThrows(RuntimeException.class, retryLogs::stop);
        Assert.assertSame(failedStop, stopFailure);
        Assert.assertEquals(retryStop.messageHandlers().size(), 1);
        retryLogs.stop();
        Assert.assertTrue(retryStop.messageHandlers().isEmpty());
    }

    @Test
    public void handlerRegistrationFailureShouldRollbackAndAllowACleanRetry() throws Exception {
        AndroidFixture failedMessageRegistration = android("android-message-handler-failure", false);
        IllegalStateException messageFailure = new IllegalStateException("message handler failed");
        AtomicInteger messageAttempts = new AtomicInteger();
        Mockito.doAnswer(invocation -> {
                    if (messageAttempts.getAndIncrement() == 0) {
                        throw messageFailure;
                    }
                    return failedMessageRegistration.messageHandlers().add(invocation.getArgument(0));
                })
                .when(failedMessageRegistration.driver()).addLogcatMessagesListener(Mockito.any());
        MobileLogActionsContract messageLogs =
                new SHAFT.GUI.WebDriver(failedMessageRegistration.driver()).mobile().logs();

        RuntimeException messageThrown = Assert.expectThrows(RuntimeException.class, messageLogs::start);

        Assert.assertSame(messageThrown, messageFailure);
        Assert.assertTrue(failedMessageRegistration.messageHandlers().isEmpty());
        Assert.assertTrue(failedMessageRegistration.errorHandlers().isEmpty());
        messageLogs.start();
        Assert.assertEquals(failedMessageRegistration.messageHandlers().size(), 1);
        Assert.assertEquals(failedMessageRegistration.errorHandlers().size(), 1);
        Mockito.verify(failedMessageRegistration.driver(), Mockito.times(1))
                .startLogcatBroadcast("grid.example", 8443);

        AndroidFixture failedErrorRegistration = android("android-error-handler-failure", false);
        IllegalStateException errorFailure = new IllegalStateException("error handler failed");
        AtomicInteger errorAttempts = new AtomicInteger();
        Mockito.doAnswer(invocation -> {
                    if (errorAttempts.getAndIncrement() == 0) {
                        throw errorFailure;
                    }
                    return failedErrorRegistration.errorHandlers().add(invocation.getArgument(0));
                })
                .when(failedErrorRegistration.driver()).addLogcatErrorsListener(Mockito.any());
        MobileLogActionsContract errorLogs =
                new SHAFT.GUI.WebDriver(failedErrorRegistration.driver()).mobile().logs();

        RuntimeException errorThrown = Assert.expectThrows(RuntimeException.class, errorLogs::start);

        Assert.assertSame(errorThrown, errorFailure);
        Assert.assertTrue(failedErrorRegistration.messageHandlers().isEmpty());
        Assert.assertTrue(failedErrorRegistration.errorHandlers().isEmpty());
        errorLogs.start();
        Assert.assertEquals(failedErrorRegistration.messageHandlers().size(), 1);
        Assert.assertEquals(failedErrorRegistration.errorHandlers().size(), 1);
        Mockito.verify(failedErrorRegistration.driver(), Mockito.times(1))
                .startLogcatBroadcast("grid.example", 8443);
    }

    @Test
    public void rollbackCleanupFailureShouldNotMaskTheProviderFailureOrSkipSiblingCleanup() throws Exception {
        AndroidFixture fixture = android("android-handler-cleanup-failure", false);
        IllegalStateException providerFailure = new IllegalStateException("broadcast start failed");
        IllegalStateException cleanupFailure = new IllegalStateException("message cleanup failed");
        Mockito.doThrow(providerFailure).when(fixture.driver())
                .startLogcatBroadcast("grid.example", 8443);
        @SuppressWarnings("unchecked")
        CopyOnWriteArrayList<Consumer<String>> hostileMessages =
                Mockito.spy(fixture.messageHandlers());
        StringWebSocketClient client = fixture.driver().getLogcatClient();
        Mockito.when(client.getMessageHandlers()).thenReturn(hostileMessages);
        Mockito.doThrow(cleanupFailure).when(hostileMessages).removeIf(Mockito.any());
        Mockito.doAnswer(invocation -> hostileMessages.add(invocation.getArgument(0)))
                .when(fixture.driver()).addLogcatMessagesListener(Mockito.any());
        MobileLogActionsContract logs = new SHAFT.GUI.WebDriver(fixture.driver()).mobile().logs();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, logs::start);

        Assert.assertSame(thrown, providerFailure);
        Assert.assertTrue(List.of(thrown.getSuppressed()).contains(cleanupFailure));
        Assert.assertFalse(hostileMessages.isEmpty());
        Assert.assertTrue(fixture.errorHandlers().isEmpty(), "Sibling error-handler cleanup must still run.");
        Mockito.verify(fixture.driver()).executeScript("mobile: stopLogsBroadcast");
    }

    @Test
    public void stopCleanupFailureShouldAttemptSiblingCleanupAndRetryWithoutStoppingBroadcastTwice() throws Exception {
        AndroidFixture fixture = android("android-stop-cleanup-failure", false);
        @SuppressWarnings("unchecked")
        CopyOnWriteArrayList<Consumer<String>> hostileMessages = Mockito.spy(fixture.messageHandlers());
        StringWebSocketClient client = fixture.driver().getLogcatClient();
        Mockito.when(client.getMessageHandlers()).thenReturn(hostileMessages);
        Mockito.doAnswer(invocation -> hostileMessages.add(invocation.getArgument(0)))
                .when(fixture.driver()).addLogcatMessagesListener(Mockito.any());
        IllegalStateException cleanupFailure = new IllegalStateException("message cleanup failed");
        Mockito.doThrow(cleanupFailure).doCallRealMethod().when(hostileMessages).removeIf(Mockito.any());
        MobileLogActionsContract logs = new SHAFT.GUI.WebDriver(fixture.driver()).mobile().logs().start();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, logs::stop);

        Assert.assertSame(thrown, cleanupFailure);
        Assert.assertFalse(hostileMessages.isEmpty());
        Assert.assertTrue(fixture.errorHandlers().isEmpty(), "Sibling error-handler cleanup must still run.");
        logs.stop();
        Assert.assertTrue(hostileMessages.isEmpty());
        Mockito.verify(fixture.driver(), Mockito.times(1)).executeScript("mobile: stopLogsBroadcast");
    }

    @Test
    public void driverTeardownShouldRemoveOnlyTheClosedSessionsOwnedHandlers() throws Exception {
        AndroidFixture first = android("android-log-close-first", true);
        AndroidFixture second = android("android-log-close-second", true);
        Consumer<String> foreignHandler = ignored -> { };
        first.messageHandlers().add(foreignHandler);
        new SHAFT.GUI.WebDriver(first.driver()).mobile().logs().start();
        new SHAFT.GUI.WebDriver(second.driver()).mobile().logs().start();

        new DriverFactoryHelper().closeDriver(first.driver());

        Assert.assertEquals(first.messageHandlers(), List.of(foreignHandler));
        Assert.assertTrue(first.errorHandlers().isEmpty());
        Assert.assertEquals(second.messageHandlers().size(), 1);
        Assert.assertEquals(second.errorHandlers().size(), 1);
        Mockito.verify(first.driver(), Mockito.never()).executeScript("mobile: stopLogsBroadcast");
    }

    private static AndroidFixture android(String id, boolean listening) throws Exception {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        StringWebSocketClient client = Mockito.mock(StringWebSocketClient.class);
        CopyOnWriteArrayList<Consumer<String>> messages = new CopyOnWriteArrayList<>();
        CopyOnWriteArrayList<Consumer<Throwable>> errors = new CopyOnWriteArrayList<>();
        configure(driver, client, messages, errors, id, listening, "https://grid.example:8443/wd/hub");
        Mockito.when(driver.getLogcatClient()).thenReturn(client);
        Mockito.doAnswer(invocation -> messages.add(invocation.getArgument(0)))
                .when(driver).addLogcatMessagesListener(Mockito.any());
        Mockito.doAnswer(invocation -> errors.add(invocation.getArgument(0)))
                .when(driver).addLogcatErrorsListener(Mockito.any());
        return new AndroidFixture(driver, messages, errors);
    }

    private static IOSFixture ios(String id, boolean listening) throws Exception {
        IOSDriver driver = Mockito.mock(IOSDriver.class);
        StringWebSocketClient client = Mockito.mock(StringWebSocketClient.class);
        CopyOnWriteArrayList<Consumer<String>> messages = new CopyOnWriteArrayList<>();
        CopyOnWriteArrayList<Consumer<Throwable>> errors = new CopyOnWriteArrayList<>();
        configure(driver, client, messages, errors, id, listening, "https://ios-grid.example/wd/hub");
        Mockito.when(driver.getSyslogClient()).thenReturn(client);
        Mockito.doAnswer(invocation -> messages.add(invocation.getArgument(0)))
                .when(driver).addSyslogMessagesListener(Mockito.any());
        Mockito.doAnswer(invocation -> errors.add(invocation.getArgument(0)))
                .when(driver).addSyslogErrorsListener(Mockito.any());
        return new IOSFixture(driver, messages, errors);
    }

    private static void configure(io.appium.java_client.AppiumDriver driver, StringWebSocketClient client,
                                  CopyOnWriteArrayList<Consumer<String>> messages,
                                  CopyOnWriteArrayList<Consumer<Throwable>> errors,
                                  String id, boolean listening, String serverUrl) throws Exception {
        HttpCommandExecutor executor = Mockito.mock(HttpCommandExecutor.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId(id));
        Mockito.when(driver.getCommandExecutor()).thenReturn(executor);
        Mockito.when(executor.getAddressOfRemoteServer()).thenReturn(URI.create(serverUrl).toURL());
        Mockito.when(client.isListening()).thenReturn(listening);
        Mockito.when(client.getMessageHandlers()).thenReturn(messages);
        Mockito.when(client.getErrorHandlers()).thenReturn(errors);
        Mockito.when(client.getConnectionHandlers()).thenReturn(new CopyOnWriteArrayList<>());
        Mockito.when(client.getDisconnectionHandlers()).thenReturn(new CopyOnWriteArrayList<>());
    }

    private record AndroidFixture(AndroidDriver driver,
                                  CopyOnWriteArrayList<Consumer<String>> messageHandlers,
                                  CopyOnWriteArrayList<Consumer<Throwable>> errorHandlers) { }

    private record IOSFixture(IOSDriver driver,
                              CopyOnWriteArrayList<Consumer<String>> messageHandlers,
                              CopyOnWriteArrayList<Consumer<Throwable>> errorHandlers) { }
}
