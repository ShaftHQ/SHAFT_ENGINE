package com.shaft.gui.mobile;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.driver.MobileLogActionsContract;
import com.shaft.gui.driver.MobileLogMessage;
import com.shaft.gui.mobile.internal.MobileLogSource;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.android.ListensToLogcatMessages;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.ws.StringWebSocketClient;
import org.mockito.Mockito;
import org.openqa.selenium.remote.HttpCommandExecutor;
import org.openqa.selenium.remote.SessionId;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.ImmutableCapabilities;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.net.URI;
import java.lang.ref.Reference;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Predicate;

@SuppressWarnings("PMD.AvoidAccessibilityAlteration") // Private state/monitor access binds teardown linearization.
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

    @Test
    public void evidenceSnapshotShouldBeNonCreatingImmutableAndIsolatedByDriverIdentity() throws Exception {
        Method snapshotMethod = java.util.Arrays.stream(MobileLogSource.class.getMethods())
                .filter(method -> method.getName().equals("snapshotIfPresent")
                        && java.util.Arrays.equals(method.getParameterTypes(), new Class<?>[]{AppiumDriver.class}))
                .findFirst().orElse(null);
        Assert.assertNotNull(snapshotMethod, "Evidence needs a non-creating log snapshot.");

        AppiumDriver absent = Mockito.mock(AppiumDriver.class);
        Assert.assertFalse(hasState(absent));
        Assert.assertEquals(snapshotMethod.invoke(null, absent), Optional.empty());
        Assert.assertFalse(hasState(absent), "Evidence reads must not create per-driver log state.");
        Mockito.verifyNoInteractions(absent);
        Assert.expectThrows(NullPointerException.class, () -> MobileLogSource.snapshotIfPresent(null));

        BlockingMessageHandlers blockingMessages = new BlockingMessageHandlers();
        EqualLogDriver first = new EqualLogDriver("evidence-log-first", blockingMessages);
        EqualLogDriver second = new EqualLogDriver("evidence-log-second");
        MobileLogActionsContract firstLogs = new SHAFT.GUI.WebDriver(first).mobile().logs().start();
        MobileLogActionsContract secondLogs = new SHAFT.GUI.WebDriver(second).mobile().logs().start();
        Consumer<String> lateMessage = first.messageHandlers.getFirst();
        Consumer<Throwable> lateError = first.errorHandlers.getFirst();
        lateMessage.accept("first");
        lateError.accept(new IllegalStateException("first-error"));
        second.messageHandlers.getFirst().accept("second");
        second.errorHandlers.getFirst().accept(new IllegalStateException("second-error"));

        Object firstSnapshot = ((Optional<?>) snapshotMethod.invoke(null, first)).orElseThrow();
        Object secondSnapshot = ((Optional<?>) snapshotMethod.invoke(null, second)).orElseThrow();
        List<?> firstMessages = (List<?>) firstSnapshot.getClass().getMethod("messages").invoke(firstSnapshot);
        List<?> firstErrors = (List<?>) firstSnapshot.getClass().getMethod("errors").invoke(firstSnapshot);
        List<?> secondMessages = (List<?>) secondSnapshot.getClass().getMethod("messages").invoke(secondSnapshot);

        Assert.assertEquals(((MobileLogMessage) firstMessages.getFirst()).text(), "first");
        Assert.assertEquals(((com.shaft.gui.driver.MobileLogError) firstErrors.getFirst()).message(), "first-error");
        Assert.assertEquals(((MobileLogMessage) secondMessages.getFirst()).text(), "second");
        Assert.expectThrows(UnsupportedOperationException.class, firstMessages::clear);
        Assert.expectThrows(UnsupportedOperationException.class, firstErrors::clear);
        Assert.assertTrue((Boolean) firstSnapshot.getClass().getMethod("started").invoke(firstSnapshot));
        Assert.assertEquals(first.messageHandlers.size(), 1);
        Assert.assertEquals(second.messageHandlers.size(), 1);

        firstLogs.stop();
        Object stoppedSnapshot = ((Optional<?>) snapshotMethod.invoke(null, first)).orElseThrow();
        Assert.assertFalse((Boolean) stoppedSnapshot.getClass().getMethod("started").invoke(stoppedSnapshot));
        Assert.assertEquals(((List<?>) stoppedSnapshot.getClass().getMethod("messages").invoke(stoppedSnapshot)).size(), 1);
        Assert.assertEquals(((List<?>) stoppedSnapshot.getClass().getMethod("errors").invoke(stoppedSnapshot)).size(), 1);

        blockingMessages.arm();
        try (var executor = Executors.newFixedThreadPool(2)) {
            var closing = executor.submit(() -> new DriverFactoryHelper().closeDriver(first));
            Assert.assertTrue(blockingMessages.removeEntered.await(10, TimeUnit.SECONDS));
            CountDownLatch snapshotStarted = new CountDownLatch(1);
            AtomicReference<Thread> snapshotThread = new AtomicReference<>();
            var racingSnapshot = executor.submit(() -> {
                snapshotThread.set(Thread.currentThread());
                snapshotStarted.countDown();
                return snapshotMethod.invoke(null, first);
            });
            Assert.assertTrue(snapshotStarted.await(10, TimeUnit.SECONDS));
            long blockedDeadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10);
            while (snapshotThread.get().getState() != Thread.State.BLOCKED
                    && System.nanoTime() < blockedDeadline) {
                Thread.onSpinWait();
            }
            Assert.assertEquals(snapshotThread.get().getState(), Thread.State.BLOCKED,
                    "Snapshot must block on the same state monitor used by teardown.");
            blockingMessages.allowRemove.countDown();
            closing.get(10, TimeUnit.SECONDS);
            Assert.assertEquals(racingSnapshot.get(10, TimeUnit.SECONDS), Optional.empty());
        }

        lateMessage.accept("late-message");
        lateError.accept(new IllegalStateException("late-error"));
        Assert.assertEquals(snapshotMethod.invoke(null, first), Optional.empty());
        Assert.assertEquals(bufferedEntryCounts(first), new int[]{0, 0},
                "Late callbacks must not retain sensitive entries in a closed tombstone.");
        Assert.expectThrows(UnsupportedOperationException.class, firstLogs::start);
        Assert.expectThrows(UnsupportedOperationException.class, firstLogs::messages);
        Assert.expectThrows(UnsupportedOperationException.class, firstLogs::errors);
        Assert.expectThrows(UnsupportedOperationException.class, firstLogs::clear);
        Assert.expectThrows(UnsupportedOperationException.class, firstLogs::stop);
        Assert.assertEquals(((Optional<?>) snapshotMethod.invoke(null, second)).orElseThrow()
                .getClass().getMethod("messages").invoke(((Optional<?>) snapshotMethod.invoke(null, second)).orElseThrow()),
                secondLogs.messages());

        EqualLogDriver closedBeforeUse = new EqualLogDriver("evidence-log-closed-before-use");
        MobileLogActionsContract staleLogs = new SHAFT.GUI.WebDriver(closedBeforeUse).mobile().logs();
        new DriverFactoryHelper().closeDriver(closedBeforeUse);
        Assert.expectThrows(UnsupportedOperationException.class, staleLogs::start);
        Assert.expectThrows(UnsupportedOperationException.class, staleLogs::messages);
        Assert.expectThrows(UnsupportedOperationException.class, staleLogs::errors);
        Assert.expectThrows(UnsupportedOperationException.class, staleLogs::clear);
        Assert.expectThrows(UnsupportedOperationException.class, staleLogs::stop);
        Assert.assertEquals(snapshotMethod.invoke(null, closedBeforeUse), Optional.empty());
        Assert.assertTrue(closedBeforeUse.messageHandlers.isEmpty());
        Assert.assertTrue(closedBeforeUse.errorHandlers.isEmpty());

        Assert.expectThrows(NullPointerException.class,
                () -> new MobileLogSource.Snapshot(false, null, List.of()));
        Assert.expectThrows(NullPointerException.class,
                () -> new MobileLogSource.Snapshot(false, List.of(), null));
    }

    private static int[] bufferedEntryCounts(AppiumDriver driver) throws Exception {
        Field statesField = MobileLogSource.class.getDeclaredField("STATES");
        statesField.setAccessible(true);
        Map<?, ?> states = (Map<?, ?>) statesField.get(null);
        synchronized (states) {
            for (Map.Entry<?, ?> entry : states.entrySet()) {
                if (entry.getKey() instanceof Reference<?> reference && reference.get() == driver) {
                    Object state = entry.getValue();
                    Field messagesField = state.getClass().getDeclaredField("messages");
                    Field errorsField = state.getClass().getDeclaredField("errors");
                    messagesField.setAccessible(true);
                    errorsField.setAccessible(true);
                    return new int[]{((Deque<?>) messagesField.get(state)).size(),
                            ((Deque<?>) errorsField.get(state)).size()};
                }
            }
        }
        throw new AssertionError("Closed log state tombstone was not retained.");
    }

    private static boolean hasState(AppiumDriver driver) throws Exception {
        Field statesField = MobileLogSource.class.getDeclaredField("STATES");
        statesField.setAccessible(true);
        Map<?, ?> states = (Map<?, ?>) statesField.get(null);
        synchronized (states) {
            return states.keySet().stream()
                    .filter(Reference.class::isInstance)
                    .map(Reference.class::cast)
                    .anyMatch(reference -> reference.get() == driver);
        }
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

    private static final class EqualLogDriver extends AppiumDriver implements ListensToLogcatMessages {
        private final SessionId sessionId;
        private final StringWebSocketClient client = Mockito.mock(StringWebSocketClient.class);
        private final CopyOnWriteArrayList<Consumer<String>> messageHandlers;
        private final CopyOnWriteArrayList<Consumer<Throwable>> errorHandlers = new CopyOnWriteArrayList<>();

        private EqualLogDriver(String id) {
            this(id, new CopyOnWriteArrayList<>());
        }

        private EqualLogDriver(String id, CopyOnWriteArrayList<Consumer<String>> messageHandlers) {
            super(Mockito.mock(HttpCommandExecutor.class), new ImmutableCapabilities());
            sessionId = new SessionId(id);
            this.messageHandlers = messageHandlers;
            Mockito.when(client.isListening()).thenReturn(true);
            Mockito.when(client.getMessageHandlers()).thenReturn(messageHandlers);
            Mockito.when(client.getErrorHandlers()).thenReturn(errorHandlers);
            Mockito.when(client.getConnectionHandlers()).thenReturn(new CopyOnWriteArrayList<>());
            Mockito.when(client.getDisconnectionHandlers()).thenReturn(new CopyOnWriteArrayList<>());
        }

        @Override
        protected void startSession(Capabilities capabilities) {
            // No remote session is needed for this identity-state regression fixture.
        }

        @Override
        public SessionId getSessionId() {
            return sessionId;
        }

        @Override
        public StringWebSocketClient getLogcatClient() {
            return client;
        }

        @Override
        public void addLogcatMessagesListener(Consumer<String> handler) {
            messageHandlers.add(handler);
        }

        @Override
        public void addLogcatErrorsListener(Consumer<Throwable> handler) {
            errorHandlers.add(handler);
        }

        @Override
        public boolean equals(Object other) {
            return other instanceof EqualLogDriver;
        }

        @Override
        public int hashCode() {
            return 1;
        }

        @Override
        public void close() {
            // Local state tests do not own a remote session.
        }

        @Override
        public void quit() {
            // Local state tests do not own a remote session.
        }
    }

    private static final class BlockingMessageHandlers extends CopyOnWriteArrayList<Consumer<String>> {
        private final CountDownLatch removeEntered = new CountDownLatch(1);
        private final CountDownLatch allowRemove = new CountDownLatch(1);
        private volatile boolean armed;

        private void arm() {
            armed = true;
        }

        @Override
        public boolean removeIf(Predicate<? super Consumer<String>> filter) {
            if (armed) {
                removeEntered.countDown();
                try {
                    if (!allowRemove.await(10, TimeUnit.SECONDS)) {
                        throw new IllegalStateException("Timed out waiting to release log-handler removal.");
                    }
                } catch (InterruptedException exception) {
                    Thread.currentThread().interrupt();
                    throw new IllegalStateException(exception);
                }
            }
            return super.removeIf(filter);
        }
    }
}
