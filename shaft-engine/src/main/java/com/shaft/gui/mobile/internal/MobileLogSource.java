package com.shaft.gui.mobile.internal;

import com.shaft.gui.driver.MobileLogError;
import com.shaft.gui.driver.MobileLogMessage;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.ListensToLogcatMessages;
import io.appium.java_client.ios.ListensToSyslogMessages;
import io.appium.java_client.ws.StringWebSocketClient;
import org.openqa.selenium.remote.HttpCommandExecutor;
import org.openqa.selenium.WebDriver;

import java.net.URL;
import java.time.Instant;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.function.Consumer;

/** Session-keyed bounded Appium logcat/syslog callback storage. */
public final class MobileLogSource {
    private static final int MAX_ENTRIES = 1_000;
    private static final String STOP_BROADCAST = "mobile: stopLogsBroadcast";
    private static final Map<AppiumDriver, State> STATES = new WeakHashMap<>();

    private MobileLogSource() {
        throw new IllegalStateException("Utility class");
    }

    public static void requireSupported(AppiumDriver driver) {
        provider(driver);
    }

    public static void start(AppiumDriver driver) {
        Provider provider = provider(driver);
        State state = state(driver, provider.source());
        synchronized (state) {
            if (state.started) {
                return;
            }
            StringWebSocketClient client = provider.client();
            boolean listening = client.isListening();
            boolean foreignHandlers = hasForeignHandlers(client, state);
            boolean broadcastStartAttempted = false;
            try {
                provider.addMessageHandler(state.messageHandler);
                provider.addErrorHandler(state.errorHandler);
                if (!listening) {
                    URL remoteServer = serverUrl(driver);
                    broadcastStartAttempted = true;
                    provider.start(remoteServer);
                }
                state.ownsBroadcast = !listening && !foreignHandlers;
                state.started = true;
            } catch (RuntimeException exception) {
                rollbackHandlers(client, state, exception);
                if (broadcastStartAttempted && !foreignHandlers) {
                    try {
                        driver.executeScript(STOP_BROADCAST);
                    } catch (RuntimeException cleanupFailure) {
                        suppressDistinct(exception, cleanupFailure);
                    }
                }
                throw exception;
            }
        }
    }

    public static List<MobileLogMessage> messages(AppiumDriver driver) {
        Provider provider = provider(driver);
        State state = state(driver, provider.source());
        synchronized (state) {
            return List.copyOf(state.messages);
        }
    }

    public static List<MobileLogError> errors(AppiumDriver driver) {
        Provider provider = provider(driver);
        State state = state(driver, provider.source());
        synchronized (state) {
            return List.copyOf(state.errors);
        }
    }

    public static void clear(AppiumDriver driver) {
        Provider provider = provider(driver);
        State state = state(driver, provider.source());
        synchronized (state) {
            state.messages.clear();
            state.errors.clear();
        }
    }

    public static void stop(AppiumDriver driver) {
        Provider provider = provider(driver);
        State state = existingState(driver);
        if (state == null) {
            return;
        }
        synchronized (state) {
            if (!state.started) {
                return;
            }
            StringWebSocketClient client = provider.client();
            if (state.ownsBroadcast && !hasForeignHandlers(client, state)) {
                driver.executeScript(STOP_BROADCAST);
                state.ownsBroadcast = false;
            }
            RuntimeException cleanupFailure = removeHandlersBestEffort(client, state);
            if (cleanupFailure != null) {
                throw cleanupFailure;
            }
            state.started = false;
            state.ownsBroadcast = false;
        }
    }

    /** Removes SHAFT-owned callbacks and buffered state without issuing commands to a closing session. */
    public static void closeAndRemove(WebDriver driver) {
        if (!(driver instanceof AppiumDriver appiumDriver)) {
            return;
        }
        State state;
        synchronized (STATES) {
            state = STATES.remove(appiumDriver);
        }
        if (state == null) {
            return;
        }
        synchronized (state) {
            try {
                Provider provider = provider(appiumDriver);
                removeHandlersBestEffort(provider.client(), state);
            } catch (RuntimeException ignored) {
                // Driver teardown must continue even when the provider client is already unavailable.
            }
            state.messages.clear();
            state.errors.clear();
            state.started = false;
            state.ownsBroadcast = false;
        }
    }

    private static State state(AppiumDriver driver, String source) {
        synchronized (STATES) {
            return STATES.computeIfAbsent(driver, ignored -> new State(source));
        }
    }

    private static State existingState(AppiumDriver driver) {
        synchronized (STATES) {
            return STATES.get(driver);
        }
    }

    private static Provider provider(AppiumDriver driver) {
        if (driver instanceof ListensToLogcatMessages logcat) {
            return new LogcatProvider(logcat);
        }
        if (driver instanceof ListensToSyslogMessages syslog) {
            return new SyslogProvider(syslog);
        }
        throw new UnsupportedOperationException(
                "The live Appium session does not support continuous device logs.");
    }

    private static URL serverUrl(AppiumDriver driver) {
        if (driver.getCommandExecutor() instanceof HttpCommandExecutor executor) {
            return executor.getAddressOfRemoteServer();
        }
        return null;
    }

    private static boolean hasForeignHandlers(StringWebSocketClient client, State state) {
        return client.getMessageHandlers().stream().anyMatch(handler -> handler != state.messageHandler)
                || client.getErrorHandlers().stream().anyMatch(handler -> handler != state.errorHandler)
                || !client.getConnectionHandlers().isEmpty()
                || !client.getDisconnectionHandlers().isEmpty();
    }

    private static void rollbackHandlers(StringWebSocketClient client, State state, RuntimeException original) {
        RuntimeException cleanupFailure = removeHandlersBestEffort(client, state);
        if (cleanupFailure != null) {
            suppressDistinct(original, cleanupFailure);
        }
    }

    private static RuntimeException removeHandlersBestEffort(StringWebSocketClient client, State state) {
        RuntimeException firstFailure = null;
        try {
            client.getMessageHandlers().removeIf(handler -> handler == state.messageHandler);
        } catch (RuntimeException cleanupFailure) {
            firstFailure = cleanupFailure;
        }
        try {
            client.getErrorHandlers().removeIf(handler -> handler == state.errorHandler);
        } catch (RuntimeException cleanupFailure) {
            if (firstFailure == null) {
                firstFailure = cleanupFailure;
            } else {
                suppressDistinct(firstFailure, cleanupFailure);
            }
        }
        return firstFailure;
    }

    @SuppressWarnings("PMD.CompareObjectsWithEquals") // Suppress only a distinct throwable instance.
    private static void suppressDistinct(RuntimeException original, RuntimeException cleanupFailure) {
        if (cleanupFailure != original) {
            original.addSuppressed(cleanupFailure);
        }
    }

    private static void startAt(Starter starter, URL serverUrl) {
        if (serverUrl == null || serverUrl.getHost() == null || serverUrl.getHost().isBlank()) {
            starter.start();
            return;
        }
        int port = serverUrl.getPort() >= 0 ? serverUrl.getPort() : serverUrl.getDefaultPort();
        if (port < 0) {
            starter.start();
            return;
        }
        starter.start(serverUrl.getHost(), port);
    }

    private interface Provider {
        String source();

        StringWebSocketClient client();

        void addMessageHandler(Consumer<String> handler);

        void addErrorHandler(Consumer<Throwable> handler);

        void start(URL serverUrl);
    }

    private interface Starter {
        void start();

        void start(String host, int port);
    }

    private record LogcatProvider(ListensToLogcatMessages listener) implements Provider {
        @Override public String source() { return "logcat"; }
        @Override public StringWebSocketClient client() { return listener.getLogcatClient(); }
        @Override public void addMessageHandler(Consumer<String> handler) { listener.addLogcatMessagesListener(handler); }
        @Override public void addErrorHandler(Consumer<Throwable> handler) { listener.addLogcatErrorsListener(handler); }
        @Override public void start(URL serverUrl) {
            startAt(new Starter() {
                @Override public void start() { listener.startLogcatBroadcast(); }
                @Override public void start(String host, int port) { listener.startLogcatBroadcast(host, port); }
            }, serverUrl);
        }
    }

    private record SyslogProvider(ListensToSyslogMessages listener) implements Provider {
        @Override public String source() { return "syslog"; }
        @Override public StringWebSocketClient client() { return listener.getSyslogClient(); }
        @Override public void addMessageHandler(Consumer<String> handler) { listener.addSyslogMessagesListener(handler); }
        @Override public void addErrorHandler(Consumer<Throwable> handler) { listener.addSyslogErrorsListener(handler); }
        @Override public void start(URL serverUrl) {
            startAt(new Starter() {
                @Override public void start() { listener.startSyslogBroadcast(); }
                @Override public void start(String host, int port) { listener.startSyslogBroadcast(host, port); }
            }, serverUrl);
        }
    }

    private static final class State {
        private final Deque<MobileLogMessage> messages = new ArrayDeque<>();
        private final Deque<MobileLogError> errors = new ArrayDeque<>();
        private final Consumer<String> messageHandler;
        private final Consumer<Throwable> errorHandler;
        private boolean started;
        private boolean ownsBroadcast;

        private State(String source) {
            messageHandler = message -> appendMessage(new MobileLogMessage(Instant.now(), source, message));
            errorHandler = error -> appendError(new MobileLogError(Instant.now(), source,
                    error == null ? null : error.getClass().getName(), error == null ? null : error.getMessage()));
        }

        private synchronized void appendMessage(MobileLogMessage message) {
            append(messages, message);
        }

        private synchronized void appendError(MobileLogError error) {
            append(errors, error);
        }

        private static <T> void append(Deque<T> entries, T entry) {
            while (entries.size() >= MAX_ENTRIES) {
                entries.removeFirst();
            }
            entries.addLast(entry);
        }
    }
}
