package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.internal.BidiConsoleLogSource;
import com.shaft.gui.browser.internal.LegacyConsoleLogSource;
import com.shaft.gui.playwright.internal.PlaywrightSessionManager;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.logging.LogEntry;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;

/**
 * Session-owned browser network, console, and capability metadata recorder for SHAFT trace artifacts.
 */
public final class BrowserObservabilityRecorder {
    private static final int NETWORK_EVENT_LIMIT = 1000;
    private static final int NETWORK_FIELD_LIMIT = 2048;
    private static final int NETWORK_FIELD_UTF8_BYTE_LIMIT = NETWORK_FIELD_LIMIT * 4;
    private static final String NETWORK_FIELD_OMITTED =
            "[omitted because browser metadata exceeded the safe redaction boundary]";
    private static final int NETWORK_HEADER_LIMIT = 64;
    private static final int NETWORK_HEADER_CHARACTER_LIMIT = 8192;
    private static final int CONSOLE_EVENT_LIMIT = 1000;
    private static final int WEBSOCKET_EVENT_LIMIT = 1000;
    private static final int WARNING_EVENT_LIMIT = 100;
    private static final int IN_FLIGHT_EXCHANGE_LIMIT = 1000;
    private static final ThreadLocal<ObservationSession> CURRENT = new ThreadLocal<>();
    private static final ThreadLocal<ObservationBinding> CURRENT_BINDING =
            ThreadLocal.withInitial(ObservationBinding::new);
    private static final ReferenceQueue<NetworkExchange> STALE_EXCHANGES = new ReferenceQueue<>();
    private static final Map<IdentityWeakReference, ObservationSession> EXCHANGE_OWNERS = new HashMap<>();
    private static final AtomicInteger IN_FLIGHT_EXCHANGES = new AtomicInteger();

    private BrowserObservabilityRecorder() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Starts a network exchange and copies the request body so downstream filters can still read it.
     *
     * @param request browser HTTP request
     * @return exchange handle, or a disabled handle when trace network capture is disabled
     */
    public static NetworkExchange startNetwork(HttpRequest request) {
        return startNetwork(currentSession(), request);
    }

    /** Starts a network exchange owned by an explicitly captured observation session. */
    public static NetworkExchange startNetwork(ObservationSession session, HttpRequest request) {
        if (request == null) {
            return NetworkExchange.disabled();
        }
        ObservationSession owner = valid(session);
        if (owner == null || !owner.networkEnabled()) {
            return NetworkExchange.disabled();
        }
        if (!reserveExchange(owner)) {
            return NetworkExchange.disabled();
        }
        int index = owner.nextNetworkId();
        try {
            byte[] requestBody = copyRequestBody(request);
            NetworkExchange exchange = new NetworkExchange(true, "network-" + index,
                    retainedNetworkText(request.getMethod().name()), retainedNetworkText(request.getUri()),
                    new LinkedHashMap<>(retainedHeaders(headers(request))),
                    requestBody.length, System.nanoTime());
            if (retainReservedExchange(exchange, owner)) {
                return exchange;
            }
        } catch (RuntimeException e) {
            IN_FLIGHT_EXCHANGES.decrementAndGet();
            throw e;
        }
        return NetworkExchange.disabled();
    }

    /**
     * Finishes a network exchange and records a redacted trace event.
     *
     * @param exchange      exchange handle returned from {@link #startNetwork(HttpRequest)}
     * @param response      browser HTTP response, or {@code null} on failure
     * @param failureReason safe failure reason
     */
    public static void finishNetwork(NetworkExchange exchange, HttpResponse response, String failureReason) {
        if (exchange == null || !exchange.enabled()) {
            return;
        }
        byte[] responseBody = copyResponseBody(response);
        ObservationSession owner;
        synchronized (EXCHANGE_OWNERS) {
            expungeStaleExchanges();
            owner = EXCHANGE_OWNERS.remove(new IdentityWeakReference(exchange));
        }
        if (owner != null) {
            IN_FLIGHT_EXCHANGES.decrementAndGet();
        }
        recordNetwork(owner == null ? currentSession() : owner, new NetworkObservation(
                exchange.method(),
                exchange.url(),
                response == null ? 0 : response.getStatus(),
                exchange.requestHeaders(),
                response == null ? Map.of() : headers(response),
                TimeUnit.NANOSECONDS.toMillis(Math.max(0, System.nanoTime() - exchange.startNanos())),
                exchange.requestSizeBytes(),
                responseBody.length,
                value(failureReason),
                preview(responseBody)));
    }

    /**
     * Records a browser network event for the current test thread.
     *
     * @param observation network exchange details
     */
    public static void recordNetwork(NetworkObservation observation) {
        recordNetwork(currentSession(), observation);
    }

    /** Starts a network exchange using a callback binding that follows report-session rollover. */
    public static NetworkExchange startNetwork(ObservationBinding binding, HttpRequest request) {
        return startNetwork(binding == null ? null : binding.session(), request);
    }

    /** Records a network event into an explicitly captured observation session. */
    public static void recordNetwork(ObservationSession session, NetworkObservation observation) {
        recordNetwork(session, observation, "selenium-http");
    }

    /** Records provider-labeled bounded metadata into an explicitly captured observation session. */
    public static void recordNetwork(ObservationSession session, NetworkObservation observation, String provider) {
        ObservationSession owner = valid(session);
        if (owner == null || !owner.networkEnabled() || observation == null) {
            return;
        }
        owner.addNetwork(new NetworkEvent(
                value(provider),
                retainedNetworkText(observation.method()),
                retainedNetworkText(observation.url()),
                observation.status(),
                retainedHeaders(observation.requestHeaders()),
                retainedHeaders(observation.responseHeaders()),
                Math.max(0, observation.durationMs()),
                Math.max(0, observation.requestSize()),
                Math.max(0, observation.responseSize()),
                retainedNetworkText(observation.failureReason()),
                retainedNetworkText(observation.bodyPreview()),
                System.currentTimeMillis()));
    }

    /** Records one bounded CDP WebSocket lifecycle or frame observation. */
    public static void recordWebSocket(ObservationSession session, WebSocketObservation observation) {
        ObservationSession owner = valid(session);
        if (owner == null || !owner.networkEnabled() || observation == null) return;
        owner.addWebSocket(new WebSocketEvent(
                retainedNetworkText(observation.requestId()), retainedNetworkText(observation.url()),
                retainedNetworkText(observation.direction()), retainedNetworkText(observation.type()),
                observation.opcode(), retainedNetworkText(observation.text()), validatedSha256(observation.sha256()),
                Math.max(0, observation.sizeBytes()), retainedNetworkText(observation.status()),
                retainedNetworkText(observation.reason()), System.currentTimeMillis()));
    }

    /**
     * Collects Selenium browser logs into the console trace section.
     *
     * @param driver active driver, or {@code null}
     */
    public static void collectConsole(WebDriver driver) {
        if (driver == null && PlaywrightSessionManager.currentSession() != null) {
            PlaywrightSessionManager.currentSession().drainConsoleToRecorder();
            return;
        }
        if (BidiConsoleLogSource.isHealthy(driver)) {
            BidiConsoleLogSource.drainToRecorder(driver);
            return;
        }
        if (!currentSession().consoleEnabled() || !isConsoleEnabled()) {
            return;
        }
        if (!tryCollectConsole(driver)) {
            recordWarning("console", driver == null
                    ? "Console capture is unavailable because no active driver is registered."
                    : "Browser console logs are not supported by this driver.");
        }
    }

    /**
     * Collects Selenium browser logs and reports whether the provider exposes the browser log type.
     *
     * @param driver active driver
     * @return {@code true} when browser logs are supported and collected
     */
    public static boolean tryCollectConsole(WebDriver driver) {
        if (driver == null) {
            return false;
        }
        try {
            var logs = driver.manage().logs();
            Set<String> logTypes = logs.getAvailableLogTypes();
            if (!logTypes.contains("browser")) {
                return false;
            }
            List<LogEntry> entries = new ArrayList<>(logs.get("browser").getAll());
            entries.sort(Comparator.comparingLong(LogEntry::getTimestamp));
            List<ConsoleSnapshotEntry> retained = new ArrayList<>(Math.min(entries.size(), CONSOLE_EVENT_LIMIT));
            for (LogEntry entry : entries) {
                recordConsole("browser", entry.getLevel().getName(), entry.getMessage(), entry.getTimestamp());
                if (retained.size() >= CONSOLE_EVENT_LIMIT) {
                    retained.removeFirst();
                }
                retained.add(consoleEntry("browser", entry.getLevel().getName(), entry.getMessage(),
                        entry.getTimestamp()));
            }
            LegacyConsoleLogSource.retain(driver, retained);
            return true;
        } catch (RuntimeException e) {
            return false;
        }
    }

    /**
     * Records a browser console event for the current test thread.
     *
     * @param source    log source
     * @param level     log level
     * @param message   log message
     * @param timestamp epoch timestamp in milliseconds
     */
    public static void recordConsole(String source, String level, String message, long timestamp) {
        recordConsole(currentSession(), source, level, message, timestamp);
    }

    /** Records a console event into an explicitly captured observation session. */
    public static void recordConsole(ObservationSession session, String source, String level, String message,
                                     long timestamp) {
        ObservationSession owner = valid(session);
        if (owner != null && owner.consoleEnabled()) {
            owner.addConsole(new ConsoleEvent(value(source), value(level), value(message), Math.max(0, timestamp)));
        }
    }

    /**
     * Records safe browser observability metadata for unsupported capabilities.
     *
     * @param source  capability source
     * @param message warning message
     */
    public static void recordWarning(String source, String message) {
        recordWarning(currentSession(), source, message);
    }

    /** Records a console event using a callback binding that follows report-session rollover. */
    public static void recordConsole(ObservationBinding binding, String source, String level, String message,
                                     long timestamp) {
        recordConsole(binding == null ? null : binding.session(), source, level, message, timestamp);
    }

    /** Records a warning into an explicitly captured observation session. */
    public static void recordWarning(ObservationSession session, String source, String message) {
        ObservationSession owner = valid(session);
        if (owner != null && owner.traceEnabled()) {
            owner.addWarning(new WarningEvent(value(source), value(message)));
        }
    }

    /**
     * Returns and clears current-thread warnings.
     *
     * @return recorded warnings
     */
    public static List<String> drainWarnings() {
        return drainWarnings(currentSession());
    }

    /** Returns and clears warnings for an explicitly owned observation session. */
    public static List<String> drainWarnings(ObservationSession session) {
        if (session == null) {
            return List.of();
        }
        return session.takeWarnings().stream()
                .map(warning -> warning.source() + ": " + warning.message())
                .toList();
    }

    static String drainNetworkJson() {
        ObservationSession session = currentSession();
        String json = networkJson(session.takeNetwork());
        return json;
    }

    /**
     * Returns a read-only, 1-based snapshot of the current thread's observed network transactions
     * without draining them, so a caller can list transactions repeatedly (for example an MCP tool
     * answering separate "list" and "get by id" requests) without racing {@link FailureTraceReporter}'s
     * end-of-test drain of the same thread-local state.
     *
     * @return immutable snapshot of currently recorded network transactions, oldest first
     */
    public static List<NetworkSnapshotEntry> snapshot() {
        return snapshot(currentSession());
    }

    /** Returns an immutable snapshot for an explicitly captured observation session. */
    public static List<NetworkSnapshotEntry> snapshot(ObservationSession session) {
        List<NetworkEvent> events = session == null ? List.of() : session.networkSnapshot();
        List<NetworkSnapshotEntry> snapshot = new ArrayList<>(events.size());
        for (int i = 0; i < events.size(); i++) {
            NetworkEvent event = events.get(i);
            snapshot.add(new NetworkSnapshotEntry(
                    i + 1,
                    boundedNetworkText(event.method()),
                    boundedNetworkText(event.url()),
                    event.status(),
                    boundedNetworkText(mimeType(event.responseHeaders())),
                    event.durationMs(),
                    event.requestSizeBytes(),
                    event.responseSizeBytes(),
                    boundedNetworkText(event.failureReason()),
                    event.timestamp(),
                    boundedNetworkText(event.bodyPreview()),
                    boundedHeaders(event.requestHeaders()),
                    boundedHeaders(event.responseHeaders())));
        }
        return List.copyOf(snapshot);
    }

    private static String mimeType(Map<String, String> responseHeaders) {
        for (Map.Entry<String, String> entry : responseHeaders.entrySet()) {
            if ("content-type".equalsIgnoreCase(entry.getKey())) {
                String value = value(entry.getValue());
                int separator = value.indexOf(';');
                return separator < 0 ? value.trim() : value.substring(0, separator).trim();
            }
        }
        return "";
    }

    /**
     * Returns and clears current-thread network observations as a HAR-like JSON document.
     *
     * @return HAR-like network JSON
     */
    public static String drainNetworkHarJson() {
        return networkHarJson(drainNetworkJson());
    }

    static String drainConsoleJson() {
        ObservationSession session = currentSession();
        if (!session.consoleEnabled()) {
            session.clearConsole();
            return "[]";
        }
        String json = consoleJson(session.takeConsole());
        return json;
    }

    /** @return immutable, redacted current-thread console snapshot, oldest first */
    public static List<ConsoleSnapshotEntry> snapshotConsole() {
        return snapshotConsole(currentSession());
    }

    /** Returns an immutable console snapshot for an explicitly captured observation session. */
    public static List<ConsoleSnapshotEntry> snapshotConsole(ObservationSession session) {
        return (session == null ? List.<ConsoleEvent>of() : session.consoleSnapshot()).stream()
                .map(event -> new ConsoleSnapshotEntry(event.source(), event.level(),
                        FailureTraceReporter.redact(event.message()), event.timestamp()))
                .toList();
    }

    /** Creates one normalized, redacted public console snapshot entry. */
    public static ConsoleSnapshotEntry consoleEntry(String source, String level, String message, long timestamp) {
        return new ConsoleSnapshotEntry(value(source), value(level), FailureTraceReporter.redact(value(message)),
                Math.max(0, timestamp));
    }

    /** Clears current-thread console observations without affecting network evidence. */
    public static void clearConsole() {
        currentSession().clearConsole();
    }

    static String drainMetadataJson() {
        ObservationSession session = currentSession();
        MetadataBatch batch = session.takeMetadata();
        String json = metadataJson(batch.warnings(), batch.webSockets());
        return json;
    }

    /** Returns immutable WebSocket evidence for an explicitly captured observation session. */
    public static List<WebSocketSnapshotEntry> snapshotWebSockets(ObservationSession session) {
        return (session == null ? List.<WebSocketEvent>of() : session.webSocketSnapshot()).stream()
                .map(event -> new WebSocketSnapshotEntry(boundedNetworkText(event.requestId()),
                        boundedNetworkText(event.url()), boundedNetworkText(event.direction()),
                        boundedNetworkText(event.type()), event.opcode(), boundedNetworkText(event.text()),
                        event.sha256(), event.sizeBytes(), boundedNetworkText(event.status()),
                        boundedNetworkText(event.reason())))
                .toList();
    }

    /**
     * Clears current-thread browser observability state.
     */
    public static void clear() {
        ObservationSession session = CURRENT.get();
        if (session != null) {
            session.close();
        }
        CURRENT.remove();
    }

    /** Starts and binds a fresh browser-observation session to the current report thread. */
    public static ObservationSession startSession() {
        clear();
        ObservationSession session = createSession();
        CURRENT.set(session);
        CURRENT_BINDING.get().bind(session);
        return session;
    }

    /** Creates a detached observation session for a concurrently active browser owner. */
    public static ObservationSession createSession() {
        return new ObservationSession();
    }

    /** Captures the current observation owner for use by asynchronous callbacks. */
    public static ObservationSession captureSession() {
        return currentSession();
    }

    /** Captures a stable callback binding whose target follows current-thread report-session rollover. */
    public static ObservationBinding captureBinding() {
        currentSession();
        return CURRENT_BINDING.get();
    }

    /** Resolves one callback binding once so a whole provider batch keeps one immutable owner. */
    public static ObservationSession resolveSession(ObservationBinding binding) {
        return binding == null ? null : binding.session();
    }

    private static ObservationSession currentSession() {
        ObservationSession session = CURRENT.get();
        if (session == null || session.closed()) {
            session = new ObservationSession();
            CURRENT.set(session);
            CURRENT_BINDING.get().bind(session);
        }
        return session;
    }

    private static ObservationSession valid(ObservationSession session) {
        return session == null || session.closed() ? null : session;
    }

    private static boolean reserveExchange(ObservationSession owner) {
        while (true) {
            synchronized (EXCHANGE_OWNERS) {
                expungeStaleExchanges();
            }
            int current = IN_FLIGHT_EXCHANGES.get();
            if (current >= IN_FLIGHT_EXCHANGE_LIMIT) {
                owner.warnExchangeLimitOnce();
                return false;
            }
            if (IN_FLIGHT_EXCHANGES.compareAndSet(current, current + 1)) {
                return true;
            }
        }
    }

    /** Marks that a provider omitted its oldest buffered console event before transfer. */
    public static void recordConsoleOmission(ObservationSession session) {
        if (session != null) session.markConsoleLimit();
    }

    private static boolean retainReservedExchange(NetworkExchange exchange, ObservationSession owner) {
        synchronized (owner) {
            if (owner.closed()) {
                IN_FLIGHT_EXCHANGES.decrementAndGet();
                return false;
            }
            synchronized (EXCHANGE_OWNERS) {
                expungeStaleExchanges();
                EXCHANGE_OWNERS.put(new IdentityWeakReference(exchange, STALE_EXCHANGES), owner);
                return true;
            }
        }
    }

    private static void expungeStaleExchanges() {
        IdentityWeakReference stale;
        while ((stale = (IdentityWeakReference) STALE_EXCHANGES.poll()) != null) {
            if (EXCHANGE_OWNERS.remove(stale) != null) {
                IN_FLIGHT_EXCHANGES.decrementAndGet();
            }
        }
    }

    private static String networkJson(List<NetworkEvent> events) {
        StringBuilder json = new StringBuilder("[");
        for (int i = 0; i < events.size(); i++) {
            NetworkEvent event = events.get(i);
            if (i > 0) {
                json.append(",");
            }
            json.append("\n    {\n");
            field(json, 3, "provider", boundedNetworkText(event.provider()), true);
            field(json, 3, "method", boundedNetworkText(event.method()), true);
            field(json, 3, "url", boundedNetworkText(event.url()), true);
            numberField(json, 3, "status", event.status(), true);
            numberField(json, 3, "timestamp", event.timestamp(), true);
            numberField(json, 3, "durationMs", event.durationMs(), true);
            numberField(json, 3, "requestSizeBytes", event.requestSizeBytes(), true);
            numberField(json, 3, "responseSizeBytes", event.responseSizeBytes(), true);
            map(json, 3, "requestHeaders", boundedHeaders(event.requestHeaders()), true);
            map(json, 3, "responseHeaders", boundedHeaders(event.responseHeaders()), true);
            field(json, 3, "failureReason", boundedNetworkText(event.failureReason()), true);
            field(json, 3, "bodyPreview", boundedNetworkText(event.bodyPreview()), false);
            indent(json, 2).append("}");
        }
        if (!events.isEmpty()) {
            json.append("\n  ");
        }
        json.append("]");
        return json.toString();
    }

    private static String consoleJson(List<ConsoleEvent> events) {
        StringBuilder json = new StringBuilder("[");
        for (int i = 0; i < events.size(); i++) {
            ConsoleEvent event = events.get(i);
            if (i > 0) {
                json.append(",");
            }
            json.append("\n    {\n");
            field(json, 3, "source", event.source(), true);
            field(json, 3, "level", event.level(), true);
            field(json, 3, "message", event.message(), true);
            numberField(json, 3, "timestamp", event.timestamp(), false);
            indent(json, 2).append("}");
        }
        if (!events.isEmpty()) {
            json.append("\n  ");
        }
        json.append("]");
        return json.toString();
    }

    private static String metadataJson(List<WarningEvent> warnings, List<WebSocketEvent> webSockets) {
        StringBuilder json = new StringBuilder("{\n");
        indent(json, 2).append("\"warnings\": [");
        for (int i = 0; i < warnings.size(); i++) {
            WarningEvent warning = warnings.get(i);
            if (i > 0) {
                json.append(", ");
            }
            json.append("{\"source\": \"")
                    .append(escapeJson(FailureTraceReporter.redactSourceText(warning.source())))
                    .append("\", \"message\": \"")
                    .append(escapeJson(FailureTraceReporter.redactSourceText(warning.message())))
                    .append("\"}");
        }
        json.append("],\n");
        indent(json, 2).append("\"webSockets\": [");
        for (int i = 0; i < webSockets.size(); i++) {
            WebSocketEvent event = webSockets.get(i);
            if (i > 0) json.append(",");
            json.append("\n    {\n");
            field(json, 3, "provider", "cdp", true);
            field(json, 3, "requestId", boundedNetworkText(event.requestId()), true);
            field(json, 3, "url", boundedNetworkText(event.url()), true);
            field(json, 3, "direction", boundedNetworkText(event.direction()), true);
            field(json, 3, "type", boundedNetworkText(event.type()), true);
            numberField(json, 3, "opcode", event.opcode(), true);
            field(json, 3, "text", boundedNetworkText(event.text()), true);
            field(json, 3, "sha256", event.sha256(), true);
            numberField(json, 3, "sizeBytes", event.sizeBytes(), true);
            field(json, 3, "status", boundedNetworkText(event.status()), true);
            field(json, 3, "reason", boundedNetworkText(event.reason()), true);
            numberField(json, 3, "timestamp", event.timestamp(), false);
            indent(json, 2).append("}");
        }
        if (!webSockets.isEmpty()) json.append("\n  ");
        json.append("]\n");
        indent(json, 1).append("}");
        return json.toString();
    }

    static String networkHarJson(String networkJson) {
        return """
                {
                  "log": {
                    "version": "1.2",
                    "creator": {
                      "name": "SHAFT",
                      "comment": "HAR-like browser network trace emitted by SHAFT observability"
                    },
                    "entries": %s
                  }
                }
                """.formatted(networkJson == null || networkJson.isBlank() ? "[]" : networkJson);
    }

    private static boolean isConsoleEnabled() {
        return isTraceEnabled() && SHAFT.Properties.reporting.traceIncludeConsole();
    }

    private static boolean isTraceEnabled() {
        try {
            return SHAFT.Properties.reporting != null
                    && SHAFT.Properties.reporting.traceEnabled();
        } catch (RuntimeException e) {
            return false;
        }
    }

    private static byte[] copyRequestBody(HttpRequest request) {
        try {
            byte[] body = Contents.bytes(request.getContent());
            request.setContent(Contents.bytes(body));
            return body;
        } catch (RuntimeException e) {
            return new byte[0];
        }
    }

    private static byte[] copyResponseBody(HttpResponse response) {
        if (response == null) {
            return new byte[0];
        }
        try {
            byte[] body = Contents.bytes(response.getContent());
            response.setContent(Contents.bytes(body));
            return body;
        } catch (RuntimeException e) {
            return new byte[0];
        }
    }

    private static Map<String, String> headers(HttpRequest request) {
        Map<String, String> headers = new LinkedHashMap<>();
        request.forEachHeader(headers::put);
        return headers;
    }

    private static Map<String, String> headers(HttpResponse response) {
        Map<String, String> headers = new LinkedHashMap<>();
        response.forEachHeader(headers::put);
        return headers;
    }

    private static Map<String, String> retainedHeaders(Map<String, String> source) {
        if (source == null || source.isEmpty()) {
            return Map.of();
        }
        Map<String, String> sanitized = new LinkedHashMap<>();
        int retainedCharacters = 0;
        for (Map.Entry<String, String> entry : source.entrySet()) {
            if (sanitized.size() >= NETWORK_HEADER_LIMIT
                    || retainedCharacters >= NETWORK_HEADER_CHARACTER_LIMIT) {
                break;
            }
            String key = retainedNetworkText(entry.getKey());
            String headerValue = retainedNetworkHeaderValue(entry.getKey(), entry.getValue());
            int required = key.length() + headerValue.length();
            if (required > NETWORK_HEADER_CHARACTER_LIMIT - retainedCharacters) {
                break;
            }
            sanitized.put(key, headerValue);
            retainedCharacters += required;
        }
        return Map.copyOf(sanitized);
    }

    private static Map<String, String> boundedHeaders(Map<String, String> source) {
        if (source == null || source.isEmpty()) {
            return Map.of();
        }
        Map<String, String> sanitized = new LinkedHashMap<>();
        int retainedCharacters = 0;
        for (Map.Entry<String, String> entry : source.entrySet()) {
            if (sanitized.size() >= NETWORK_HEADER_LIMIT || retainedCharacters >= NETWORK_HEADER_CHARACTER_LIMIT) {
                break;
            }
            String key = boundedNetworkText(entry.getKey());
            String headerValue = boundedNetworkHeaderValue(entry.getKey(), entry.getValue());
            int remaining = NETWORK_HEADER_CHARACTER_LIMIT - retainedCharacters - key.length();
            if (remaining < 0) {
                break;
            }
            if (headerValue.length() > remaining) {
                headerValue = headerValue.substring(0, remaining);
            }
            sanitized.put(key, headerValue);
            retainedCharacters += key.length() + headerValue.length();
        }
        return Map.copyOf(sanitized);
    }

    /** Retains complete bounded callback text, or a fail-closed marker that cannot contain a partial credential. */
    public static String retainedNetworkText(String source) {
        String redacted = FailureTraceReporter.redact(value(source));
        return redacted.length() <= NETWORK_FIELD_LIMIT ? redacted : NETWORK_FIELD_OMITTED;
    }

    /** Retains one bounded callback header value while masking known sensitive header names immediately. */
    public static String retainedNetworkHeaderValue(String key, String source) {
        return isSensitiveKey(key) ? "********" : retainedNetworkText(source);
    }

    public static String boundedNetworkText(String source) {
        String redacted = FailureTraceReporter.redactSourceText(value(source));
        return redacted.length() <= NETWORK_FIELD_LIMIT ? redacted : redacted.substring(0, NETWORK_FIELD_LIMIT);
    }

    public static String boundedNetworkHeaderValue(String key, String source) {
        return isSensitiveKey(key) ? "********" : boundedNetworkText(source);
    }

    private static String validatedSha256(String source) {
        String candidate = value(source);
        return candidate.matches("(?i)[0-9a-f]{64}") ? candidate.toLowerCase(Locale.ROOT) : "";
    }

    private static void map(StringBuilder json, int indent, String key, Map<String, String> values, boolean comma) {
        indent(json, indent).append("\"").append(key).append("\": {");
        int index = 0;
        for (Map.Entry<String, String> entry : values.entrySet()) {
            if (index++ > 0) {
                json.append(",");
            }
            json.append("\n");
            indent(json, indent + 1)
                    .append("\"")
                    .append(escapeJson(FailureTraceReporter.redactSourceText(entry.getKey())))
                    .append("\": \"")
                    .append(escapeJson(FailureTraceReporter.redactSourceText(entry.getValue())))
                    .append("\"");
        }
        if (!values.isEmpty()) {
            json.append("\n");
            indent(json, indent);
        }
        json.append("}").append(comma ? "," : "").append("\n");
    }

    private static void field(StringBuilder json, int indent, String key, String value, boolean comma) {
        indent(json, indent).append("\"")
                .append(escapeJson(FailureTraceReporter.redactSourceText(key)))
                .append("\": \"")
                .append(escapeJson(FailureTraceReporter.redactSourceText(value)))
                .append("\"")
                .append(comma ? "," : "")
                .append("\n");
    }

    private static void numberField(StringBuilder json, int indent, String key, long value, boolean comma) {
        indent(json, indent).append("\"").append(key).append("\": ")
                .append(value)
                .append(comma ? "," : "")
                .append("\n");
    }

    private static boolean isSensitiveKey(String key) {
        String normalized = value(key).toLowerCase(Locale.ROOT);
        return normalized.contains("authorization")
                || normalized.contains("cookie")
                || normalized.contains("password")
                || normalized.contains("passwd")
                || normalized.contains("secret")
                || normalized.contains("token")
                || normalized.contains("api-key")
                || normalized.contains("apikey")
                || normalized.contains("access-key")
                || normalized.contains("accesskey");
    }

    private static String preview(byte[] bytes) {
        if (bytes == null || bytes.length == 0) {
            return "";
        }
        if (bytes.length > NETWORK_FIELD_UTF8_BYTE_LIMIT) {
            return NETWORK_FIELD_OMITTED;
        }
        String decoded = new String(bytes, java.nio.charset.StandardCharsets.UTF_8);
        return decoded.length() > NETWORK_FIELD_LIMIT ? NETWORK_FIELD_OMITTED : retainedNetworkText(decoded);
    }

    private static StringBuilder indent(StringBuilder builder, int level) {
        return builder.append("  ".repeat(level));
    }

    private static String escapeJson(String value) {
        return JsonEscapes.escape(value);
    }

    private static String value(String value) {
        return value == null ? "" : value;
    }

    /**
     * Active network exchange handle.
     */
    public record NetworkExchange(boolean enabled, String id, String method, String url,
                                  Map<String, String> requestHeaders, long requestSizeBytes, long startNanos) {
        static NetworkExchange disabled() {
            return new NetworkExchange(false, "", "", "", Map.of(), 0L, 0L);
        }
    }

    /** Opaque owner handle captured by asynchronous browser callbacks. */
    public static final class ObservationSession implements AutoCloseable {
        private final List<NetworkEvent> network = new ArrayList<>();
        private final List<ConsoleEvent> console = new ArrayList<>();
        private final List<WebSocketEvent> webSockets = new ArrayList<>();
        private final List<WarningEvent> warnings = new ArrayList<>();
        private int nextNetworkId;
        private boolean closed;
        private boolean exchangeLimitWarned;
        private boolean networkLimitWarned;
        private boolean consoleLimitWarned;
        private boolean webSocketLimitWarned;
        private boolean warningLimitWarned;
        private final boolean traceEnabled = isTraceEnabled();
        private final boolean networkEnabled = traceEnabled && SHAFT.Properties.reporting.traceIncludeNetwork();
        private final boolean consoleEnabled = traceEnabled && SHAFT.Properties.reporting.traceIncludeConsole();

        private synchronized int nextNetworkId() { return ++nextNetworkId; }
        private synchronized boolean closed() { return closed; }
        private boolean traceEnabled() { return traceEnabled; }
        private boolean networkEnabled() { return networkEnabled; }
        private boolean consoleEnabled() { return consoleEnabled; }
        private synchronized void addNetwork(NetworkEvent event) {
            if (!closed) {
                if (network.size() >= NETWORK_EVENT_LIMIT) {
                    network.removeFirst();
                    networkLimitWarned = true;
                }
                network.add(event);
            }
        }
        private synchronized void addConsole(ConsoleEvent event) {
            if (!closed) {
                if (console.size() >= CONSOLE_EVENT_LIMIT) {
                    console.removeFirst();
                    consoleLimitWarned = true;
                }
                console.add(event);
            }
        }
        private synchronized void addWebSocket(WebSocketEvent event) {
            if (!closed) {
                if (webSockets.size() >= WEBSOCKET_EVENT_LIMIT) {
                    webSockets.removeFirst();
                    webSocketLimitWarned = true;
                }
                webSockets.add(event);
            }
        }
        private synchronized void markConsoleLimit() {
            if (!closed && consoleEnabled) consoleLimitWarned = true;
        }
        private synchronized void addWarning(WarningEvent event) {
            if (!closed) {
                if (warnings.size() >= WARNING_EVENT_LIMIT) {
                    warnings.removeFirst();
                    warningLimitWarned = true;
                }
                warnings.add(event);
            }
        }
        private synchronized void warnExchangeLimitOnce() {
            if (!closed && !exchangeLimitWarned) {
                exchangeLimitWarned = true;
            }
        }
        private synchronized List<NetworkEvent> networkSnapshot() { return List.copyOf(network); }
        private synchronized List<ConsoleEvent> consoleSnapshot() { return List.copyOf(console); }
        private synchronized List<WebSocketEvent> webSocketSnapshot() { return List.copyOf(webSockets); }
        private synchronized List<NetworkEvent> takeNetwork() {
            List<NetworkEvent> result = List.copyOf(network);
            network.clear();
            nextNetworkId = 0;
            return result;
        }
        private synchronized List<ConsoleEvent> takeConsole() {
            List<ConsoleEvent> result = List.copyOf(console);
            console.clear();
            return result;
        }
        private synchronized MetadataBatch takeMetadata() {
            List<WarningEvent> warningSnapshot = effectiveWarnings();
            List<WebSocketEvent> webSocketSnapshot = List.copyOf(webSockets);
            warnings.clear();
            webSockets.clear();
            warningLimitWarned = false;
            exchangeLimitWarned = false;
            networkLimitWarned = false;
            consoleLimitWarned = false;
            webSocketLimitWarned = false;
            return new MetadataBatch(warningSnapshot, webSocketSnapshot);
        }
        private synchronized List<WarningEvent> takeWarnings() {
            List<WarningEvent> result = effectiveWarnings();
            warnings.clear();
            warningLimitWarned = false;
            exchangeLimitWarned = false;
            networkLimitWarned = false;
            consoleLimitWarned = false;
            webSocketLimitWarned = false;
            return result;
        }
        private synchronized List<WarningEvent> effectiveWarnings() {
            List<WarningEvent> result = new ArrayList<>(warnings);
            int markerCount = (networkLimitWarned ? 1 : 0) + (consoleLimitWarned ? 1 : 0)
                    + (webSocketLimitWarned ? 1 : 0)
                    + (exchangeLimitWarned ? 1 : 0)
                    + (warningLimitWarned ? 1 : 0);
            while (result.size() + markerCount > WARNING_EVENT_LIMIT) result.removeFirst();
            if (warningLimitWarned) result.add(new WarningEvent("observability",
                    "The oldest browser observability warnings were omitted because the session limit was reached."));
            if (networkLimitWarned) result.add(new WarningEvent("network",
                    "The oldest network events were omitted because the session limit was reached."));
            if (consoleLimitWarned) result.add(new WarningEvent("console",
                    "The oldest console events were omitted because the session limit was reached."));
            if (webSocketLimitWarned) result.add(new WarningEvent("websocket",
                    "The oldest WebSocket events were omitted because the session limit was reached."));
            if (exchangeLimitWarned) result.add(new WarningEvent("network",
                    "A network exchange was omitted because the in-flight session limit was reached."));
            return List.copyOf(result);
        }
        private synchronized void clearConsole() { console.clear(); }

        @Override
        public synchronized void close() {
            closed = true;
            network.clear();
            console.clear();
            webSockets.clear();
            warnings.clear();
            nextNetworkId = 0;
            networkLimitWarned = false;
            consoleLimitWarned = false;
            webSocketLimitWarned = false;
            warningLimitWarned = false;
            exchangeLimitWarned = false;
        }
    }

    private static final class IdentityWeakReference extends WeakReference<NetworkExchange> {
        private final int identityHash;

        private IdentityWeakReference(NetworkExchange exchange) {
            super(exchange);
            identityHash = System.identityHashCode(exchange);
        }

        private IdentityWeakReference(NetworkExchange exchange, ReferenceQueue<NetworkExchange> queue) {
            super(exchange, queue);
            identityHash = System.identityHashCode(exchange);
        }

        @Override
        public int hashCode() {
            return identityHash;
        }

        @Override
        public boolean equals(Object other) {
            if (this == other) {
                return true;
            }
            if (!(other instanceof IdentityWeakReference reference)) {
                return false;
            }
            NetworkExchange exchange = get();
            return exchange != null && exchange == reference.get();
        }
    }

    /** Stable callback owner whose current session is replaced by the runner lifecycle. */
    public static final class ObservationBinding {
        private volatile ObservationSession session;

        private ObservationBinding() {
        }

        private void bind(ObservationSession session) {
            this.session = session;
        }

        private ObservationSession session() {
            return session;
        }
    }

    /**
     * Browser network exchange details recorded into the trace.
     */
    public record NetworkObservation(String method, String url, int status, Map<String, String> requestHeaders,
                                     Map<String, String> responseHeaders, long durationMs, long requestSize,
                                     long responseSize, String failureReason, String bodyPreview) {
    }

    /** Bounded WebSocket lifecycle/frame metadata. */
    public record WebSocketObservation(String requestId, String url, String direction, String type, int opcode,
                                       String text, String sha256, long sizeBytes, String status, String reason) { }

    /** Read-only WebSocket evidence used by focused diagnostics and tests. */
    public record WebSocketSnapshotEntry(String requestId, String url, String direction, String type, int opcode,
                                         String text, String sha256, long sizeBytes, String status, String reason) { }

    private record NetworkEvent(String provider, String method, String url, int status, Map<String, String> requestHeaders,
                                Map<String, String> responseHeaders, long durationMs, long requestSizeBytes,
                                long responseSizeBytes, String failureReason, String bodyPreview, long timestamp) {
    }

    private record WebSocketEvent(String requestId, String url, String direction, String type, int opcode,
                                  String text, String sha256, long sizeBytes, String status, String reason,
                                  long timestamp) { }

    /**
     * Read-only snapshot of one observed network transaction, safe to hand to a caller outside this
     * class (unlike {@link NetworkEvent}, which stays private since it backs the mutable trace list).
     *
     * @param id                 1-based position in the snapshot this entry was read from
     * @param method             HTTP method
     * @param url                request URL
     * @param status             HTTP response status code, or {@code 0} when the exchange failed
     * @param mimeType           response {@code Content-Type}, without parameters (blank when absent)
     * @param durationMs         exchange duration in milliseconds
     * @param requestSizeBytes   request body size in bytes
     * @param responseSizeBytes  response body size in bytes
     * @param failureReason      safe failure reason, blank on success
     * @param timestamp          epoch millis when the exchange finished
     * @param bodyPreview        bounded, redacted response preview, or an explicit safe-boundary omission marker
     * @param requestHeaders     sanitized request headers
     * @param responseHeaders    sanitized response headers
     */
    public record NetworkSnapshotEntry(int id, String method, String url, int status, String mimeType,
                                       long durationMs, long requestSizeBytes, long responseSizeBytes,
                                       String failureReason, long timestamp, String bodyPreview,
                                       Map<String, String> requestHeaders, Map<String, String> responseHeaders) {
    }

    private record MetadataBatch(List<WarningEvent> warnings, List<WebSocketEvent> webSockets) { }

    private record ConsoleEvent(String source, String level, String message, long timestamp) {
    }

    /** Read-only console observation for namespace consumers. */
    public record ConsoleSnapshotEntry(String source, String level, String message, long timestamp) {
    }

    private record WarningEvent(String source, String message) {
    }
}
