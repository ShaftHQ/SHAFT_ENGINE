package com.shaft.driver.internal.DriverFactory;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.SessionNotCreatedException;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicBoolean;

final class RemoteGridPreflight {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final int UNKNOWN = -1;
    private static final ConcurrentMap<String, SessionLimiter> LIMITERS = new ConcurrentHashMap<>();
    private static final Set<String> ATTACHED_SUMMARIES = ConcurrentHashMap.newKeySet();

    private RemoteGridPreflight() {
        throw new IllegalStateException("Utility class");
    }

    static SessionPermit beforeSession(String targetHubUrl, Capabilities capabilities) {
        if (!SHAFT.Properties.platform.remotePreflightEnabled()) {
            return SessionPermit.noop();
        }

        var report = inspect(targetHubUrl, capabilities);
        warnOrFail(report);
        if (SHAFT.Properties.platform.remoteAdaptiveSessionThrottling() && report.matchingSlots() > 0) {
            return acquirePermit(report.limiterKey(), report.matchingSlots());
        }
        return SessionPermit.noop();
    }

    static void beforeSuite(String targetHubUrl, Capabilities capabilities) {
        if (!SHAFT.Properties.platform.remotePreflightEnabled()) {
            return;
        }
        warnOrFail(inspect(targetHubUrl, capabilities));
    }

    static PreflightReport inspect(String targetHubUrl, Capabilities capabilities) {
        var requestedBrowserName = requestedBrowserName(capabilities);
        var endpoint = targetHubUrl == null ? "" : targetHubUrl.trim();
        var redactedEndpoint = DriverFactoryHelper.redactUriCredentials(endpoint);
        StatusSnapshot statusSnapshot = StatusSnapshot.unavailable();
        int queueDepth = UNKNOWN;
        String source = "unavailable";

        try {
            var gridRoot = gridRoot(endpoint);
            statusSnapshot = readStatus(gridRoot.resolve("/status"), requestedBrowserName);
            queueDepth = readQueueDepth(gridRoot.resolve("/graphql"));
            source = statusSnapshot.available() ? "status+graphql" : (queueDepth >= 0 ? "graphql" : "unavailable");
        } catch (Throwable throwable) {
            ReportManagerHelper.logDiscrete(throwable, Level.DEBUG);
        }

        var report = new PreflightReport(
                statusSnapshot.available() || queueDepth >= 0,
                redactedEndpoint,
                requestedBrowserName,
                statusSnapshot.totalSlots(),
                statusSnapshot.availableSlots(),
                statusSnapshot.matchingSlots(),
                statusSnapshot.matchingAvailableSlots(),
                queueDepth,
                source);
        attachSummary(report);
        return report;
    }

    static String classifyRemoteSessionFailure(Throwable throwable) {
        var failureText = collectFailureText(throwable);
        if (failureText.contains("no selenium grid slot matches")
                || failureText.contains("cannot find a matching set of capabilities")
                || failureText.contains("couldn't find a node")
                || failureText.contains("couldnt find a node")
                || failureText.contains("missing in the capabilities")) {
            return "incompatible capabilities";
        }
        if (failureText.contains("has been exhausted")
                || failureText.contains("not allowed on your current plan")
                || failureText.contains("capacity")
                || failureText.contains("quota")
                || failureText.contains("max session")) {
            return "remote capacity exhausted";
        }
        if (failureText.contains("timeout")) {
            return "remote session timeout";
        }
        if (failureText.contains("connection refused")
                || failureText.contains("connectionfailedexception")
                || failureText.contains("unable to connect")
                || failureText.contains("unreachable")) {
            return "remote endpoint unavailable";
        }
        if (failureText.contains("invalid remote server url")
                || failureText.contains("urisyntax")
                || failureText.contains("numberformatexception")) {
            return "invalid remote endpoint";
        }
        return "unclassified remote failure";
    }

    static void resetForTests() {
        LIMITERS.clear();
        ATTACHED_SUMMARIES.clear();
    }

    private static void warnOrFail(PreflightReport report) {
        if (report.matchingSlots() == 0 && !report.requestedBrowserName().isBlank()) {
            var message = "No Selenium Grid slot matches requested browser/platform `" + report.requestedBrowserName()
                    + "/" + Properties.platform.targetPlatform()
                    + "` at `" + report.redactedEndpoint() + "`.";
            ReportManagerHelper.logDiscrete(message, Level.WARN);
            if (SHAFT.Properties.platform.remotePreflightFailFast()) {
                throw new SessionNotCreatedException(message);
            }
        }
        if (report.matchingSlots() > 0 && report.matchingAvailableSlots() == 0) {
            var message = "Selenium Grid has no currently available `" + report.requestedBrowserName()
                    + "` slots at `" + report.redactedEndpoint() + "`.";
            ReportManagerHelper.logDiscrete(message, Level.WARN);
            if (SHAFT.Properties.platform.remotePreflightFailFast()
                    && !SHAFT.Properties.platform.remoteAdaptiveSessionThrottling()) {
                throw new SessionNotCreatedException(message);
            }
        }
    }

    private static SessionPermit acquirePermit(String limiterKey, int capacity) {
        var limiter = LIMITERS.computeIfAbsent(limiterKey,
                ignored -> new SessionLimiter(capacity, new Semaphore(capacity, true)));
        if (limiter.capacity() != capacity) {
            // ponytail: keep the first detected capacity per endpoint/browser; restart the JVM if Grid capacity changes mid-suite.
            ReportManagerHelper.logDiscrete("Selenium Grid preflight limiter already uses capacity "
                    + limiter.capacity() + " for `" + limiterKey + "`.", Level.DEBUG);
        }
        try {
            limiter.semaphore().acquire();
            return new SessionPermit(limiter.semaphore());
        } catch (InterruptedException interruptedException) {
            Thread.currentThread().interrupt();
            throw new RuntimeException("Interrupted while waiting for Selenium Grid capacity.", interruptedException);
        }
    }

    private static StatusSnapshot readStatus(URI statusUri, String requestedBrowserName) throws IOException, InterruptedException {
        var responseBody = get(statusUri);
        var root = JSON.readTree(responseBody);
        var nodes = root.path("value").path("nodes");
        if (!nodes.isArray()) {
            return StatusSnapshot.unavailable();
        }

        var totalSlots = 0;
        var availableSlots = 0;
        var matchingSlots = 0;
        var matchingAvailableSlots = 0;
        for (JsonNode node : nodes) {
            if ("DOWN".equalsIgnoreCase(node.path("status").asText())) {
                continue;
            }
            var slots = node.path("slots");
            if (!slots.isArray()) {
                continue;
            }
            for (JsonNode slot : slots) {
                totalSlots++;
                var available = isSlotAvailable(slot);
                if (available) {
                    availableSlots++;
                }
                if (matchesRequestedSlot(slot.path("stereotype"), requestedBrowserName)) {
                    matchingSlots++;
                    if (available) {
                        matchingAvailableSlots++;
                    }
                }
            }
        }
        return new StatusSnapshot(true, totalSlots, availableSlots, matchingSlots, matchingAvailableSlots);
    }

    private static int readQueueDepth(URI graphqlUri) {
        try {
            var requestBody = "{\"query\":\"{ grid { sessionQueueSize } }\"}";
            var responseBody = post(graphqlUri, requestBody);
            var queueDepth = JSON.readTree(responseBody).at("/data/grid/sessionQueueSize");
            return queueDepth.isMissingNode() ? UNKNOWN : queueDepth.asInt(UNKNOWN);
        } catch (Throwable throwable) {
            ReportManagerHelper.logDiscrete(throwable, Level.DEBUG);
            return UNKNOWN;
        }
    }

    private static String get(URI uri) throws IOException, InterruptedException {
        var request = HttpRequest.newBuilder(uri)
                .timeout(preflightTimeout())
                .GET()
                .build();
        return send(request);
    }

    private static String post(URI uri, String body) throws IOException, InterruptedException {
        var request = HttpRequest.newBuilder(uri)
                .timeout(preflightTimeout())
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(body))
                .build();
        return send(request);
    }

    private static String send(HttpRequest request) throws IOException, InterruptedException {
        var response = HttpClient.newBuilder()
                .connectTimeout(preflightTimeout())
                .build()
                .send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() < 200 || response.statusCode() >= 300) {
            throw new IOException("Selenium Grid preflight endpoint returned HTTP " + response.statusCode());
        }
        return response.body();
    }

    private static Duration preflightTimeout() {
        return Duration.ofSeconds(Math.max(1, SHAFT.Properties.platform.remotePreflightTimeoutSeconds()));
    }

    private static URI gridRoot(String targetHubUrl) throws URISyntaxException {
        if (targetHubUrl == null || targetHubUrl.isBlank()) {
            throw new URISyntaxException("", "Remote server URL must not be null or blank.");
        }
        var normalizedUrl = targetHubUrl.trim();
        if (!normalizedUrl.toLowerCase(Locale.ROOT).startsWith("http")) {
            normalizedUrl = "http://" + normalizedUrl;
        }
        var uri = URI.create(normalizedUrl);
        return new URI(uri.getScheme(), uri.getUserInfo(), uri.getHost(), uri.getPort(), "/", null, null);
    }

    private static boolean matchesRequestedSlot(JsonNode stereotype, String requestedBrowserName) {
        if (requestedBrowserName == null || requestedBrowserName.isBlank()) {
            return true;
        }
        var slotBrowserName = stereotype.path("browserName").asText("");
        if (!requestedBrowserName.equalsIgnoreCase(slotBrowserName)) {
            return false;
        }
        var slotPlatformName = stereotype.path("platformName").asText("");
        var requestedPlatformName = Properties.platform.targetPlatform();
        return slotPlatformName.isBlank()
                || requestedPlatformName == null
                || requestedPlatformName.isBlank()
                || slotPlatformName.equalsIgnoreCase(requestedPlatformName);
    }

    private static boolean isSlotAvailable(JsonNode slot) {
        var session = slot.get("session");
        return session == null || session.isNull();
    }

    private static String requestedBrowserName(Capabilities capabilities) {
        var browserName = capabilities == null ? "" : capabilities.getBrowserName();
        if (browserName == null || browserName.isBlank()) {
            browserName = SHAFT.Properties.web.targetBrowserName();
        }
        return browserName == null ? "" : browserName.trim().toLowerCase(Locale.ROOT);
    }

    private static void attachSummary(PreflightReport report) {
        if (!ATTACHED_SUMMARIES.add(report.limiterKey())) {
            return;
        }
        var summary = "endpoint=" + report.redactedEndpoint() + "\n"
                + "requestedBrowser=" + report.requestedBrowserName() + "\n"
                + "gridMetadataAvailable=" + report.gridMetadataAvailable() + "\n"
                + "source=" + report.source() + "\n"
                + "totalSlots=" + report.totalSlots() + "\n"
                + "availableSlots=" + report.availableSlots() + "\n"
                + "matchingSlots=" + report.matchingSlots() + "\n"
                + "matchingAvailableSlots=" + report.matchingAvailableSlots() + "\n"
                + "queueDepth=" + report.queueDepth() + "\n";
        ReportManagerHelper.attach("Text", "Selenium Grid Preflight", summary);
        ReportManagerHelper.logDiscrete("Selenium Grid preflight summary: "
                + summary.replace("\n", ", "), Level.INFO);
    }

    private static String collectFailureText(Throwable throwable) {
        if (throwable == null) {
            return "";
        }
        var text = new StringBuilder();
        var current = throwable;
        while (current != null) {
            text.append(current.getClass().getName()).append(' ')
                    .append(current.getMessage()).append(' ');
            for (var suppressed : current.getSuppressed()) {
                text.append(suppressed.getClass().getName()).append(' ')
                        .append(suppressed.getMessage()).append(' ');
            }
            current = current.getCause();
        }
        return text.toString().toLowerCase(Locale.ROOT);
    }

    record PreflightReport(boolean gridMetadataAvailable, String redactedEndpoint, String requestedBrowserName,
                           int totalSlots, int availableSlots, int matchingSlots, int matchingAvailableSlots,
                           int queueDepth, String source) {
        private String limiterKey() {
            return redactedEndpoint + "|" + requestedBrowserName;
        }
    }

    private record StatusSnapshot(boolean available, int totalSlots, int availableSlots, int matchingSlots,
                                  int matchingAvailableSlots) {
        private static StatusSnapshot unavailable() {
            return new StatusSnapshot(false, UNKNOWN, UNKNOWN, UNKNOWN, UNKNOWN);
        }
    }

    private record SessionLimiter(int capacity, Semaphore semaphore) {
    }

    static final class SessionPermit implements AutoCloseable {
        private static final SessionPermit NOOP = new SessionPermit(null);
        private final Semaphore semaphore;
        private final AtomicBoolean released = new AtomicBoolean(false);

        private SessionPermit(Semaphore semaphore) {
            this.semaphore = semaphore;
        }

        static SessionPermit noop() {
            return NOOP;
        }

        @Override
        public void close() {
            if (semaphore != null && released.compareAndSet(false, true)) {
                semaphore.release();
            }
        }
    }
}
