package com.shaft.gui.browser.internal;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.bidi.module.Network;
import org.openqa.selenium.bidi.network.BeforeRequestSent;
import org.openqa.selenium.bidi.network.FetchError;
import org.openqa.selenium.bidi.network.Header;
import org.openqa.selenium.bidi.network.ResponseDetails;

import java.time.Duration;
import java.util.List;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Locale;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.LongSupplier;
import java.util.function.BooleanSupplier;

/**
 * Advisory, best-effort BiDi network-activity signal that {@link JavaScriptWaitManager} folds
 * into its existing JS-derived quiet-window engine (issue #3749, Increment B).
 *
 * <p>Wraps {@link Network} (Selenium's {@code org.openqa.selenium.bidi.module.Network}) to observe
 * {@code network.beforeRequestSent}, {@code network.responseCompleted}, and
 * {@code network.fetchError} events, which today's JS-only marker cannot see at all: worker/
 * {@code sendBeacon}-adjacent traffic issued before SHAFT's monkey-patch runs, requests on
 * CSP/frozen-prototype pages where the monkey-patch fails to install, etc.
 *
 * <p><b>Everything here is advisory, never a hard gate.</b> {@link #inFlightCount()} can only
 * extend {@code JavaScriptWaitManager}'s quiet window the same way a JS marker change does; it is
 * never compared against zero as a pass/fail condition. This is deliberate: BiDi, unlike the JS
 * layer, genuinely observes SSE (EventSource) and WebSocket-upgrade requests, which by design stay
 * "in flight" for the entire page lifetime. Treating {@code inFlightCount() > 0} as a hard
 * requirement would regress today's tolerance of long-lived connections (the JS layer's
 * {@code activeRequests} simply can't see them) into a wait that never completes.
 *
 * <p><b>Lifecycle contract.</b> One instance is created lazily per {@link WebDriver} session
 * (see {@link #forDriver(WebDriver)}) the first time it is needed, cached for the life of that
 * session, and torn down via {@link #closeAndRemove(WebDriver)} during driver teardown. Instances
 * are only ever constructed when {@code SHAFT.Properties.platform.enableBiDi()} is {@code true}
 * <i>and</i> the underlying {@code new Network(driver)} call succeeds; {@code new Network(driver)}
 * throws for any session that does not support/enable the BiDi protocol
 * (see {@link #attach(WebDriver)}), and construction is <b>never retried</b> for a given driver
 * instance once it has failed -- the failure is logged once (discretely) and the driver falls back
 * to the JS-only signal for the rest of its session.
 *
 * <p><b>Thread-safety.</b> BiDi event callbacks fire on Selenium's websocket reader thread while
 * {@link #activityMarker()}/{@link #inFlightCount()} are read from the test thread inside
 * {@code JavaScriptWaitManager}'s poll loop. All mutable state is therefore
 * {@link ConcurrentHashMap}/{@code Atomic*}-backed.
 *
 * <p><b>Close discipline.</b> {@link #close()} only removes the listeners this instance
 * registered via {@link Network#close()}; it never issues a {@code Network.disable}-style command
 * and never touches DevTools/CDP. Per the review-corrected ownership contract for issue #3749, the
 * DevTools/BiDi session itself is shared and unowned by design -- passive listening never conflicts
 * with another component's {@code NetworkInterceptor} Fetch filter, so the only safe lifecycle rule
 * is "never disable, only detach your own listeners" (see {@code BrowserNetworkInterceptor} for the
 * analogous CDP-side contract). Note: Selenium 4.46's {@code Network.close()} clears the shared
 * BiDi connection's listeners for the {@code beforeRequestSent}/{@code responseStarted}/
 * {@code responseCompleted}/{@code authRequired} event types outright (not scoped to the calling
 * instance) and does not clear {@code fetchError} listeners at all; harmless here because this
 * class is the sole registrant of BiDi {@code network.*} listeners in SHAFT today, and because
 * {@code close()} is only called from driver teardown, where the whole BiDi connection is about to
 * go away with the driver regardless.
 */
public class BidiNetworkActivitySource implements AutoCloseable {

    /**
     * Requests still open longer than this are dropped from {@link #inFlightCount()} even though no
     * completion/error event was ever observed for them.
     *
     * <p>SSE (EventSource), long-poll, and WebSocket-upgrade requests legitimately stay "in flight"
     * for the entire page lifetime -- without this age-out, a single such request would pin
     * {@code inFlightCount() > 0} forever. Because {@link #inFlightCount()} is advisory only (see
     * class Javadoc), the practical effect of a stale entry before it ages out is just an
     * unnecessary quiet-window extension in {@code JavaScriptWaitManager}, bounded to at most this
     * duration per request -- never a wait that hangs past the existing 30s ceiling.
     */
    static final Duration IN_FLIGHT_AGE_OUT_WINDOW = Duration.ofSeconds(10);
    private static final int TRACE_REQUEST_LIMIT = 1000;
    private static final int TRACE_FIELD_CHARACTER_LIMIT = 2048;
    private static final int TRACE_HEADER_LIMIT = 64;
    private static final int TRACE_HEADER_CHARACTER_LIMIT = 8192;

    private static final ConcurrentHashMap<WebDriver, BidiNetworkActivitySource> CACHE = new ConcurrentHashMap<>();

    private final ConcurrentHashMap<String, Long> inFlightStartNanos = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, TraceRequest> traceRequests = new ConcurrentHashMap<>();
    private final AtomicLong activitySequence = new AtomicLong();
    private final AtomicBoolean healthy = new AtomicBoolean(false);
    private final LongSupplier nanoTimeSource;
    private final BooleanSupplier detailedObservationActive;
    private final BrowserObservabilityRecorder.ObservationBinding observationBinding;
    private final AtomicBoolean closed = new AtomicBoolean();
    private BrowserObservabilityRecorder.ObservationSession traceRequestLimitWarningOwner;
    private BrowserObservabilityRecorder.ObservationSession traceMetadataLimitWarningOwner;
    private volatile Network network;

    /**
     * Package-private test seam: builds a source with pure in-flight/marker state wired to an
     * injected clock and skips BiDi wiring entirely, so aging/advisory logic can be unit tested
     * without a real (or fake) BiDi connection. Production code never calls this directly; use
     * {@link #forDriver(WebDriver)}.
     *
     * @param nanoTimeSource clock used for age-out bookkeeping, in nanoseconds
     */
    BidiNetworkActivitySource(LongSupplier nanoTimeSource) {
        this.nanoTimeSource = nanoTimeSource;
        this.detailedObservationActive = () -> false;
        this.observationBinding = BrowserObservabilityRecorder.captureBinding();
    }

    BidiNetworkActivitySource(WebDriver driver, LongSupplier nanoTimeSource) {
        this.nanoTimeSource = nanoTimeSource;
        this.detailedObservationActive = () -> BrowserNetworkInterceptor.observationCountIfPresent(driver).isPresent();
        this.observationBinding = BrowserObservabilityRecorder.captureBinding();
        attach(driver);
    }

    /**
     * Returns the cached {@link BidiNetworkActivitySource} for {@code driver}, constructing one on
     * first use. Returns {@code null} when {@code driver} is {@code null} or
     * {@code SHAFT.Properties.platform.enableBiDi()} is {@code false} -- callers must treat a
     * {@code null} return exactly like an unhealthy source (fall back to the JS-only signal).
     *
     * <p>Construction is attempted at most once per driver instance: a failed attempt is cached too
     * (as an unhealthy source) so repeated calls for the same session never retry {@code new
     * Network(driver)}.
     *
     * @param driver the active WebDriver session
     * @return the cached source for {@code driver}, or {@code null} when BiDi is disabled
     */
    static BidiNetworkActivitySource forDriver(WebDriver driver) {
        if (driver == null || !SHAFT.Properties.platform.enableBiDi()) {
            return null;
        }
        return CACHE.computeIfAbsent(driver, d -> new BidiNetworkActivitySource(d, System::nanoTime));
    }

    /**
     * Closes and removes the cached {@link BidiNetworkActivitySource} for {@code driver}, if one
     * exists. Wired into driver teardown (see {@code DriverFactoryHelper.closeDriver}) so a source
     * is never leaked past its driver's session; safe to call even when no source was ever created
     * for {@code driver} (no-op).
     *
     * @param driver the WebDriver session being torn down
     */
    public static void closeAndRemove(WebDriver driver) {
        if (driver == null) {
            return;
        }
        BidiNetworkActivitySource source = CACHE.remove(driver);
        if (source != null) {
            source.close();
        }
    }

    private void attach(WebDriver driver) {
        try {
            Network candidate = new Network(driver);
            candidate.onBeforeRequestSent(this::handleBeforeRequestSent);
            candidate.onResponseCompleted(this::handleResponseCompleted);
            candidate.onFetchError(this::handleFetchError);
            this.network = candidate;
            healthy.set(true);
        } catch (RuntimeException e) {
            // new Network(driver) throws for any session that doesn't support/enable BiDi:
            // IllegalArgumentException when the driver doesn't implement HasBiDi at all, or a
            // BiDiException (via HasBiDi.getBiDi()) when it does but no BiDi connection could be
            // established. Either way: mark unhealthy, log once, never retry for this driver.
            ReportManagerHelper.logDiscrete("BiDi network-activity source unavailable for this driver "
                    + "session; browser-readiness waits will use the JS-only network signal. "
                    + e.getMessage(), Level.DEBUG);
        }
    }

    /**
     * @return {@code true} when this source successfully attached to the driver's BiDi Network
     * module and is actively receiving events
     */
    boolean healthy() {
        return healthy.get();
    }

    /**
     * @return a monotonically-changing marker; any change between two reads means a
     * beforeRequestSent/responseCompleted/fetchError event was observed in between
     */
    String activityMarker() {
        return Long.toString(activitySequence.get());
    }

    /**
     * Current advisory count of requests believed to still be in flight, after dropping entries
     * older than {@link #IN_FLIGHT_AGE_OUT_WINDOW}. Callers must never treat this as a hard
     * zero-required gate -- see class Javadoc.
     *
     * @return the advisory in-flight count; never negative
     */
    synchronized int inFlightCount() {
        if (closed.get()) {
            return 0;
        }
        ageOutStaleEntries();
        return inFlightStartNanos.size();
    }

    synchronized int retainedTraceRequestCount() {
        return traceRequests.size();
    }

    private void ageOutStaleEntries() {
        long now = nanoTimeSource.getAsLong();
        long thresholdNanos = IN_FLIGHT_AGE_OUT_WINDOW.toNanos();
        inFlightStartNanos.entrySet().removeIf(entry -> {
            boolean stale = now >= entry.getValue() && now - entry.getValue() >= thresholdNanos;
            if (stale) {
                publish(traceRequests.remove(entry.getKey()), null,
                        "BiDi request completion was unavailable after the bounded age-out window.");
            }
            return stale;
        });
    }

    synchronized void handleBeforeRequestSent(BeforeRequestSent event) {
        if (closed.get()) {
            return;
        }
        org.openqa.selenium.bidi.network.RequestData request = event == null ? null : event.getRequest();
        activitySequence.incrementAndGet();
        boolean longLived = isLongLivedUpgrade(request);
        recordRequestStart(requestIdOf(request), longLived);
        recordTraceRequest(request, longLived);
    }

    synchronized void handleResponseCompleted(ResponseDetails event) {
        if (closed.get()) {
            return;
        }
        activitySequence.incrementAndGet();
        org.openqa.selenium.bidi.network.RequestData request = event == null ? null : event.getRequest();
        String requestId = requestIdOf(request);
        recordRequestEnd(requestId);
        completeTraceRequest(requestId, event == null ? null : event.getResponseData(), "");
    }

    synchronized void handleFetchError(FetchError event) {
        if (closed.get()) {
            return;
        }
        activitySequence.incrementAndGet();
        org.openqa.selenium.bidi.network.RequestData request = event == null ? null : event.getRequest();
        String requestId = requestIdOf(request);
        recordRequestEnd(requestId);
        completeTraceRequest(requestId, null, "BiDi request failed.");
    }

    private synchronized void recordTraceRequest(org.openqa.selenium.bidi.network.RequestData request,
                                                 boolean longLived) {
        if (request == null || detailedObservationActive.getAsBoolean()) {
            return;
        }
        BrowserObservabilityRecorder.ObservationSession owner =
                BrowserObservabilityRecorder.resolveSession(observationBinding);
        BoundedHeaders requestHeaders = headers(request.getHeaders());
        RetainedText method = retainedText(request.getMethod());
        RetainedText url = retainedText(request.getUrl());
        if (metadataWasBounded(request, method, url, requestHeaders)) {
            warnMetadataLimit(owner);
        }
        TraceRequest traceRequest = new TraceRequest(owner, method.value(), url.value(), requestHeaders.values(),
                method.context(), url.context(), requestHeaders.contexts(),
                Math.max(0L, request.getBodySize() == null ? 0L : request.getBodySize()), nanoTimeSource.getAsLong());
        if (longLived) {
            publish(traceRequest, null, "BiDi long-lived request observed; completion metadata is unavailable.");
            return;
        }
        if (traceRequests.size() >= TRACE_REQUEST_LIMIT) {
            if (!java.util.Objects.equals(traceRequestLimitWarningOwner, owner)) {
                traceRequestLimitWarningOwner = owner;
                BrowserObservabilityRecorder.recordWarning(owner, "network",
                        "A BiDi network request was omitted because the in-flight trace limit was reached.");
            }
            return;
        }
        String requestId = request.getRequestId();
        if (requestId != null) {
            traceRequests.put(requestId, traceRequest);
        }
    }

    private synchronized void completeTraceRequest(String requestId,
                                                   org.openqa.selenium.bidi.network.ResponseData response,
                                                   String failureReason) {
        TraceRequest pending = requestId == null ? null : traceRequests.remove(requestId);
        publish(pending, response, failureReason);
    }

    private void publish(TraceRequest request, org.openqa.selenium.bidi.network.ResponseData response,
                         String failureReason) {
        if (request == null) {
            return;
        }
        long now = nanoTimeSource.getAsLong();
        long elapsed = now >= request.startNanos() ? now - request.startNanos() : 0L;
        long responseSize = response == null ? 0L : Math.max(0L,
                response.getBodySize() == null ? response.getBytesReceived() : response.getBodySize());
        BoundedHeaders responseHeaders = response == null ? new BoundedHeaders(Map.of(), Map.of(), false)
                : headers(response.getHeaders());
        if (responseHeaders.truncated()) {
            warnMetadataLimit(request.owner());
        }
        BrowserObservabilityRecorder.recordNetwork(request.owner(),
                new BrowserObservabilityRecorder.NetworkObservation(
                        completeText(request.method(), request.methodContext()),
                        completeText(request.url(), request.urlContext()), response == null ? 0 : response.getStatus(),
                        completeHeaders(request.requestHeaders(), request.requestHeaderContexts()),
                        completeHeaders(responseHeaders.values(), responseHeaders.contexts()),
                        java.util.concurrent.TimeUnit.NANOSECONDS.toMillis(elapsed), request.requestSize(),
                        responseSize, failureReason, ""), "bidi");
    }

    static BoundedHeaders headers(List<Header> source) {
        if (source == null || source.isEmpty()) {
            return new BoundedHeaders(Map.of(), Map.of(), false);
        }
        Map<String, String> values = new LinkedHashMap<>();
        Map<String, HeaderContext> contexts = new LinkedHashMap<>();
        int retainedCharacters = 0;
        boolean truncated = false;
        for (Header header : source) {
            if (headerLimitReached(values.size(), retainedCharacters)) {
                truncated = true;
                break;
            }
            RetainedHeader retained = retainedHeader(header);
            if (retained == null) {
                continue;
            }
            truncated |= retained.bounded();
            if (retained.characters() > TRACE_HEADER_CHARACTER_LIMIT - retainedCharacters) {
                truncated = true;
                break;
            }
            values.put(retained.name().value(), retained.value().value());
            if (retained.hasContext()) {
                contexts.put(retained.name().value(),
                        new HeaderContext(retained.name().context(), retained.value().context()));
            }
            retainedCharacters += retained.characters();
        }
        return new BoundedHeaders(Map.copyOf(values), Map.copyOf(contexts), truncated || values.size() < source.size());
    }

    private static boolean headerLimitReached(int retainedHeaders, int retainedCharacters) {
        return retainedHeaders >= TRACE_HEADER_LIMIT || retainedCharacters >= TRACE_HEADER_CHARACTER_LIMIT;
    }

    private static RetainedHeader retainedHeader(Header header) {
        if (header == null || header.getName() == null || header.getValue() == null) {
            return null;
        }
        String sourceValue = safe(header.getValue().getValue());
        boolean sensitive = isSensitiveHeader(header.getName());
        RetainedText name = retainedText(header.getName());
        RetainedText value = sensitive ? new RetainedText("********", "") : retainedText(sourceValue);
        boolean bounded = !name.value().equals(header.getName()) || !name.context().isEmpty()
                || (!sensitive && (!value.value().equals(sourceValue) || !value.context().isEmpty()));
        return new RetainedHeader(name, value, bounded);
    }

    private static boolean metadataWasBounded(org.openqa.selenium.bidi.network.RequestData request,
                                              RetainedText method, RetainedText url, BoundedHeaders headers) {
        return !method.value().equals(safe(request.getMethod())) || !method.context().isEmpty()
                || !url.value().equals(safe(request.getUrl())) || !url.context().isEmpty() || headers.truncated();
    }

    private static RetainedText retainedText(String source) {
        String retained = BrowserObservabilityRecorder.retainedNetworkText(source);
        int boundary = Math.min(TRACE_FIELD_CHARACTER_LIMIT, retained.length());
        return new RetainedText(retained.substring(0, boundary), retained.substring(boundary));
    }

    private static String completeText(String value, String context) {
        return safe(value) + safe(context);
    }

    private static Map<String, String> completeHeaders(Map<String, String> values,
                                                        Map<String, HeaderContext> contexts) {
        if (values.isEmpty()) return values;
        Map<String, String> completed = new LinkedHashMap<>();
        values.forEach((name, value) -> {
            HeaderContext context = contexts.getOrDefault(name, new HeaderContext("", ""));
            completed.put(name + context.name(), value + context.value());
        });
        return Map.copyOf(completed);
    }

    private static boolean isSensitiveHeader(String key) {
        return "********".equals(BrowserObservabilityRecorder.retainedNetworkHeaderValue(key, "probe"));
    }

    private void warnMetadataLimit(BrowserObservabilityRecorder.ObservationSession owner) {
        if (!java.util.Objects.equals(traceMetadataLimitWarningOwner, owner)) {
            traceMetadataLimitWarningOwner = owner;
            BrowserObservabilityRecorder.recordWarning(owner, "network",
                    "BiDi network metadata was truncated to the bounded trace limit.");
        }
    }

    private static String safe(String value) {
        return value == null ? "" : value;
    }

    /**
     * Pure state-machine step for a request starting, exposed package-private so unit tests can
     * exercise in-flight/aging semantics directly without a real or fake BiDi connection.
     *
     * @param requestId          the BiDi request id, or {@code null} to no-op the in-flight tracking
     * @param excludeFromInFlight {@code true} for requests that are expected to legitimately never
     *                            complete (SSE/WebSocket upgrade) -- see {@link #isLongLivedUpgrade}
     */
    synchronized void recordRequestStart(String requestId, boolean excludeFromInFlight) {
        if (!closed.get() && requestId != null && !excludeFromInFlight
                && inFlightStartNanos.size() < TRACE_REQUEST_LIMIT) {
            inFlightStartNanos.put(requestId, nanoTimeSource.getAsLong());
        }
    }

    /**
     * Pure state-machine step for a request completing or erroring, exposed package-private for the
     * same reason as {@link #recordRequestStart}.
     *
     * @param requestId the BiDi request id, or {@code null} to no-op
     */
    synchronized void recordRequestEnd(String requestId) {
        if (requestId != null) {
            inFlightStartNanos.remove(requestId);
        }
    }

    private static String requestIdOf(org.openqa.selenium.bidi.network.RequestData request) {
        return request == null ? null : request.getRequestId();
    }

    /**
     * Cheap, request-time exclusion for connection types that are expected to legitimately stay
     * open for the whole page lifetime: an EventSource ({@code Accept: text/event-stream}) or a
     * WebSocket handshake ({@code Upgrade: websocket}) request never fires
     * {@code network.responseCompleted} while the connection is alive, so without this exclusion
     * every SSE/WS page would pin {@link #inFlightCount()} for the full
     * {@link #IN_FLIGHT_AGE_OUT_WINDOW} on every such connection instead of never counting it at
     * all. Detected from the request headers available at {@code beforeRequestSent} time -- no
     * response wait needed. Mirrors the JS layer's EventSource/WebSocket exclusion (see
     * {@code JavaScriptHelper.BROWSER_READINESS_STATE}) for requests the JS monkey-patch can't see.
     *
     * @param request the beforeRequestSent event's request data, or {@code null}
     * @return {@code true} when the request headers indicate an SSE or WebSocket-upgrade request
     */
    static boolean isLongLivedUpgrade(org.openqa.selenium.bidi.network.RequestData request) {
        if (request == null) {
            return false;
        }
        List<Header> headers = request.getHeaders();
        if (headers == null) {
            return false;
        }
        for (Header header : headers) {
            if (header == null || header.getValue() == null || header.getValue().getValue() == null) {
                continue;
            }
            String name = header.getName();
            String value = header.getValue().getValue().toLowerCase(Locale.ROOT);
            if ("upgrade".equalsIgnoreCase(name) && value.contains("websocket")) {
                return true;
            }
            if ("accept".equalsIgnoreCase(name) && value.contains("text/event-stream")) {
                return true;
            }
        }
        return false;
    }

    /**
     * Detaches this source's listeners from the driver's BiDi connection via {@link Network#close()}
     * and marks the source unhealthy. Best-effort: exceptions during close (for example a session
     * that already tore down) are swallowed, matching the driver-teardown close idioms elsewhere in
     * SHAFT (see {@code BrowserNetworkInterceptor#closeActiveInterceptor}).
     */
    @Override
    public void close() {
        Network toClose;
        synchronized (this) {
            if (!closed.compareAndSet(false, true)) {
                return;
            }
            toClose = this.network;
            this.network = null;
            healthy.set(false);
            traceRequests.clear();
            inFlightStartNanos.clear();
        }
        if (toClose != null) {
            try {
                toClose.close();
            } catch (Exception ignored) {
                // Closing an already-torn-down BiDi connection during driver teardown is harmless.
            }
        }
    }

    private record TraceRequest(BrowserObservabilityRecorder.ObservationSession owner, String method, String url,
                                Map<String, String> requestHeaders, String methodContext, String urlContext,
                                Map<String, HeaderContext> requestHeaderContexts, long requestSize, long startNanos) {
    }

    private record RetainedText(String value, String context) {
    }

    private record RetainedHeader(RetainedText name, RetainedText value, boolean bounded) {
        private int characters() {
            return name.value().length() + value.value().length();
        }

        private boolean hasContext() {
            return !name.context().isEmpty() || !value.context().isEmpty();
        }
    }

    record HeaderContext(String name, String value) {
    }

    record BoundedHeaders(Map<String, String> values, Map<String, HeaderContext> contexts, boolean truncated) {
    }
}
