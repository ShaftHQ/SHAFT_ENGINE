package com.shaft.gui.browser.internal;

import com.shaft.driver.SHAFT;
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
import java.util.Locale;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.LongSupplier;

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

    private static final ConcurrentHashMap<WebDriver, BidiNetworkActivitySource> CACHE = new ConcurrentHashMap<>();

    private final ConcurrentHashMap<String, Long> inFlightStartNanos = new ConcurrentHashMap<>();
    private final AtomicLong activitySequence = new AtomicLong();
    private final AtomicBoolean healthy = new AtomicBoolean(false);
    private final LongSupplier nanoTimeSource;
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
    }

    private BidiNetworkActivitySource(WebDriver driver, LongSupplier nanoTimeSource) {
        this.nanoTimeSource = nanoTimeSource;
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
    int inFlightCount() {
        ageOutStaleEntries();
        return inFlightStartNanos.size();
    }

    private void ageOutStaleEntries() {
        long now = nanoTimeSource.getAsLong();
        long thresholdNanos = IN_FLIGHT_AGE_OUT_WINDOW.toNanos();
        inFlightStartNanos.entrySet().removeIf(entry -> (now - entry.getValue()) >= thresholdNanos);
    }

    void handleBeforeRequestSent(BeforeRequestSent event) {
        org.openqa.selenium.bidi.network.RequestData request = event == null ? null : event.getRequest();
        activitySequence.incrementAndGet();
        recordRequestStart(requestIdOf(request), isLongLivedUpgrade(request));
    }

    void handleResponseCompleted(ResponseDetails event) {
        activitySequence.incrementAndGet();
        recordRequestEnd(requestIdOf(event == null ? null : event.getRequest()));
    }

    void handleFetchError(FetchError event) {
        activitySequence.incrementAndGet();
        recordRequestEnd(requestIdOf(event == null ? null : event.getRequest()));
    }

    /**
     * Pure state-machine step for a request starting, exposed package-private so unit tests can
     * exercise in-flight/aging semantics directly without a real or fake BiDi connection.
     *
     * @param requestId          the BiDi request id, or {@code null} to no-op the in-flight tracking
     * @param excludeFromInFlight {@code true} for requests that are expected to legitimately never
     *                            complete (SSE/WebSocket upgrade) -- see {@link #isLongLivedUpgrade}
     */
    void recordRequestStart(String requestId, boolean excludeFromInFlight) {
        if (requestId != null && !excludeFromInFlight) {
            inFlightStartNanos.put(requestId, nanoTimeSource.getAsLong());
        }
    }

    /**
     * Pure state-machine step for a request completing or erroring, exposed package-private for the
     * same reason as {@link #recordRequestStart}.
     *
     * @param requestId the BiDi request id, or {@code null} to no-op
     */
    void recordRequestEnd(String requestId) {
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
        Network toClose = this.network;
        this.network = null;
        healthy.set(false);
        if (toClose != null) {
            try {
                toClose.close();
            } catch (Exception ignored) {
                // Closing an already-torn-down BiDi connection during driver teardown is harmless.
            }
        }
    }
}
